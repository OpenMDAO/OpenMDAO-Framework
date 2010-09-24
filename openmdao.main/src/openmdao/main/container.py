"""
The Container class
"""

#public symbols
__all__ = ["Container", "set_as_top", "get_default_name", "dump"]

import datetime
import copy
import traceback
import re
import pprint

import weakref
# the following is a monkey-patch to correct a problem with
# copying/deepcopying weakrefs There is an issue in the python issue tracker
# regarding this, but it isn't fixed yet.

# pylint: disable-msg=W0212
copy._copy_dispatch[weakref.ref] = copy._copy_immutable  
copy._deepcopy_dispatch[weakref.ref] = copy._deepcopy_atomic
copy._deepcopy_dispatch[weakref.KeyedRef] = copy._deepcopy_atomic
# pylint: enable-msg=W0212

# pylint apparently doesn't understand namespace packages...
# pylint: disable-msg=E0611,F0401

from enthought.traits.api import HasTraits, Missing, TraitError, Undefined, \
                                 push_exception_handler, Python, TraitType, \
                                 Property, Interface, Instance
from enthought.traits.trait_handlers import NoDefaultSpecified
from enthought.traits.has_traits import FunctionType
from enthought.traits.trait_base import not_none, not_event
from enthought.traits.trait_types import validate_implements

from openmdao.main.filevar import FileRef
from openmdao.util.log import Logger, logger, LOG_DEBUG
from openmdao.util import eggloader, eggsaver, eggobserver
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.main.interfaces import ICaseIterator, IResourceAllocator

_copydict = {
    None: lambda obj: obj,
    'deep': copy.deepcopy,
    'shallow': copy.copy
    }

def set_as_top(cont):
    """Specifies that the given Container is the top of a 
    Container hierarchy.
    """
    cont.tree_rooted()
    return cont

def _deep_setattr(obj, path, value):
    """A multi-level setattr, setting the value of an
    attribute specified by a dotted path. For example,
    deep_settattr(obj, 'a.b.c', value).
    """
    tup = path.split('.')
    for name in tup[:-1]:
        obj = getattr(obj, name)
    setattr(obj, tup[-1], value)


# TODO: implement get_closest_proxy, along with a way to detect
# when a Container is proxy so we can differentiate between
# failure to find an attribute vs. failure to find a local
# version of the attribute
#def get_closest_proxy(start_scope, pathname):
    #"""Resolve down to the closest in-process parent object
    #of the object indicated by pathname.
    #Returns a tuple containing (proxy_or_parent, rest_of_pathname)
    #"""
    

# this causes any exceptions occurring in trait handlers to be re-raised.
# Without this, the default behavior is for the exception to be logged and not
# re-raised.
push_exception_handler(handler = lambda o, t, ov, nv: None,
                       reraise_exceptions = True,
                       main = True,
                       locked = True )

# regex to check for valid names.  Added '.' as allowed because
# npsscomponent uses it...
_namecheck_rgx = re.compile(
    '([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')
    
class _DumbTmp(object):
    pass

class _PathProperty(TraitType):
    """A trait that allows attributes in child objects to be referenced
    using an alias in a parent scope.  We don't use a delegate because
    we can't be sure that the attribute we want is found in a HasTraits
    object.
    """
    def __init__ ( self, default_value = NoDefaultSpecified, **metadata ):
        ref_name = metadata.get('ref_name')
        if not ref_name:
            raise TraitError("_PathProperty constructor requires a"
                             " 'ref_name' argument.")
        self._names = ref_name.split('.')
        if len(self._names) < 2:
            raise TraitError("_PathProperty ref_name must have at least "
                             "two entries in the path."
                             " The given ref_name was '%s'" % ref_name)        
        #make weakref to a transient object to force a re-resolve later
        #without checking for self._ref being equal to None
        self._ref = weakref.ref(_DumbTmp()) 
        super(_PathProperty, self).__init__(default_value, **metadata)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_ref'] = self._ref()
        return state
    
    def __setstate__(self, state):
        self.__dict__.update(state)
        if self._ref is None:
            self._ref = weakref.ref(_DumbTmp())
        else:
            self._ref = weakref.ref(self._ref)
    
    def _resolve(self, obj):
        """Try to resolve down to the last containing object in the path and
        store a weakref to that object.
        """
        # TODO - need to handle only being able to resolve down to 
        #        the nearest proxy here
        try:
            for name in self._names[:-1]:
                obj = getattr(obj, name)
        except AttributeError:
            raise TraitError("_PathProperty cannot resolve path '%s'" % 
                             '.'.join(self._names))
        self._last_name = self._names[len(self._names)-1]
        self._ref = weakref.ref(obj)
        return obj
            
    def get(self, obj, name):
        """Return the value of the referenced attribute."""
        return getattr(self._ref() or self._resolve(obj), self._last_name)

    def set(self, obj, name, value):
        """Set the value of the referenced attribute."""
        if self.iotype == 'out':
            raise TraitError('%s is an output trait and cannot be set' % name)
        
        if self.trait:
            value = self.trait.validate(obj, name, value)
        
        setattr(self._ref() or self._resolve(obj), self._last_name, value)
    
        
class Container(HasTraits):
    """ Base class for all objects having Traits that are visible 
    to the framework"""
   
    #parent = WeakRef(Container, allow_none=True, adapt='no', transient=True)
    parent = Python()
    
    # this will automagically call _get_log_level and _set_log_level when needed
    log_level = Property(desc='Logging message level')
    
    __ = Python()
    
    def __init__(self, doc=None):
        super(Container, self).__init__()
        self._sources = {}  # for checking that destination traits cannot be 
                          # set by other objects
        # for keeping track of dynamically added traits for serialization
        self._added_traits = {}  
                          
        self.parent = None
        self._name = None
        
        self._call_tree_rooted = True
        
        if doc is not None:
            self.__doc__ = doc

        # TODO: see about turning this back into a regular logger and just
        # handling its unpickleability in __getstate__/__setstate__ in
        # order to avoid the extra layer of function calls when logging
        self._logger = Logger('')

        # Create per-instance initial FileRefs for File traits. There ought
        # to be a better way to not share default initial values, but
        # FileRef.get_default_value/make_default won't pickle.
        for name, obj in self.items():
            if isinstance(obj, FileRef):
                setattr(self, name, obj.copy(owner=self))

        # Call _io_trait_changed if any trait having 'iotype' metadata is
        # changed. We originally used the decorator @on_trait_change for this,
        # but it failed to be activated properly when our objects were
        # unpickled.
        self.on_trait_change(self._io_trait_changed, '+iotype')
        
        # keep track of modifications to our parent
        self.on_trait_change(self._parent_modified, 'parent')
                
    def _parent_modified(self, obj, name, value):
        """This is called when the parent attribute is changed."""
        self._logger.rename(self.get_pathname().replace('.', ','))
        self._branch_moved()
        
    def _branch_moved(self):
        self._call_tree_rooted = True
        [x._branch_moved() for x in self.values() if isinstance(x, Container)]
 
    def _get_name(self):
        if self._name is None:
            if self.parent:
                self._name = findname(self.parent, self)
        return self._name

    def _set_name(self, name):
        match = _namecheck_rgx.search(name)
        if match is None or match.group() != name:
            raise NameError("name '%s' contains illegal characters" % name)
        self._name = name
        self._logger.rename(self._name)
        
    name = property(_get_name, _set_name, doc="Name of the Container")
    
    def get_pathname(self, rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        *rel_to_scope*. If *rel_to_scope* is *None*, return the full pathname.
        """
        path = []
        obj = self
        name = obj.name
        while obj is not rel_to_scope and name:
            path.append(name)
            obj = obj.parent
            if obj is None:
                break
            name = obj.name
        return '.'.join(path[::-1])
            
    #
    #  HasTraits overrides
    #
    
    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(Container, self).__getstate__()
        dct = {}
        for name, trait in state['_added_traits'].items():
            if trait.transient is not True:
                dct[name] = trait
        state['_added_traits'] = dct
        
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        super(Container, self).__setstate__({})
        self.__dict__.update(state)
        
        # restore call to _io_trait_changed to catch changes to any trait
        # having 'iotype' metadata
        self.on_trait_change(self._io_trait_changed, '+iotype')
        
        # restore dynamically added traits, since they don't seem
        # to get restored automatically
        for name, trait in self._added_traits.items():
            self.add_trait(name, trait)
         
        # after unpickling, implicitly defined traits disappear, so we have to
        # recreate them by assigning them to themselves.       
        #TODO: I'm probably missing something. There has to be a better way to
        #      do this...
        for name, val in self.__dict__.items():
            if not self.trait(name) and not name.startswith('__'):
                setattr(self, name, val) # force def of implicit trait

    def add_trait(self, name, trait):
        """Overrides HasTraits definition of *add_trait* in order to
        keep track of dynamically added traits for serialization.
        """
        self._added_traits[name] = trait
        super(Container, self).add_trait(name, trait)
        getattr(self, name)  # this causes instance traits to show up in traits()
        
    def remove_trait(self, name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """
        # this just forces the regeneration (lazily) of the lists of
        # inputs, outputs, and containers
        self._trait_added_changed(name)
        try:
            del self._added_traits[name]
        except KeyError:
            pass
        super(Container, self).remove_trait(name)
            
    #def trait_get(self, *names, **metadata):
        #"""Override the HasTraits version of this because if we don't,
        #HasTraits.__getstate__ won't return our instance traits.
        #"""
        #if len(names) == 0:
            #names = self._alltraits(None, **metadata).keys()
        #return super(Container, self).trait_get(*names, **metadata)
    
        
    # call this if any trait having 'iotype' metadata is changed
    #@on_trait_change('+iotype') 
    def _io_trait_changed(self, obj, name, old, new):
        # setting old to Undefined is a kludge to bypass the destination check
        # when we call this directly from Assembly as part of setting this
        # attribute from an existing connection.
        if self.trait(name).iotype == 'in':
            if old is not Undefined and name in self._sources:
                # bypass the callback here and set it back to the old value
                self._trait_change_notify(False)
                try:
                    setattr(obj, name, old)
                finally:
                    self._trait_change_notify(True)
                self.raise_exception(
                    "'%s' is already connected to source '%s' and "
                    "cannot be directly set"%
                    (name, self._sources[name]), TraitError)
            self._call_execute = True
            self._input_changed(name)
            
    def _input_changed(self, name):
        pass

    # error reporting stuff
    def _get_log_level(self):
        """Return logging message level."""
        return self._logger.level

    def _set_log_level(self, level):
        """Set logging message level."""
        self._logger.level = level

    def get_wrapped_attr(self, name):
        """If the named trait can return a TraitValMetaWrapper, then this
        function will return that, with the value set to the current value of
        the named variable. Otherwise, it functions like *getattr*, just
        returning the value of the named variable. Raises an exception if the
        named trait cannot be found. The value will be copied if the trait has
        a 'copy' metadata attribute that is not None. Possible values for
        'copy' are 'shallow' and 'deep'.
        """
        parts = name.split('.', 1)
        if len(parts) > 1:
            obj = getattr(self, parts[0])
            return obj.get_wrapped_attr(parts[1])

        trait = self.traits().get(name)
        if trait is None:
            self.raise_exception("trait '%s' does not exist" %
                                 name, TraitError)
            
        # trait itself is most likely a CTrait, which doesn't have
        # access to member functions on the original trait, aside
        # from validate and one or two others, so we need to get access 
        # to the original trait which is held in the 'trait_type' attribute.
        ttype = trait.trait_type
        getwrapper = getattr(ttype, 'get_val_meta_wrapper', None)
        if getwrapper is not None:
            wrapper = getwrapper()
            # copy value if 'copy' found in metadata
            wrapper.value = _copydict[ttype.copy](getattr(self, name))
            return wrapper
        
        # copy value if 'copy' found in metadata
        return _copydict[ttype.copy](getattr(self, name)) 
        
    def add(self, name, obj, **kw_args):
        """Add a Container object to this Container.
        Returns the added Container object.
        """
        if '.' in name:
            self.raise_exception(
                'add does not allow dotted path names like %s' %
                name, ValueError)
        if obj == self:
            self.raise_exception('cannot make an object a child of itself',
                                 RuntimeError)
            
        if isinstance(obj, Container):
            obj.parent = self
            # if an old child with that name exists, remove it
            if self.contains(name) and getattr(self, name):
                self.remove(name)
            setattr(self, name, obj)
            obj.name = name
            # if this object is already installed in a hierarchy, then go
            # ahead and tell the obj (which will in turn tell all of its
            # children) that its scope tree back to the root is defined.
            if self._call_tree_rooted is False:
                obj.tree_rooted()
        else:
            self.raise_exception("'"+str(type(obj))+
                    "' object is not an instance of Container.",
                    TypeError)
        return obj
        
    def remove(self, name):
        """Remove the specified child from this container and remove any
        public trait objects that reference that child. Notify any
        observers."""
        if '.' in name:
            self.raise_exception(
                'remove does not allow dotted path names like %s' %
                                 name, ValueError)
        trait = self.traits().get(name)
        if trait is not None:
            # for Instance traits, set their value to None but don't remove
            # the trait
            obj = getattr(self, name)
            if obj is not None and not isinstance(obj, Container):
                self.raise_exception('attribute %s is not a Container' % name,
                                     RuntimeError)
            if trait.is_trait_type(Instance):
                try:
                    setattr(self, name, None)
                except TraitError as err:
                    self.raise_exception(str(err), RuntimeError)
            else:
                self.remove_trait(name)
            return obj       
        else:
            self.raise_exception("cannot remove container '%s': not found"%
                                 name, TraitError)

    def tree_rooted(self):
        """Called after the hierarchy containing this Container has been
        defined back to the root. This does not guarantee that all sibling
        Containers have been defined. It also does not guarantee that this
        component is fully configured to execute. Classes that override this
        function must call their base class version.
        
        This version calls tree_rooted() on all of its child Containers.
        """
        self._logger.rename(self.get_pathname().replace('.', ','))
        self._call_tree_rooted = False
        for cont in self.list_containers():
            getattr(self, cont).tree_rooted()
            
    def revert_to_defaults(self, recurse=True):
        """Sets the values of all of the inputs to their default values."""
        self.reset_traits(iotype='in')
        if recurse:
            for cname in self.list_containers():
                getattr(self, cname).revert_to_defaults(recurse)
            
    def items(self, recurse=False, **metadata):
        """Return a list of tuples of the form (rel_pathname, obj) for each
        trait of this Container that matches the given metadata. If recurse is
        True, also iterate through all child Containers of each Container
        found.
        """
        return self._items(set([id(self.parent)]), recurse, **metadata)
        
    def keys(self, recurse=False, **metadata):
        """Return a list of the relative pathnames of children of this
        Container that match the given metadata. If recurse is True, child
        Containers will also be iterated over.
        """
        return [tup[0] for tup in self._items(set([id(self.parent)]), 
                                              recurse, **metadata)]
        
    def values(self, recurse=False, **metadata):
        """Return a list of children of this Container that have matching 
        trait metadata. If recurse is True, child Containers will also be 
        iterated over.
        """
        return [tup[1] for tup in self._items(set([id(self.parent)]), 
                                              recurse, **metadata)]

    def list_containers(self):
        """Return a list of names of child Containers."""
        return [n for n, v in self.items() if isinstance(v, Container)]
    
    def _alltraits(self, traits=None, **metadata):
        """This returns a dict that contains all traits (class and instance)
        that match the given metadata.
        """
        if traits is None:
            traits = self.traits()  # don't pass **metadata here
            #ss = set(self._instance_traits().keys())-set(traits.keys())
            #if len(ss): print '\n                            **** ', ss
            traits.update(self._instance_traits())
            
        result = {}
        for name, trait in traits.items():
            if trait.type is 'event':
                continue
            for meta_name, meta_eval in metadata.items():
                if type( meta_eval ) is FunctionType:
                    if not meta_eval(getattr(trait, meta_name)):
                        break
                elif meta_eval != getattr(trait, meta_name):
                    break
            else:
                result[ name ] = trait

        return result
    
    def _items(self, visited, recurse=False, **metadata):
        """Return an iterator that returns a list of tuples of the form 
        (rel_pathname, obj) for each trait of this Container that matches
        the given metadata. If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        if id(self) not in visited:
            visited.add(id(self))
            if 'type' not in metadata:
                metadata['type'] = not_event
            match_dict = self._alltraits(**metadata)
            #match_dict = self.traits(**metadata)
            
            if recurse:
                for name in self.list_containers():
                    obj = getattr(self, name)
                    if name in match_dict and id(obj) not in visited:
                        yield(name, obj)
                    if obj:
                        for chname, child in obj._items(visited, recurse, 
                                                        **metadata):
                            yield ('.'.join([name, chname]), child)
                            
            for name, trait in match_dict.items():
                obj = getattr(self, name)
                if id(obj) not in visited:
                    if isinstance(obj, Container):
                        if not recurse:
                            yield (name, obj)
                    elif trait.iotype is not None:
                        yield (name, obj)

    
    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container. 
        """
        tup = path.split('.', 1)
        if len(tup) == 1:
            return hasattr(self, path)
        
        obj = getattr(self, tup[0], None)
        if obj is not None:
            if isinstance(obj, Container):
                return obj.contains(tup[1])
            else:
                return hasattr(obj, tup[1])
        return False
    
    def invoke(self, path, *args, **kwargs):
        """Call the callable specified by **path**, which may be a simple
        name or a dotted path, passing the given arguments to it, and 
        return the result.
        """
        if path:
            tup = path.split('.')
            if len(tup) == 1:
                return getattr(self, path)(*args, **kwargs)
            else:
                obj = getattr(self, tup[0], Missing)
                if obj is Missing:
                    self.raise_exception("object has no attribute '%s'" % 
                                         tup[0], AttributeError)
                if len(tup) == 2:
                    return getattr(obj, tup[1])(*args, **kwargs)
                else:
                    return obj.invoke('.'.join(tup[1:]), *args, **kwargs)
        else:
            self.raise_exception("this object is not callable",
                                 RuntimeError)        
    
    def get_metadata(self, traitpath, metaname=None):
        """Retrieve the metadata associated with the trait found using
        traitpath.  If metaname is None, return the entire metadata dictionary
        for the specified trait. Otherwise, just return the specified piece
        of metadata.  If the specified piece of metadata is not part of
        the trait, None is returned.
        """
        parts = traitpath.split('.', 1)
        if len(parts) > 1:
            obj = getattr(self, parts[0])
            return obj.get_metadata(parts[1], metaname)
            
        t = self.traits().get(traitpath)
        if not t:
            self.raise_exception("Couldn't find trait %s" % traitpath,
                                 AttributeError)
        if metaname is None:
            return t.trait_type._metadata.copy()
        else:
            return getattr(t, metaname)
        
        
    def get(self, path, index=None):
        """Return the object specified by the given 
        path, which may contain '.' characters.  
        """
        tup = path.split('.')
        if len(tup) == 1:
            if index is None:
                obj = getattr(self, path, Missing)
                if obj is Missing:
                    self.raise_exception(
                        "object has no attribute '%s'" % path, 
                        AttributeError)
                return obj
            else:
                return self._array_get(path, index)
        else:
            obj = getattr(self, tup[0], Missing)
            if obj is Missing:
                self.raise_exception(
                    "object has no attribute '%s'" % tup[0], 
                    AttributeError)
            if len(tup) == 2 and index is None:
                return getattr(obj, tup[1])
            
            if isinstance(obj, Container):
                return obj.get('.'.join(tup[1:]), index)
            elif index is None:
                return getattr(obj, '.'.join(tup[1:]))
            else:
                return obj._array_get('.'.join(tup[1:]), index)
     
    def set_source(self, name, source):
        """Mark the named io trait as a destination by registering a source
        for it, which will prevent it from being set directly or connected 
        to another source.
        """
        if name in self._sources:
            self.raise_exception(
                "'%s' is already connected to source '%s'" % 
                (name, self._sources[name]), TraitError)
        self._sources[name] = source
            
    def remove_source(self, destination):
        """Remove the source from the given destination io trait. This will
        allow the destination to later be connected to a different source or
        to have its value directly set.
        """
        del self._sources[destination]    
        
    def _check_trait_settable(self, name, srcname=None, force=False):
        if force:
            src = None
        else:
            src = self._sources.get(name, None)
        trait = self.traits().get(name)
        if trait:
            if trait.iotype != 'in' and src is not None and src != srcname:
                self.raise_exception(
                    "'%s' is not an input trait and cannot be set" %
                    name, TraitError)
                
            if src is not None and src != srcname:
                self.raise_exception(
                    "'%s' is connected to source '%s' and cannot be "
                    "set by source '%s'" %
                    (name,src,srcname), TraitError)
        else:
            self.raise_exception("object has no attribute '%s'" % name,
                                 TraitError)
        return trait

    def set(self, path, value, index=None, srcname=None, force=False):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints. *index*, if not None,
        should be a list of ints, at most one for each array dimension of the
        target value.
        """ 
        if path is None:
            if index is None:
                # should never get down this far
                self.raise_exception('this object cannot replace itself')
            else:
                self.raise_exception(
                    'Cannot set value at index %s'%
                    str(index), AttributeError)
                    
        tup = path.split('.')
        if len(tup) == 1:
            trait = self._check_trait_settable(path, srcname, force)
            if trait.type == 'event':
                setattr(self, path, value)
            else:
                if index is None:
                    if trait is None:
                        self.raise_exception("object has no attribute '%s'" %
                                             path, TraitError)
                    # bypass the callback here and call it manually after 
                    # with a flag to tell it not to check if it's a destination
                    self._trait_change_notify(False)
                    try:
                        setattr(self, path, value)
                    finally:
                        self._trait_change_notify(True)
                    # now manually call the notifier with old set to Undefined
                    # to avoid the destination check
                    self._io_trait_changed(self, path, Undefined, 
                                           getattr(self, path))
                else:
                    self._array_set(path, value, index)
        else:
            obj = getattr(self, tup[0], Missing)
            if obj is Missing:
                self.raise_exception("object has no attribute '%s'" % tup[0], 
                                     TraitError)
            if len(tup) == 2:
                if isinstance(obj, Container):
                    obj.set(tup[1], value, index, srcname=srcname, 
                            force=force)
                elif index is None:
                    setattr(obj, tup[1], value)
                else:
                    obj._array_set(tup[1], value, index)
            else:
                if isinstance(obj, Container):
                    obj.set('.'.join(tup[1:]), value, index, force=force)
                elif index is not None:
                    obj._array_set('.'.join(tup[1:]), value, index)
                else:
                    try:
                        _deep_setattr(obj, '.'.join(tup[1:]), value)
                    except Exception:
                        self.raise_exception("object has no attribute '%s'" % 
                                             path, TraitError)

    def _array_set(self, name, value, index):
        arr = getattr(self, name)
        
        length = len(index)
        if length == 1:
            old = arr[index[0]]
            arr[index[0]] = value
        elif length == 2:
            old = arr[index[0]][index[1]]
            arr[index[0]][index[1]] = value
        elif length == 3:
            old = arr[index[0]][index[1]][index[2]]
            arr[index[0]][index[1]][index[2]] = value
        else:
            for idx in index[:-1]:
                arr = arr[idx]
            old = arr[index[length-1]]
            arr[index[length-1]] = value
                
        # setting of individual Array values doesn't seem to trigger
        # _io_trait_changed, so do it manually
        if old != value:
            self._io_trait_changed(self, name, arr, arr)
            
    def _array_get(self, name, index):
        arr = getattr(self, name)
        length = len(index)
        if length == 1:
            return arr[index[0]]
        elif length == 2:
            return arr[index[0]][index[1]]
        elif length == 3:
            return arr[index[0]][index[1]][index[2]]
        else:
            for idx in index:
                arr = arr[idx]
            return arr
    
    def save_to_egg(self, name, version, py_dir=None, src_dir=None,
                    src_files=None, child_objs=None, dst_dir=None,
                    fmt=SAVE_CPICKLE, proto=-1, use_setuptools=False,
                    observer=None):
        """Save state and other files to an egg.  Typically used to copy all or
        part of a simulation to another user or machine.  By specifying child
        containers in `child_objs`, it will be possible to create instances of
        just those containers from the installed egg.  Child container names
        should be specified relative to this container.

        name: string
            Name for egg, must be an alphanumeric string.

        version: string
            Version for egg,  must be an alphanumeric string.

        py_dir: string
            The (root) directory for local Python files. It defaults to
            the current directory.

        src_dir: string
            The root of all (relative) `src_files`.

        src_files: list
            List of paths to files to be included in the egg.

        child_objs: list
            List of child objects for additional entry points.

        dst_dir: string
            The directory to write the egg in.

        fmt: int
            Passed to :meth:`eggsaver.save`.

        proto: int
            Passed to :meth:`eggsaver.save`.

        use_setuptools: bool
            Passed to :meth:`eggsaver.save_to_egg`.

        observer: callable
            Will be called via an :class:`EggObserver`.

        After collecting entry point information, calls
        :meth:`eggsaver.save_to_egg`.
        Returns ``(egg_filename, required_distributions, orphan_modules)``.
        """
        assert name and isinstance(name, basestring)
        assert version and isinstance(version, basestring)
        if not version.endswith('.'):
            version += '.'
        now = datetime.datetime.now()  # Could consider using utcnow().
        tstamp = '%d.%02d.%02d.%02d.%02d' % \
                 (now.year, now.month, now.day, now.hour, now.minute)
        version += tstamp

        observer = eggobserver.EggObserver(observer, self._logger)

        # Child entry point names are the pathname, starting at self.
        entry_pts = [(self, name, _get_entry_group(self))]
        if child_objs is not None:
            root_pathname = self.get_pathname()
            root_start = root_pathname.rfind('.')
            root_start = root_start+1 if root_start >= 0 else 0
            root_pathname += '.'
            for child in child_objs:
                pathname = child.get_pathname()
                if not pathname.startswith(root_pathname):
                    msg = '%s is not a child of %s' % (pathname, root_pathname)
                    observer.exception(msg)
                    self.raise_exception(msg, RuntimeError)
                entry_pts.append((child, pathname[root_start:],
                                  _get_entry_group(child)))

        parent = self.parent
        self.parent = None  # Don't want to save stuff above us.
        try:
            return eggsaver.save_to_egg(entry_pts, version, py_dir,
                                        src_dir, src_files, dst_dir,
                                        fmt, proto, self._logger,
                                        use_setuptools, observer.observer)
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        finally:
            self.parent = parent

    def save(self, outstream, fmt=SAVE_CPICKLE, proto=-1):
        """Save the state of this object and its children to the given
        output stream. Pure Python classes generally won't need to
        override this because the base class version will suffice, but
        Python extension classes will have to override. The format
        can be supplied in case something other than cPickle is needed.

        outstream: file or string
            Stream to save to.

        fmt: int
            Format for saved data.

        proto: int
            Protocol used.
        """
        parent = self.parent
        self.parent = None  # Don't want to save stuff above us.
        try:
            eggsaver.save(self, outstream, fmt, proto, self._logger)
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        finally:
            self.parent = parent

    @staticmethod
    def load_from_eggfile(filename, observer=None):
        """Extract files in egg to a subdirectory matching the saved object
        name and then load object graph state.

        filename: string
            Name of egg file to be loaded.

        observer: callable
            Will be called via an :class:`EggObserver`.

        Returns the root object.
        """
        # Load from file gets everything.
        entry_group = 'openmdao.top'
        entry_name = 'top'
        return eggloader.load_from_eggfile(filename, entry_group, entry_name,
                                           logger, observer)

    @staticmethod
    def load_from_eggpkg(package, entry_name=None, instance_name=None,
                         observer=None):
        """Load object graph state by invoking the given package entry point.
        If specified, the root object is renamed to `instance_name`.

        package: string
            Package name.

        entry_name: string
            Name of entry point.

        instance_name: string
            Name for root object.

        observer: callable
            Will be called via an :class:`EggObserver`.

        Returns the root object.
        """
        entry_group = 'openmdao.component'
        if not entry_name:
            entry_name = package  # Default component is top.
        return eggloader.load_from_eggpkg(package, entry_group, entry_name,
                                          instance_name, logger, observer)

    @staticmethod
    def load(instream, fmt=SAVE_CPICKLE, package=None, call_post_load=True,
             name=None):
        """Load object(s) from the input stream. Pure Python classes generally
        won't need to override this, but extensions will. The format can be
        supplied in case something other than cPickle is needed.

        instream: file or string
            Stream to load from.

        fmt: int
            Format of state data.

        package: string
            Name of package to look for `instream`, if `instream` is a string
            that is not an existing file.

        call_post_load: bool
            If True, call :meth:`post_load`.

        name: string
            Name for root object

        Returns the root object.
        """
        top = eggloader.load(instream, fmt, package, logger)
        if name:
            top.name = name
        if call_post_load:
            top.parent = None
            top.post_load()
        return top

    def post_load(self):
        """Perform any required operations after model has been loaded."""
        [x.post_load() for x in self.values() if isinstance(x, Container)]

    def pre_delete(self):
        """Perform any required operations before the model is deleted."""
        [x.pre_delete() for x in self.values() if isinstance(x, Container)]
    
    def _build_trait(self, pathname, iotype=None, trait=None):
        """Asks the component to dynamically create a trait for the 
        attribute given by ref_name, based on whatever knowledge the
        component has of that attribute.
        """
        objtrait, value = self._find_trait_and_value(pathname)
        if iotype is None and objtrait is not None:
            iotype = objtrait.iotype
        if trait is None:
            trait = objtrait
        # if we make it to here, object specified by ref_name exists
        return _PathProperty(ref_name=pathname, iotype=iotype, 
                            trait=trait)
    
    def _find_trait_and_value(self, pathname):
        """Return a tuple of the form (trait, value) for the given dotted
        pathname. Raises an exception if the value indicated by the pathname
        is not found. If the value is found but has no trait, then (None, value) 
        is returned.
        """
        if pathname:
            names = pathname.split('.')
            obj = self
            for name in names:
                if isinstance(obj, HasTraits):
                    objtrait = obj.traits().get(name)
                else:
                    objtrait = None
                obj = getattr(obj, name)
            return (objtrait, obj)
        else:
            return (None, None)

    def _create_io_traits(self, obj_info, iotype='in'):
        """Create io trait(s) specified by the contents of obj_info. Calls
        _build_trait(), which can be overridden by subclasses, to create each
        trait.
        
        obj_info is assumed to be either a string, a tuple, or an iterator
        that returns strings or tuples. Tuples must contain a name and an
        alias, and my optionally contain an iotype and a validation trait.
        
        For example, the following are valid calls:

        obj._create_io_traits('foo')
        obj._create_io_traits(['foo','bar','baz'])
        obj._create_io_traits(('foo', 'foo_alias', 'in', some_trait))
        obj._create_io_traits([('foo', 'fooa', 'in'),('bar', 'barb', 'out'),('baz', 'bazz')])
        """
        if isinstance(obj_info, basestring) or isinstance(obj_info, tuple):
            lst = [obj_info]
        else:
            lst = obj_info

        for entry in lst:
            iostat = iotype
            trait = None
            
            if isinstance(entry, basestring):
                name = entry
                ref_name = name
            elif isinstance(entry, tuple):
                name = entry[0]  # wrapper name
                ref_name = entry[1] or name # internal name
                try:
                    iostat = entry[2] # optional iotype
                    trait = entry[3]  # optional validation trait
                except IndexError:
                    pass
            else:
                self.raise_exception('_create_io_traits cannot add trait %s' % entry,
                                     TraitError)
            self.add_trait(name, 
                           self._build_trait(ref_name, iostat, trait))
        
    def get_dyn_trait(self, name, iotype=None):
        """Retrieves the named trait, attempting to create it on-the-fly if
        it doesn't already exist.
        """
        trait = self.traits().get(name)
        if trait:
            return trait
        try:
            return self._create_alias(name, iotype)
        except AttributeError:
            self.raise_exception("Cannot locate trait named '%s'" %
                                 name, NameError)

    def _create_alias(self, path, io_status=None, trait=None, alias=None):
        """Create a trait that maps to some internal variable designated by a
        dotted path. If a trait is supplied as an argument, use that trait as
        a validator for the aliased value. The resulting trait will have the
        dotted path as its name (or alias if specified) and will be added to 
        self.  An exception will be raised if the trait already exists.
        """
        if alias is None:
            alias = path
        oldtrait = self.traits().get(alias)
        if oldtrait is None:
            newtrait = self._build_trait(path, iotype=io_status, trait=trait)
            self.add_trait(alias, newtrait)
            return newtrait
        else:
            self.raise_exception(
                "Can't create alias '%s' because it already exists." % alias, 
                RuntimeError)
    
    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        full_msg = '%s: %s' % (self.get_pathname(), msg)
        self._logger.error(msg)
        raise exception_class(full_msg)


def _get_entry_group(obj):
    """Return entry point group for given object type."""
    if _get_entry_group.group_map is None:
        # Fill-in here to avoid import loop.
        from openmdao.main.component import Component
        from openmdao.main.driver import Driver

        # Entry point definitions taken from plugin-guide.
        # Order should be from most-specific to least.
        _get_entry_group.group_map = [
            (TraitType,          'openmdao.variable'),
            (Driver,             'openmdao.driver'),
            (ICaseIterator,      'openmdao.case_iterator'),
            (IResourceAllocator, 'openmdao.resource_allocator'),
            (Component,          'openmdao.component'),
            (Container,          'openmdao.container'),
        ]

    for cls, group in _get_entry_group.group_map:
        if issubclass(cls, Interface):
            if validate_implements(obj, cls):
                return group
        else:
            if isinstance(obj, cls):
                return group

    raise TypeError('No entry point group defined for %r' % obj)

_get_entry_group.group_map = None  # Map from class/interface to group name.


def dump(cont, recurse=False, stream=None):
    """Print all items having iotype metadata and
    their corresponding values to the given stream. If the stream
    is not supplied, it defaults to *sys.stdout*.
    """
    pprint.pprint(dict([(n, str(v)) 
                    for n, v in cont.items(recurse=recurse, 
                                          iotype=not_none)]),
                  stream)


def findname(parent, obj):
    """Find the given object in the specified parent and return its name 
    in the parent's __dict__.
    
    Return '' if not found.
    """
    for name,val in parent.__dict__.items():
        if val is obj:
            return name
    return ''


def get_default_name(obj, scope):
    """Return a unique name for the given object in the given scope."""
    classname = obj.__class__.__name__.lower()
    if scope is None:
        sdict = {}
    else:
        sdict = scope.__dict__
        
    ver = 1
    while '%s%d' % (classname, ver) in sdict:
        ver += 1
    return '%s%d' % (classname, ver)
        


