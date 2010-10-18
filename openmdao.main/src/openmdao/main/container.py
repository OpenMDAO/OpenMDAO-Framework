"""
The Container class
"""

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
                                 Interface, Instance
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
    'deep': copy.deepcopy,
    'shallow': copy.copy
    }

_iodict = { 'out': 'output', 'in': 'input' }

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
            
class _ContainerDepends(object):
    """An object that bookkeeps connections to/from Container variables."""
    def __init__(self):
        self._srcs = {}
        
    def connect(self, srcpath, destpath):
        if destpath in self._srcs:
            raise RuntimeError("'%s' is already connected to source '%s'" %
                               (destpath,self._srcs[destpath]))
        self._srcs[destpath] = srcpath
        
    def disconnect(self, srcpath, destpath):
        del self._srcs[destpath]
    
    def get_source(self, destname):
        """For a given destination name, return the connected source, 
        or None if not connected.
        """
        return self._srcs.get(destname)
    
    def get_connected_inputs(self):
        return [n for n in self._srcs.keys() if not n.startswith('parent.')]
    
    def get_connected_outputs(self):
        return [n for n in self._srcs.values() if not n.startswith('parent.')]


class Container(HasTraits):
    """ Base class for all objects having Traits that are visible 
    to the framework"""
   
    def __init__(self, doc=None, iotype=None):
        super(Container, self).__init__()
        
        self._depgraph = _ContainerDepends()
                          
        # for keeping track of dynamically added traits for serialization
        self._added_traits = {}
        
        self._parent = None
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
                
        # register callbacks for all of our 'in' traits
        for name,trait in self.class_traits().items():
            if trait.iotype == 'in':
                self.on_trait_change(self._input_trait_modified, name)

    @property
    def parent(self):
        """The parent Container of this Container."""
        return self._parent
    
    @parent.setter
    def parent(self, value):
        """This is called when the parent attribute is changed."""
        if self._parent is not value:
            self._parent = value
            self._logger.rename(self.get_pathname().replace('.', ','))
            self._branch_moved()
        
    def _branch_moved(self):
        self._call_tree_rooted = True
        for n,cont in self.items():
            if isinstance(cont, Container):
                cont._branch_moved()
 
    @property
    def name(self):
        """The name of this Container."""
        if self._name is None:
            if self.parent:
                self._name = findname(self.parent, self)
        return self._name

    @name.setter
    def name(self, name):
        """Sets the name of this Container."""
        match = _namecheck_rgx.search(name)
        if match is None or match.group() != name:
            raise NameError("name '%s' contains illegal characters" % name)
        self._name = name
        self._logger.rename(self._name)
        
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
            
    def connect(self, srcpath, destpath, value=Missing):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable
            
        destpath: str
            Pathname of destination variable

        value: object, optional
            A value used for validation by the destination variable
        """
        if not srcpath.startswith('parent.'):
            if not self.contains(srcpath):
                self.raise_exception("Can't find '%s'" % srcpath, AttributeError)
            cname, _, restofpath = srcpath.partition('.')
            if restofpath:
                getattr(self, cname).connect(restofpath, 'parent.'+destpath, value)
        if not destpath.startswith('parent.'):
            if not self.contains(destpath):
                self.raise_exception("Can't find '%s'" % destpath, AttributeError)
            sname = self._depgraph.get_source(destpath)
            if sname is not None:
                self.raise_exception(
                    "'%s' is already connected to source '%s'" % 
                    (destpath, sname), RuntimeError)
            cname, _, restofpath = destpath.partition('.')
            if restofpath:
                getattr(self, cname).connect('parent.'+srcpath, restofpath, value)
            else:
                if value is not Missing: #if we're given a value, use it for validation
                    trait = get_trait(self, destpath)
                    if trait is None:
                        self.raise_exception("No trait '%s' found" % destpath, AttributeError)
                    trait.validate(self, destpath, value)

        self._depgraph.connect(srcpath, destpath)
                        
    def disconnect(self, srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
        """
        srcpath = srcpath.replace('@in.','').replace('@out.','').replace('@self.','')
        destpath = destpath.replace('@in.','').replace('@out.','').replace('@self.','')
        
        if not srcpath.startswith('parent.'):
            if not self.contains(srcpath):
                self.raise_exception("Can't find '%s'" % srcpath, AttributeError)
            cname, _, restofpath = srcpath.partition('.')
            if restofpath:
                getattr(self, cname).disconnect(restofpath, 'parent.'+destpath)
        if not destpath.startswith('parent.'):
            if not self.contains(destpath):
                self.raise_exception("Can't find '%s'" % destpath, AttributeError)
            cname, _, restofpath = destpath.partition('.')
            if restofpath:
                getattr(self, cname).disconnect('parent.'+srcpath, restofpath)
        
        self._depgraph.disconnect(srcpath, destpath)

    #
    #  HasTraits overrides
    #
    
    def __deepcopy__ ( self, memo ):
        """ Overrides deepcopy for HasTraits because otherwise we lose instance
        traits when we copy. :(
        """
        id_self = id( self )
        if id_self in memo:
            return memo[ id_self ]
        result = super(Container, self).__deepcopy__(memo)
        olditraits = self._instance_traits()
        newtraits = result._instance_traits()
        newtraits.update(result.traits())
        for name, trait in olditraits.items():
            if trait.type is not 'event' and name not in newtraits:
                result.add_trait(name, copy.copy(trait.trait_type))
                setattr(result, name, getattr(self, name))
        return result

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
        
        ## restore call to _io_trait_modified to catch changes to any trait
        ## having 'iotype' metadata
        #self.on_trait_change(self._io_trait_modified, '+iotype')
        
        # restore dynamically added traits, since they don't seem
        # to get restored automatically
        traits = self.alltraits()
        for name, trait in self._added_traits.items():
            if name not in traits:
                self.add_trait(name, trait)
        
        # make sure all input callbacks are in place.  If callback is
        # already there, this will have no effect. 
        for name, trait in self.alltraits().items():
            if trait.iotype == 'in':
                self.on_trait_change(self._input_trait_modified, name)
            
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
        #FIXME: saving our own list of added traits shouldn't be necessary...
        self._added_traits[name] = trait
        super(Container, self).add_trait(name, trait)
        getattr(self, name)  # this causes (non-property) instance traits to show up in traits()
        
        # if it's an input trait, register a callback to be called whenever it's changed
        if trait.iotype == 'in':
            self.on_trait_change(self._input_trait_modified, name)
        
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
        
        trait = self.trait(name)
        if trait and trait.iotype == 'in':
            self.on_trait_change(self._input_trait_modified, name, remove=True)

        super(Container, self).remove_trait(name)
            
    # call this if any trait having 'iotype' metadata is changed
    def _input_trait_modified(self, obj, name, old, new):
        # setting old to Undefined is a kludge to bypass the destination check
        # when we call this directly from Assembly as part of setting this
        # attribute from an existing connection.
        if old is not Undefined and self._depgraph.get_source(name):
            # bypass the callback here and set it back to the old value
            self._trait_change_notify(False)
            try:
                setattr(obj, name, old)
            finally:
                self._trait_change_notify(True)
            self.raise_exception(
                "'%s' is already connected to source '%s' and "
                "cannot be directly set"%
                (name, self._depgraph.get_source(name)), TraitError)
        self._call_execute = True
        self._input_changed(name)
            
    def _input_changed(self, name):
        # this is here so inherited classes can take actions when inputs are changed
        pass

    def get_wrapped_attr(self, name):
        """If the named trait can return a TraitValMetaWrapper, then this
        function will return that, with the value set to the current value of
        the named variable. Otherwise, it functions like *getattr*, just
        returning the value of the named variable. Raises an exception if the
        named trait cannot be found. The value will be copied if the trait has
        a 'copy' metadata attribute that is not None. Possible values for
        'copy' are 'shallow' and 'deep'.
        """
        scopename, _, restofpath = name.partition('.')
        if restofpath:
            if scopename == 'parent':
                return self.parent.get_wrapped_attr(name[7:])
            obj = getattr(self, scopename)
            if isinstance(obj, HasTraits):
                return obj.get_wrapped_attr(restofpath)
            else:
                return getattr(obj, restofpath)
        trait = get_trait(self, name)
        if trait is None:
            self.raise_exception("trait '%s' does not exist" %
                                 name, TraitError)
            
        # trait itself is most likely a CTrait, which doesn't have
        # access to member functions on the original trait, aside
        # from validate and one or two others, so we need to get access 
        # to the original trait which is held in the 'trait_type' attribute.
        ttype = trait.trait_type
        getwrapper = getattr(ttype, 'get_val_meta_wrapper', None)
        val = getattr(self, name)
        # copy value if 'copy' found in metadata
        if ttype.copy:
            val = _copydict[ttype.copy](val)
        if getwrapper is not None:
            wrapper = getwrapper()
            wrapper.value = val
            return wrapper
        
        return val
        
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
        trait = get_trait(self, name)
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
            match_dict = self.alltraits(**metadata)
            
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

    def items(self, recurse=False, **metadata):
        """Return a list of tuples of the form (rel_pathname, obj) for each
        trait of this Container that matches the given metadata. If recurse is
        True, also iterate through all child Containers of each Container
        found.
        """
        return self._items(set([id(self.parent)]), recurse, **metadata)
        
    def list_containers(self):
        """Return a list of names of child Containers."""
        return [n for n, v in self.items() if isinstance(v, Container)]
    
    def alltraits(self, traits=None, **metadata):
        """This returns a dict that contains all traits (class and instance)
        that match the given metadata.
        """
        if traits is None:
            traits = self.traits()  # don't pass **metadata here
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
    
    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container. 
        """
        childname, _, restofpath = path.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing:
                return False
            elif isinstance(obj, Container):
                return obj.contains(restofpath)
            else:
                return hasattr(obj, restofpath)
        return hasattr(self, path)
    
    def invoke(self, path, *args, **kwargs):
        """Call the callable specified by **path**, which may be a simple
        name or a dotted path, passing the given arguments to it, and 
        return the result.
        """
        if path:
            childname, _, restofpath = path.partition('.')
            if restofpath:
                obj = getattr(self, childname, Missing)
                if obj is Missing:
                    self.raise_exception("object has no attribute '%s'" % childname, 
                                         AttributeError)
                return obj.invoke(restofpath, *args, **kwargs)
            
            return getattr(self, path)(*args, **kwargs)
        else:
            self.raise_exception("invoke: no path given",
                                 RuntimeError)
    
    def get_metadata(self, traitpath, metaname=None):
        """Retrieve the metadata associated with the trait found using
        traitpath.  If metaname is None, return the entire metadata dictionary
        for the specified trait. Otherwise, just return the specified piece
        of metadata.  If the specified piece of metadata is not part of
        the trait, None is returned.
        """
        childname, _, restofpath = traitpath.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing:
                self.raise_exception("object has no attribute '%s'" % childname, 
                                     AttributeError)
            return obj.get_metadata(restofpath, metaname)
            
        t = get_trait(self, traitpath)
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
        childname, _, restofpath = path.partition('.')
        if not restofpath:
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
            obj = getattr(self, childname, Missing)
            if obj is Missing:
                self.raise_exception(
                    "object has no attribute '%s'" % childname, 
                    AttributeError)
            if isinstance(obj, Container):
                return obj.get(restofpath, index)
            elif index is None:
                return getattr(obj, restofpath)
            else:
                return obj._array_get(restofpath, index)
     
    def _check_trait_settable(self, name, source=None, force=False):
        trait = get_trait(self, name)
        if trait:
            if trait.iotype == 'in':
                src = None if force else self._depgraph.get_source(name)
                if src is not None and src != source:
                    if trait.iotype != 'in':
                        self.raise_exception(
                            "'%s' is not an input trait and cannot be set" %
                            name, TraitError)
                    else:
                        self.raise_exception(
                            "'%s' is connected to source '%s' and cannot be "
                            "set by source '%s'" %
                            (name,src,source), TraitError)
        else:
            self.raise_exception("object has no attribute '%s'" % name,
                                 TraitError)
        return trait

    def set(self, path, value, index=None, src=None, force=False):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints. *index*, if not None,
        should be a list of ints, at most one for each array dimension of the
        target value.
        """ 
        if path is None:
            self.raise_exception('set: no path specified', NameError)
                    
        childname, _, restofpath = path.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing:
                self.raise_exception("object has no attribute '%s'" % childname, 
                                     TraitError)
            if isinstance(obj, Container):
                if src is not None:
                    src = 'parent.'+src
                obj.set(restofpath, value, index, src=src, 
                        force=force)
            elif index is None:
                setattr(obj, restofpath, value)
            else:
                obj._array_set(restofpath, value, index)
        else:
            trait = self._check_trait_settable(path, src, force)
            if trait.type == 'event':
                setattr(self, path, value)
            else:
                if index is None:
                    if trait.iotype == 'in':
                        # bypass the callback here and call it manually after 
                        # with a flag to tell it not to check if it's a destination
                        self._trait_change_notify(False)
                        try:
                            setattr(self, path, value)
                        finally:
                            self._trait_change_notify(True)
                        # now manually call the notifier with old set to Undefined
                        # to avoid the destination check
                        self._input_trait_modified(self, path, Undefined, 
                                               getattr(self, path))
                    else:
                        setattr(self, path, value)
                else:
                    self._array_set(path, value, index)

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
        # _input_trait_modified, so do it manually
        if old != value:
            self._input_trait_modified(self, name, arr, arr)
            
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
        [x.post_load() for n,x in self.items() if isinstance(x, Container)]

    def pre_delete(self):
        """Perform any required operations before the model is deleted."""
        [x.pre_delete() for n,x in self.items() if isinstance(x, Container)]
    
    def _build_trait(self, pathname, iotype=None, trait=None):
        """Asks the object to dynamically create a trait for the 
        attribute given by pathname, based on whatever knowledge the
        component has of that attribute.
        
        pathname: str
            The dotted path to the specified attribute.
            
        iotype: str, optional
            The data direction, either 'in' or 'out'.
            
        trait: TraitType, optional
            A validation trait for the given attribute.
        """
        self.raise_exception("Unable to create a new trait automatically", 
                             RuntimeError)
        
    def find_trait(self, pathname):
        """Returns a trait if a trait with the given pathname exists.
        If an attribute exists with the given
        pathname but no trait is found, None will be returned. If
        no attribute exists, an AttributeError will be raised.
        
        pathname: str
            pathname of the desired trait
        """
        cname, _, restofpath = pathname.partition('.')
        if restofpath:
            child = getattr(self, cname)
            if isinstance(child, Container):
                return child.find_trait(restofpath)
            else:
                if deep_hasattr(child, restofpath):
                    return None
        else:
            trait = get_trait(self, cname)
            if trait is not None:
                return trait
            elif trait is None and self.contains(cname):
                return None
            
        self.raise_exception("Cannot locate variable named '%s'" %
                             pathname, AttributeError)


    def _create_alias(self, path, io_status=None, trait=None, alias=None):
        """Create a trait that maps to some internal variable designated by a
        dotted path. If a trait is supplied as an argument, use that trait as
        a validator for the aliased value. The resulting trait will have the
        dotted path as its name (or alias if specified) and will be added to 
        self.  An exception will be raised if the trait already exists.
        """
        if alias is None:
            alias = path
        oldtrait = get_trait(self, alias)
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
        
def get_trait(obj, name):
    """obj is assumed to be a HasTraits object.  Returns the
    trait indicated by name, or None if not found.  No recursive
    search is performed if name contains dots.  This is a replacement
    for the trait() method on HasTraits objects, because that method
    can return traits that shouldn't exist.
    """
    trait = obj.traits().get(name)
    return trait if trait else obj._instance_traits().get(name)

def deep_hasattr(obj, pathname):
    """Returns True if the attrbute indicated by the give pathname
    exists, False otherwise.
    """
    try:
        parts = pathname.split('.')
        for name in parts[:-1]:
            obj = getattr(obj, name)
    except Exception:
        return False
    return hasattr(obj, parts[-1])

def find_trait_and_value(obj, pathname):
    """Return a tuple of the form (trait, value) for the given dotted
    pathname. Raises an exception if the value indicated by the pathname
    is not found in obj. If the value is found but has no trait, then (None, value) 
    is returned.
    """
    if pathname:
        names = pathname.split('.')
        for name in names[:-1]:
            obj = getattr(obj, name)
        if isinstance(obj, HasTraits):
            objtrait = get_trait(obj, names[-1])
        else:
            objtrait = None
        return (objtrait, getattr(obj, names[-1]))
    else:
        return (None, None)


def create_io_traits(cont, obj_info, iotype='in'):
    """Create io trait(s) specified by the contents of obj_info. Calls
    _build_trait() on the scoping object, which can be overridden by 
    subclasses, to create each trait.
    
    obj_info is assumed to be either a string, a tuple, or an iterator
    that returns strings or tuples. Tuples must contain a name and an
    alias, and my optionally contain an iotype and a validation trait.
    
    For example, the following are valid calls:

    create_io_traits(obj, 'foo')
    create_io_traits(obj, ['foo','bar','baz'])
    create_io_traits(obj, ('foo', 'foo_alias', 'in', some_trait))
    create_io_traits(obj, [('foo', 'fooa', 'in'),('bar', 'barb', 'out'),('baz', 'bazz')])
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
            cont.raise_exception('create_io_traits cannot add trait %s' % entry,
                                 TraitError)
        cont.add_trait(name, 
                       cont._build_trait(ref_name, iostat, trait))
