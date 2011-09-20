"""
The Container class.
"""

import datetime
import copy
import traceback
import re
import pprint
import socket

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

import zope.interface

from enthought.traits.api import HasTraits, Missing, Undefined, \
                                 push_exception_handler, TraitType, CTrait
from enthought.traits.trait_handlers import NoDefaultSpecified
from enthought.traits.has_traits import FunctionType, _clone_trait
from enthought.traits.trait_base import not_none, not_event

from multiprocessing import connection

from openmdao.main.variable import Variable
from openmdao.main.filevar import FileRef
from openmdao.main.slot import Slot

from openmdao.main.mp_support import ObjectManager, OpenMDAO_Proxy, is_instance, has_interface, CLASSES_TO_PROXY
from openmdao.main.rbac import rbac

from openmdao.util.log import Logger, logger, LOG_DEBUG
from openmdao.util import eggloader, eggsaver, eggobserver
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.main.interfaces import ICaseIterator, IResourceAllocator, obj_has_interface
from openmdao.main.expreval import INDEX,ATTR,CALL,SLICE

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

def get_closest_proxy(start_scope, pathname):
    """Resolve down to the closest in-process parent object
    of the object indicated by pathname.
    Returns a tuple containing (proxy_or_parent, rest_of_pathname)
    """
    obj = start_scope
    names = pathname.split('.')
    i = -1
    for i,name in enumerate(names[:-1]):
        if isinstance(obj, Container):
            obj = getattr(obj, name)
        else:
            break
    else:
        i += 1
    return (obj, '.'.join(names[i:]))


def build_container_hierarchy(dct):
    """Create a hierarchy of Containers based on the contents of a nested dict.
    There will always be a single top level scoping Container regardless of the
    contents of dct.
    """
    top = Container()
    for key,val in dct.items():
        if isinstance(val, dict): # it's a dict, so this is a Container
            top.add(key, build_container_hierarchy(val))
        else:
            setattr(top, key, val)
    return top
        

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


class Container(HasTraits):
    """ Base class for all objects having Traits that are visible 
    to the framework"""
   
    def __init__(self, doc=None):
        super(Container, self).__init__()
        
        self._managers = {}  # Object manager for remote access by authkey.
        self._depgraph = _ContainerDepends()
                          
        # for keeping track of dynamically added traits for serialization
        self._added_traits = {}
        
        self._parent = None
        self._name = None
        self._cached_traits_ = None
        
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
            if is_instance(cont, Container):
                cont._branch_moved()
 
    @property
    def name(self):
        """The name of this Container."""
        if self._name is None:
            if self.parent:
                self._name = find_name(self.parent, self)
            elif self._call_tree_rooted is False:
                self._name = ''
            else:
                return ''
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
            
    @rbac(('owner', 'user'))
    def connect(self, srcpath, destpath):
        """Connects one source variable to one destination variable. 
        When a pathname begins with 'parent.', that indicates
        that it is referring to a variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable.
            
        destpath: str
            Pathname of destination variable.
        """
        cname = None
        if not srcpath.startswith('parent.'):
            if not self.contains(srcpath):
                self.raise_exception("Can't find '%s'" % srcpath, AttributeError)
            cname, _, restofpath = srcpath.partition('.')
            if restofpath:
                child = getattr(self, cname)
                if is_instance(child, Container):
                    child.connect(restofpath, 'parent.'+destpath)
        if not destpath.startswith('parent.'):
            if not self.contains(destpath):
                self.raise_exception("Can't find '%s'" % destpath, AttributeError)
            sname = self._depgraph.get_source(destpath)
            if sname is not None:
                self.raise_exception(
                    "'%s' is already connected to source '%s'" % 
                    (destpath, sname), RuntimeError)
            cname2, _, restofpath = destpath.partition('.')
            if cname == cname2 and cname is not None:
                self.raise_exception("Can't connect '%s' to '%s'. Both variables are on the same component"%
                                     (srcpath,destpath), RuntimeError)

            if restofpath:
                child = getattr(self, cname2)
                if is_instance(child, Container):
                    child.connect('parent.'+srcpath, restofpath)
                
        self._depgraph.connect(srcpath, destpath)


    @rbac(('owner', 'user'))
    def disconnect(self, srcpath, destpath):
        """Removes the connection between one source variable and one 
        destination variable.
        """
        cname = cname2 = None
        if not srcpath.startswith('parent.'):
            if not self.contains(srcpath):
                self.raise_exception("Can't find '%s'" % srcpath, AttributeError)
            cname, _, restofpath = srcpath.partition('.')
            if restofpath:
                getattr(self, cname).disconnect(restofpath, 'parent.'+destpath)
        if not destpath.startswith('parent.'):
            if not self.contains(destpath):
                self.raise_exception("Can't find '%s'" % destpath, AttributeError)
            cname2, _, restofpath = destpath.partition('.')
            if restofpath:
                getattr(self, cname2).disconnect('parent.'+srcpath, restofpath)
        
        if cname == cname2 and cname is not None:
            self.raise_exception("Can't disconnect '%s' from '%s'. "
                                 "Both variables are on the same component" %
                                 (srcpath,destpath), RuntimeError)

        self._depgraph.disconnect(srcpath, destpath)

    #TODO: get rid of any notion of valid/invalid from Containers.  If they have
    # no execute, they can have no inputs/outputs, which means that validity should have
    # no meaning for them.
    def is_valid(self):
        return True

    def get_trait ( self, name, copy = False ):
        """Returns the trait indicated by name, or None if not found.  No recursive
        search is performed if name contains dots.  This is a replacement
        for the trait() method on HasTraits objects, because that method
        can return traits that shouldn't exist. DO NOT use the trait() function
        as a way to determine the existence of a trait.
        """
        if self._cached_traits_ is None:
            self._cached_traits_ = self.traits()
            self._cached_traits_.update(self._instance_traits())
        trait = self._cached_traits_.get(name)
        if copy and trait:
            return self.trait(name, copy=copy)
        return trait

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
        result._cached_traits_ = None
        olditraits = self._instance_traits()
        newtraits = result._instance_traits()
        newtraits.update(result.traits())
        for name, trait in olditraits.items():
            if trait.type is not 'event' and name not in newtraits:
                result.add_trait(name, _clone_trait(trait))
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
        state['_cached_traits_'] = None
        
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        super(Container, self).__setstate__({})
        self.__dict__.update(state)

        # restore dynamically added traits, since they don't seem
        # to get restored automatically
        self._cached_traits_ = None
        traits = self._alltraits()
        for name, trait in self._added_traits.items():
            if name not in traits:
                self.add_trait(name, trait)

        # Fix property traits that were not just added above.
        # For some reason they lost 'get()', but not 'set()' capability.
        for name, trait in traits.items():
            try:
                get = trait.trait_type.get
            except AttributeError:
                continue
            if get is not None:
                if name not in self._added_traits:
                    val = getattr(self, name)
                    self.remove_trait(name)
                    self.add_trait(name, trait)
                    setattr(self, name, val)

        # make sure all input callbacks are in place.  If callback is
        # already there, this will have no effect. 
        for name, trait in self._alltraits().items():
            if trait.iotype == 'in':
                self.on_trait_change(self._input_trait_modified, name)
            
        # after unpickling, implicitly defined traits disappear, so we have to
        # recreate them by assigning them to themselves.       
        #TODO: I'm probably missing something. There has to be a better way to
        #      do this...
        for name, val in self.__dict__.items():
            if not name.startswith('__') and not self.get_trait(name) :
                setattr(self, name, val) # force def of implicit trait
                
        self._cached_traits_ = None

    def add_trait(self, name, trait):
        """Overrides HasTraits definition of *add_trait* in order to
        keep track of dynamically added traits for serialization and to add
        callbacks for input Variables.
        """
        # When a trait with sub-traits is added (like a List or Dict),
        # HasTraits calls add_trait AGAIN for the sub-trait, so we
        # just detect it here and pass it through
        if name.endswith('_items') and trait.type == 'event':
            super(Container, self).add_trait(name, trait)
            return
        
        #FIXME: saving our own list of added traits shouldn't be necessary...
        self._added_traits[name] = trait
        super(Container, self).add_trait(name, trait)
        if self._cached_traits_ is not None:
            self._cached_traits_[name] = self.trait(name)
        
        # if it's an input trait, register a callback to be called whenever it's changed
        if trait.iotype == 'in':
            self.on_trait_change(self._input_trait_modified, name)
        
    def remove_trait(self, name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """
        ## this just forces the regeneration (lazily) of the lists of
        ## inputs, outputs, and containers
        #self._trait_added_changed(name)
        try:
            del self._added_traits[name]
        except KeyError:
            pass
        try:
            del self._cached_traits_[name]
        except (KeyError, TypeError):
            pass
        
        # remove the callback if it's an input trait
        trait = self.get_trait(name)
        if trait and trait.iotype == 'in':
            self.on_trait_change(self._input_trait_modified, name, remove=True)

        super(Container, self).remove_trait(name)
            
    # call this if any trait having 'iotype' metadata of 'in' is changed
    def _input_trait_modified(self, obj, name, old, new):
        self._input_check(name, old)
        self._call_execute = True
        self._input_updated(name)
            
    def _input_check(self, name, old):
        """This raises an exception if the specified input is attached
        to a source.
        """
        if self._depgraph.get_source(name):
            # bypass the callback here and set it back to the old value
            self._trait_change_notify(False)
            try:
                setattr(self, name, old)
            finally:
                self._trait_change_notify(True)
            self.raise_exception(
                "'%s' is already connected to source '%s' and "
                "cannot be directly set"%
                (name, self._depgraph.get_source(name)), RuntimeError)
            
    def _input_nocheck(self, name, old):
        """This method is substituted for _input_check to avoid source
        checking during a set() call when we've already verified the source.
        """
        pass
    
    def _input_updated(self, name):
        """This just exists so inherited classes can add behavior when
        inputs are set.
        """
        pass

    @rbac(('owner', 'user'))
    def get_wrapped_attr(self, name):
        """If the variable can return an AttrWrapper, then this
        function will return that, with the value set to the current value of
        the variable. Otherwise, it functions like *getattr*, just
        returning the value of the variable. Raises an exception if the
        variable cannot be found. The value will be copied if the variable has
        a 'copy' metadata attribute that is not None. Possible values for
        'copy' are 'shallow' and 'deep'.
        """
        scopename, _, restofpath = name.partition('.')
        if restofpath:
            if scopename == 'parent':
                return self.parent.get_wrapped_attr(name[7:])
            obj = getattr(self, scopename)
            if is_instance(obj, Container):
                return obj.get_wrapped_attr(restofpath)
            else:
                return getattr(obj, restofpath)
        
        trait = self.get_trait(name)
        if trait is None:
            self.raise_exception("trait '%s' does not exist" %
                                 name, AttributeError)
            
        # trait itself is most likely a CTrait, which doesn't have
        # access to member functions on the original trait, aside
        # from validate and one or two others, so we need to get access 
        # to the original trait which is held in the 'trait_type' attribute.
        ttype = trait.trait_type
        getwrapper = ttype.get_val_wrapper
        val = getattr(self, name)
        # copy value if 'copy' found in metadata
        if ttype.copy:
            val = _copydict[ttype.copy](val)
        if getwrapper is not None:
            return getwrapper(val)
        
        return val
        
    def add(self, name, obj):
        """Add an object to this Container.
        Returns the added object.
        """
        if '.' in name:
            self.raise_exception(
                'add does not allow dotted path names like %s' %
                name, ValueError)
        if obj == self:
            self.raise_exception('cannot make an object a child of itself',
                                 RuntimeError)
        if is_instance(obj, Container):
            if isinstance(obj, OpenMDAO_Proxy):
                obj.parent = self._get_proxy(obj)
            else:
                obj.parent = self
            # if an old child with that name exists, remove it
            if self.contains(name) and getattr(self, name):
                self.remove(name)
            obj.name = name
            setattr(self, name, obj)
            if self._cached_traits_ is None:
                self.get_trait(name)
            else:
                self._cached_traits_[name] = self.trait(name)
            # if this object is already installed in a hierarchy, then go
            # ahead and tell the obj (which will in turn tell all of its
            # children) that its scope tree back to the root is defined.
            if self._call_tree_rooted is False:
                obj.tree_rooted()
        elif is_instance(obj, TraitType):
            self.add_trait(name, obj)
        else:
            self.raise_exception("'"+str(type(obj))+
                    "' object is not an instance of Container.",
                    TypeError)
        return obj

    def _get_proxy(self, proxy):
        """
        Return :class:`OpenMDAO_Proxy` for self usable by `proxy`.
        We create a manager for each access type.
        In addition, to avoid having to (remotely) manipulate a server's
        `allowed_hosts`, we use a separate manager for each client accessing
        via AF_INET from a unique host.
        """
        addr_type = connection.address_type(proxy._token.address)
        addr = proxy._token.address[0] if addr_type == 'AF_INET' else None
        key = (addr_type, addr, proxy._authkey)
        try:
            manager = self._managers[key]
        except KeyError:
            if addr_type == 'AF_INET':
                ip_addr = socket.gethostbyname(socket.gethostname())
                address = (ip_addr, 0)
                allowed_hosts = [addr]
            else:
                address = None
                allowed_hosts = None

            name = self.name or 'parent'
            access = addr if addr_type == 'AF_INET' else addr_type
            name = '%s-cb-%s' % (name, access)
            manager = ObjectManager(self, address, authkey=proxy._authkey,
                                    name=name, allowed_hosts=allowed_hosts)
            self._managers[key] = manager
        return manager.proxy
        
    def remove(self, name):
        """Remove the specified child from this container and remove any
        public trait objects that reference that child. Notify any
        observers.
        """
        if '.' in name:
            self.raise_exception(
                'remove does not allow dotted path names like %s' %
                                 name, NameError)
        trait = self.get_trait(name)
        if trait is not None:
            obj = getattr(self, name)
            # for Slot traits, set their value to None but don't remove
            # the trait
            if trait.is_trait_type(Slot):
                try:
                    setattr(self, name, None)
                except TypeError as err:
                    self.raise_exception(str(err), RuntimeError)
            else:
                self.remove_trait(name)
            return obj
        else:
            self.raise_exception("cannot remove '%s': not found"%
                                 name, AttributeError)

    @rbac(('owner', 'user'))
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
            match_dict = self._alltraits(**metadata)
            
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
                obj = getattr(self, name, Missing)
                # In some components with complex loading behavior (like NPSSComponent), 
                # we can have a temporary situation during loading
                # where there are traits that don't point to anything,
                # so check for them here and skip them if they don't point to anything.
                if obj is not Missing:
                    if is_instance(obj, Container) and id(obj) not in visited:
                        if not recurse:
                            yield (name, obj)
                    elif trait.iotype is not None and id(trait) not in visited:
                        visited.add(id(trait))
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
        return [n for n, v in self.items() if is_instance(v, Container)]
    
    def _alltraits(self, traits=None, events=False, **metadata):
        """This returns a dict that contains all traits (class and instance)
        that match the given metadata.
        """
        if traits is None:
            if self._cached_traits_:
                traits = self._cached_traits_
            else:
                traits = self.traits()  # don't pass **metadata here
                traits.update(self._instance_traits())
                self._cached_traits_ = traits
            
        result = {}
        for name, trait in traits.items():
            if not events and trait.type is 'event':
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
    
    @rbac(('owner', 'user'))
    def contains(self, path):
        """Return True if the child specified by the given dotted path
        name is contained in this Container. 
        """
        childname, _, restofpath = path.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing:
                return False
            elif is_instance(obj, Container):
                return obj.contains(restofpath)
            else:
                return hasattr(obj, restofpath)
        return hasattr(self, path)
    
    def _get_metadata_failed(self, traitpath, metaname):
        self.raise_exception("Couldn't find metadata for trait %s" % traitpath,
                             AttributeError)

    @rbac(('owner', 'user'))
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
            if obj is Missing or not is_instance(obj, Container):
                return self._get_metadata_failed(traitpath, metaname)
            return obj.get_metadata(restofpath, metaname)
            
        t = self.get_trait(traitpath)
        if not t:
            return self._get_metadata_failed(traitpath, metaname)
        t = t.trait_type
        if metaname is None:
            mdict = t._metadata.copy()
            mdict.setdefault('vartypename', t.__class__.__name__)
            return mdict
        else:
            val = t._metadata.get(metaname, None)
            # vartypename isn't present in the metadata of traits
            # that don't inherit from Variable, so fake it out here
            # so we'll be consistent across all traits
            if val is None:
                if metaname == 'vartypename':
                    return t.__class__.__name__
            return val

    def _get_failed(self, path, index=None):
        """If get() cannot locate the variable specified by the given
        path, raise an exception.  Inherited classes can override this
        to return the value of the specified variable.
        """
        self.raise_exception(
            "object has no attribute '%s'" % path, 
            AttributeError)
        
    @rbac(('owner', 'user'), proxy_types=[FileRef])
    def get(self, path, index=None):
        """Return the object specified by the given path, which may 
        contain '.' characters.  *index*, if not None,
        should be either a list of non-tuple hashable objects, at most one 
        for each array dimension of the target value, or a list of tuples of 
        the form (operation_id, stuff).
              
        The forms of the various tuples are:
        
        ::
        
            INDEX:   (0, idx)  
                where idx is some hashable value
            ATTR:    (1, name) 
                where name is the attribute name
            CALL:    (2, args, kwargs) 
                where args is a list of values and kwargs is a list of 
                tuples of the form (keyword,value).
                kwargs can be left out if empty.  args can be left out 
                if empty as long as kwargs are also empty, for example, 
                (2,) and (2,[],[('foo',1)]) are valid but (2,[('foo',1)]) is not.
            SLICE:   (3, lower, upper, step) 
                All members must be present and should have a value 
                of None if not set.

        If you want to use a tuple as a key into a dict, you'll have to
        nest your key tuple inside of an INDEX tuple to avoid ambiguity, 
        for example, (0, my_tuple)
        """
        childname, _, restofpath = path.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing or not is_instance(obj, Container):
                return self._get_failed(path, index)
            return obj.get(restofpath, index)
        else:
            obj = getattr(self, path, Missing)
            if obj is Missing:
                return self._get_failed(path, index)
            if index is None:
                return obj
            else:
                for idx in index:
                    obj = self._process_index_entry(obj, idx)
                return obj
     
    def _set_failed(self, path, value, index=None, src=None, force=False):
        """If set() cannot locate the specified variable, raise an exception.
        Inherited classes can override this to locate the variable elsewhere
        and set its value.
        """
        self.raise_exception("object has no attribute '%s'" % path, 
                             AttributeError)
        
    def _check_source(self, path, src):
        """Raise an exception if the given source variable is not the one
        that is connected to the destination variable specified by 'path'.
        """
        source = self._depgraph.get_source(path)
        if source is not None and src != source:
            self.raise_exception(
                "'%s' is connected to source '%s' and cannot be "
                "set by source '%s'" %
                (path,source,src), RuntimeError)

    @rbac(('owner', 'user'))
    def set(self, path, value, index=None, src=None, force=False):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints. *index*, if not None,
        should be either a list of non-tuple hashable objects, at most one 
        for each array dimension of the target value, or a list of tuples of 
        the form (operation_id, stuff).
              
        The forms of the various tuples are:
        
        ::
        
            INDEX:   (0, idx)  
                where idx is some hashable value
            ATTR:    (1, name) 
                where name is the attribute name
            CALL:    (2, args, kwargs) 
                where args is a list of values and kwargs is a list of 
                tuples of the form (keyword,value).
                kwargs can be left out if empty.  args can be left out 
                if empty as long as kwargs are also empty, for example, 
                (2,) and (2,[],[('foo',1)]) are valid but (2,[('foo',1)]) is not.
            SLICE:   (3, lower, upper, step) 
                All members must be present and should have a value 
                of None if not set.

        If you want to use a tuple as a key into a dict, you'll have to
        nest your key tuple inside of an INDEX tuple to avoid ambiguity, 
        for example, (0, my_tuple)
        """ 
        childname, _, restofpath = path.partition('.')
        if restofpath:
            obj = getattr(self, childname, Missing)
            if obj is Missing or not is_instance(obj, Container):
                return self._set_failed(path, value, index, src, force)
            if src is not None:
                src = 'parent.'+src
            obj.set(restofpath, value, index, src=src, force=force)
        else:
            trait = self.get_trait(path)
            if trait:
                if trait.iotype == 'in': # setting an input, so have to check source
                    if not force:
                        self._check_source(path, src)
                    if index is None:
                        # bypass input source checking
                        chk = self._input_check
                        self._input_check = self._input_nocheck
                        try:
                            setattr(self, path, value)
                        finally:
                            self._input_check = chk
                        # Note: This was done to make foo.bar = 3 behave the
                        # same as foo.set('bar', 3).
                        # Without this, the output of the comp was
                        # always invalidated when you call set_parameters.
                        # This meant that component was always executed
                        # even when the inputs were unchanged.
                        # _call_execute is set in the on-trait-changed
                        # callback, so it's a good test for whether the
                        # value changed.
                        if hasattr(self, "_call_execute") and self._call_execute:
                            self._input_updated(path)
                    else:  # array index specified
                        self._index_set(path, value, index)
                elif index:  # array index specified for output
                    self._index_set(path, value, index)
                else: # output
                    setattr(self, path, value)
            else:
                return self._set_failed(path, value, index, src, force)

    def _process_index_entry(self, obj, idx):
        """Return a new object based on a starting object and some operation
        indicated by idx that can be either an index into a container, an 
        attribute access, or a fuction call.  idx can be non-tuple hashable
        object, which will be interpreted as an index to a container, or it can
        be a tuple of the form (operation_id, stuff)
            
            where operation_id is as follows (the named constants are defined in expreval.py):
              INDEX = 0
              ATTR = 1
              CALL = 2
              SLICE = 3
              
        On the off chance that you want to use a tuple as a key into a dict, you'll have to
        nest your key tuple inside of an INDEX tuple to avoid ambiguity, e.g., (INDEX, my_tuple)
              
        The forms of the various tuples are:
              
              INDEX:   (0, idx)  where idx is some hashable value
              ATTR:    (1, name) where name is the attribute name
              CALL:    (2, args, kwargs) where args is a list of values and kwargs
                                         is a list of tuples of the form (keyword,value).
                                         kwargs can be left out if empty.  args
                                         can be left out if empty as long as kwargs
                                         are also empty, for example, (2,) and 
                                         (2,[],[('foo',1)]) are valid
                                         but (2,[('foo',1)]) is not.
              SLICE:   (3, lower, upper, step) All members must be present and should
                                               have a value of None if not set.
        """
        if not isinstance(idx, tuple):
            return obj[idx]
        if idx[0] == INDEX:
            return obj[idx[1]]
        elif idx[0] == ATTR:
            return getattr(obj, idx[1])
        elif idx[0] == CALL:
            if len(idx) == 1:
                return obj.__call__()
            else:
                args = idx[1]
                if len(idx) == 3:
                    kwargs = dict(idx[2])
                else:
                    kwargs = {}
                return obj.__call__(*args, **kwargs)
        elif idx[0] == SLICE:
            return obj.__getitem__(slice(idx[1],idx[2],idx[3]))
        
        self.raise_exception("invalid index: %s" % idx, ValueError)
        
    def _index_set(self, name, value, index):
        obj = getattr(self, name)
        for idx in index[:-1]:
            obj = self._process_index_entry(obj, idx)
        idx = index[-1]
        old = self._process_index_entry(obj, idx)
        if isinstance(idx, tuple):
            if idx[0] == INDEX:
                obj[idx[1]] = value
            elif idx[0] == ATTR:
                setattr(obj, idx[1], value)
            elif idx[0] == SLICE:
                obj.__setitem__(slice(idx[1],idx[2],idx[3]), value)
        else:
            obj[idx] = value
                
        # setting of individual Array entries or sub attributes doesn't seem to trigger
        # _input_trait_modified, so do it manually
        # FIXME: if people register other callbacks on a trait, they won't
        #        be called if we do it this way
        if old != value:
            self._call_execute = True
            self._input_updated(name)
            
    def save_to_egg(self, name, version, py_dir=None, src_dir=None,
                    src_files=None, child_objs=None, dst_dir=None,
                    observer=None, need_requirements=True):
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

        observer: callable
            Will be called via an :class:`EggObserver`.

        need_requirements: bool
            Passed to :meth:`eggsaver.save_to_egg`.

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
                                        self._logger, observer.observer,
                                        need_requirements)
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
    def load_from_eggfile(filename, observer=None, log=None):
        """Extract files in egg to a subdirectory matching the saved object
        name and then load object graph state.

        filename: string
            Name of egg file to be loaded.

        observer: callable
            Will be called via an :class:`EggObserver`.

        log: :class:`logging.Logger`
            Used for logging progress, default is root logger.

        Returns the root object.
        """
        # Load from file gets everything.
        entry_group = 'openmdao.top'
        entry_name = 'top'
        log = log or logger
        return eggloader.load_from_eggfile(filename, entry_group, entry_name,
                                           log, observer)

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
            Name for root object.

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
        for name in self.list_containers():
            getattr(self, name).post_load()

    @rbac('owner')
    def pre_delete(self):
        """Perform any required operations before the model is deleted."""
        for name in self.list_containers():
            getattr(self, name).pre_delete()
            
    @rbac(('owner', 'user'), proxy_types=[CTrait])
    def get_dyn_trait(self, pathname, iotype=None, trait=None):
        """Returns a trait if a trait with the given pathname exists, possibly
        creating it "on-the-fly" and adding its Container. If an attribute exists
        with the given pathname but no trait is found or can be created, or if
        pathname references a trait in a parent scope, None will be returned.
        If no attribute exists with the given pathname within this scope, an
        AttributeError will be raised.
        
        pathname: str
            Pathname of the desired trait.  May contain dots.
            
        iotype: str (optional)
            Expected iotype of the trait.
            
        trait: TraitType (optional)
            Trait to be used for validation.
        """
        if pathname.startswith('parent.'):
            return None
        cname, _, restofpath = pathname.partition('.')
        if restofpath:
            child = getattr(self, cname)
            if is_instance(child, Container):
                return child.get_dyn_trait(restofpath, iotype, trait)
            else:
                if deep_hasattr(child, restofpath):
                    return None
        else:
            trait = self.get_trait(cname)
            if trait is not None:
                if iotype is not None and trait.iotype != iotype:
                    self.raise_exception('%s must be an %s variable' % 
                                         (pathname, _iodict[iotype]),
                                         RuntimeError)
                return trait
            elif trait is None and self.contains(cname):
                return None

        self.raise_exception("Cannot locate variable named '%s'" %
                             pathname, AttributeError)

    @rbac(('owner', 'user'))
    def get_trait_typenames(self, pathname, iotype=None):
        """Return names of the 'final' type (bypassing passthrough traits)
        for `pathname` using :meth:`get_dyn_trait`. Used by dynamic wrappers
        to determine the type of variable to wrap. The returned list is a
        depth-first traversal of the class hierarchy.

        pathname: str
            Pathname of the desired trait. May contain dots.
            
        iotype: str (optional)
            Expected iotype of the trait.
        """
        trait = self.get_dyn_trait(pathname, iotype=iotype)
        if trait is None:
            return []

        trait = trait.trait_type or trait.trait or trait
        if trait.target:  # PassthroughTrait, PassthroughProperty
            trait = self.get_dyn_trait(trait.target)
            try:
                ttype = trait.trait_type
            except AttributeError:
                pass
            else:
                if ttype is not None:
                    trait = ttype

        def _bases(cls, names):
            names.append('%s.%s' % (cls.__module__, cls.__name__))
            for base in cls.__bases__:
                _bases(base, names)
        
        names = []
        _bases(type(trait), names)
        return names

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        full_msg = "%s: " % self.get_pathname() + msg
        self._logger.error(msg)
        raise exception_class(full_msg)
    

# By default we always proxy Containers.
CLASSES_TO_PROXY.append(Container)

    
# Some utility functions


def _get_entry_group(obj):
    """Return entry point group for given object type."""
    if _get_entry_group.group_map is None:
        # Fill-in here to avoid import loop.
        from openmdao.main.component import Component
        from openmdao.main.driver import Driver

        # Entry point definitions taken from plugin-guide.
        # Order should be from most-specific to least.
        _get_entry_group.group_map = [
            (Variable,           'openmdao.variable'),
            (Driver,             'openmdao.driver'),
            (ICaseIterator,      'openmdao.case_iterator'),
            (IResourceAllocator, 'openmdao.resource_allocator'),
            (Component,          'openmdao.component'),
            (Container,          'openmdao.container'),
        ]

    for cls, group in _get_entry_group.group_map:
        if issubclass(cls, zope.interface.Interface):
            if cls.providedBy(obj):
                return group
        else:
            if isinstance(obj, cls):
                return group
    return None

_get_entry_group.group_map = None  # Map from class/interface to group name.


def dump(cont, recurse=False, stream=None, **metadata):
    """Print all items having specified metadata and
    their corresponding values to the given stream. If the stream
    is not supplied, it defaults to *sys.stdout*.
    """
    pprint.pprint(dict([(n, str(v)) 
                    for n, v in cont.items(recurse=recurse, 
                                           **metadata)]),
                  stream)


def find_name(parent, obj):
    """Find the given object in the specified parent and return its name 
    in the parent's __dict__.  There could be multiple names bound to a
    given object. Only the first name found is returned.
    
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
    names = pathname.split('.')
    for name in names[:-1]:
        obj = getattr(obj, name)
    if is_instance(obj, Container):
        objtrait = obj.get_trait(names[-1])
    elif isinstance(obj, HasTraits):
        objtrait = obj.trait(names[-1])
    else:
        objtrait = None
    return (objtrait, getattr(obj, names[-1]))


def create_io_traits(cont, obj_info, iotype='in'):
    """Create io trait(s) specified by the contents of obj_info. Calls
    build_trait() on the scoping object, which can be overridden by 
    subclasses, to create each trait.
    
    obj_info is assumed to be either a string, a tuple, or a list
    that contains strings and/or tuples. Tuples must contain a name and an
    'internal' name, and may optionally contain an iotype and a validation trait.
    The first name is the one that will be used to access the trait's attribute
    in the Container, while the second name represents some alternate naming 
    scheme within the Container.
    
    For example, the following are valid calls:

    create_io_traits(obj, 'foo')
    create_io_traits(obj, ['foo','bar','baz'])
    create_io_traits(obj, ('foo', 'foo_alias', 'in', some_trait), 'bar')
    create_io_traits(obj, [('foo', 'fooa', 'in'),('bar', 'barb', 'out'),('baz', 'bazz')])
    
    The newly created traits are added to the specified Container.
    """
    if isinstance(obj_info, (basestring, tuple)):
        it = [obj_info]
    else:
        it = obj_info

    for entry in it:
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
                                 RuntimeError)
        cont.add_trait(name, cont.build_trait(ref_name, iostat, trait))
