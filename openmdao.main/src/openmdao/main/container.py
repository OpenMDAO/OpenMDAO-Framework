"""
The Container class.
"""

import datetime
import copy
import pprint
import socket
import sys
import weakref
# the following is a monkey-patch to correct a problem with
# copying/deepcopying weakrefs There is an issue in the python issue tracker
# regarding this, but it isn't fixed yet.

# pylint: disable=W0212
copy._copy_dispatch[weakref.ref] = copy._copy_immutable
copy._deepcopy_dispatch[weakref.ref] = copy._deepcopy_atomic
copy._deepcopy_dispatch[weakref.KeyedRef] = copy._deepcopy_atomic
# pylint: enable=W0212

# pylint apparently doesn't understand namespace packages...
# pylint: disable=E0611,F0401

from zope.interface import Interface, implements

from numpy import ndarray

from traits.api import HasTraits, Missing, Python, \
                       push_exception_handler, TraitType, CTrait
from traits.has_traits import FunctionType, _clone_trait, MetaHasTraits
from traits.trait_base import not_none

from multiprocessing import connection

from openmdao.main.datatypes.file import FileRef
from openmdao.main.datatypes.list import List
from openmdao.main.datatypes.slot import Slot
from openmdao.main.datatypes.vtree import VarTree
from openmdao.main.interfaces import ICaseIterator, IResourceAllocator, \
                                     IContainer, \
                                     IVariableTree, IContainerProxy, IOverrideSet
#from openmdao.main.index import get_indexed_value, deep_hasattr, \
                                #INDEX, ATTR, SLICE, _index_functs
from openmdao.main.mp_support import ObjectManager, \
                                     is_instance, CLASSES_TO_PROXY, \
                                     has_interface
from openmdao.main.rbac import rbac
from openmdao.main.variable import Variable, is_legal_name, _missing
from openmdao.main.array_helpers import flattened_value, get_index

from openmdao.util.log import Logger, logger
from openmdao.util import eggloader, eggsaver, eggobserver
from openmdao.util.eggsaver import SAVE_CPICKLE
from openmdao.util.typegroups import int_types, complex_or_real_types

_copydict = {
    'deep': copy.deepcopy,
    'shallow': copy.copy
}

_iodict = {'out': 'output', 'in': 'input'}

__missing__ = object()

def get_closest_proxy(obj, pathname):
    """Returns a tuple of the form (val, restofpath), where val
    is either the object specified by dotted name 'pathname'
    within obj, or the closest in-process proxy object that can be
    resolved.  If val is a proxy, restofpath will contain the
    remaining part of pathname needed to resolve the desired attribute
    within the proxy.  Otherwise, val is the actual desired attribute
    and restofpath is the empty string.
    """
    names = pathname.split('.')

    i = 0
    for name in names:
        if IContainerProxy.providedBy(obj):
            return (obj, '.'.join(names[i:]))

        try:
            obj = getattr(obj, name)
        except AttributeError:
            break
        i += 1

    return (obj, '.'.join(names[i:]))


def proxy_parent(obj, pathname):
    """Returns a tuple of the form (par, restofpath), where par
    is either the parent of the object specified by dotted name 'pathname'
    within obj, or the closest in-process proxy object that can be
    resolved.  restofpath will contain the
    remaining part of pathname needed to resolve the desired attribute
    within the parent or proxy object.
    """
    names = pathname.split('.')

    i = 0
    for name in names[:-1]:
        if IContainerProxy.providedBy(obj):
            return (obj, '.'.join(names[i:]))

        try:
            obj = getattr(obj, name)
        except AttributeError:
            break
        i += 1

    return (obj, '.'.join(names[i:]))


# this causes any exceptions occurring in trait handlers to be re-raised.
# Without this, the default behavior is for the exception to be logged and not
# re-raised.
push_exception_handler(handler=lambda o, t, ov, nv: None,
                       reraise_exceptions=True,
                       main=True,
                       locked=True)


class _MetaSafe(MetaHasTraits):
    """ Tries to keep users from shooting themselves in the foot. """

    def __new__(mcs, class_name, bases, class_dict):
        # Check for overwrite of something that shouldn't be.
        for name, obj in class_dict.items():
            if isinstance(obj, Variable):
                for base in bases:
                    if name in base.__dict__:
                        raise NameError('%s overrides attribute %r of %s'
                                        % (class_name, name, base.__name__))
        return super(_MetaSafe, mcs).__new__(mcs, class_name, bases, class_dict)


class SafeHasTraits(HasTraits):
    """
    Special :class:`HasTraits` which is configured such that the class is
    checked for any :class:`Variable` which might override an existing
    attribute in any base class.
    """
    # Doing this in Container breaks implements(IContainer) such that
    # implementedBy() would return False.
    __metaclass__ = _MetaSafe


def _check_bad_default(name, trait, obj=None):
    if trait.vartypename not in ['Slot', 'VarTree'] and trait.required is True \
       and not trait.assumed_default and trait._illegal_default_ is True:

        msg = "variable '%s' is required and cannot have a default value" % name
        if obj is None:
            raise RuntimeError(msg)
        else:
            obj.raise_exception(msg, RuntimeError)


class Container(SafeHasTraits):
    """ Base class for all objects having Traits that are visible
    to the framework"""

    implements(IContainer)

    def __init__(self):
        self._parent = None  # Define these now for easier debugging during
        self._name = None    # deepcopy operations of superclass.
        super(Container, self).__init__()

        self._call_cpath_updated = True
        self._call_configure = True

        self._managers = {}  # Object manager for remote access by authkey.

        # for keeping track of dynamically added traits for serialization
        self._added_traits = {}

        # keep track of compiled expressions to save some overhead
        self._getcache = {}
        self._setcache = {}
        self._copycache = {}

        self._cached_traits_ = None
        self._repair_trait_info = None

        # Locally set metadata, overrides trait metadata.
        self._trait_metadata = {}

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

        # Similarly, create per-instance VariableTrees for VarTree traits.
        for name, obj in self.__class__.__dict__['__class_traits__'].items():
            ttype = obj.trait_type
            if isinstance(ttype, VarTree):
                variable_tree = getattr(self, name)
                if not obj.required:
                    new_tree = variable_tree.copy()
                    setattr(self, name, new_tree)

            if obj.required:
                _check_bad_default(name, obj, self)

    @property
    def parent(self):
        """The parent Container of this Container."""
        return self._parent

    @parent.setter
    def parent(self, value):
        """This is called when the parent attribute is changed."""
        if self._parent is not value:
            self._parent = value
            self._fix_loggers(self, recurse=True)
            self._branch_moved()

    def _branch_moved(self):
        self._call_cpath_updated = True
        for n, cont in self.items():
            if is_instance(cont, Container) and cont is not self._parent:
                cont._branch_moved()

    @property
    def name(self):
        """The name of this Container."""
        if self._name is None:
            if self.parent:
                self._name = find_name(self.parent, self)
                self._fix_loggers(self, recurse=True)
            elif self._call_cpath_updated is False:
                self._name = ''
            else:
                return ''
        return self._name

    @name.setter
    def name(self, name):
        """Sets the name of this Container."""
        if not is_legal_name(name):
            raise NameError("name '%s' contains illegal characters" % name)
        if self._name != name:
            self._name = name
            self._fix_loggers(self, recurse=True)

    def _fix_loggers(self, container, recurse):
        """Fix loggers starting from `container`."""
        container._logger.rename(container.get_pathname().replace('.', ','))
        if recurse:
            for name in container.list_containers():
                obj = getattr(container, name)
                self._fix_loggers(obj, recurse)

    @rbac(('owner', 'user'))
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

    def get_trait(self, name, copy=False):
        """Returns the trait indicated by name, or None if not found. No
        recursive search is performed if name contains dots. This is a
        replacement for the trait() method on HasTraits objects because that
        method can return traits that shouldn't exist. DO NOT use the trait()
        function as a way to determine the existence of a trait.
        """
        if self._cached_traits_ is None:
            self._cached_traits_ = self.traits()
            self._cached_traits_.update(self._instance_traits())
        if copy:
            if self._cached_traits_.get(name):
                return self.trait(name, copy=copy)
            else:
                return None
        else:
            return self._cached_traits_.get(name)

    #
    #  HasTraits overrides
    #

    def __deepcopy__(self, memo):
        """ Overrides deepcopy for HasTraits because otherwise we lose instance
        traits when we copy. :(
        """
        id_self = id(self)
        if id_self in memo:
            return memo[id_self]

        # Make sure HasTraits is performing all deepcopies. We need this
        # in order for our sub-components and objects to get deep-copied.
        memo['traits_copy_mode'] = "deep"

        saved_p = self._parent
        saved_c = self._cached_traits_
        saved_s = self._setcache
        saved_g = self._getcache
        self._parent = None
        self._cached_traits_ = None
        self._getcache = {}
        self._setcache = {}
        try:
            result = super(Container, self).__deepcopy__(memo)
        finally:
            self._parent = saved_p
            self._cached_traits_ = saved_c
            self._getcache = saved_g
            self._setcache = saved_s

        # Instance traits are not created properly by deepcopy, so we need
        # to manually recreate them. Note, self._added_traits is the most
        # accurate listing of them. Self._instance_traits includes some
        # extra stuff.
        olditraits = self._instance_traits()
        for name, trait in olditraits.items():
            if trait.type is not 'event' and name in self._added_traits:
                if isinstance(trait.trait_type, VarTree):
                    if name not in result._added_traits:
                        result.add_trait(name, _clone_trait(trait))
                else:
                    result.add_trait(name, _clone_trait(trait))
                if name in self.__dict__:  # Not true with VarTree
                    result.__dict__[name] = copy.deepcopy(self.__dict__[name])

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
        state['_getcache'] = {}
        state['_setcache'] = {}
        return state

    def __setstate__(self, state):
        """Restore this container's state. Components that need to do some
        restore operations (such as connecting to a remote server or loading
        a model file) before any get()/set() is attempted will generate
        exceptions. However, the complete set of local traits must be available
        since restore operations may depend on knowing what to restore.

        Here we swallow errors from 'special' components and remember to retry
        the operation in _repair_traits(), called by post_load(). Persistent
        problems will be reported there.
        """
        super(Container, self).__setstate__({})
        self.__dict__.update(state)
        self._repair_trait_info = {}

        # restore dynamically added traits, since they don't seem
        # to get restored automatically
        self._cached_traits_ = None
        traits = self._alltraits()
        for name, trait in self._added_traits.items():
            if name not in traits:
                self.add_trait(name, trait, refresh=False)

        # Fix property traits that were not just added above.
        # For some reason they lost 'get()', but not 'set()' capability.
        fixups = []
        for name, trait in traits.items():
            try:
                get = trait.trait_type.get
            except AttributeError:
                continue
            if get is not None:
                if name not in self._added_traits:
                    try:
                        val = getattr(self, name)
                        self.remove_trait(name)
                        self.add_trait(name, trait)
                        setattr(self, name, val)
                    except Exception as exc:
                        self._logger.warning('Initial fix of %s (%s) failed: %s',
                                             name, val, exc)
                        fixups.append((name, trait))
        self._repair_trait_info['property'] = fixups

        # after unpickling, implicitly defined traits disappear, so we have to
        # recreate them by assigning them to themselves.
        #TODO: I'm probably missing something. There has to be a better way to
        #      do this...
        fixups = []
        for name, val in self.__dict__.items():
            if not name.startswith('__') and not self.get_trait(name):
                try:
                    setattr(self, name, val)  # force def of implicit trait
                except Exception as exc:
                    self._logger.warning('Initial fix of %s (%s) failed: %s',
                                         name, val, exc)
                    fixups.append((name, val))
        self._repair_trait_info['implicit'] = fixups

        # Fix List traits so they can be deepcopied.
        # (they lose their 'trait' attribute and direct setting doesn't work)
        fixups = []
        for name, trait in self._alltraits().items():
            if isinstance(trait.trait_type, List):
                try:
                    setattr(self, name, getattr(self, name))
                except Exception as exc:
                    self._logger.warning('Initial fix of %s (%s) failed: %s',
                                         name, val, exc)
                    fixups.append(name)
        self._repair_trait_info['list'] = fixups

        self._cached_traits_ = None

    def _repair_traits(self):
        """To be called after loading a pickled state, but *after* any
        post_load() processing (to handle cases where a component needs
        to do internal setup before being used to get/set a trait).
        This retries failed operations recorded in __setstate__().
        """
        if self._repair_trait_info is None:
            # We've already repaired this object.
            return

        for name, trait in self._repair_trait_info['property']:
            val = getattr(self, name)
            self.remove_trait(name)
            self.add_trait(name, trait)
            setattr(self, name, val)

        for name, val in self._repair_trait_info['implicit']:
            setattr(self, name, val)

        for name in self._repair_trait_info['list']:
            setattr(self, name, getattr(self, name))

        self._repair_trait_info = None

    @classmethod
    def add_class_trait(cls, name, *trait):
        """Overrides HasTraits definition of *add_class_trait* to
        try to keep from clobbering framework stuff.
        """
        bases = [cls]
        bases.extend(cls.__bases__)
        for base in bases:
            if name in base.__dict__:
                raise NameError('Would override attribute %r of %s'
                                % (name, base.__name__))

        for t in trait:
            _check_bad_default(name, t)
            break  # just check the first arg in the list

        if name in cls._trait_metadata:
            del cls._trait_metadata[name]  # Invalidate.

        return super(Container, cls).add_class_trait(name, *trait)

    def add_trait(self, name, trait, refresh=True):
        """Overrides HasTraits definition of *add_trait* in order to
        keep track of dynamically added traits for serialization.
        """
        # When a trait with sub-traits is added (like a List or Dict),
        # HasTraits calls add_trait AGAIN for the sub-trait, so we
        # just detect it here and pass it through
        if name.endswith('_items') and trait.type == 'event':
            super(Container, self).add_trait(name, trait)
            return

        # Try to keep from clobbering framework stuff.
        bases = [self.__class__]
        bases.extend(self.__class__.__bases__)
        for base in bases:
            if name in base.__dict__:
                raise NameError('Would override attribute %r of %s'
                                % (name, base.__name__))

        _check_bad_default(name, trait, self)

        #FIXME: saving our own list of added traits shouldn't be necessary...
        if name not in self._added_traits:
            self._added_traits[name] = trait
        super(Container, self).add_trait(name, trait)
        if self._cached_traits_ is not None:
            self._cached_traits_[name] = self.trait(name)

        if name in self._trait_metadata:
            del self._trait_metadata[name]  # Invalidate.

        if refresh:
            getattr(self, name)  # For VariableTree subtree/leaf update in GUI.

    def remove_trait(self, name):
        """Overrides HasTraits definition of remove_trait in order to
        keep track of dynamically added traits for serialization.
        """
        try:
            del self._added_traits[name]
        except KeyError:
            pass
        try:
            del self._cached_traits_[name]
        except (KeyError, TypeError):
            pass
        try:
            del self._trait_metadata[name]
        except KeyError:
            pass

        super(Container, self).remove_trait(name)

    @rbac(('owner', 'user'))
    def get_attr_w_copy(self, path):
        """Same as the 'get' method, except that the value will be copied
        if the variable has a 'copy' metadata attribute that is not None.
        Possible values for 'copy' are 'shallow' and 'deep'.
        Raises an exception if the variable cannot be found.

        """
        obj = self.get(path)

        copy = self._copycache.get(path, _missing)
        if copy is _missing:
            copy = self.get_metadata(path.split('[',1)[0], 'copy')
            self._copycache[path] = copy

        # copy value if 'copy' found in metadata
        if copy:
            if isinstance(obj, Container):
                obj = obj.copy()
            else:
                obj = _copydict[copy](obj)

        return obj

    def _add_after_parent_set(self, name, obj):
        pass

    def _prep_for_add(self, name, obj):
        """Check for illegal adds and update the new child
        object in preparation for insertion into self.
        """
        if '.' in name:
            self.raise_exception(
                'add does not allow dotted path names like %s' %
                name, ValueError)
        elif not is_legal_name(name):
            self.raise_exception("'%s' is a reserved or invalid name" % name,
                                 NameError)

        removed = False
        if has_interface(obj, IContainer):
            # if an old child with that name exists, remove it
            if self.contains(name) and getattr(self, name):
                self.remove(name)
                removed = True

        if has_interface(obj, IContainer):
            self._check_recursion(obj)
            if IContainerProxy.providedBy(obj):
                obj.parent = self._get_proxy(obj)
            else:
                obj.parent = self
            obj.name = name

            self._add_after_parent_set(name, obj)

            # if this object is already installed in a hierarchy, then go
            # ahead and tell the obj (which will in turn tell all of its
            # children) that its scope tree back to the root is defined.
            if self._call_cpath_updated is False:
                obj.cpath_updated()

        return removed

    def _post_container_add(self, name, obj, removed):
        pass

    def add(self, name, obj):
        """Add an object to this Container.
        Returns the added object.
        """
        removed = self._prep_for_add(name, obj)

        if has_interface(obj, IContainer):
            setattr(self, name, obj)

            if self._cached_traits_ is None:
                self.get_trait(name)
            else:
                self._cached_traits_[name] = self.trait(name)

            self._post_container_add(name, obj, removed)

        elif is_instance(obj, TraitType):
            self.add_trait(name, obj)
        else:
            setattr(self, name, obj)

        return obj

    def _check_recursion(self, obj):
        """ Check if adding `obj` will cause container recursion. """
        ancestor = self
        while is_instance(ancestor, Container):
            if obj is ancestor:
                self.raise_exception('add would cause container recursion',
                                     ValueError)
            ancestor = ancestor.parent

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
                if addr == ip_addr:
                    allowed_hosts.append('127.0.0.1')
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

    def _check_rename(self, oldname, newname):
        if '.' in oldname or '.' in newname:
            self.raise_exception("can't rename '%s' to '%s': rename only works"
                                 " within a single scope." %
                                 (oldname, newname), RuntimeError)
        if not self.contains(oldname):
            self.raise_exception("can't rename '%s' to '%s': '%s' was not found." %
                                 (oldname, newname, oldname), RuntimeError)
        if self.contains(newname):
            self.raise_exception("can't rename '%s' to '%s': '%s' already exists." %
                                 (oldname, newname, newname), RuntimeError)

    def rename(self, oldname, newname):
        """Renames a child of this object from oldname to newname."""
        self._check_rename(oldname, newname)
        obj = self.remove(oldname)
        self.add(newname, obj)

    def remove(self, name):
        """Remove the specified child from this container and remove any
        public trait objects that reference that child. Notify any
        observers.
        """
        if '.' in name:
            self.raise_exception(
                'remove does not allow dotted path names like %s' %
                                 name, NameError)
        try:
            obj = getattr(self, name)
        except AttributeError:
            return None

        trait = self.get_trait(name)
        if trait is None:
            delattr(self, name)
        else:
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

    @rbac(('owner', 'user'))
    def configure(self):
        pass

    @rbac(('owner', 'user'))
    def copy(self):
        """Returns a deep copy without deepcopying the parent.
        """
        cp = copy.deepcopy(self)
        cp._relink()
        return cp

    def _relink(self):
        """Restore parent links in copy."""
        for name in self.list_containers():
            container = getattr(self, name)
            if container is not self._parent:
                container._parent = self
                container._relink()

    @rbac(('owner', 'user'))
    def cpath_updated(self):
        """Called after the hierarchy containing this Container has been
        defined back to the root. This does not guarantee that all sibling
        Containers have been defined. It also does not guarantee that this
        component is fully configured to execute. Classes that override this
        function must call their base class version.

        This version calls cpath_updated() on all of its child Containers.
        """
        self._fix_loggers(self, recurse=False)
        self._call_cpath_updated = False
        for cont in self.list_containers():
            cont = getattr(self, cont)
            if cont is not self._parent:
                cont.cpath_updated()

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
                            yield ('.'.join((name, chname)), child)

            for name, trait in match_dict.items():
                obj = getattr(self, name, Missing)
                # In some components with complex loading behavior (like
                # NPSSComponent), we can have a temporary situation during
                # loading where there are traits that don't point to anything,
                # so check for them here and skip them if they don't point to
                # anything.
                if obj is not Missing:
                    if is_instance(obj, (Container, VarTree)) and \
                       id(obj) not in visited:
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
        return [n for n, v in self.items() if is_instance(v, Container)]

    def list_vars(self):
        """Return a list of Variables in this Container."""
        return [k for k, v in self.items(iotype=not_none)]

    # Can't use items() since it returns a generator (won't pickle).
    @rbac(('owner', 'user'))
    def _alltraits(self, traits=None, events=False, **metadata):
        """This returns a dict that contains traits (class and instance)
        that match the given metadata.  If the 'traits' argument is not
        None, then it is assumed to be the dict of traits to be filtered.
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
                if type(meta_eval) is FunctionType:
                    if not meta_eval(getattr(trait, meta_name)):
                        break
                elif meta_eval != getattr(trait, meta_name):
                    break
            else:
                result[name] = trait

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
            if obj is Missing:
                return self._get_metadata_failed(traitpath, metaname)
            elif hasattr(obj, 'get_metadata'):
                return obj.get_metadata(restofpath, metaname)
            else:
                # if the thing being accessed is an attribute of a Variable's
                # data object, then we can assume that the iotype of the
                # attribute is the same as the iotype of the Variable.
                t = self.get_trait(childname)
                if t is not None and t.iotype and metaname == 'iotype':
                    return t.iotype
                else:
                    self._get_metadata_failed(traitpath, metaname)

        varname, _, _ = traitpath.partition('[')
        try:
            mdict = self._trait_metadata[varname]
        except KeyError:
            t = self.get_trait(varname)
            if t:
                t = t.trait_type
                mdict = t._metadata.copy()
                # vartypename isn't present in the metadata of traits
                # that don't inherit from Variable, so fake it out here
                # so we'll be consistent across all traits
                mdict.setdefault('vartypename', t.__class__.__name__)
            else:
                mdict = self._get_metadata_failed(traitpath, None)
            self._trait_metadata[varname] = mdict

        if metaname is None:
            return mdict
        else:
            return mdict.get(metaname, None)

    @rbac(('owner', 'user'))
    def set_metadata(self, traitpath, metaname, value):
        """Set the metadata associated with the trait found using traitpath."""
        if metaname in ('iotype',):
            self.raise_exception("Can't set %s on %s, read-only"
                                 % (metaname, traitpath), TypeError)
        self.get_metadata(traitpath)[metaname] = value

    @rbac(('owner', 'user'), proxy_types=[FileRef])
    def get(self, path):
        """Return the object specified by the given path, which may
        contain '.' characters.
        """
        expr = self._getcache.get(path)
        if expr is not None:
            return eval(expr, self.__dict__)

        obj, restofpath = get_closest_proxy(self, path)
        # if restofpath is truthy, it means either that path
        # contains a proxy or it contains some syntax that causes
        # getattr to fail, e.g., a function eval, array element ref, etc.
        if restofpath and IContainerProxy.providedBy(obj):
            return obj.get(restofpath)

        # assume all local.  just compile the expr and cache it if
        # it can be evaluated
        expr = compile(path, path, mode='eval')
        try:
            val = eval(expr, self.__dict__)
        except (AttributeError, NameError) as err:
            if not restofpath: # to get around issue with PassthroughProperty
                return obj
            self.raise_exception(str(err), AttributeError)
        else:
            self._getcache[path] = expr
            return val

    @rbac(('owner', 'user'), proxy_types=[FileRef])
    def get_flattened_value(self, path):
        """Return the named value, which may include
        an array index, as a flattened array of floats.  If
        the value is not flattenable into an array of floats,
        raise a TypeError.
        """
        return flattened_value(path, self.get(path))

    @rbac(('owner', 'user'))
    def set_flattened_value(self, path, value):
        obj, restofpath = proxy_parent(self, path)
        # if restofpath is truthy, it means either that path
        # contains a proxy or it contains some syntax that causes
        # getattr to fail, e.g., a function eval, array element ref, etc.
        if restofpath and IContainerProxy.providedBy(obj):
            obj.set_flattened_value(restofpath, value)
            return

        # get current value
        val = self.get(path)
        if not isinstance(val, int_types) and isinstance(val, complex_or_real_types):
            self.set(path, value[0])
            return
        elif hasattr(val, 'set_flattened_value'):
            val.set_flattened_value(value)
            return
        elif isinstance(val, ndarray):
            try:
                newshape = value.shape
                self.set(path, value.reshape(val.shape))
            except Exception as err:
                self.reraise_exception("ERROR setting value '%s.%s' shape: %s to shape %s"
                                       % (self.get_pathname(), path, val.shape, newshape),
                                          sys.exc_info())
            return

        # now get the non-indexed value and the index
        val = self.get(path.split('[',1)[0])
        idx = get_index(path)

        if isinstance(val, int_types):
            pass  # fall through to exception
        elif hasattr(val, '__setitem__') and idx is not None:
            if isinstance(val[idx], complex_or_real_types):
                val[idx] = value[0]
            else:
                val[idx] = value
            return
        elif IVariableTree.providedBy(val):
            raise NotImplementedError("no support for setting flattened values into vartrees")
        elif hasattr(val, 'set_flattened_value'):
            val.set_flattened_value(value)
            return

        self.raise_exception("Failed to set flattened value to variable %s" % path, TypeError)

    def get_iotype(self, name):
        return self.get_trait(name).iotype

    @rbac(('owner', 'user'))
    def set(self, path, value):
        """Set the value of the Variable specified by the given path, which
        may contain '.' characters. The Variable will be set to the given
        value, subject to validation and constraints.
        """
        _local_setter_ = value
        expr = self._setcache.get(path)
        if expr is not None:
            exec(expr)
            return

        obj, restofpath = proxy_parent(self, path)
        # if restofpath is truthy, it means either that path
        # contains a proxy or it contains some syntax that causes
        # getattr to fail, e.g., a function eval, array element ref, etc.

        if IOverrideSet.providedBy(obj) or (restofpath and IContainerProxy.providedBy(obj)):
            obj.set(restofpath, value)
            return

        # assume all local.  just compile the expr and cache it if
        # it can be evaluated
        assign = "self.%s=_local_setter_" % path
        expr = compile(assign, assign, mode='exec')
        try:
            exec(expr)
        except Exception as err:
            self.raise_exception(str(err), err.__class__)
        else:
            self._setcache[path] = expr

    def save_to_egg(self, name, version, py_dir=None, src_dir=None,
                    src_files=None, child_objs=None, dst_dir=None,
                    observer=None, need_requirements=True):
        """Save state and other files to an egg.  Typically used to copy all or
        part of a simulation to another user or machine.  By specifying child
        containers in `child_objs`, it will be possible to create instances of
        just those containers from the installed egg.  Child container names
        should be specified relative to this container.

        name: string
            Name for egg; must be an alphanumeric string.

        version: string
            Version for egg; must be an alphanumeric string.

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
        except Exception:
            self.reraise_exception(info=sys.exc_info())  # Just to get a pathname.
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
        except Exception:
            self.reraise_exception(info=sys.exc_info())  # Just to get a pathname.
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
        top.cpath_updated()
        if name:
            top.name = name
        if call_post_load:
            top.parent = None
            top.post_load()
        return top

    def post_load(self):
        """Perform any required operations after the model has been loaded.
        At this point the local configuration of the Component is valid,
        but 'remote' traits may need 'repairing' which can't be done until
        the remote environment is ready. Components with remote environments
        should override this and restore the remote environment first, then
        call the superclass.
        """
        self._repair_traits()
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
                if iotype is not None:
                    if isinstance(trait.trait_type, Python):  # VariableTree
                        obj = getattr(self, cname)
                        t_iotype = getattr(obj, 'iotype', None)
                    else:  # Variable
                        t_iotype = self.get_iotype(cname)
                    if (iotype == 'in' and t_iotype not in ('in', 'state')) or \
                       (iotype == 'out' and t_iotype not in ('out', 'in', 'state', 'residual')):
                        self.raise_exception("'%s' must be an %s variable" %
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
        if not pathname:
            obj = self
        else:
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

            if isinstance(trait, Python):  # Container
                obj = self.get(pathname)
            else:  # Variable
                obj = trait

        names = []
        Container._bases(type(obj), names)
        return names

    @staticmethod
    def _bases(cls, names):
        """ Helper for :meth:`get_trait_typenames`. """
        names.append('%s.%s' % (cls.__module__, cls.__name__))
        for base in cls.__bases__:
            Container._bases(base, names)

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        coords = ''
        obj = self
        while obj is not None:
            try:
                coords = obj.get_itername()
            except AttributeError:
                try:
                    obj = obj.parent
                except AttributeError:
                    break
            else:
                break
        if coords:
            full_msg = '%s (%s): %s' % (self.get_pathname(), coords, msg)
        else:
            full_msg = '%s: %s' % (self.get_pathname(), msg)
        #self._logger.error(msg)
        raise exception_class(full_msg)

    def reraise_exception(self, msg='', info=None):
        """Re-raise an exception with updated message and original traceback."""
        if info is None:
            exc_type, exc_value, exc_traceback = sys.exc_info()
        else:
            exc_type, exc_value, exc_traceback = info

        if msg:
            msg = '%s: %s' % (msg, exc_value)
        else:
            msg = '%s' % exc_value

        prefix = '%s: ' % self.get_pathname()

        if not msg.startswith(prefix):
            msg = prefix + msg

        new_exc = exc_type(msg)

        raise exc_type, new_exc, exc_traceback

    def build_trait(self, ref_name, iotype=None, trait=None):
        """Build a trait referring to `ref_name`.
        This is called by :meth:`create_io_traits`.
        This must be overridden.

        iotype: str or dict
            If `iotype` is a string it specifies the trait's iotype.
            If it's a dictionary, it provides metadata.

        trait: Trait
            If `trait` is not None, use that trait rather than building one.
        """
        self.raise_exception('build_trait()', NotImplementedError)


# By default we always proxy Containers and FileRefs.
CLASSES_TO_PROXY.append(Container)
CLASSES_TO_PROXY.append(FileRef)


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
            (Variable,             'openmdao.variable'),
            (Driver,               'openmdao.driver'),
            (ICaseIterator,        'openmdao.case_iterator'),
            (IResourceAllocator,   'openmdao.resource_allocator'),
            (Component,            'openmdao.component'),
            (Container,            'openmdao.container'),
        ]

    for cls, group in _get_entry_group.group_map:
        if issubclass(cls, Interface):
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
    in the parent's `__dict__`.  There could be multiple names bound to a
    given object. Only the first name found is returned.

    Return '' if not found.
    """
    for name, val in parent.__dict__.items():
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


def find_trait_and_value(obj, pathname):
    """Return a tuple of the form (trait, value) for the given dotted
    pathname. Raises an exception if the value indicated by the pathname
    is not found in obj. If the value is found but has no trait, then
    (None, value) is returned.
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
    """Create io trait(s) specified by the contents of `obj_info`. Calls
    :meth:`build_trait` on :class:`Container` `cont`, which can be overridden
    by subclasses, to create each trait.  One use of this is to provide traits
    mapping to variables inside a :class:`Component` implemented as a Python
    extension module.

    `obj_info` is assumed to be either a string, a tuple, or a list
    that contains strings and/or tuples.  The information is used to specify
    the "internal" and "external" names of the variable.
    The "internal" name uses the naming scheme within the Container.
    The "external" name is the one that will be used to access the trait
    from outside the Container; it must not contain any '.' characters.

    A string specifies the "internal" name for the variable.  The "external"
    name will be the "internal" name with any '.' characters replaced by '_'.

    Tuples must contain the "internal" name followed by the "external" name
    and may optionally contain an iotype and a validation trait. If the iotype
    is a dictionary rather than a string, it is used for trait metadata (it may
    include the ``iotype`` key but does not have to).

    `iotype` is the default I/O type to be used.

    The newly created traits are added to the specified Container.

    For example, the following are valid calls::

        # Create an input trait 'foo' referring to 'foo' on 'obj'.
        create_io_traits(obj, 'foo')

        # Create an input trait 'inputs_foo' referring to 'inputs.foo'.
        create_io_traits(obj, 'inputs.foo')

        # Create outputs 'foo', 'bar', and 'baz'.
        create_io_traits(obj, ['foo','bar','baz'], iotype='out')

        # Use Bool trait named 'foo_alias' to refer to 'foo', and create 'bar'.
        create_io_traits(obj, ('foo', 'foo_alias', 'in', Bool()), 'bar')

        # Create inputs 'fooa' and 'bazz', and output 'barb'.
        # 'fooa' will have the specified metadata.
        create_io_traits(obj, [('foo', 'fooa', {low=-1, high=10}),
                               ('bar', 'barb', 'out'),
                               ('baz', 'bazz')])

    """
    if isinstance(obj_info, (basestring, tuple)):
        it = [obj_info]
    else:
        it = obj_info

    for entry in it:
        iostat = iotype
        trait = None

        if isinstance(entry, basestring):
            ref_name = entry
            name = entry.replace('.', '_')
        elif isinstance(entry, tuple):
            ref_name = entry[0]  # internal name
            name = entry[1] or ref_name.replace('.', '_')  # wrapper name
            try:
                iostat = entry[2]  # optional iotype/metadata
                trait = entry[3]   # optional validation trait
            except IndexError:
                pass
        else:
            cont.raise_exception('create_io_traits cannot add trait %s' % entry,
                                 RuntimeError)

        if '.' in name:
            cont.raise_exception("Can't create '%s' because it's a"
                                 " dotted pathname" % name, NameError)

        newtrait = cont.get_trait(name)
        if newtrait is not None:
            cont.raise_exception(
                "Can't create '%s' because it already exists." % name,
                RuntimeError)

        if not cont.contains(ref_name):
            cont.raise_exception("Can't create trait for '%s' because it wasn't"
                                 " found" % ref_name, AttributeError)

        cont.add_trait(name, cont.build_trait(ref_name, iostat, trait))
