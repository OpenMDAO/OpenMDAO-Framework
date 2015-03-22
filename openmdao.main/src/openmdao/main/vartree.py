""" VariableTree class definition
"""
import weakref

from zope.interface import implements

# pylint: disable=E0611,F0401
from traits.has_traits import FunctionType
from traits.trait_base import not_event

from openmdao.main.interfaces import IVariable, IVariableTree
from openmdao.main.container import Container
from openmdao.main.datatypes.str import Str
from openmdao.main.datatypes.vtree import VarTree
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance
from openmdao.main.array_helpers import flattened_size



class VariableTree(Container):
    """A tree of variables having the same input or output sense."""

    _iotype = Str('')

    implements(IVariableTree)

    def __init__(self, iotype=''):
        self._parent_ref = None
        super(VariableTree, self).__init__()

        self._iotype = iotype

        # Check for nested VariableTrees in the class definition.
        for name, obj in self.__class__.__dict__.items():
            if isinstance(obj, VariableTree):
                raise TypeError('Nested VariableTrees are not supported,'
                                ' please wrap %s.%s in a VarTree'
                                % (self.__class__.__name__, name))

        self.install_callbacks()

        # register callbacks for our class traits
        for name, trait in self.class_traits().items():
            if not name.startswith('_'):
                self.on_trait_change(self._trait_modified, name)

    @property
    def _parent(self):
        """ Return dereferenced weakref to parent. """
        return None if self._parent_ref is None else self._parent_ref()

    @_parent.setter
    def _parent(self, value):
        """ Set weakref to parent. """
        self._parent_ref = None if value is None else weakref.ref(value)

    def __getstate__(self):
        """ Return state after dereferencing weakref to parent. """
        state = super(VariableTree, self).__getstate__()
        if self._parent_ref is not None:
            state['_parent_ref'] = self._parent_ref()
        return state

    def __setstate__(self, state):
        """ Set state and set weakref to parent. """
        super(VariableTree, self).__setstate__(state)
        if self._parent_ref is not None:
            self._parent_ref = weakref.ref(self._parent_ref)

    @property
    def iotype(self):
        if not self._iotype and isinstance(self.parent, VariableTree):
            self._iotype = self.parent.iotype
        return self._iotype

    @rbac(('owner', 'user'))
    def cpath_updated(self):
        if self.parent:
            if isinstance(self.parent, VariableTree):
                self._iotype = self.parent.iotype
            else:
                t = self.parent.trait(self.name)
                if t and t.iotype:
                    self._iotype = t.iotype
        super(VariableTree, self).cpath_updated()

    @rbac(('owner', 'user'))
    def get_metadata(self, traitpath, metaname=None):
        if metaname == 'iotype':
            return self.iotype
        elif metaname is None:
            meta = super(VariableTree, self).get_metadata(traitpath, metaname)
            meta['iotype'] = self.iotype
            return meta
        else:
            return super(VariableTree, self).get_metadata(traitpath, metaname)

    def copy(self):
        """Returns a deep copy of this VariableTree, without deepcopying its
        parent.  Also installs necessary trait callbacks.
        """
        cp = super(VariableTree, self).copy()
        cp.install_callbacks()
        return cp

    def __deepcopy__(self, memo):
        id_self = id(self)
        if id_self in memo:
            return memo[id_self]

        cp = super(VariableTree, self).__deepcopy__(memo)
        cp.install_callbacks()
        return cp

    def install_callbacks(self):
        """Install trait callbacks on deep-copied VariableTree."""
        self.on_trait_change(self._iotype_modified, '_iotype')
        # _alltraits() is missing some traits after a deepcopy, so use the
        # union of _alltraits() and everything in self.__dict__
        allset = set(self._alltraits().keys())
        allset.update(self.__dict__.keys())
        for name in allset:
            if name not in ('trait_added', 'trait_modified') \
               and not name.startswith('_') and hasattr(self, name):
                self.on_trait_change(self._trait_modified, name)
                obj = getattr(self, name)
                if isinstance(obj, VariableTree) and obj is not self.parent:
                    obj.install_callbacks()

    def add(self, name, obj):
        if not (IVariable.providedBy(obj) or isinstance(obj, VarTree)):
            msg = "a VariableTree may only contain Variables or VarTrees"
            self.raise_exception(msg, TypeError)
        return super(VariableTree, self).add(name, obj)

    def add_trait(self, name, trait, refresh=True):
        super(VariableTree, self).add_trait(name, trait, refresh)
        if not name.startswith('_'):
            self.on_trait_change(self._trait_modified, name)

    def remove_trait(self, name):
        trait = self.get_trait(name)

        # remove the callback
        if trait:
            self.on_trait_change(self._trait_modified, name, remove=True)

        super(VariableTree, self).remove_trait(name)

    def list_vars(self):
        """Return a list of Variables in this VariableTree."""
        return [k for k in self.__dict__.keys() if not k.startswith('_')]

    def _iotype_modified(self, obj, name, old, new):
        for v in self.__dict__.values():
            if isinstance(v, (VariableTree, VarTree)) and v is not self.parent:
                v._iotype = new

    def _trait_modified(self, obj, name, old, new):
        # handle weird traits side-effect from hasattr call
        if name == 'trait_added':
            return
        if isinstance(new, VariableTree):
            obj = getattr(self, name)
            obj.parent = self
            obj._iotype = self._iotype
        if self._iotype == 'in':
            p = self
            path = [name]
            while isinstance(p, VariableTree):
                vt = p
                p = p.parent
                path.append(vt.name)
            # notify parent Component that this VariableTree has been modified
            if p is not None:
                t = p.trait(vt.name)
                if t and t.iotype == 'in':
                    # we need to pass the full pathname of the child that was
                    # actually modified up to the parent component, and we can't
                    # modify the arglist of _input_trait_modified, so instead
                    # call _input_updated explicitly
                    p._input_updated(vt.name, fullpath='.'.join(path[::-1]))

    def get_iotype(self, name):
        """Return the iotype of the Variable with the given name"""
        if self.get_trait(name) is None:
            self.raise_exception("'%s' not found" % name)
        return self.iotype

    def _items(self, visited, recurse=False, **metadata):
        """Return an iterator that returns a list of tuples of the form
        (rel_pathname, obj) for each trait of this VariableTree that matches
        the given metadata. If recurse is True, also iterate through all
        child Containers of each Container found.
        """
        if id(self) not in visited:
            visited.add(id(self))

            if 'iotype' in metadata:
                meta_io = metadata['iotype']
                matches_io = False
                if type(meta_io) is FunctionType:
                    if meta_io(self.iotype):
                        matches_io = True
                elif meta_io == self.iotype:
                    matches_io = True
                if matches_io:
                    newdict = metadata.copy()
                    del newdict['iotype']
            else:
                matches_io = True
                newdict = metadata

            if matches_io:
                for name in self._alltraits(**newdict):
                    if name.startswith('_'):
                        continue
                    obj = getattr(self, name)
                    yield (name, obj)
                    if recurse and is_instance(obj, VariableTree) and \
                       id(obj) not in visited:
                        for chname, child in obj._items(visited, recurse,
                                                        **metadata):
                            yield ('.'.join((name, chname)), child)

    @rbac(('owner', 'user'))
    def get_req_default(self, vt_required=None):
        """Returns a list of all inputs that are required but still have
        their default value.
        """
        req = []
        if vt_required:
            req_test = [True, False, None]
        else:
            req_test = [True]

        for name, trait in self.traits(type=not_event).items():
            obj = getattr(self, name)
            if obj is self.parent:
                continue
            if is_instance(obj, VariableTree):
                req.extend(['.'.join((self.name, n))
                                 for n in obj.get_req_default(vt_required)])
            elif trait.required in req_test:
                try:
                    trait = trait.trait_type
                except:
                    unset = (obj == trait.default)
                else:
                    unset = (obj == trait.default_value)
                if not isinstance(unset, bool):
                    try:
                        unset = unset.all()
                    except:
                        pass
                if unset:
                    req.append('.'.join((self.name, name)))

        return req

    def list_all_vars(self):
        """Return a list of all variables in this VarTree (recursive)."""
        vnames = []
        for name, obj in self.__dict__.items():
            if name.startswith('_'):
                continue
            if isinstance(obj, VariableTree):
                vnames.extend(['.'.join((self.name,n)) for n in obj.list_all_vars()])
            else:
                vnames.append('.'.join((self.name, name)))
        return vnames

    def get_flattened_size(self):
        """Return the size of a flattened float array containing
        all values in the vartree that are flattenable to float
        arrays.  Any values not flattenable to float arrays will
        raise a NoFlatError.
        """
        size = 0
        for key in self.list_vars():
            size += flattened_size(key, getattr(self, key), scope=self)
        return size

    def get_flattened_index(self, name):
        """Return the slice within the flattened array of the
        current vartree that is occupied by the named
        subvar.
        """
        raise NotImplementedError('get_flattened_index not implemented for VarTrees yet')  # FIXME


# register a flattener for Cases
from openmdao.main.case import flatteners, flatten_obj


def _flatten_vartree(name, vt):
    ret = []
    for n, v in vt._items(set()):
        ret.extend([('.'.join((name, k)), v) for k, v in flatten_obj(n, v)])
    return ret

flatteners[VariableTree] = _flatten_vartree
