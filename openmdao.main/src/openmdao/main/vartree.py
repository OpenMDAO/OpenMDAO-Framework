""" VariableTree class definition
"""
import copy

# pylint: disable-msg=E0611,F0401
from enthought.traits.has_traits import FunctionType

from openmdao.main.interfaces import IVariable
from openmdao.main.container import Container
from openmdao.main.datatypes.slot import Slot
from openmdao.main.datatypes.str import Str
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance


class VariableTree(Container):
    """A container of variables with an input or output sense."""

    _iotype = Str('')

    def __init__(self, iotype=''):
        super(VariableTree, self).__init__()
        self._iotype = iotype
        self.on_trait_change(self._iotype_modified, '_iotype')

        # Wrap subtrees in VarTree (similar to Slot).
        for name, obj in self.__class__.__dict__.items():
            if name not in self.class_traits():
                if isinstance(obj, VariableTree):
                    # Avoids an import loop.
                    from openmdao.main.datatypes.vtree import VarTree
                    new_obj = obj.copy()  # Otherwise we share the default value
                    new_obj.install_callbacks()
                    trait = VarTree(new_obj)

                    self._added_traits[name] = trait
                    super(Container, self).add_trait(name, trait)
#                    setattr(self, name, obj)
                    if self._cached_traits_ is None:
                        self.get_trait(name)
                    else:
                        self._cached_traits_[name] = self.trait(name)

                    if not name.startswith('_'):
                        self.on_trait_change(self._trait_modified, name)

        # register callbacks for our class traits
        for name, trait in self.class_traits().items():
            if not name.startswith('_'):
                self.on_trait_change(self._trait_modified, name)

    @property
    def iotype(self):
        return self._iotype

    @rbac(('owner', 'user'))
    def cpath_updated(self):
        if self.parent:
            if isinstance(self.parent, VariableTree):
                self._iotype = self.parent._iotype
            else:
                t = self.parent.trait(self.name)
                if t and t.iotype:
                    self._iotype = t.iotype
        super(VariableTree, self).cpath_updated()

    @rbac(('owner', 'user'))
    def get_metadata(self, traitpath, metaname=None):
        if metaname == 'iotype':
            return self._iotype
        elif metaname is None:
            meta = super(VariableTree, self).get_metadata(traitpath, metaname)
            meta['iotype'] = self._iotype
            return meta
        else:
            return super(VariableTree, self).get_metadata(traitpath, metaname)

    def copy(self):
        """Returns a deep copy of this VariableTree."""
        return copy.deepcopy(self)

    def install_callbacks(self):
        """Install trait callbacks on deep-copied VariableTree."""
        self.on_trait_change(self._iotype_modified, '_iotype')
        for name, trait in self.traits().items():
            if not name.startswith('_'):
                if name not in ('trait_added', 'trait_modified'):
                    self.on_trait_change(self._trait_modified, name)

        for name, trait in self._added_traits.items():
            self.on_trait_change(self._trait_modified, name)
            obj = getattr(self, name)
            if isinstance(obj, VariableTree) and obj is not self.parent:
                obj.install_callbacks()

    def add(self, name, obj):
        if isinstance(obj, VariableTree):
            if self.trait(name) is None:
#Slot
#                self.add_trait(name, Slot(obj.__class__, iotype=obj._iotype))
                from openmdao.main.datatypes.vtree import VarTree
                self.add_trait(name, VarTree(obj.__class__, iotype=obj._iotype))
                self.on_trait_change(self._trait_modified, name)
        elif not IVariable.providedBy(obj):
            msg = "a VariableTree may only contain Variables or other " + \
                  "VariableTrees"
            self.raise_exception(msg, TypeError)
        return super(VariableTree, self).add(name, obj)

    def add_trait(self, name, trait):
        super(VariableTree, self).add_trait(name, trait)
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

    @rbac(('owner', 'user'))
    def invalidate_deps(self, varnames=None, force=False):
        return None

    def _iotype_modified(self, obj, name, old, new):
        from openmdao.main.datatypes.vtree import VarTree
        for k, v in self.__dict__.items():
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
            while isinstance(p, VariableTree):
                vt = p
                p = p.parent
            # notify parent Component that this VariableTree has been modified
            if p is not None:
                t = p.trait(vt.name)
                if t and t.iotype == 'in':
                    p._input_trait_modified(p, vt.name, vt, vt)

    def get_iotype(self, name):
        """Return the iotype of the Variable with the given name"""
        if self.get_trait(name) is None:
            self.raise_exception("'%s' not found" % name)
        return self._iotype

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
                    if meta_io(self._iotype):
                        matches_io = True
                elif meta_io == self._iotype:
                    matches_io = True
                if matches_io:
                    newdict = metadata.copy()
                    del newdict['iotype']
            else:
                matches_io = True
                newdict = metadata

            if matches_io:
                for name, trait in self._alltraits(**newdict).items():
                    if name.startswith('_'):
                        continue
                    obj = getattr(self, name)
                    yield (name, obj)
                    if recurse and is_instance(obj, VariableTree) and \
                       id(obj) not in visited:
                        for chname, child in obj._items(visited, recurse,
                                                        **metadata):
                            yield ('.'.join([name, chname]), child)

    def get_attributes(self, io_only=True, indent=0, parent='', valid='false'):
        """ Get attributes for this variable tree. Variables may also include
        slots. Used by the GUI.

        io_only: bool
            Set to True if we only want to populate the input and output
            fields of the attributes dictionary.

        indent: int
            Recursion level (for collapsing tables).

        parent: str
            ID name of parent table line

        valid: str
            Validity state of the parent table"""

        attrs = {}
        attrs['type'] = type(self).__name__

        # Connection information found in parent comp's parent assy
        if not self.parent or not self.parent._parent or \
           isinstance(self.parent, VariableTree):
            connected = []
        else:
            graph = self.parent._parent._depgraph
            if self._iotype == 'in':
                connected = graph.get_connected_inputs()
            else:
                connected = graph.get_connected_outputs()

        variables = []
        for name in self.list_vars():

            trait = self.get_trait(name)
            meta = self.get_metadata(name)
            value = getattr(self, name)
            ttype = trait.trait_type

            # Each variable type provides its own basic attributes
            attr, slot_attr = ttype.get_attribute(name, value, trait, meta)
            attr['valid'] = valid

            # Let the GUI know that this var is the top element of a
            # variable tree
            if attr.get('ttype') in ('vartree', 'slot'):
                vartable = self.get(name)
                if isinstance(vartable, VariableTree):
                    attr['vt'] = 'vt'

            # Support for expand/collapse
            attr['indent'] = indent
            attr['id'] = '%s.%s' % (parent, name)
            if parent:
                attr['parent'] = parent

            attr['connected'] = ''
            if name in connected:
                connections = self.parent._depgraph.connections_to(name)

                if self._iotype == 'in':
                    # there can be only one connection to an input
                    attr['connected'] = str([src for src, dst in \
                                            connections]).replace('@xin.', '')
                else:
                    attr['connected'] = str([dst for src, dst in \
                                            connections]).replace('@xout.', '')
            variables.append(attr)

            # For variables trees only: recursively add the inputs and outputs
            # into this variable list
            if 'vt' in attr:
                vt_attrs = vartable.get_attributes(io_only, indent=indent + 1,
                                                   parent=attr['id'],
                                                   valid=valid)
                if self._iotype == 'in':
                    variables += vt_attrs['Inputs']
                else:
                    variables += vt_attrs['Outputs']

        if self._iotype == 'in':
            panel = 'Inputs'
        else:
            panel = 'Outputs'

        attrs[panel] = variables
        return attrs


# register a flattener for Cases
from openmdao.main.case import flatteners, flatten_obj


def _flatten_vartree(name, vt):
    ret = []
    for n, v in vt._items(set()):
        ret.extend([('.'.join([name, k]), v) for k, v in flatten_obj(n, v)])
    return ret

flatteners[VariableTree] = _flatten_vartree
