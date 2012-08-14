""" VariableTree class definition
"""
import copy

from zope.interface import implementedBy

from enthought.traits.api import Str
from enthought.traits.has_traits import FunctionType

from openmdao.main.variable import Variable
from openmdao.main.container import Container
from openmdao.main.datatypes.slot import Slot
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance

class VariableTree(Container):
    """A container of variables with an input or output sense."""
    
    _iotype = Str('')
    
    def __init__(self, iotype='', doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self._iotype = iotype
        self.on_trait_change(self._iotype_modified, '_iotype')
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
    
    def add(self, name, obj):
        if isinstance(obj, VariableTree):
            if self.trait(name) is None:
                self.add_trait(name, Slot(VariableTree, iotype=obj._iotype))
                self.on_trait_change(self._trait_modified, name)
        elif not isinstance(obj, Variable):
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
        for k, v in self.__dict__.items():
            if isinstance(v, VariableTree) and v is not self.parent:
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
                if type( meta_io ) is FunctionType:
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

    def get_attributes(self, io_only=True):
        """ get attributes for this variable tree. Variables may also include
        slots. Used by the GUI.
        
        io_only: Bool
            Set to true if we only want to populate the input and output
            fields of the attributes dictionary."""
            
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
            
        # Keep track of slot names so we don't put them in the variable list.
        slot_names = []
        
        # Do slots first for the same reason
        slots = []
        for name, value in self.traits().items():
            if value.is_trait_type(Slot):
                attr = {}
                attr['name'] = name
                attr['klass'] = value.trait_type.klass.__name__
                inames = []
                for klass in list(implementedBy(attr['klass'])):
                    inames.append(klass.__name__)
                attr['interfaces'] = inames
                if getattr(self, name) is None:
                    attr['filled'] = False
                else:
                    attr['filled'] = True
                meta = self.get_metadata(name)
                if meta:
                    for field in ['desc']:    # just desc?
                        if field in meta:
                            attr[field] = meta[field]
                        else:
                            attr[field] = ''
                    attr['type'] = meta['vartypename']
                    
                slots.append(attr)
                slot_names.append(name)
                
        if not io_only:
            attrs['Slots'] = slots
                
        variables = []
        for vname in self.list_vars():
            if vname not in slot_names:
                var = self.get(vname)
                attr = {}
                    
                attr['name'] = vname
                attr['type'] = type(var).__name__
                attr['value'] = str(var)
                #attr['valid'] = self.get_valid([vname])[0]
                meta = self.get_metadata(vname)
                    
                if meta:
                    for field in ['units', 'high', 'low', 'desc']:
                        if field in meta:
                            attr[field] = meta[field]
                        else:
                            attr[field] = ''
                            
                attr['connected'] = ''
                if vname in connected:
                    connections = self.parent._depgraph.connections_to(vname)
                    
                    if self._iotype == 'in':
                        # there can be only one connection to an input
                        attr['connected'] = str([src for src, dst in \
                                                 connections]).replace('@xin.', '')
                    else:
                        attr['connected'] = str([dst for src, dst in \
                                                 connections]).replace('@xout.', '')
                        
                variables.append(attr)
            
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
