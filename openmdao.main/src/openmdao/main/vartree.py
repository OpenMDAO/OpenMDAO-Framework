
import weakref

from enthought.traits.api import Python, Str
from enthought.traits.has_traits import FunctionType

from openmdao.main.variable import Variable
from openmdao.main.container import Container, Slot
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance

class VariableTree(Container):
    
    _iotype = Str('')
    
    def __init__(self, iotype='', doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self._iotype = iotype
        self.on_trait_change(self._iotype_modified, '_iotype')

    @property
    def iotype(self):
        return self._iotype

    @rbac(('owner', 'user'))
    def tree_rooted(self):
        if self.parent:
            if isinstance(self.parent, VariableTree):
                self._iotype = self.parent._iotype
            else:
                t = self.parent.trait(self.name)
                if t and t.iotype:
                    self._iotype = t.iotype
        super(VariableTree, self).tree_rooted()
    
    def add(self, name, obj):
        if isinstance(obj, VariableTree):
            if self.trait(name) is None:
                self.add_trait(name, Slot(VariableTree(), iotype=obj._iotype))
                self.on_trait_change(self._trait_modified, name)
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

    def _iotype_modified(self, obj, name, old, new):
        for k,v in self.__dict__.items():
            if isinstance(v, VariableTree) and v is not self.parent:
                v._iotype = new
        
    def _trait_modified(self, obj, name, old, new):
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
                match_dict = dict([(k,v) for k,v in self._alltraits(**newdict).items() 
                                        if not k.startswith('_')])
            else:
                return  #our children have same iotype as we do, so won't match if we didn't
            
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
                if is_instance(obj, Container) and id(obj) not in visited:
                    if not recurse:
                        yield (name, obj)
                else:
                    yield (name, obj)
