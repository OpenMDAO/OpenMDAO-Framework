
import weakref

from enthought.traits.api import Python
from enthought.traits.trait_base import not_none
from enthought.traits.has_traits import FunctionType

from openmdao.main.variable import Variable
from openmdao.main.container import Container, Slot
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance

class VariableTree(Container):
    
    _iotype = Python()
    
    def __init__(self, iotype=None, doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self._iotype = iotype
        self.on_trait_change(self._iotype_modified, '_iotype')

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
                self.add_trait(name, Slot(VariableTree()))
                self.on_trait_change(self._trait_modified, name)
        super(VariableTree, self).add(name, obj)
        
    def add_trait(self, name, trait):
        super(VariableTree, self).add_trait(name, trait)
        self.on_trait_change(self._trait_modified, name)
            
    def remove_trait(self, name):
        trait = self.get_trait(name)
        
        # remove the callback
        if trait:
            self.on_trait_change(self._trait_modified, name, remove=True)
            
        super(VariableTree, self).remove_trait(name)

    def _iotype_modified(self, obj, name, old, new):
        for k,v in self.__dict__.items():
            if isinstance(v, VariableTree) and k is not self:
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
            if p is not None:
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
        if 'iotype' not in metadata:
            for k,v in super(VariableTree, self)._items(visited, recurse=recurse,
                                                        **metadata):
                yield k,v
            return
        
        if id(self) not in visited:
            visited.add(id(self))
            
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
                match_dict = dict([(k,v) for k,v in self._filtertraits(**newdict).items() 
                                        if not k.startswith('_')])
            else:
                return  #iotype won't match any of our children either
            
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
