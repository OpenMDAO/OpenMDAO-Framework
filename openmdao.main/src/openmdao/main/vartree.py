
from enthought.traits.api import Python

from openmdao.main.container import Container
from openmdao.main.rbac import rbac


class VariableTree(Container):
    
    def __init__(self, iotype=None, doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self._iotype = None
        self.iotype = iotype
                        
    @property
    def iotype(self):
        return self._iotype
    
    @iotype.setter
    def iotype(self, val):
        if val != self._iotype:
            if val == 'in':
                remove = False
            else:
                remove = True
            self.on_trait_change(self._input_trait_modified, '+', remove=remove)
            for k,v in self.__dict__.items():
                if isinstance(v, VariableTree) and k != '_parent':
                    v.iotype = val

    @rbac(('owner', 'user'))
    def tree_rooted(self):
        if self.parent and not isinstance(self.parent, VariableTree):
            t = self.parent.trait(self.name)
            if t and t.iotype:
                self.iotype = t.iotype
        super(VariableTree, self).tree_rooted()
    
    def _input_trait_modified(self, obj, name, old, new):
        if name.startswith('_'):
            return
        p = self
        while isinstance(p, VariableTree):
            vt = p
            p = p.parent
        if p is not None:
            p._input_trait_modified(p, vt.name, vt, vt)
        
    def get_iotype(self, name):
        """Return the iotype of the Variable with the given name"""
        t = self.get_trait(name)
        if t is None:
            self.raise_exception("'%s' not found" % name)
        if self.iotype is None:
            self.iotype = self._find_iotype()
            
        return self.iotype
        
    def _find_iotype(self):
        """Figure out our iotype if it hasn't been set"""
        p = self
        while isinstance(p, VariableTree):
            if p.iotype:
                return p.iotype
            vt = p
            p = p.parent
        if isinstance(p, Container):
            t = p.trait(vt.name)
            if t:
                return t.iotype
        return None
