
import weakref

from openmdao.main.variable import Variable
from openmdao.main.container import Container, Slot
from openmdao.main.rbac import rbac

class VariableTree(Container):
    
    def __init__(self, iotype=None, doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self.iotype = iotype
        self.on_trait_change(self._trait_modified, '+')

    @rbac(('owner', 'user'))
    def tree_rooted(self):
        if self.parent:
            if isinstance(self.parent, VariableTree):
                self.iotype = self.parent.iotype
            else:
                t = self.parent.trait(self.name)
                if t and t.iotype:
                    self.iotype = t.iotype
        super(VariableTree, self).tree_rooted()
    
    def add(self, name, obj):
        if isinstance(obj, VariableTree):
            if self.trait(name) is None:
                self.add_trait(name, Slot(VariableTree()))
        super(VariableTree, self).add(name, obj)
        
    def _trait_modified(self, obj, name, old, new):
    #def _anytrait_changed(self, name, old, new):
        print 'name = %s' % name
        if name == 'iotype':
            for k,v in self.__dict__.items():
                if isinstance(v, VariableTree) and k is not self:
                    v.iotype = new
        elif not (name=='trait_added' or name.startswith('_')):
            if isinstance(new, VariableTree):
                obj = getattr(self, name)
                obj.parent = self
                obj.iotype = self.iotype
            if self.iotype == 'in':
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
        return self.iotype
