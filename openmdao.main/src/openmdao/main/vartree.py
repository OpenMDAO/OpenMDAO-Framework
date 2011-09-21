
import weakref

from enthought.traits.api import Python

from openmdao.main.container import Container
from openmdao.main.rbac import rbac

class VariableTree(Container):
    
    def __init__(self, iotype=None, doc=None):
        super(VariableTree, self).__init__(doc=doc)
        #self._iotype = None
        self.iotype = iotype
        self.on_trait_change(self._input_trait_modified, '+')

    #@property
    #def iotype(self):
        #return self._iotype

    #@iotype.setter
    #def iotype(self, val):
        #if val != self._iotype:
            #self._iotype = val
            #if val == 'in':
                #remove = False
            #else:
                #remove = True

            ##self.on_trait_change(self._input_trait_modified, '+', remove=remove)
            #for k,v in self.__dict__.items():
                #if isinstance(v, VariableTree) and k is not self:
                    #v.iotype = val

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
        super(VariableTree, self).add(name, obj)
        self.on_trait_change(self._input_trait_modified, '+')
        
    def _input_trait_modified(self, obj, name, old, new):
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
        t = self.get_trait(name)
        if t is None:
            self.raise_exception("'%s' not found" % name)
        return self.iotype
