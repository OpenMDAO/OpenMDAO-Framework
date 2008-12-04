
#public symbols
__all__ = []
__version__ = "0.1"

import copy

from openmdao.main.variable import Variable
from openmdao.main.container import Container
from openmdao.main.interfaces import IVariable
from openmdao.main.vartypemap import add_var_type_map


class ContainerVariable(Variable):
    """A Variable class that refers to a container class instance."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, default=None, desc=None):
        Variable.__init__(self, name, parent, iostatus, ref_name=ref_name, 
                          default=default, desc=desc)


    def getvar(self, path):
        if path is None:
            return self
        container = getattr(self._parent, self.ref_name)
        return container.getvar(path)


    def get(self, path):
        if path is None:
            return self.value
        try:
            return getattr(self, path)
        except:
            container = getattr(self._parent, self.ref_name)
            return container.get(path)


    def set(self, path, value):
        if path is None:
            val = self._pre_assign(value)
            newcont = copy.deepcopy(val)
            newcont.name = self.name
            newcont._parent = self._parent
            setattr(self._parent, self.ref_name, newcont)  # this will replace the container
            return
        
        container = getattr(self._parent, self.ref_name)
        container.set(path, value)


add_var_type_map(ContainerVariable, Container)
