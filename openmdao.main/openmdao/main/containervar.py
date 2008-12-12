
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
        container = getattr(self.parent, self.ref_name)
        return container.getvar(path)

    def get(self, path, index=None):
        if path is None:
            val = self.value
            if index is None:
                return val
            for i in index:
                val = val[i]
            return val
        try:
            if index is None:
                return getattr(self, path)
            else:
                val = getattr(self, path)
                for i in index:
                    val = val[i]
                return val
        except:
            container = getattr(self.parent, self.ref_name)
            return container.get(path, index)


    def set(self, path, value, index=None):
        if path is None and index is None:
            val = self._pre_assign(value)
            newcont = copy.deepcopy(val)
            newcont.name = self.name
            newcont.parent = self.parent
            setattr(self.parent, self.ref_name, newcont)  # this will replace the container
            return
        
        container = getattr(self.parent, self.ref_name)
        container.set(path, value, index)

    def contains(self, path):
        try:
            base, name = path.split('.',1)
        except ValueError:
            if hasattr(self, path):
                return True
        container = getattr(self.parent, self.ref_name)
        return container.contains(path)

add_var_type_map(ContainerVariable, Container)
