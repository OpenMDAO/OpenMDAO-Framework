
#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.variable import Variable
from openmdao.main.interfaces import IVariable
from copy import deepcopy


class ContainerVariable(Variable):
    """A Variable class that refers to a class instance."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, default=None, desc=None):
        Variable.__init__(self, name, parent, iostatus, ref_name=ref_name, 
                          default=default, desc=desc)


    def get(self, path):
        cont = self._parent.get(self.ref_name)
        if path is None: # set our value
            return cont
        else:
            return cont.get(path)

    def set(self, path, value):
        cont = self._parent.get(self.ref_name)

        cont.set(path, value)

