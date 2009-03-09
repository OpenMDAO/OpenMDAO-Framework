#public symbols
__all__ = ['ContainerVariable']
__version__ = "0.1"

import copy

from openmdao.main import Variable
from openmdao.main import Container
from openmdao.main.vartypemap import add_var_type_map


class ContainerVariable(Variable):
    """A Variable class that wraps a Container."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None):
        super(ContainerVariable, self).__init__(name, parent, iostatus, 
                                                ref_name=ref_name, 
                                                default=default, desc=desc)

    def getvar(self, name=None):
        """Return this Variable or a Variable child of our value, which is
        itself a Container.
        """
        if name is None:
            return self
        container = getattr(self.parent, self.ref_name)
        return container.getvar(name)

    def get(self, name=None, index=None):
        """Return the value of this Variable, one of its attributes,
        or one of the children of its value, which is itself a Container.
        """
        if name is None:
            val = self.value
            if index is None:
                return val
            for i in index:
                val = val[i]
            return val
        
        if hasattr(self, name):
            if index is None:
                return getattr(self, name)
            else:
                val = getattr(self, name)
                try:
                    for i in index:
                        val = val[i]
                    return val
                except TypeError, err:
                    self.raise_exception(str(err), TypeError)
        else:
            container = getattr(self.parent, self.ref_name)
            return container.get(name, index)


    def set(self, path, value, index=None):
        """Set the value of this Variable, one of its attributes, or one of the
        children of its value, which is itself a Container.
        """
        if path is None and index is None:
            val = self._pre_assign(value)
            newcont = copy.deepcopy(val)
            newcont.name = self.name
            newcont.parent = self.parent
            # this will replace the container
            setattr(self.parent, self.ref_name, newcont)  
            return
        
        container = getattr(self.parent, self.ref_name)
        container.set(path, value, index)

    def contains(self, path):
        """Return true if a child with the given path name exists in the public
        interface of this object.
        """
        if hasattr(self, path):
            return True
        else:
            container = getattr(self.parent, self.ref_name)
            return container.contains(path)

add_var_type_map(ContainerVariable, Container)
