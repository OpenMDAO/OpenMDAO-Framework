
#public symbols
__all__ = []
__version__ = "0.1"

import numpy

from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable


class ArrayVariable(Variable):
    """A Variable object that wraps a numpy array."""

    def __init__(self, name, iostatus, entry_type, fixed_size=None,
                 ref_name=None, parent=None, default=None, desc=None):
        self.dtype = numpy.dtype(entry_type)
        self.fixed_size = fixed_size
        Variable.__init__(self, name, iostatus, ref_name, parent, default, desc)

    def _pre_assign(self, var, attribname=None):
        """Returns the transformed, validated value, or raises a ValueError."""
        if isinstance(var,ArrayVariable):
            if var.dtype is self.dtype:
                check_val = var.value
            else:
                raise ValueError(self.get_pathname()+': incompatable numpy data types')
        else:
            if isinstance(var,list):
                try:
                    tmp = numpy.array(var)
                except ValueError, err:
                    raise ValueError(self.get_pathname()+': '+str(err))
                var = tmp
            if isinstance(var,numpy.ndarray):
                if var.dtype is self.dtype:
                    check_val = var
                else:
                    raise ValueError(self.get_pathname()+': incompatable numpy data types') 
            else:
                raise ValueError(self.get_pathname()+': incompatable with type '+str(type(var)))
            
        if self.fixed_size is not None and self.fixed_size != check_val.shape:
            raise ValueError(self.get_pathname()+': expected array of size '+
                             str(self.fixed_size)+' but got '+str(check_val.shape))
        return check_val
                   
    def _pre_connect(self, variable, attribname=None):
        """Raise a TypeError if the connecting variable is not compatible with this one.
        This function should be overridden in any derived classes that require validation 
        at connect time.
        """
        self._pre_assign(variable, attribname)

        
