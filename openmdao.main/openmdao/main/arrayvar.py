
#public symbols
__all__ = []
__version__ = "0.1"

import numpy

from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable, undefined


class ArrayVariable(Variable):
    """A Variable object that wraps a numpy array."""

    def __init__(self, name, parent, iostatus, entry_type, fixed_size=None,
                 ref_name=None, default=undefined, desc=None):
        self.dtype = numpy.dtype(entry_type)
        self.fixed_size = fixed_size
        Variable.__init__(self, name, parent, iostatus, 
                          ref_name=ref_name, default=default, desc=desc)

        
    def validate_var(self, var):
        """Raise a TypeError if the connecting Variable is incompatible. This is called
        on the INPUT side of a connection."""
        Variable.validate_var(self, var)
        if var.dtype is not self.dtype:
            self.raise_exception('numpy data type '+str(self.dtype)+
                                 'is not compatible with type '+str(var.dtype), ValueError)
        
        
    def _pre_assign(self, val):
        """Returns the transformed, validated value, or raises a ValueError."""
        if isinstance(val, numpy.ndarray):
            if val.dtype is self.dtype:
                check_val = val
            else:
                self.raise_exception('incompatable numpy data types', ValueError)
        else:
            self.raise_exception('incompatable with type '+str(type(val)), ValueError)
            
        if self.fixed_size is not None and self.fixed_size != check_val.shape:
            self.raise_exception('expected array of size '+
                                 str(self.fixed_size)+' but got '+
                                 str(check_val.shape), ValueError)
        return check_val
                   
