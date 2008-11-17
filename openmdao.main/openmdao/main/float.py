
#public symbols
__all__ = ["Float"]

__version__ = "0.1"


from openmdao.main.interfaces import IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable


class Float(Variable):
    """A float Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None, units=None, min_limit=None, max_limit=None):
        self.units = units
        self.min_limit = min_limit
        self.max_limit = max_limit
        Variable.__init__(self, name, parent, iostatus, ref_name=ref_name, 
                          default=default, desc=desc)


    def incompatible_units(self, variable, units):
        raise TypeError(variable.get_pathname()+' units ('+
                        units+') are '+
                        'incompatible with units ('+self.units+') of '+ 
                        self.get_pathname())
        
            
    def _pre_assign(self, var, attrname=None):
        """Return the value of the specified Variable after
        converting units if necessary and checking against min and max limits.
        """
        if attrname is not None:
            var = var.get(attrname)
            
        tvar = type(var)
        if IVariable.providedBy(var):
            if tvar is Float:
                if self.units is None:
                    newval = var.value
                else:
                    # for now, force exact unit match
                    if var.units is not None and var.units != self.units: 
                        self.incompatible_units(var, var.units)
                    # ??? eventually do unit conversion here
                    # for now, allow value without units to be assigned to val with units
                    newval = var.value
            elif tvar is Int:
                if self.units is not None:
                    raise ValueError(self.get_pathname()+': cannot assign an Int to a Float with units')
                else:
                    newval = var.value
            else:
                raise ValueError(self.get_pathname()+': assignment to incompatible type '+str(type(var)))
        else:
            if tvar is float or tvar is int:
                newval = var
            else:
                raise ValueError(self.get_pathname()+': assignment to incompatible type '+str(type(var)))
            
        # check against min and max limits
        if self.min_limit is not None and newval < self.min_limit:
            raise ConstraintError(self.get_pathname()+' min_limit violated: '+
                                  str(newval)+' < '+str(self.min_limit))
        if self.max_limit is not None and newval > self.max_limit:
            raise ConstraintError(self.get_pathname()+' max_limit violated: '+
                                  str(newval)+' > '+str(self.max_limit))
            
        return newval
        
        
    def _pre_connect(self, variable, attrname=None):
        """Raise a TypeError if the connecting Variable is incompatible."""
        self._pre_assign(variable, attrname)
