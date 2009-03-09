
#public symbols
__all__ = ["Float"]

__version__ = "0.1"


from openmdao.main.variable import Variable,UNDEFINED
from openmdao.main.vartypemap import add_var_type_map
from openmdao.main.constraint import MinConstraint, MaxConstraint
from Scientific.Physics.PhysicalQuantities import PhysicalQuantity

class Float(Variable):
    """A Variable wrapper for floats"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=UNDEFINED, desc=None, units=UNDEFINED, 
                 min_limit=None, max_limit=None):
        self.units = units
        super(Float, self).__init__(name, parent, iostatus, 
                                    val_type=(float,int,long), 
                                    ref_name=ref_name, 
                                    default=default, desc=desc)
        self._min_limit = None
        self._max_limit = None
        self.min_limit = min_limit
        self.max_limit = max_limit
            
        # test default value against constraints
        self.set_default(default)
    
    def _get_min_limit(self):
        if self._min_limit is None:
            return None
        else:
            return self._min_limit.min
        
    def _set_min_limit(self, value):
        if self._min_limit is None:
            if value is not None:
                self._min_limit = MinConstraint(value)
                self.add_constraint(self._min_limit)
        else:
            if value is None:
                self.remove_constraint(self._min_limit)
                self._min_limit = None
            else:
                self._min_limit.min = value
    
    min_limit = property(_get_min_limit, _set_min_limit, None,
                         'Sets a lower limit on the value of this Variable')
    
    def _get_max_limit(self):
        if self._max_limit is None:
            return None
        else:
            return self._max_limit.max
        
    def _set_max_limit(self, value):
        if self._max_limit is None:
            if value is not None:
                self._max_limit = MaxConstraint(value)
                self.add_constraint(self._max_limit)
        else:
            if value is None:
                self.remove_constraint(self._max_limit)
                self._max_limit = None
            else:
                self._max_limit.max = value
    
    max_limit = property(_get_max_limit, _set_max_limit, None,
                         'Sets an upper limit on the value of this Variable')

    def _get_units(self):
        return self._units
    
    def _set_units(self, units):
        if units is not UNDEFINED:
            # this will raise an exception if units are bad
            try:
                mypq = PhysicalQuantity(1.,units)
            except NameError:
                msg = "Units of '"+str(units)+"' are invalid"
                if '^' in units:
                    msg += ". Exponentiation is indicated by '**', not '^'."
                self.raise_exception(msg, ValueError)
        self._units = units      
        
    units = property(_get_units, _set_units, None,
                     'Sets the units string for this Variable')
    
    def _incompatible_units(self, variable, units):
        raise TypeError(variable.get_pathname()+' units ('+
                        units+') are '+
                        'incompatible with units ('+self.units+') of '+ 
                        self.get_pathname())
        
    def validate_var(self, var):
        """Raise a TypeError if the connecting Variable is incompatible. This
        is called on the INPUT side of a connection."""
        super(Float,self).validate_var(var)
        
        # allow assignment if either Variable has unassigned units
        if self.units is UNDEFINED or var.units is UNDEFINED:
            pass
        else:
            # if units are defined, force compatibility
            mypq = PhysicalQuantity(1.,self.units)
            if not mypq.isCompatible(var.units):
                self._incompatible_units(var, var.units)
                
            # allow value without units to be assigned to val with units

     
    def _convert(self, var):
        """Perform unit conversion here. Validation is not necessary because 
        it's already been done in validate_var.
        """
        if self.units is UNDEFINED or var.units is UNDEFINED:
            return var.value
        else:
            pq = PhysicalQuantity(var.value, var.units)
            pq.convertToUnit(self.units)
            return pq.value
    
    
add_var_type_map(Float, float)

