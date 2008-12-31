"""
Int: a Variable wrapper for ints
"""

#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.vartypemap import add_var_type_map
from openmdao.main.constraint import MinConstraint, MaxConstraint
            
class Int(Variable):
    """An integer Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=UNDEFINED, desc=None, min_limit=None, max_limit=None):
        super(Int, self).__init__(name, parent, iostatus, val_type=[int,long], 
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
    
    min_limit = property(_get_min_limit, _set_min_limit)
    
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
    
    max_limit = property(_get_max_limit, _set_max_limit)
            

add_var_type_map(Int, int)
