"""
String: a Variable wrapper for python strings"""

#public symbols
__all__ = ['String']
__version__ = "0.1"


from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.vartypemap import add_var_type_map
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
            
class String(Variable):
    """A Variable wrapper for a python string attribute."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, refparent=None,
                 default=UNDEFINED, desc=None, min_length=None, max_length=None):
        super(String, self).__init__(name, parent, iostatus, 
                                     val_type=basestring, 
                                     ref_name=ref_name, refparent=refparent,
                                     default=default, desc=desc)
        self._min_length = None
        self._max_length = None
        self.min_length = min_length
        self.max_length = max_length
            
        # test default value against constraints
        self.set_default(default)
    
    def _get_min_length(self):
        if self._min_length is None:
            return None
        else:
            return self._min_length.minlen
        
    def _set_min_length(self, value):
        if self._min_length is None:
            if value is not None:
                self._min_length = MinLengthConstraint(value)
                self.add_constraint(self._min_length)
        else:
            if value is None:
                self.remove_constraint(self._min_length)
                self._min_length = None
            else:
                self._min_length.minlen = value
    
    min_length = property(_get_min_length, _set_min_length)
    
    def _get_max_length(self):
        if self._max_length is None:
            return None
        else:
            return self._max_length.maxlen
        
    def _set_max_length(self, value):
        if self._max_length is None:
            if value is not None:
                self._max_length = MaxLengthConstraint(value)
                self.add_constraint(self._max_length)
        else:
            if value is None:
                self.remove_constraint(self._max_length)
                self._max_length = None
            else:
                self._max_length.maxlen = value
    
    max_length = property(_get_max_length, _set_max_length)
    
    
add_var_type_map(String, basestring)
