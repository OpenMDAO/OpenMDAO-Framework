
# this is from the rearranged branch ...


#public symbols
__all__ = ['StringList']
__version__ = "0.1"


from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
            

class StringList(Variable):
    """A Variable wrapper for a list of python strings."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, ref_parent=None,
                 default=UNDEFINED, doc=None, min_length=None, max_length=None):
        super(StringList, self).__init__(name, parent, iostatus, val_types=(list), 
                                         ref_name=ref_name, ref_parent=ref_parent,
                                         default=default, doc=doc)            
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
        
            
    def _pre_assign(self, val):
        """Return the value of the specified Variable after
        checking against min and max length limits and verifying that
        all contents are strings.
        """
        newval = super(StringList, self)._pre_assign(val)
        
        nonstrings = [s for s in newval if not isinstance(s,basestring)]
        if len(nonstrings) > 0:
            self.raise_exception('list contains non-string entries',
                                 ValueError)            
            
        return newval
        
    def _pre_assign_entry(self, val, index):
        """Called prior to assigning to an entry of the array value in order to 
        perform validation."""
        if not isinstance(val, basestring):
            self.raise_exception('cannot assign a value of type '+
                                 str(type(val))+' to a StringList entry',
                                 ValueError)
        if not isinstance(index, list) or not isinstance(index[0], int):
            self.raise_exception('invalid list index: '+str(index),
                                 IndexError)            
        if len(index) > 1:
            self.raise_exception('StringList does not support nested lists',
                                 IndexError)
        if index[0] >= len(self.get_value()) or index[0] < 0:
            self.raise_exception('index '+str(index[0])+' out of range',
                                 IndexError)
        return val
            
