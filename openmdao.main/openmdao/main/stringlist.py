"""
StringList: a Variable wrapper for a list of python strings.
"""

#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
            

class StringList(Variable):
    """A list of strings"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=UNDEFINED, desc=None, min_length=None, max_length=None):
        super(StringList, self).__init__(name, parent, iostatus, val_type=list, 
                                         ref_name=ref_name, 
                                         default=default, desc=desc)            
        if min_length is not None:
            self.minlen_constraint = MinLengthConstraint(min_length)
            self.add_constraint(self.minlen_constraint)
        if max_length is not None:
            self.maxlen_constraint = MaxLengthConstraint(max_length)
            self.add_constraint(self.maxlen_constraint)
        
        # test default value against constraints
        self.set_default(default)
        
            
    def _pre_assign(self, val):
        """Return the value of the specified Variable after
        checking against min and max length limits.
        """
        newval = super(StringList,self)._pre_assign(val)
        
        for strng in newval:
            if not isinstance(strng, basestring):
                raise ValueError(self.get_pathname()+
                                 ': list contains non-string entries')
            
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
        if index[0] >= len(self.value) or index[0] < 0:
            self.raise_exception('index '+str(index[0])+' out of range',
                                 IndexError)
        return val
            
