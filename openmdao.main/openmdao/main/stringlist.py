
#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.interfaces import IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
            

class StringList(Variable):
    """A list of strings"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None, min_length=None, max_length=None):
        Variable.__init__(self, name, parent, iostatus, val_type=list, 
                          ref_name=ref_name, 
                          default=default, desc=desc)
        if min_length is not None:
            self.minlen_constraint = MinLengthConstraint(min_length)
            self.add_constraint(self.minlen_constraint)
        if max_length is not None:
            self.maxlen_constraint = MaxLengthConstraint(max_length)
            self.add_constraint(self.maxlen_constraint)
            
    def _pre_assign(self, val):
        """Return the value of the specified Variable after
        checking against min and max length limits.
        """
        newval = Variable._pre_assign(val)
        
        for strng in newval:
            if not isinstance(strng, basestring):
                raise ValueError(self.get_pathname()+': list contains non-string entries')
            
        return newval
        
