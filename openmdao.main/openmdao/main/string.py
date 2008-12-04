
#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.interfaces import IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable
from openmdao.main.vartypemap import add_var_type_map
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
            
class String(Variable):
    """A string Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None, min_length=None, max_length=None):
        Variable.__init__(self, name, parent, iostatus, val_type=basestring, 
                          ref_name=ref_name, 
                          default=default, desc=desc)
        if min_length is not None:
            self.minlen_constraint = MinLengthConstraint(min_length)
            self.add_constraint(self.minlen_constraint)
        if max_length is not None:
            self.maxlen_constraint = MaxLengthConstraint(max_length)
            self.add_constraint(self.maxlen_constraint)
            
            
        
        
add_var_type_map(String, basestring)
