"""
Int: a Variable wrapper for ints
"""

#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.variable import Variable
from openmdao.main.vartypemap import add_var_type_map
from openmdao.main.constraint import MinConstraint, MaxConstraint
            
class Int(Variable):
    """An integer Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None, min_limit=None, max_limit=None):
        super(Int, self).__init__(name, parent, iostatus, val_type=[int,long], 
                                  ref_name=ref_name, 
                                  default=default, desc=desc)
        if min_limit is not None:
            self.min_constraint = MinConstraint(min_limit)
            self.add_constraint(self.min_constraint)
        if max_limit is not None:
            self.max_constraint = MaxConstraint(max_limit)
            self.add_constraint(self.max_constraint)
        
            

add_var_type_map(Int, int)
