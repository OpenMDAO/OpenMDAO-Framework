"""
Bool: a Variable wrapper for booleans
"""

#public symbols
__all__ = []
__version__ = "0.1"

from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.vartypemap import add_var_type_map
            
class Bool(Variable):
    """A boolean Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=UNDEFINED, desc=None):
        super(Bool, self).__init__(name, parent, iostatus, val_type=[bool], 
                                   ref_name=ref_name, 
                                   default=default, desc=desc)
        self.set_default(default)
    

add_var_type_map(Bool, bool)

