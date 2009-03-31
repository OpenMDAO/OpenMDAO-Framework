#public symbols
__all__ = ['Bool']
__version__ = "0.1"

from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.vartypemap import add_var_type_map
            
class Bool(Variable):
    """A Variable wrapper for booleans."""
    
    def __init__(self, name, parent, iostatus, ref_name=None, ref_parent=None,
                 default=UNDEFINED, doc=None):
        super(Bool, self).__init__(name, parent, iostatus,
                                   val_type=[bool, int], 
                                   ref_name=ref_name, ref_parent=ref_parent,
                                   default=default, doc=doc)
        self.set_default(default)
    
    def _pre_assign(self, val):
        """ If valid type, convert to bool. """
        super(Bool, self)._pre_assign(val)
        return bool(val)


add_var_type_map(Bool, bool)

