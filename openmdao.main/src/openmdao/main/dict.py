#public symbols
__all__ = ['Dict']
__version__ = "0.1"

from openmdao.main.variable import Variable, UNDEFINED
from openmdao.main.vartypemap import add_var_type_map

class Dict(Variable):
    """A Variable wrapper for a dictionary."""

    def __init__(self, name, parent, iostatus, ref_name=None, ref_parent=None,
                 default=UNDEFINED, doc=None):
        super(Dict, self).__init__(name, parent, iostatus, val_types=(dict,),
                                   ref_name=ref_name, ref_parent=ref_parent,
                                   default=default, doc=doc)
        self.set_default(default)

add_var_type_map(Dict, dict)

