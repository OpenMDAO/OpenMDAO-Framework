
#public symbols
__all__ = []
__version__ = "0.1"


from openmdao.main.interfaces import IVariable
from openmdao.main.exceptions import ConstraintError
from openmdao.main.variable import Variable
            
class Int(Variable):
    """An integer Variable"""
    
    def __init__(self, name, parent, iostatus, ref_name=None, 
                 default=None, desc=None, min_limit=None, max_limit=None):
        Variable.__init__(self, parent, name, iostatus, ref_name=ref_name, 
                          default=default, desc=desc)
        self.min_limit = min_limit
        self.max_limit = max_limit


    def _pre_assign(self, var):
        """Return the value of the specified Variable after
        converting units if necessary and checking against min and max limits.
        """
        if IVariable.providedBy(var) and isinstance(var, Int):
            newval = var.value
        elif isinstance(var,int):
            newval = var
        else:
            raise ValueError(self.get_pathname()+': assignment to incompatible type '+str(type(var)))
            
        # check against min and max limits
        if self.min_limit is not None and newval < self.min_limit:
            raise ConstraintError(self.get_pathname()+' min_limit violated: '+
                                  str(newval)+' < '+str(self.min_limit))
        if self.max_limit is not None and newval > self.max_limit:
            raise ConstraintError(self.get_pathname()+' max_limit violated: '+
                                  str(newval)+' > '+str(self.max_limit))
            
        return newval
        
        
    def _pre_connect(self, variable, attrname=None):
        """Raise a TypeError if the connecting Variable is incompatible."""
        self._pre_assign(variable, attrname)
            
