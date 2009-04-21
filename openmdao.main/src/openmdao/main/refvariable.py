#public symbols
__all__ = ['RefVariable']
__version__ = "0.1"


from openmdao.main.variable import Variable, INPUT, OUTPUT, UNDEFINED
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
from openmdao.main import ExprEvaluator
            
class RefVariable(Variable):
    """A Variable that references, via a pathname, another Variable in the
    framework.
    """
    
    def __init__(self, name, parent, iostatus, default=UNDEFINED, doc=None):
        self._expr = None
        if parent:
            if not hasattr(parent.__class__, name) and not hasattr(parent, name):
                setattr(parent.__class__, name,
                    property(lambda parent : parent.getvar(name), None, None))
                             #lambda parent, val : parent.getvar(name).set_value(val),
                             #None))
        super(RefVariable, self).__init__(name, parent, iostatus, doc=doc,
                                          val_types=(basestring,str,unicode), 
                                          implicit_creation=False)
        if default != UNDEFINED:
            self.set_value(default)
            
            
    def _getexpr(self):
        if self._expr is None:
            self.raise_exception('reference is undefined', RuntimeError)
        return self._expr
    
    def set_value(self, refval):
        if isinstance(refval, basestring):
            if self.iostatus == OUTPUT:
                single_name = True
            else:
                single_name = False
            self._expr = ExprEvaluator(refval, self.parent, 
                                       single_name=single_name)
        else:
            self.raise_exception('reference must be a string', TypeError)
    
    def get_value(self):
        return self._getexpr().text
    
    value = property(get_value, set_value)
    
    def _get_referenced_value(self):
        """Evaluate the string expression and return the result."""
        return self._getexpr().evaluate()
    
    def _set_referenced_value(self, val):
        """Set the value of the object referred to by our reference string."""
        self._getexpr().set(val)
        
    refvalue = property(_get_referenced_value, _set_referenced_value)
    
    def get_referenced_varpaths(self):
        """Return a tuple of the form (src_set, dest_set) based on the
        names of Variables referenced in the string expression.
        """
        if self._expr is None or self._expr._text != self.get_value():
            self._expr = ExprEvaluator(self.get_value(), self.parent)
        return (self._expr.get_external_outputs(), self._expr.get_external_inputs())
    
    def get_referenced_components(self):
        """Return a tuple of the form (src_comps, dest_comps) based on
        the names of Variables referenced in the string expression.
        """
        raise NotImplementedError('get_referenced_components')
        
    