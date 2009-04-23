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
        # install a property in our parent's class if the name isn't already used
        if parent and not hasattr(parent.__class__, name) and not hasattr(parent, name):
            setattr(parent.__class__, name,
                property(lambda parent : parent.getvar(name), None, None))
        super(RefVariable, self).__init__(name, parent, iostatus, doc=doc,
                                          val_types=(basestring,str,unicode), 
                                          implicit_creation=False)
            
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
        names of Variables referenced in the string expression. The contents
        of the sets are full Variable pathnames.
        """
        return (self._getexpr().get_external_outputs(), 
                self._getexpr().get_external_inputs())
    
    def get_referenced_components(self):
        """Return a tuple of the form (src_comps, dest_comps) based on
        the names of Variables referenced in the string expression.
        """
        raise NotImplementedError('get_referenced_components')
        

class RefVariableArray(Variable):
    """A Variable that contains an array of pathnames that reference other 
    Variables in the framework.
    """
    
    def __init__(self, name, parent, iostatus, default=UNDEFINED, doc=None):
        self._exprs = []
        # install a property in our parent's class if the name isn't already used
        if parent and not hasattr(parent.__class__, name) and not hasattr(parent, name):
            setattr(parent.__class__, name,
                property(lambda parent : parent.getvar(name), None, None))
        super(RefVariable, self).__init__(name, parent, iostatus, doc=doc,
                                          val_types=(list), 
                                          implicit_creation=False)
            
            
    def _pre_assign(self, val):
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
                                 str(type(val))+' to a RefVarArray entry',
                                 ValueError)
        if not isinstance(index, list) or not isinstance(index[0], int):
            self.raise_exception('invalid list index: '+str(index),
                                 IndexError)            
        if len(index) > 1:
            self.raise_exception('RefVarArray does not support nested lists',
                                 IndexError)
        if index[0] >= len(self.get_value()) or index[0] < 0:
            self.raise_exception('index '+str(index[0])+' out of range',
                                 IndexError)
        return val
            
    def set_value(self, refval):
        refval = self._pre_assign(refval)
        
        self._exprs = []
        if self.iostatus == OUTPUT:
            single_name = True
        else:
            single_name = False
        try:
            for s in refval:
                self._exprs.append(ExprEvaluator(s, self.parent, 
                                                 single_name=single_name))
        except Exception, err:
            self._exprs = []
            raise err
    
    def get_value(self):
        return [s.text for s in self._exprs]
    
    value = property(get_value, set_value)
    
    def _get_referenced_values(self):
        """Evaluate the string expressions and return the result."""
        return [x.evaluate() for x in self._exprs]
    
    def _set_referenced_values(self, vals):
        """Set the values of the objects referred to by our reference strings."""
        if len(vals) != len(self._exprs):
            self.raise_exception('RefVar array and list of assigned values have different lengths')
        for val,expr in zip(vals, self._exprs):
            expr.set(val)
        
    refvalue = property(_get_referenced_values, _set_referenced_values)
    
    def get_referenced_varpaths(self):
        """Return a tuple of the form (src_set, dest_set) based on the
        names of Variables referenced in the string expression.
        """
        outputs = []
        inputs = []
        for expr in self._exprs:
            outputs.extend(expr.get_external_outputs())
            inputs.extend(expr.get_external_inputs())
        return (set(outputs), set(inputs))
    
    def get_referenced_components(self):
        """Return a tuple of the form (src_comps, dest_comps) based on
        the names of Variables referenced in the string expression.
        """
        raise NotImplementedError('get_referenced_components')
    