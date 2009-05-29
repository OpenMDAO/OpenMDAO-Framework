#public symbols
__all__ = ['RefVariable', 'RefVariableArray']
__version__ = "0.1"


from openmdao.main.variable import Variable, INPUT, OUTPUT, UNDEFINED
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
from openmdao.main import ExprEvaluator

    
class RefVariable(Variable):
    """A Variable that references, via a pathname, another Variable in the
    framework. If it's an OUTPUT, then the string may only be the pathname of
    a single variable (with optional array indexing), but if it's an INPUT,
    it may be any valid expression and may reference any number of other
    variables.
    """
    
    def __init__(self, name, parent, iostatus, default=UNDEFINED, doc=None):
        self._expr = None
        self._value = None
        # put self in our parent's __dict__ if the name isn't already used
        if parent and not hasattr(parent.__class__, name) and not hasattr(parent, name):
            parent.__dict__[name] = self
        else:
            self.warning('unable to register RefVariable %s in parent __dict__ due to name conflict'%
                         name)
        super(RefVariable, self).__init__(name, parent, iostatus, doc=doc,
                                          val_types=(basestring,str,unicode), 
                                          implicit_creation=False)

    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(RefVariable, self).__getstate__()
        state['_expr'] = None
        return state

    def _getexpr(self):
        if self._expr is None:
            if self._value is not None:
                self.set_value(self._value)
            else:
                self.raise_exception('reference is undefined', RuntimeError)
        return self._expr
    
    def set_value(self, refval):
        if isinstance(refval, basestring):
            if self.iostatus == OUTPUT:
                single_name = True
            else:
                single_name = False
            if self.parent:
                try:
                    self._expr = ExprEvaluator(refval, self.parent, 
                                               single_name=single_name)
                except RuntimeError, err:
                    msg = str(err)
                    if msg.startswith('Expected'):
                        direction = 'output' if self.iostatus==OUTPUT else 'input'
                        self.raise_exception("invalid %s ref variable value '%s'" %
                                             (direction, refval), RuntimeError)
                    else:
                        self.raise_exception(str(err), RuntimeError)
            else:
                self.raise_exception('RefVariable requires self.parent to exist.',
                                     RuntimeError)
            self._value = refval
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
        """Return a set of source or dest Variable pathnames relative to
        self.parent and based on the names of Variables referenced in our 
        reference string. 
        """
        if self.iostatus == OUTPUT:
            return self._getexpr().output_names
        else:
            return self._getexpr().input_names
    
    def get_referenced_compnames(self):
        """Return a set of source or dest Component names based on the 
        pathnames of Variables referenced in our reference string. 
        """
        return set([x.split('.')[0] for x in self.get_referenced_varpaths()])
        

class RefVariableArray(Variable):
    """A Variable that contains an array of pathnames that reference other 
    Variables in the framework.
    """
    
    def __init__(self, name, parent, iostatus, default=UNDEFINED, doc=None):
        self._exprs = []
        self._value = []
        # put ourself in our parent's __dict__ if the name isn't already used
        if parent and not hasattr(parent.__class__, name) and not hasattr(parent, name):
            parent.__dict__[name] = self
        else:
            self.warning('unable to register RefVariableArray %s in parent __dict__ due to name conflict'%
                         name)
        super(RefVariableArray, self).__init__(name, parent, iostatus, doc=doc,
                                          val_types=(list), 
                                          implicit_creation=False)
            
            
    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(RefVariableArray, self).__getstate__()
        state['_exprs'] = None
        return state

    def __len__(self):
        return len(self._get_exprs())
    
    def _get_exprs(self):
        # self._exprs should only be none after we've been unpickled
        if self._exprs is None:
            self.set_value(self._value)
        return self._exprs
    
    def _pre_assign(self, val):
        newval = super(RefVariableArray, self)._pre_assign(val)
        
        if not all([isinstance(s,basestring) for s in newval]):
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
            self.raise_exception('RefVariableArray does not support nested lists',
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
            if self.parent:
                for s in refval:
                    try:
                        self._exprs.append(ExprEvaluator(s, self.parent, 
                                                         single_name=single_name))
                    except RuntimeError, err:
                        self.raise_exception(str(err), RuntimeError)
                self._value = refval
            else:
                self.raise_exception('RefVariableArray requires self.parent to exist.',
                                     RuntimeError)
        except Exception, err:
            self._exprs = []
            raise err
    
    def get_value(self):
        return [s.text for s in self._get_exprs()]
    
    value = property(get_value, set_value)
    
    def _get_referenced_values(self):
        """Evaluate the string expressions and return the result."""
        return [x.evaluate() for x in self._get_exprs()]
    
    def _set_referenced_values(self, vals):
        """Set the values of the objects referred to by our reference strings."""
        if len(vals) != len(self._get_exprs()):
            self.raise_exception('RefVariableArray and list of assigned values have different lengths')
        for val,expr in zip(vals, self._get_exprs()):
            #if __debug__: self.debug('setting refvar %s to %s' % (expr.scoped_assignment_text, str(val)))
            expr.set(val)
        
    refvalue = property(_get_referenced_values, _set_referenced_values)
    
    def get_referenced_varpaths(self):
        """Return the set of Variables referenced in the string expression."""
        if self.iostatus == OUTPUT:
            sets = [ex.output_names for ex in self._get_exprs()]
        else:
            sets = [ex.input_names for ex in self._get_exprs()]
        ret = set()
        for s in sets:
            ret.update(s)
        return ret
    
    def get_referenced_compnames(self):
        """Return a set of Component names based on the 
        pathnames of Variables referenced in our reference string. 
        """
        return set([x.split('.')[0] for x in self.get_referenced_varpaths()])
        
    