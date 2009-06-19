#public symbols
__all__ = ['StringRef', 'StringRefArray']
__version__ = "0.1"

from enthought.traits.api import BaseStr, List, TraitError
from enthought.traits.trait_handlers import NoDefaultSpecified

from openmdao.main.api import ExprEvaluator


class StringRef(BaseStr):
    """A trait that references, via a pathname, another trait in the
    framework. If it's an OUTPUT, then the string may only be the pathname of
    a single variable (with optional array indexing), but if it's an INPUT,
    it may be any valid expression and may reference any number of other
    variables.
    """
    
    def __init__(self, default_value=NoDefaultSpecified, **metadata):
        super(StringRef, self).__init__(default_value, **metadata)
        self.iostatus = metadata.get('iostatus', 'in')

    def validate(self, object, name, value):
        s = super(StringRef, self).validate(object, name, value) # normal string validation
        try:
            if self.iostatus == 'out':
                s = ExprEvaluator(s, object, single_name=True)
            else:
                s = ExprEvaluator(s, object)
        except RuntimeError:
            raise TraitError("invalid %sput ref variable value '%s'"%(self.iostatus,
                                                                        str(value)))
        return s
    
    
class StringRefArray(List):
    """A Variable that contains an array of pathnames that reference other 
    Variables in the framework.
    """
    
    def __init__(self, **metadata):
        self.iostatus = metadata.get('iostatus', 'in')
        super(StringRefArray, self).__init__(trait=StringRef(iostatus=self.iostatus), 
                                             **metadata)
    
    #def _get_referenced_values(self):
        #"""Evaluate the string expressions and return the result."""
        #return [x.evaluate() for x in self._get_exprs()]
    
    #def _set_referenced_values(self, vals):
        #"""Set the values of the objects referred to by our reference strings."""
        #if len(vals) != len(self._get_exprs()):
            #self.raise_exception('StringRefArray and list of assigned values have different lengths')
        #for val,expr in zip(vals, self._get_exprs()):
            ##if __debug__: self.debug('setting refvar %s to %s' % (expr.scoped_assignment_text, str(val)))
            #expr.set(val)
        
    #refvalue = property(_get_referenced_values, _set_referenced_values)
    
    #def get_referenced_varpaths(self):
        #"""Return the set of Variables referenced in the string expression."""
        #if self.iostatus == 'out':
            #sets = [ex.output_names for ex in self._get_exprs()]
        #else:
            #sets = [ex.input_names for ex in self._get_exprs()]
        #ret = set()
        #for s in sets:
            #ret.update(s)
        #return ret
    
    #def get_referenced_compnames(self):
        #"""Return a set of Component names based on the 
        #pathnames of Variables referenced in our reference string. 
        #"""
        #return set([x.split('.')[0] for x in self.get_referenced_varpaths()])
        
    #def refs_invalid(self):
        #if self.parent:
            #parpar = self.parent.parent
            
        #if parpar:
            #for varname in self.get_referenced_varpaths():
                #try:
                    #var = parpar.getvar(varname)
                    #return var.valid is False
                #except NameError:
                    #parts = varname.split('.')
                    #if len(parts) > 2:
                        #var = parpar.getvar('.'.join(parts[:-1]))
                        #return var.valid is False
                    #else:
                        #self.raise_exception("invalid referenced varpath '%s'" %
                                             #varname, NameError)
        #return False
    