#public symbols
__all__ = ['StringRef', 'StringRefArray']
__version__ = "0.1"

from enthought.traits.api import BaseStr, List, TraitError
from enthought.traits.trait_handlers import NoDefaultSpecified

from openmdao.main.api import ExprEvaluator

class DumbDefault(object):
    def __getattr__(self, name):
        raise TraitError('StringRef: string reference is undefined')
            
class StringRef(BaseStr):
    """A trait that references, via a pathname, another trait in the
    framework. If it has iostatus of 'out', then the string may only be the pathname of
    a single variable (with optional array indexing), but if iostatus is 'in',
    it may be any valid expression and may reference any number of other
    variables.
    """
    
    def __init__(self, default_value=NoDefaultSpecified, **metadata):
        if default_value is NoDefaultSpecified:
            default_value = DumbDefault()
#            default_value = ExprEvaluator('0')
        super(StringRef, self).__init__(default_value, **metadata)
        #self.iostatus = metadata.get('iostatus', 'in')

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
    """A List of StringRef traits."""
    
    def __init__(self, **metadata):
        self.iostatus = metadata.get('iostatus', 'in')
        super(StringRefArray, self).__init__(trait=StringRef(iostatus=self.iostatus), 
                                             **metadata)
    
