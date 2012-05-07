import re

from enthought.traits.api import TraitType
from enthought.traits.trait_handlers import NoDefaultSpecified
from openmdao.main.interfaces import implements, IVariable

# regex to check for valid names. 
namecheck_rgx = re.compile(
    '([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')
            
def is_legal_name(name):
    match = namecheck_rgx.match(name)
    return not (match is None or match.group() != name)

class Variable(TraitType):
    """An OpenMDAO-specific trait type that serves as a common base
    class for framework visible inputs and outputs.
    """
    implements(IVariable)
    
    def __init__ ( self, default_value = NoDefaultSpecified, **metadata ):
        if 'vartypename' not in metadata:
            metadata['vartypename'] = self.__class__.__name__
        super(Variable, self).__init__(default_value=default_value, **metadata)



