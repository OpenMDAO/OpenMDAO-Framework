""" Base class for all OpenMDAO variables
"""

#public symbols
__all__ = ["Variable", "gui_excludes"]

import re

from enthought.traits.api import TraitType
from enthought.traits.trait_handlers import NoDefaultSpecified
from openmdao.main.interfaces import implements, IVariable

# regex to check for valid names. 
namecheck_rgx = re.compile(
    '([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')
         
gui_excludes = ['type', 'vartypename', 'iotype', 'copy']
            
def is_legal_name(name):
    '''Verifies a Pythonic legal name for use as an OpenMDAO variable.'''
    
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
        
    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. The basic functionality that
        most variables need is provided here; you can overload this for
        special cases, like lists and dictionaries, or custom datatypes.
        
        name: str
          Name of variable
          
        value: object
          The value of the variable
          
        trait: CTrait
          The variable's trait
          
        meta: dict
          Dictionary of metadata for this variable
        """
        
        attr = {}
        
        attr['name'] = name
        attr['type'] = type(value).__name__
        attr['value'] = value
        
        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]
        
        return attr, None



