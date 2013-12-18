"""
Trait for string variables.
"""

#public symbols
__all__ = ["Str"]

# pylint: disable-msg=E0611,F0401
from traits.api import Str as Enthought_Str

from openmdao.main.variable import Variable

class Str(Variable):
    """A variable wrapper for a string variable.
       """
    
    def __init__(self, default_value='', iotype=None, desc=None, 
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        self._validator = Enthought_Str(default_value=default_value, **metadata)
            
        super(Str, self).__init__(default_value=default_value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        return self._validator.validate(obj, name, value)

    def create_editor(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.create_editor()

