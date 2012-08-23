"""
Trait for boolean variables.
"""

#public symbols
__all__ = ["Bool"]

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Bool as Enthought_Bool

from openmdao.main.variable import Variable

class Bool(Variable):
    """A variable wrapper for a boolean variable.
       """
    
    def __init__(self, default_value=None, iotype=None, desc=None, 
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        self._validator = Enthought_Bool(value=default_value, **metadata)
            
        super(Bool, self).__init__(default_value=default_value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        return self._validator.validate(obj, name, value)

    def create_editor(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.create_editor()

