"""
Trait for complex variables.
"""

#public symbols
__all__ = ["Complex"]

# pylint: disable-msg=E0611,F0401
from traits.api import Complex as Enthought_Complex

from openmdao.main.variable import Variable


class Complex(Variable):
    """A variable wrapper for a complex variable.
    """

    def __init__(self, default_value=(0.+0.j), iotype=None, desc=None,
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype

        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        self._validator = Enthought_Complex(default_value=default_value,
                                            **metadata)

        super(Complex, self).__init__(default_value=default_value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        return self._validator.validate(obj, name, value)

