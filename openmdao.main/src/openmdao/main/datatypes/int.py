"""
Trait for integer variables, with optional high and low.
"""

#public symbols
__all__ = ["Int"]

from sys import maxint

# pylint: disable=E0611,F0401
from traits.api import Range

from openmdao.main.variable import Variable

class Int(Variable):
    """A variable wrapper for an integer valid within a
       specified range of values.
       """

    def __init__(self, default_value=None, iotype=None, desc=None,
                 low=None, high=None, exclude_low=False, exclude_high=False,
                 **metadata):

        # Range trait didn't seem to handle "None" correctly when passed on
        # the  command line.
        assumed_default = False
        if default_value is None:
            assumed_default = True
            if low is None and high is None:
                default_value = 0
            elif low is None:
                default_value = high
            else:
                default_value = low

        if low is None:
            low = -maxint
        if high is None:
            high = maxint

        if not isinstance(default_value, int):
            raise ValueError("Default value for an Int must be an integer.")

        if not isinstance(low, int):
            raise ValueError("Lower bound for an Int must be an integer.")

        if not isinstance(high, int):
            raise ValueError("Upper bound for an Int must be an integer.")

        if low > high:
            raise ValueError("Lower bound is greater than upper bound.")

        if default_value > high or default_value < low:
            raise ValueError("Default value is outside of bounds [%s, %s]." %
                             (str(low), str(high)))

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype

        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        if 'assumed_default' in metadata:
            del metadata['assumed_default']

        self._validator = Range(value=default_value, low=low,
                                high=high, exclude_low=exclude_low,
                                exclude_high=exclude_high, **metadata)

        # Add low and high to the trait's dictionary so they can be accessed
        metadata['low'] = low
        metadata['high'] = high
        metadata['exclude_low'] = exclude_low
        metadata['exclude_high'] = exclude_high

        super(Int, self).__init__(default_value=default_value,
                                  assumed_default=assumed_default, **metadata)

    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait."""
        try:
            return self._validator.validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)

    def error(self, obj, name, value):
        """Returns a descriptive error string."""

        # pylint: disable=E1101
        right = left = '='
        if self.exclude_high is True:
            right = ''
        if self.exclude_low is True:
            left = ''

        if self.low is None and self.high is None:
            info = "an int"
        elif self.low is not None and self.high is not None:
            info = "%s <%s an integer <%s %s"% (self.low, left,
                                                right, self.high)
        elif self.low is not None:
            info = "a float with a value >%s %s"% (left, self.low)
        else: # self.high is not None
            info = "a float with a value <%s %s"% (right, self.high)

        vtype = type(value)
        msg = "Variable '%s' must be %s, but a value of %s %s was specified." % \
                               (name, info, value, vtype)
        try:
            obj.raise_exception(msg, ValueError)
        except AttributeError:
            raise ValueError(msg)
