"""
Trait for floating point variables, with optional min and max
"""

#public symbols
__all__ = ["Int"]

from sys import maxint

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Range, TraitError, TraitType

    
class Int(TraitType):
    """A Public Variable wrapper for an integer valid within a
       specified range of values.
       """
    
    def __init__(self, default_value=None, iotype=None, desc=None, 
                 low=None, high=None, exclude_low=False, exclude_high=False, 
                 **metadata):

        # Range trait didn't seem to handle "None" correctly when passed on
        # the  command line.
        if default_value is None:
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
            
        if low > high:
            raise TraitError("Lower bounds is greater than upper bounds.")
        
        if not isinstance(default_value, int):
            raise TraitError("Default value for an Int must be an integer.")
        
        if not isinstance(low, int):
            raise TraitError("Lower bounds for an Int must be an integer.")
        
        if not isinstance(high, int):
            raise TraitError("Upper bounds for an Int must be an integer.")
        
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        self._validator = Range(value=default_value, low=low,
                                  high=high, exclude_low=exclude_low,
                                  exclude_high=exclude_high, **metadata)
            
        # Add low and high to the trait's dictionary so they can be accessed
        metadata['low'] = low
        metadata['high'] = high
        metadata['exclude_low'] = exclude_low
        metadata['exclude_high'] = exclude_high
        
        super(Int, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        """ Validates that a specified value is valid for this trait."""
        
        try:
            return self._validator.validate(object, name, value)
        except TraitError:
            self.error(object, name, value)

    def error(self, object, name, value):
        """Returns a string describing the type handled by Int."""
        
        # pylint: disable-msg=E1101
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

        vtype = type( value )
        msg = "Trait '%s' must be %s, but a value of %s %s was specified." % \
                               (name, info, value, vtype)
        object.raise_exception(msg, TraitError)       
