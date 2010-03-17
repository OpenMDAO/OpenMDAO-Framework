
#public symbols
__all__ = ["Int"]

from sys import maxint

from enthought.traits.api import Range, TraitError

    
class Int(Range):
    """A Public Variable wrapper for an integer valid within a
       specified range of values.
       """
    
    def __init__(self, default_value=None, iotype=None, desc=None, \
                 low=None, high=None, exclude_low=False, exclude_high=False, \
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
        
        if not isinstance(default_value, int):
            raise TraitError("Default value for an Int must be an integer.")
        
        if not isinstance(low, int):
            raise TraitError("Lower bounds for an Int must be an integer.")
        
        if not isinstance(high, int):
            raise TraitError("Upper bounds for an Int must be an integer.")
        
        # Put units in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put units in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        super(Int, self).__init__(value=default_value, low=low,
                                  high=high, exclude_low=exclude_low,
                                  exclude_high=exclude_high, **metadata)

