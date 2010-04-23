"""
Trait for enumerations, with optional alias list
"""

#public symbols
__all__ = ["Enum"]

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Enum as TraitEnum
from enthought.traits.api import TraitError, TraitType

    
class Enum(TraitType):
    """A Public Variable wrapper for an enumeration, which is a variable that
       can assume one value from a set of specified values.
       """
    
    def __init__(self, default_value=None, index_values=(), iotype=None, 
                        alias_values=(), desc=None, **metadata):

        # Allow some variant constructors (no default, no index)
        if not index_values:
            if default_value is None:
                raise TraitError("Enum must contain at least one value.")
            else:
                index_values = default_value
                if isinstance(index_values, tuple) or \
                   isinstance(index_values, list):
                    default_value = index_values[0]
        else:
            if default_value is None:
                default_value = index_values[0]

        # We need tuples or a list for the index
        if not ( isinstance(index_values, tuple) or \
                 isinstance(index_values, list) ):
            index_values = (index_values,)
                
        if alias_values:
            
            if not ( isinstance(alias_values, tuple) or \
                     isinstance(alias_values, list) ):
                alias_values = (alias_values,)
                
            if len(alias_values) != len(index_values):
                raise TraitError("Length of alias_values does not match " + \
                                 "length of index_values.")
            
        if default_value not in index_values:
            raise TraitError("Default value not in index_values.")
            
        self._validator = TraitEnum(default_value, index_values, **metadata)
            
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        # Put index_values in the metadata dictionary
        if index_values:
            metadata['index_values'] = index_values

        # Put alias_values in the metadata dictionary
        if alias_values:
            metadata['alias_values'] = alias_values

        super(Enum, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        """ Validates that a specified value is valid for this trait."""
        
        try:
            return self._validator.validate(object, name, value)
        except TraitError:
            self.error(object, name, value)

    def error(self, object, name, value):
        """Returns a general error string for Enum."""
        
        # pylint: disable-msg=E1101
        vtype = type( value )
        if value not in self.index_values:
            info = str(self.index_values)
            msg = "Trait '%s' must be in %s, " % (name, info) + \
                "but a value of %s %s was specified." % (value, vtype)
        else:
            msg = "Unknown error while setting trait '%s';" % (name) +\
                  "a value of %s %s was specified." % (value, vtype)
            
        object.raise_exception(msg, TraitError)       
