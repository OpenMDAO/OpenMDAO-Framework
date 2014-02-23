"""
Trait for enumerations, with optional alias list.
"""

#public symbols
__all__ = ["Enum"]

# pylint: disable-msg=E0611,F0401
from traits.api import Enum as TraitEnum

from openmdao.main.variable import Variable, gui_excludes

class Enum(Variable):
    """A variable wrapper for an enumeration, which is a variable that
       can assume one value from a set of specified values.
       """
    
    def __init__(self, default_value=None, values=(), iotype=None, 
                        aliases=(), desc=None, **metadata):

        assumed_default = False

        # Allow some variant constructors (no default, no index)
        if not values:
            if default_value is None:
                raise ValueError("Enum must contain at least one value.")
            else:
                values = default_value
                if isinstance(values, (tuple, list)):
                    default_value = values[0]
        else:
            if default_value is None:
                default_value = values[0]
                assumed_default = True

        # We need tuples or a list for the index
        if not isinstance(values, (tuple, list)):
            values = (values,)
                
        if aliases:
            
            if not isinstance(aliases, (tuple, list)):
                aliases = (aliases,)
                
            if len(aliases) != len(values):
                raise ValueError("Length of aliases does not match " + \
                                 "length of values.")
            
        if default_value not in values:
            raise ValueError("Default value not in values.")
            
        self._validator = TraitEnum(default_value, values, **metadata)
            
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        # Put values in the metadata dictionary
        if values:
            metadata['values'] = values
            
            # We also need to store the values in a dict, to get around
            # a weak typechecking (i.e., enum of [1,2,3] can be 1.0)
            self.valuedict = {}
            
            for val in values:
                self.valuedict[val] = val

        # Put aliases in the metadata dictionary
        if aliases:
            metadata['aliases'] = aliases

        super(Enum, self).__init__(default_value=default_value, assumed_default=assumed_default,
                                         **metadata)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. 
        
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
        attr['type'] = "enum"
        attr['value'] = value
        
        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]
        
        attr['value_types'] = [type(val).__name__ for val in meta['values']]
            
        return attr, None


    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait."""
        
        try:
            val = self._validator.validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)

        # if someone uses a float to set an int-valued Enum, we want it to
        # be an int. Enthought's Enum allows a float value, unfortunately.
        return self.valuedict[val]
        
    def error(self, obj, name, value):
        """Returns a general error string for Enum."""
        
        # pylint: disable-msg=E1101
        vtype = type( value )
        if value not in self.values:
            info = str(self.values)
            msg = "Variable '%s' must be in %s, " % (name, info) + \
                "but a value of %s %s was specified." % (value, vtype)
        else:
            msg = "Unknown error while setting trait '%s';" % (name) +\
                  "a value of %s %s was specified." % (value, vtype)
            
        try:
            obj.raise_exception(msg, ValueError)
        except AttributeError:
            raise ValueError(msg)
