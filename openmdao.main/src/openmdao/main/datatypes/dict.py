"""
Trait for dictionary variables.
"""

#public symbols
__all__ = ["Dict"]

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Dict as Enthought_Dict

from openmdao.main.interfaces import implements, IVariable
from openmdao.main.variable import gui_excludes


class Dict(Enthought_Dict):
    ''' Variable wrapper for dictionary variables.'''
    
    implements(IVariable)
    
    def get_attribute(self, name, value, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Dicts are containers that
        have a key trait and a value trait.
        
        name: str
          Name of variable
          
        value: object
          Value of variable
          
        meta: dict
          Dictionary of metadata for this variable
        """
        
        attr = {}
        
        attr['name'] = name
        attr['type'] = 'dict'
        attr['value'] = str(value)
        
        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]
        
        return attr, None
    

'''
# This was my original attempt to inherit from Variable instead. Unfortunately,
# Traits was too tricky for me.

class Dict(Variable):
    """A variable wrapper for a dictionary variable.
       """
    
    info_trait         = None
    default_value_type = 6
    _items_event       = None

    def __init__(self, key_trait=None, value_trait=None, value=None,
                   items=True, iotype=None, desc=None, **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        self._validator = Enthought_Dict(key_trait, value_trait, value, \
                                         items, **metadata)
            
        super(Dict, self).__init__(default_value=value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        return self._validator.validate(obj, name, value)
    
    def inner_traits(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.inner_traits()
    
    def full_info(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.full_info()

    def create_editor(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.create_editor()

    def items_event(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.items_event()
'''