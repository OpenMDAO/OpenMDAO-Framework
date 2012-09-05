"""
Trait for dictionary variables.
"""

#public symbols
__all__ = ["Dict"]

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Dict as Enthought_Dict

from openmdao.main.datatypes.slot import Slot
from openmdao.main.interfaces import implements, IVariable
from openmdao.main.variable import gui_excludes


class Dict(Enthought_Dict):
    ''' Variable wrapper for dictionary variables.'''
    
    implements(IVariable)
    
    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Dicts are containers that
        have a key trait and a value trait.
        
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
        slot_attr = None
        
        attr['name'] = name
        attr['type'] = 'dict'
        attr['value'] = value
        attr['key_type'] = type(value.keys()[0]).__name__
        attr['value_type'] = type(value.values()[0]).__name__
        
        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]
        
        # Handling for a List of Slots
        inner = trait.inner_traits[-1]
        if inner.is_trait_type(Slot):
                    
            if len(value) < 1:
                inner_value = None
            else:
                # Just grab first item to get object type
                inner_value = value[value.keys()[0]]
                
            _, slot_attr = inner.trait_type.get_attribute(name, inner_value, 
                                                          inner, meta)
            slot_attr['containertype'] = 'dict'
            slot_attr['filled'] = value.keys()
            
        return attr, slot_attr
    
