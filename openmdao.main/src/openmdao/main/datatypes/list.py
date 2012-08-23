"""
Trait for list variables.
"""

#public symbols
__all__ = ["List"]

# pylint: disable-msg=E0611,F0401
from zope.interface import implementedBy

from enthought.traits.api import List as Enthought_List

from openmdao.main.datatypes.slot import Slot
from openmdao.main.interfaces import implements, IVariable
from openmdao.main.variable import gui_excludes


class List(Enthought_List):
    ''' Variable wrapper for list variables.'''
    
    implements(IVariable)
    
    def get_attribute(self, name, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Lists are containers and
        can have slots.
        
        name: str
          Name of variable
          
        trait: CTrait
          The variable's trait
          
        meta: dict
          Dictionary of metadata for this variable
        """
        
        attr = {}
        slot_attr = None
        value = trait.value
        
        attr['name'] = name
        attr['type'] = 'list'
        attr['value'] = str(value)
        
        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]
                
        # Handling for a List of Slots
        inner = trait.inner_traits[0]
        if inner.is_trait_type(Slot):
            
            _, slot_attr = inner.trait_type.get_attribute(name, inner, meta)
            slot_attr['containertype'] = 'list'

        return attr, slot_attr
    
'''
# This was my original attempt to inherit from Variable instead. Unfortunately,
# Traits was too tricky for me.

class List(Variable):
    """A variable wrapper for a list variable.
       """
    
    info_trait         = None
    default_value_type = 5
    _items_event       = None

    def __init__(self, trait = None, value = None, minlen = 0,
                   maxlen = maxint, items = True, 
                   iotype=None, desc=None, 
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        #self.item_trait = trait_from( trait )
        self.minlen     = max( 0, minlen )
        self.maxlen     = max( minlen, maxlen )
        self.has_items  = items

        self._validator = Enthought_List(trait=trait, value=value, 
                                         minlen=minlen, maxlen=maxlen, 
                                         items=items, **metadata)
            
        super(List, self).__init__(value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        return self._validator.validate(obj, name, value)

    def inner_traits(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.inner_traits()
    
    def full_info(self, object, name, value):
        """ User the one in the Enthought trait.
        """
        return self._validator.full_info(object, name, value)

    def create_editor(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.create_editor()

    def items_event(self):
        """ User the one in the Enthought trait.
        """
        return self._validator.items_event()
'''