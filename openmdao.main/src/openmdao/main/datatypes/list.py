"""
Trait for list variables.
"""

#public symbols
__all__ = ["List"]

# pylint: disable-msg=E0611,F0401

import types
import numpy as np

from traits.api import List as Enthought_List

from openmdao.main.datatypes.slot import Slot
from openmdao.main.interfaces import implements, IVariable
from openmdao.main.variable import gui_excludes


class List(Enthought_List):
    ''' Variable wrapper for list variables.'''

    implements(IVariable)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Lists are containers and
        can have slots.

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
        attr['type'] = 'list'
        attr['value'] = str(list(value))

        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]

        # Handling for a List of Slots
        inner = trait.inner_traits[0]
        if inner.is_trait_type(Slot):

            if len(value) < 1:
                inner_value = None
            else:
                inner_value = value[0]

            _, slot_attr = inner.trait_type.get_attribute(name, inner_value,
                                                          inner, meta)
            slot_attr['containertype'] = 'list'

            # for the value, just report the types of the list entries
            valtypes = []
            for val in value:
                valtypes.append(type(val).__name__)
            slot_attr['filled'] = valtypes

        return attr, slot_attr

    def validate ( self, object, name, value ):
        if isinstance(value, types.GeneratorType):
            value = list(value)
        elif isinstance(value, np.ndarray):
            value = value.tolist()
        elif isinstance(value, types.XRangeType):
            value = list(value)

        value = super(List, self).validate(object, name, value)
        return value
