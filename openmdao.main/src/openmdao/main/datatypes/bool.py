"""
Trait for boolean variables.
"""

#public symbols
__all__ = ["Bool"]

# pylint: disable=E0611,F0401
from traits.api import Bool as Enthought_Bool

from numpy import bool_

from openmdao.main.variable import Variable, gui_excludes


class Bool(Variable):
    """A variable wrapper for a boolean variable.
    """

    def __init__(self, default_value=False, iotype=None, desc=None,
                 **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype

        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        assumed_default = (default_value is None)
        if 'assumed_default' in metadata:
            del metadata['assumed_default']

        self._validator = Enthought_Bool(value=default_value, **metadata)

        super(Bool, self).__init__(default_value=default_value,
                                   assumed_default=assumed_default, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        if isinstance(value, bool_):
            value = bool(value)
        return self._validator.validate(obj, name, value)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Bools need to turn
        their value into a string for compatibility.

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
        attr['type'] = type(value).__name__
        attr['value'] = str(value)

        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]

        return attr, None
