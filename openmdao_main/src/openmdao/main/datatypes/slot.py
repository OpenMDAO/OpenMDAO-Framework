"""
Trait for a Slot meant to contain an object of a particular type
or having a particular interface (either a Traits interface or a
zope.interface).
"""

# The regular Instance class that comes with Traits will only check public
# methods on an object if that object is not a HasTraits object, which means
# we get essentially no error checking for things like Case iterators where
# their API doesn't include any public methods. If we use zope.interface we
# don't have this problem.

#public symbols
__all__ = ["Slot"]

# pylint: disable-msg=E0611,F0401

from openmdao.main.variable import gui_excludes
from openmdao.main.datatypes.instance import Base


import warnings

class Slot(Base):

    """A trait for an object of a particular type or implementing a particular
    interface. Both Traits Interfaces and zope.interface.Interfaces are
    supported.
    """

    def __init__(self, klass=object, allow_none=True, factory=None,
                 args=None, kw=None, **metadata):

        if 'iotype' in metadata:
            warnings.warn(FutureWarning( \
                    "The use of 'iotype' as metadata for 'Slot' will no longer be supported.\nUse 'Instance' in place of 'Slot' if you need to use 'iotype'.\n"), stacklevel=2)

        super(Slot, self).__init__(klass, allow_none, factory, args, kw, **metadata)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Slots also return an
        attribute dictionary for the slot pane.

        name: str
          Name of variable

        value: object
          The value of the variable

        trait: CTrait
          The variable's trait

        meta: dict
          Dictionary of metadata for this variable
        """

        io_attr = {}
        io_attr['name'] = name
        io_attr['type'] = trait.trait_type.klass.__name__
        io_attr['ttype'] = 'slot'

        slot_attr = {}
        slot_attr['name'] = name

        if value is None:
            slot_attr['filled'] = None
        elif value is []:
            slot_attr['filled'] = []
        else:
            slot_attr['filled'] = type(value).__name__

        slot_attr['klass'] = io_attr['type']
        slot_attr['containertype'] = 'singleton'

        for field in meta:
            if field not in gui_excludes:
                slot_attr[field] = meta[field]
                io_attr[field] = meta[field]

        return io_attr, slot_attr
