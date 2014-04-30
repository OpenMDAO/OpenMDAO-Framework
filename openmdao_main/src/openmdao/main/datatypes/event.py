'''
Trait for Events
'''

#public symbols
__all__ = ["Event"]

# pylint: disable-msg=E0611,F0401

from traits.api import Event as Enthought_Event


class Event(Enthought_Event):
    ''' Variable wrapper for events.'''

    def get_attribute(self, name, meta):
        """Return the attribute dictionaries for this variable. This dict is
        used by the GUI to populate the object editor pane.

        name: str
          Name of variable

        meta: dict
          Dictionary of metadata for this variable
        """

        attr = {}
        attr['name'] = name

        for field in meta:
            attr[field] = meta[field]

        return attr
