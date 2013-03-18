"""
OpenMDAO variable type for a geometry object that can be passed between
components and queried.
"""

#public symbols
__all__ = ["Geom"]

# pylint: disable-msg=E0611, F0401
from openmdao.main.interfaces import IStaticGeometry
from openmdao.main.variable import Variable, gui_excludes


class Geom(Variable):
    """OpenMDAO variable type for a geometry object that can be passed between
    components and queried. The geometry object must conform to
    IStaticGeometry.
    """

    def __init__(self, default_value=None, iotype=None, **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype

        super(Geom, self).__init__(default_value, **metadata)

    def validate(self, obj, name, value):
        ''' Just check for interface'''

        if value is None:
            return value

        if not IStaticGeometry.providedBy(value):
            self._iface_error(obj, name, 'IStaticGeometry')

        return value

    def _iface_error(self, obj, name, iface_name):
        obj.raise_exception("%s must provide interface '%s'" %
                            (name, iface_name), TypeError)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. Geometries don't return
        much (yet.)

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
        attr['value'] = 'Geometry'

        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]

        return attr, None
