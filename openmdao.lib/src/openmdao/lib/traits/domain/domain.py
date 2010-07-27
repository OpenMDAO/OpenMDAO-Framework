import copy

from openmdao.lib.traits.domain.zone import Zone
from openmdao.util.log import NullLogger


class DomainObj(object):
    """
    A :class:`DomainObj` represents a (possibly multi-zoned) mesh and
    data related to that mesh.
    """

    def __init__(self):
        self.reference_state = None
        # Zones are kept in an explicit list to retain the order
        # that they were added.
        self.zones = []

    @property
    def shape(self):
        """ Returns list of coordinate index limits for each zone. """
        return [zone.shape for zone in self.zones]

    @property
    def extent(self):
        """ Returns list of coordinate ranges for each zone. """
        return [zone.extent for zone in self.zones]

    def add_domain(self, other, prefix='', make_copy=False):
        """
        Add zones from `other` to self, retaining names where possible.
        Zone names will be prepended with `prefix`. If `make_copy` is True,
        then a deep copy of each zone is made rather than just referring
        to a shared instance.
        """
        for zone in other.zones:
            name = other.zone_name(zone)
            if hasattr(self, name):
                name = ''
            self.add_zone(name, zone, prefix, make_copy)

    def add_zone(self, name, zone, prefix='', make_copy=False):
        """
        Add a :class:`Zone`. If `name` is None or blank, then a default
        of the form ``zone_N`` is used. The resulting name will be prepended
        with `prefix`. If `make_copy` is True, then a deep copy of each zone is
        made rather than just referring to a shared instance.
        Returns the added zone.
        """
        if not name:
            name = 'zone_%d' % (len(self.zones) + 1)
        name = prefix+name
        if hasattr(self, name):
            raise ValueError("name '%s' is already bound" % name)
        if make_copy:
            zone = copy.deepcopy(zone)
        setattr(self, name, zone)
        self.zones.append(zone)
        return zone

    def remove_zone(self, zone):
        """
        Remove `zone`, specified either by name or object reference.
        Returns the removed zone.
        """
        if isinstance(zone, basestring):
            name = zone
            zone = getattr(self, zone)
        else:
            name = self.zone_name(zone)
        delattr(self, name)
        self.zones.remove(zone)
        return zone

    def rename_zone(self, name, zone):
        """ Rename `zone` to `name`. """
        if hasattr(self, name):
            raise ValueError("name '%s' is already bound" % name)
        current = self.zone_name(zone)
        delattr(self, current)
        setattr(self, name, zone)

    def zone_name(self, zone):
        """ Return name that `zone` is bound to. """
        for name, value in self.__dict__.items():
            if value is zone:
                return name
        raise ValueError('cannot find zone!')

    def copy(self):
        """ Returns a deep copy of self. """
        return copy.deepcopy(self)

    def deallocate(self):
        """ Deallocate resources. """
        for zone in self.zones:
            delattr(self, self.zone_name(zone))
        self.zones = []

    def is_equivalent(self, other, logger=None, tolerance=0.):
        """
        Test if self and `other` are equivalent.
        `tolerance` is the maximum relative difference in array values
        to be considered equivalent.
        """
        logger = logger or NullLogger()
        if not isinstance(other, DomainObj):
            logger.debug('other is not a DomainObj object.')
            return False

        if len(self.zones) != len(other.zones):
            logger.debug('zone count mismatch.')
            return False

        for zone in self.zones:
            name = self.zone_name(zone)
            try:
                other_zone = getattr(other, name)
            except AttributeError:
                logger.debug("other is missing zone '%s'.", name)
                return False
            if not zone.is_equivalent(other_zone, logger, tolerance):
                logger.debug("zone '%s' equivalence failed.", name)
                return False
        return True

    def make_cartesian(self, axis='z'):
        """
        Convert to cartesian coordinate system.
        `axis` specifies which is the cylinder axis ('z' or 'x').
        """
        for zone in self.zones:
            zone.make_cartesian(axis)

    def make_cylindrical(self, axis='z'):
        """
        Convert to cylindrical coordinate system.
        `axis` specifies which is the cylinder axis ('z' or 'x').
        """
        for zone in self.zones:
            zone.make_cylindrical(axis)

    def make_left_handed(self):
        """ Convert to left-handed coordinate system. """
        for zone in self.zones:
            zone.make_left_handed()

    def make_right_handed(self):
        """ Convert to right-handed coordinate system. """
        for zone in self.zones:
            zone.make_right_handed()

    def translate(self, delta_x, delta_y, delta_z):
        """ Translate coordinates. """
        for zone in self.zones:
            zone.translate(delta_x, delta_y, delta_z)

    def rotate_about_x(self, deg):
        """ Rotate about the X axis by `deg` degrees. """
        for zone in self.zones:
            zone.rotate_about_x(deg)

    def rotate_about_y(self, deg):
        """ Rotate about the Y axis by `deg` degrees. """
        for zone in self.zones:
            zone.rotate_about_y(deg)

    def rotate_about_z(self, deg):
        """ Rotate about the Z axis by `deg` degrees. """
        for zone in self.zones:
            zone.rotate_about_z(deg)

