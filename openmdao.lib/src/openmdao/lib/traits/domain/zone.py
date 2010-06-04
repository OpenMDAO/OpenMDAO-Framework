from openmdao.lib.traits.domain.flow import FlowSolution
from openmdao.lib.traits.domain.grid import GridCoordinates


class Zone(object):
    """ One zone in a possibly multi-zone :class:`DomainObj`. """

    def __init__(self):
        self.grid_coordinates = GridCoordinates()
        self.flow_solution = FlowSolution()
        self.reference_state = None

        self.symmetry = None
        self.symmetry_axis = None
        self.symmetry_instances = 1

    @property
    def shape(self):
        """ Returns tuple of index limits. """
        return self.grid_coordinates.shape

    @property
    def extent(self):
        """ Returns tuple of coordinate ranges. """
        return self.grid_coordinates.extent

    def is_equivalent(self, other, logger):
        """ Test if self and `other` are equivalent. """
        if not isinstance(other, Zone):
            logger.debug('other is not a Zone object.')
            return False

        if not self.grid_coordinates.is_equivalent(other.grid_coordinates,
                                                   logger):
            return False

        if not self.flow_solution.is_equivalent(other.flow_solution, logger):
            return False

        return True

    def name_of_obj(self, obj):
        """ Return name of object. """
        for name, value in self.__dict__.items():
            if value is obj:
                return name
        return None

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        self.grid_coordinates.flip_z()
        self.flow_solution.flip_z()

    def make_cartesian(self):
        """ Convert to cartesian coordinate system. """
        self.grid_coordinates.make_cartesian()
        self.flow_solution.make_cartesian()

    def make_cylindrical(self):
        """ Convert to cylindrical coordinate system. """
        self.grid_coordinates.make_cylindrical()
        self.flow_solution.make_cylindrical()

    def translate(self, delta_x, delta_y, delta_z):
        """ Translate coordinates. """
        self.grid_coordinates.translate(delta_x, delta_y, delta_z)

    def rotate_about_x(self, deg):
        """ Rotate about the X axis by `deg` degrees. """
        self.grid_coordinates.rotate_about_x(deg)
        self.flow_solution.rotate_about_x(deg)

    def rotate_about_y(self, deg):
        """ Rotate about the Y axis by `deg` degrees. """
        self.grid_coordinates.rotate_about_y(deg)
        self.flow_solution.rotate_about_y(deg)

    def rotate_about_z(self, deg):
        """ Rotate about the Z axis by `deg` degrees. """
        self.grid_coordinates.rotate_about_z(deg)
        self.flow_solution.rotate_about_z(deg)

