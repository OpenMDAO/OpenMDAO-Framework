from openmdao.lib.datatypes.domain.flow import FlowSolution
from openmdao.lib.datatypes.domain.grid import GridCoordinates

CARTESIAN = 'Cartesian'
CYLINDRICAL = 'Cylindrical'
_COORD_SYSTEMS = (CARTESIAN, CYLINDRICAL)


class Zone(object):
    """ One zone in a possibly multi-zone :class:`DomainObj`. """

    def __init__(self):
        self.grid_coordinates = GridCoordinates()
        self.flow_solution = FlowSolution()
        self.reference_state = None

        self._coordinate_system = CARTESIAN
        self.right_handed = True
        self.symmetry = None
        self.symmetry_axis = None
        self.symmetry_instances = 1

    @property
    def shape(self):
        """ Tuple of coordinate index limits. """
        return self.grid_coordinates.shape

    @property
    def extent(self):
        """ Tuple of coordinate ranges. """
        return self.grid_coordinates.extent

    def _get_coord_sys(self):
        return self._coordinate_system

    def _set_coord_sys(self, sys):
        if sys in _COORD_SYSTEMS:
            self._coordinate_system = sys
        else:
            raise ValueError("invalid coordinate system '%s'" % sys)

    coordinate_system = property(_get_coord_sys, _set_coord_sys,
                                 doc='Coordinate system in use.')

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other : Zone
            Zone to check against.

        logger : Logger or None
            Used to log debug messages that will indicate what if anything is
            not equivalent.

        tolerance : float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, Zone):
            logger.debug('other is not a Zone object.')
            return False

        if self.coordinate_system != other.coordinate_system:
            logger.debug('coordinate_systems are not equal.')
            return False

        if self.right_handed != other.right_handed:
            logger.debug('handedness is not equal.')
            return False

        if self.symmetry != other.symmetry:
            logger.debug('symmetry is not equal.')
            return False

        if self.symmetry_axis != other.symmetry_axis:
            logger.debug('symmetry_axis is not equal.')
            return False

        if self.symmetry_instances != other.symmetry_instances:
            logger.debug('symmetry_instances is not equal.')
            return False

        if not self.grid_coordinates.is_equivalent(other.grid_coordinates,
                                                   logger, tolerance):
            return False

        if not self.flow_solution.is_equivalent(other.flow_solution, logger,
                                                tolerance):
            return False

        return True

    def make_cartesian(self, axis='z'):
        """
        Convert to cartesian coordinate system.

        axis : string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        if self.coordinate_system != CARTESIAN:
            self.flow_solution.make_cartesian(self.grid_coordinates, axis)
            self.grid_coordinates.make_cartesian(axis)
            self.coordinate_system = CARTESIAN

    def make_cylindrical(self, axis='z'):
        """
        Convert to cylindrical coordinate system.

        axis : string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        if self.coordinate_system != CYLINDRICAL:
            self.grid_coordinates.make_cylindrical(axis)
            self.flow_solution.make_cylindrical(self.grid_coordinates, axis)
            self.coordinate_system = CYLINDRICAL

    def make_left_handed(self):
        """ Convert to left-handed coordinate system. """
        if self.right_handed:
            self.grid_coordinates.flip_z()
            self.flow_solution.flip_z()
            self.right_handed = False

    def make_right_handed(self):
        """ Convert to right-handed coordinate system. """
        if not self.right_handed:
            self.grid_coordinates.flip_z()
            self.flow_solution.flip_z()
            self.right_handed = True

    def translate(self, delta_x, delta_y, delta_z):
        """
        Translate coordinates.

        delta_x, delta_y, delta_z : float
            Amount of translation along the corresponding axis.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.translate(delta_x, delta_y, delta_z)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

    def rotate_about_x(self, deg):
        """
        Rotate about the X axis.

        deg : float (degrees)
            Amount of rotation.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.rotate_about_x(deg)
            self.flow_solution.rotate_about_x(deg)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

    def rotate_about_y(self, deg):
        """
        Rotate about the Y axis.

        deg : float (degrees)
            Amount of rotation.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.rotate_about_y(deg)
            self.flow_solution.rotate_about_y(deg)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

    def rotate_about_z(self, deg):
        """
        Rotate about the Z axis.

        deg : float (degrees)
            Amount of rotation.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.rotate_about_z(deg)
            self.flow_solution.rotate_about_z(deg)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

