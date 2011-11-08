import copy

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
        """ Coordinate index limits, not including 'ghost/rind' planes. """
        return self.grid_coordinates.shape

    @property
    def extent(self):
        """ Coordinate ranges, not including 'ghost/rind' planes. """
        return self.grid_coordinates.extent

    def _get_coord_sys(self):
        return self._coordinate_system

    def _set_coord_sys(self, sys):
        if sys in _COORD_SYSTEMS:
            self._coordinate_system = sys
        else:
            raise ValueError('invalid coordinate system %r' % sys)

    coordinate_system = property(_get_coord_sys, _set_coord_sys,
                                 doc='Coordinate system in use.')

    def copy(self):
        """ Returns a deep copy of self. """
        return copy.deepcopy(self)

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`Zone`
            Zone to check against.

        logger: :class:`Logger` or None
            Used to log debug messages that will indicate what if anything is
            not equivalent.

        tolerance: float
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

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None,
                grid_ghosts=None, flow_ghosts=None):
        """
        Construct a new :class:`Zone` from grid and flow data extracted
        from the specified region. Symmetry data is copied.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract neglecting ghost/rind planes.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            For 1D zones omit jmin, jmax, kmin, and kmax.

        grid_ghosts: int[]
            The number of ghost/rind planes for the new zone's grid.
            If ``None`` the grid's existing specification is used.

        flow_ghosts: int[]
            The number of ghost/rind planes for the new zone's flow solution.
            If ``None`` the flow's existing specification is used.
        """
        zone = Zone()
        zone.grid_coordinates = \
            self.grid_coordinates.extract(imin, imax, jmin, jmax, kmin, kmax,
                                          grid_ghosts)
        zone.flow_solution = \
            self.flow_solution.extract(imin, imax, jmin, jmax, kmin, kmax,
                                       flow_ghosts)
        if self.reference_state is not None:
            zone.reference_state = self.reference_state.copy()
        zone.coordinate_system = self.coordinate_system
        zone.right_handed = self.right_handed
        zone.symmetry = self.symmetry
        zone.symmetry_axis = self.symmetry_axis
        zone.symmetry_instances = self.symmetry_instances
        return zone

    def extend(self, axis, delta, grid_points, flow_points, normal=None):
        """
        Construct a new :class:`Zone` by linearly extending the grid and
        replicating the flow. Symmetry data is copied.

        axis: 'i', 'j', or 'k'
            Index axis to extend.

        delta: float.
            Fractional amount to move for each point. Multiplies the 'edge'
            delta in the `axis` direction or the appropriate component of
            `normal`.  A negative value adds points before the current
            zero-index of `axis`. 

        grid_points: int >= 0
            Number of points to add in `axis` dimension.

        flow_points: int >= 0
            Number of points to add in `axis` dimension.

        normal: float[]
            For cases where only a single point exists in the `axis` direction,
            this specifies the direction to move. If not specified, an
            axis-aligned direction is selected based on minimum grid extent.
        """
        zone = Zone()
        if grid_points > 0:
            zone.grid_coordinates = \
                self.grid_coordinates.extend(axis, delta, grid_points, normal)
        else:
            zone.grid_coordinates = self.grid_coordinates.copy()
        if flow_points > 0:
            zone.flow_solution = \
                self.flow_solution.extend(axis, delta, flow_points)
        else:
            zone.flow_solution = self.flow_solution.copy()
        if self.reference_state is not None:
            zone.reference_state = self.reference_state.copy()
        zone.coordinate_system = self.coordinate_system
        zone.right_handed = self.right_handed
        zone.symmetry = self.symmetry
        zone.symmetry_axis = self.symmetry_axis
        zone.symmetry_instances = self.symmetry_instances
        return zone

    def make_cartesian(self, axis='z'):
        """
        Convert to Cartesian coordinate system.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        if self.coordinate_system != CARTESIAN:
            self.flow_solution.make_cartesian(self.grid_coordinates, axis)
            self.grid_coordinates.make_cartesian(axis)
            self.coordinate_system = CARTESIAN

    def make_cylindrical(self, axis='z'):
        """
        Convert to cylindrical coordinate system.

        axis: string
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

        delta_x, delta_y, delta_z: float
            Amount of translation along the corresponding axis.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.translate(delta_x, delta_y, delta_z)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

    def rotate_about_x(self, deg):
        """
        Rotate about the X axis.

        deg: float (degrees)
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

        deg: float (degrees)
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

        deg: float (degrees)
            Amount of rotation.
        """
        if self.coordinate_system == CARTESIAN:
            self.grid_coordinates.rotate_about_z(deg)
            self.flow_solution.rotate_about_z(deg)
        else:
            raise RuntimeError('Zone not in cartesian coordinates')

    def promote(self):
        """ Promote from N-dimensional to N+1 dimensional index space. """
        self.grid_coordinates.promote()
        self.flow_solution.promote()

    def demote(self):
        """ Demote from N-dimensional to N-1 dimensional index space. """
        self.grid_coordinates.demote()
        self.flow_solution.demote()

