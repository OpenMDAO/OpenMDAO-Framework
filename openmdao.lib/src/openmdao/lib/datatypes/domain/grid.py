from math import atan2, cos, hypot, sin

from openmdao.lib.datatypes.domain.vector import Vector


class GridCoordinates(Vector):
    """ Coordinate data for a :class:`Zone`. """

    def __init__(self):
        super(GridCoordinates, self).__init__()
        self._ghosts = [0, 0, 0, 0, 0, 0]

    def _get_ghosts(self):
        return self._ghosts

    def _set_ghosts(self, ghosts):
        if len(ghosts) != 6:
            raise ValueError('ghosts must be a 6-element array')
        for i in ghosts:
            if i < 0:
                raise ValueError('All ghost values must be >= 0')
        self._ghosts = ghosts

    ghosts = property(_get_ghosts, _set_ghosts,
                      doc='Number of ghost cells for each index direction.')

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: GridCoordinates
            The grid to check against.

        logger: Logger or None
            Used to log debug messages that will indicate what if anything is
            not equivalent.

        tolerance: float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, GridCoordinates):
            logger.debug('other is not a GridCoordinates object.')
            return False

        if not super(GridCoordinates, self).is_equivalent(other, 'grid',
                                                          logger, tolerance):
            return False

        if self.ghosts != other.ghosts:
            logger.debug('ghost cell counts are not equal.')
            return False

        return True

    def make_cartesian(self, axis='z'):
        """
        Convert to cartesian coordinate system.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z':
            self.x = self.r.copy()
            self.y = self.r.copy()
            x_flat = self.x.flat
            y_flat = self.y.flat
            for i in range(self.r.size):
                r = r_flat[i]
                t = t_flat[i]
                x_flat[i] = r * cos(t)
                y_flat[i] = r * sin(t)
            self.r = None
            self.t = None

        elif axis == 'x':
            self.x = self.z
            self.y = self.r.copy()
            self.z = self.r.copy()
            y_flat = self.y.flat
            z_flat = self.z.flat
            for i in range(self.r.size):
                r = r_flat[i]
                t = t_flat[i]
                y_flat[i] = r * cos(t)
                z_flat[i] = r * sin(t)
            self.r = None
            self.t = None

        else:
            raise ValueError("axis must be 'z' or 'x'")

    def make_cylindrical(self, axis='z'):
        """
        Convert to cylindrical coordinate system.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        self.r = self.x.copy()
        self.t = self.x.copy()
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z':
            x_flat = self.x.flat
            y_flat = self.y.flat
            for i in range(self.x.size):
                x = x_flat[i]
                y = y_flat[i]
                r_flat[i] = hypot(x, y)
                t_flat[i] = atan2(y, x)
            self.x = None
            self.y = None

        elif axis == 'x':
            y_flat = self.y.flat
            z_flat = self.z.flat
            for i in range(self.y.size):
                y = y_flat[i]
                z = z_flat[i]
                r_flat[i] = hypot(y, z)
                t_flat[i] = atan2(z, y)
            self.z = self.x
            self.x = None
            self.y = None

        else:
            raise ValueError("axis must be 'z' or 'x'")

    def translate(self, delta_x, delta_y, delta_z):
        """
        Translate coordinates.

        delta_x, delta_y, delta_z: float
            Amount of translation along the corresponding axis.
        """
        if delta_x:
            if self.x is None:
                raise AttributeError('no X coordinates')
            else:
                self.x += delta_x

        if delta_y:
            if self.y is None:
                raise AttributeError('no Y coordinates')
            else:
                self.y += delta_y

        if delta_z:
            if self.z is None:
                raise AttributeError('no Z coordinates')
            else:
                self.z += delta_z

