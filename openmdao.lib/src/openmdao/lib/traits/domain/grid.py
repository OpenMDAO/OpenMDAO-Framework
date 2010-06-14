from math import atan2, cos, hypot, sin

from openmdao.lib.traits.domain.vector import Vector


class GridCoordinates(Vector):
    """ Coordinate data for a :class:`Zone`. """

    def __init__(self):
        super(GridCoordinates, self).__init__()
        self.ghosts = [0, 0, 0, 0, 0, 0]

    def _get_ghosts(self):
        return self._ghosts

    def _set_ghosts(self, ghosts):
        if len(ghosts) != 6:
            raise ValueError('ghosts must be a 6-element array')
        for i in ghosts:
            if i < 0:
                raise ValueError('All ghost values must be >= 0')
        self._ghosts = ghosts

    ghosts = property(_get_ghosts, _set_ghosts)

    def is_equivalent(self, other, logger, tolerance=0.):
        """ Test if self and `other` are equivalent. """
        if not isinstance(other, GridCoordinates):
            logger.debug('other is not a GridCoordinates object.')
            return False

        if not super(GridCoordinates, self).is_equivalent(other, 'grid',
                                                          logger, tolerance):
            return False

        return other.ghosts == self.ghosts

    def make_cartesian(self):
        """ Convert to cartesian coordinate system. """
        y_flat = self.y.flat
        z_flat = self.z.flat
        for i in range(len(y_flat)):
            y = y_flat[i]  # r
            z = z_flat[i]  # theta
            y_flat[i] = y * cos(z)
            z_flat[i] = y * sin(z)

    def make_cylindrical(self):
        """ Convert to cylindrical coordinate system. """
        y_flat = self.y.flat
        z_flat = self.z.flat
        for i in range(len(y_flat)):
            y = y_flat[i]
            z = z_flat[i]
            y_flat[i] = hypot(y, z)  # r
            z_flat[i] = atan2(z, y)  # theta

    def translate(self, delta_x, delta_y, delta_z):
        """ Translate coordinates. """
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

