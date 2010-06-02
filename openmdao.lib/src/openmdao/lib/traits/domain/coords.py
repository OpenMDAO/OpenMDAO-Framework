from math import asin, atan2, cos, sin, sqrt

_DEG2RAD = asin(1.) / 90.


class GridCoordinates(object):
    """ Coordinate data for a :class:`Zone`. """

    def __init__(self):
        self.x = None
        self.y = None
        self.z = None

    @property
    def shape(self):
        """ Returns tuple of index limits. """
        if self.x is not None:
            return self.x.shape
        return ()

    @property
    def extent(self):
        """ Returns tuple of coordinate ranges. """
        if self.x is not None:
            if self.y is not None:
                if self.z is not None:
                    return (self.x.min(), self.x.max(),
                            self.y.min(), self.y.max(),
                            self.z.min(), self.z.max())
                return (self.x.min(), self.x.max(),
                        self.y.min(), self.y.max())
            return (self.x.min(), self.x.max())
        return ()

    def is_equivalent(self, other, logger):
        """ Test if self and `other` are equivalent. """
        if not isinstance(other, GridCoordinates):
            logger.debug('other is not a GridCoordinates object.')
            return False

        if self.x is None:
            if other.x is not None:
                logger.debug("'self' has no X coordinates but 'other' does.")
                return False
        else:
            if (other.x != self.x).any():
                logger.debug('X coordinates are not equal.')
                return False

        if self.y is None:
            if other.y is not None:
                logger.debug("'self' has no Y coordinates but 'other' does.")
                return False
        else:
            if (other.y != self.y).any():
                logger.debug('Y coordinates are not equal.')
                return False

        if self.z is None:
            if other.z is not None:
                logger.debug("'self' has no Z coordinates but 'other' does.")
                return False
        else:
            if (other.z != self.z).any():
                logger.debug('Z coordinates are not equal.')
                return False

        return True

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        if self.z is None:
            raise AttributeError('no Z coordinates')
        self.z *= -1.

    def make_cartesian(self):
        """ Convert to cartesian coordinate system. """
        y_get = self.y.item
        y_set = self.y.itemset
        z_get = self.z.item
        z_set = self.z.itemset
# TODO: try 'ravel' to avoid structured grid dependence.
        imax, jmax, kmax = self.x.shape
        for i in range(imax):
            for j in range(jmax):
                for k in range(kmax):
                    y = y_get(i, j, k)  # r
                    z = z_get(i, j, k)  # theta
                    y_set(i, j, k, y * sin(z))
                    z_set(i, j, k, y * cos(z))

    def make_cylindrical(self):
        """ Convert to cylindrical coordinate system. """
        y_get = self.y.item
        y_set = self.y.itemset
        z_get = self.z.item
        z_set = self.z.itemset
# TODO: try 'ravel' to avoid structured grid dependence.
        imax, jmax, kmax = self.x.shape
        for i in range(imax):
            for j in range(jmax):
                for k in range(kmax):
                    y = y_get(i, j, k)
                    z = z_get(i, j, k)
                    y_set(i, j, k, sqrt(y*y + z*z))  # r
                    z_set(i, j, k, atan2(z, y))      # theta

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

    def rotate_about_x(self, deg):
        """ Rotate about the X axis by `deg` degrees. """
        if self.y is None:
            raise AttributeError('no Y coordinates')
        if self.z is None:
            raise AttributeError('no Z coordinates')

        sine   = sin(deg * _DEG2RAD)
        cosine = cos(deg * _DEG2RAD)
        y_new  = self.y*cosine - self.z*sine
        self.z = self.z*cosine + self.y*sine
        self.y = y_new

    def rotate_about_y(self, deg):
        """ Rotate about the Y axis by `deg` degrees. """
        if self.x is None:
            raise AttributeError('no X coordinates')
        if self.z is None:
            raise AttributeError('no Z coordinates')

        sine   = sin(deg * _DEG2RAD)
        cosine = cos(deg * _DEG2RAD)
        x_new  = self.x*cosine - self.z*sine
        self.z = self.z*cosine + self.x*sine
        self.x = x_new

    def rotate_about_z(self, deg):
        """ Rotate about the Z axis by `deg` degrees. """
        if self.x is None:
            raise AttributeError('no X coordinates')
        if self.y is None:
            raise AttributeError('no Y coordinates')

        sine   = sin(deg * _DEG2RAD)
        cosine = cos(deg * _DEG2RAD)
        x_new  = self.x*cosine - self.y*sine
        self.y = self.y*cosine + self.x*sine
        self.x = x_new

