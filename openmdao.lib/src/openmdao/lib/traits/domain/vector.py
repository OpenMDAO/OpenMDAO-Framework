from math import atan2, cos, hypot, radians, sin

import numpy


class Vector(object):
    """ Vector data for a :class:`Zone`. """

    def __init__(self):
        self.x = None
        self.y = None
        self.z = None

    @property
    def shape(self):
        """ Returns tuple of index limits. """
        if self.x is not None:
            return self.x.shape
        else:
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

    def is_equivalent(self, other, name, logger, tolerance=0.):
        """ Test if self and `other` are equivalent. """
        if not isinstance(other, Vector):
            logger.debug('other is not a Vector object.')
            return False

        if not self._check_equivalent(other, name, 'x', logger, tolerance):
            return False
        if not self._check_equivalent(other, name, 'y', logger, tolerance):
            return False
        if not self._check_equivalent(other, name, 'z', logger, tolerance):
            return False

        return True

    def _check_equivalent(self, other, name, component, logger, tolerance):
        """ Check equivalence to a component array. """
        arr = getattr(self, component)
        other_arr = getattr(other, component)
        if arr is None:
            if other_arr is not None:
                logger.debug("%s has no %s component but 'other' does.", name,
                             component.upper())
                return False
        else:
            if tolerance > 0.:
                if not numpy.allclose(other_arr, arr, tolerance, tolerance):
                    logger.debug("%s %s values are not 'close'.", name,
                                 component.upper())
                    print 'arr:', arr
                    print 'other_arr:', other_arr
                    return False
            else:
                if (other_arr != arr).any():
                    logger.debug('%s %s values are not equal.', name,
                                 component.upper())
                    return False
        return True

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        if self.z is None:
            raise AttributeError('flip_z: no Z component')
        self.z *= -1.

    def make_cartesian(self, grid):
        """
        Convert to cartesian coordinate system.
        The associated :class:`GridCoordinates` must be in cylindrical form.
        """
        if grid.shape != self.shape:
            raise NotImplementedError('make_cartesian: shape mismatch'
                                      ' not supported')
        t_flat = grid.z.flat  # theta
        y_flat = self.y.flat
        z_flat = self.z.flat
        for i in range(len(y_flat)):
            t = t_flat[i]
            sine = sin(t)
            cosine = cos(t)

            y = y_flat[i]  # Radial component
            z = z_flat[i]  # Tangential component

            y_flat[i] = y*cosine - z*sine
            z_flat[i] = y*sine   + z*cosine

    def make_cylindrical(self, grid):
        """
        Convert to cylindrical coordinate system.
        The associated :class:`GridCoordinates` must be in cylindrical form.
        """
        if grid.shape != self.shape:
            raise NotImplementedError('make_cylindrical: shape mismatch'
                                      ' not supported')
        t_flat = grid.z.flat  # theta
        y_flat = self.y.flat
        z_flat = self.z.flat
        for i in range(len(y_flat)):
            t = t_flat[i]
            y = y_flat[i]
            z = z_flat[i]

            magnitude = hypot(y, z)
            rel_theta = atan2(z, y) - t

            y_flat[i] = magnitude * cos(rel_theta)  # Radial component
            z_flat[i] = magnitude * sin(rel_theta)  # Tangential component

    def rotate_about_x(self, deg):
        """ Rotate about the X axis by `deg` degrees. """
        if self.y is None:
            raise AttributeError('rotate_about_x: no Y component')
        if self.z is None:
            raise AttributeError('rotate_about_x: no Z component')

        sine   = sin(radians(deg))
        cosine = cos(radians(deg))
        y_new  = self.y*cosine - self.z*sine
        self.z = self.z*cosine + self.y*sine
        self.y = y_new

    def rotate_about_y(self, deg):
        """ Rotate about the Y axis by `deg` degrees. """
        if self.x is None:
            raise AttributeError('rotate_about_y: no X component')
        if self.z is None:
            raise AttributeError('rotate_about_y: no Z component')

        sine   = sin(radians(deg))
        cosine = cos(radians(deg))
        x_new  = self.x*cosine - self.z*sine
        self.z = self.z*cosine + self.x*sine
        self.x = x_new

    def rotate_about_z(self, deg):
        """ Rotate about the Z axis by `deg` degrees. """
        if self.x is None:
            raise AttributeError('rotate_about_z: no X component')
        if self.y is None:
            raise AttributeError('rotate_about_z: no Y component')

        sine   = sin(radians(deg))
        cosine = cos(radians(deg))
        x_new  = self.x*cosine - self.y*sine
        self.y = self.y*cosine + self.x*sine
        self.x = x_new

