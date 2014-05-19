import copy
from math import atan2, cos, hypot, sin

import numpy

from openmdao.lib.datatypes.domain.vector import Vector

class GridCoordinates(Vector):
    """
    Coordinate data for a :class:`Zone`.
    Currently limited to structured grids.
    1D spatial data must set 'x'.
    2D spatial data must set 'x', and 'y' or 'r' and 't'.
    3D spatial data must set 'x', 'y', and 'z' or 'r', 't', and 'z'.
    Note that index dimension is not the same as spatial dimension.
    For example, a curve in 3D space is represented by 1D 'x', 'y', and 'z' arrays.
    It can also be represented by 2D or 3D arrays where only one index
    dimension varies.
    """

    def __init__(self, vec=None):
        super(GridCoordinates, self).__init__()
        if vec is not None:  # Used by extract().
            self.x = vec.x
            self.y = vec.y
            self.z = vec.z
            self.r = vec.r
            self.t = vec.t
            self.ghosts = vec.ghosts

    @property
    def extent(self):
        """
        Coordinate ranges, not including 'ghost/rind' planes.
        If cartesian ``(xmin,xmax,ymin,ymax,zmin,zmax)``.
        Otherwise ``(rmin,rmax,tmin,tmax,zmin,zmax)``.
        """
        i = len(super(GridCoordinates, self).shape)
        if i == 3:
            return self._extent_3d()
        elif i == 2:
            return self._extent_2d()
        elif i == 1:
            return self._extent_1d()
        else:
            return ()

    def _extent_3d(self):
        """ 3D (index space) coordinate ranges. """
        ijk = self.shape
        ghosts = self.ghosts

        imin = ghosts[0]
        jmin = ghosts[2]
        kmin = ghosts[4]

        imax = imin + ijk[0]
        jmax = jmin + ijk[1]
        kmax = kmin + ijk[2]

        if self.x is not None:
            x = self.x[imin:imax, jmin:jmax, kmin:kmax]
            y = self.y[imin:imax, jmin:jmax, kmin:kmax]
            z = self.z[imin:imax, jmin:jmax, kmin:kmax]
            return (x.min(), x.max(), y.min(), y.max(), z.min(), z.max())
        else:
            r = self.r[imin:imax, jmin:jmax, kmin:kmax]
            t = self.t[imin:imax, jmin:jmax, kmin:kmax]
            z = self.z[imin:imax, jmin:jmax, kmin:kmax]
            return (r.min(), r.max(), t.min(), t.max(), z.min(), z.max())

    def _extent_2d(self):
        """ 2D (index space) coordinate ranges. """
        ijk = self.shape
        ghosts = self.ghosts

        imin = ghosts[0]
        jmin = ghosts[2]

        imax = imin + ijk[0]
        jmax = jmin + ijk[1]

        if self.x is not None:
            x = self.x[imin:imax, jmin:jmax]
            y = self.y[imin:imax, jmin:jmax]
            if self.z is not None:
                z = self.z[imin:imax, jmin:jmax]
                return (x.min(), x.max(), y.min(), y.max(), z.min(), z.max())
            return (x.min(), x.max(), y.min(), y.max())
        else:
            r = self.r[imin:imax, jmin:jmax]
            t = self.t[imin:imax, jmin:jmax]
            if self.z is not None:
                z = self.z[imin:imax, jmin:jmax]
                return (r.min(), r.max(), t.min(), t.max(), z.min(), z.max())
            return (r.min(), r.max(), t.min(), t.max())

    def _extent_1d(self):
        """ 1D (index space) coordinate ranges. """
        ijk = self.shape
        ghosts = self.ghosts

        imin = ghosts[0]
        imax = imin + ijk[0]

        if self.x is not None:
            x = self.x[imin:imax]
            if self.y is not None:
                y = self.y[imin:imax]
                if self.z is not None:
                    z = self.z[imin:imax]
                    return (x.min(), x.max(), y.min(), y.max(), z.min(), z.max())
                return (x.min(), x.max(), y.min(), y.max())
            return (x.min(), x.max())
        else:
            r = self.r[imin:imax]
            t = self.t[imin:imax]
            if self.z is not None:
                z = self.z[imin:imax]
                return (r.min(), r.max(), t.min(), t.max(), z.min(), z.max())
            return (r.min(), r.max(), t.min(), t.max())

    def copy(self):
        """ Returns a deep copy of self. """
        return copy.deepcopy(self)

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`GridCoordinates`
            The grid to check against.

        logger: :class:`Logger` or None
            Used to log debug messages that will indicate what, if anything, is
            not equivalent.

        tolerance: float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, GridCoordinates):
            logger.debug('other is not a GridCoordinates object.')
            return False

        return super(GridCoordinates, self).is_equivalent(other, 'grid',
                                                          logger, tolerance)

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None,
                ghosts=None):
        """
        Construct a new :class:`GridCoordinates` from data extracted from the
        specified region.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract neglecting ghost/rind planes.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            Similarly, for 1D zones only specify imin and imax.

        ghosts: int[]
            Number of ghost/rind planes for the new zone.
            If ``None``, the existing specification is used.
        """
        vec = super(GridCoordinates, self).extract(imin, imax, jmin, jmax,
                                                   kmin, kmax, ghosts)
        return GridCoordinates(vec)

    def extend(self, axis, delta, npoints, normal=None):
        """
        Construct a new :class:`GridCoordinates` by linear extrapolation.
        The existing ghosts/rind planes specification is retained.

        axis: 'i', 'j', or 'k'
            Index axis to extend.

        delta: float
            Fractional amount to move for each point. Multiplies the 'edge'
            delta in the `axis` direction or the appropriate component of
            `normal`.  A negative value adds points before the current
            zero-index of `axis`. 

        npoints: int > 0
            Number of points to add in `axis` dimension.

        normal: float[]
            For cases where only a single point exists in the `axis` direction,
            this specifies the direction to move. If not specified, an
            axis-aligned direction is selected based on minimum grid extent.
        """
        if not delta:
            raise ValueError('delta must be non-zero')
        if npoints < 1:
            raise ValueError('npoints must be >= 1')
        i = len(super(GridCoordinates, self).shape)
        if i == 3:
            if axis not in ('i', 'j', 'k'):
                raise ValueError('axis must be i, j, or k')
            return self._extend_3d(axis, delta, npoints, normal)
        elif i == 2:
            if axis not in ('i', 'j'):
                raise ValueError('axis must be i or j')
            return self._extend_2d(axis, delta, npoints, normal)
        elif i == 1:
            if axis != 'i':
                raise ValueError('axis must be i')
            return self._extend_1d(delta, npoints, normal)
        else:
            raise RuntimeError('Grid is empty!')

    def _extend_3d(self, axis, delta, npoints, normal):
        """ 3D (index space) extension. """
        imax, jmax, kmax = self.real_shape
        need_normal = False
        if axis == 'i':
            new_shape = (imax + npoints, jmax, kmax)
            if imax < 2:
                need_normal = True
        elif axis == 'j':
            new_shape = (imax, jmax + npoints, kmax)
            if jmax < 2:
                need_normal = True
        else:
            new_shape = (imax, jmax, kmax + npoints)
            if kmax < 2:
                need_normal = True

        # Select normal if necessary.
        if need_normal:
            if normal is None:
                min1, max1, min2, max2, min3, max3 = self.extent
                delta1 = max1 - min1
                delta2 = max2 - min2
                delta3 = max3 - min3
                if delta1 <= delta2:
                    if delta1 <= delta3:
                        normal = (1., 0., 0.)
                    else:
                        normal = (0., 0., 1.)
                elif delta2 <= delta3:
                    normal = (0., 1., 0.)
                else:
                    normal = (0., 0., 1.)
            elif len(normal) != 3:
                raise ValueError('normal needs 3 elements')
        else:
            normal = (0., 0., 0.)

        # Extend.
        grid = GridCoordinates()
        if self.x is not None:
            grid.x = self._extrap_3d(axis, delta, npoints, self.x, new_shape,
                                     normal[0])
            grid.y = self._extrap_3d(axis, delta, npoints, self.y, new_shape,
                                     normal[1])
        else:
            grid.r = self._extrap_3d(axis, delta, npoints, self.r, new_shape,
                                     normal[0])
            grid.t = self._extrap_3d(axis, delta, npoints, self.t, new_shape,
                                     normal[1])
        grid.z = self._extrap_3d(axis, delta, npoints, self.z, new_shape,
                                 normal[2])
        grid.ghosts = copy.copy(self._ghosts)
        return grid

    @staticmethod
    def _extrap_3d(axis, delta, npoints, arr, new_shape, normal):
        """ Return extrapolated `arr`. """
        imax, jmax, kmax = arr.shape
        new_arr = numpy.zeros(new_shape)

        if axis == 'i':
            if delta > 0:
                v = arr[-1, :, :]
                if imax > 1:
                    dv = (v - arr[-2, :, :]) * delta
                else:
                    dv = normal * delta
                indx = imax
                new_arr[0:indx, :, :] = arr
                for i in range(npoints):
                    new_arr[indx+i, :, :] = v + (i+1)*dv
            else:
                v = arr[0, :, :]
                if imax > 1:
                    dv = (arr[1, :, :] - v) * delta
                else:
                    dv = normal * -delta
                indx = npoints
                new_arr[indx:, :, :] = arr
                indx -= 1
                for i in range(npoints):
                    new_arr[indx-i, :, :] = v + (i+1)*dv

        elif axis == 'j':
            if delta > 0:
                v = arr[:, -1, :]
                if jmax > 1:
                    dv = (v - arr[:, -2, :]) * delta
                else:
                    dv = normal * delta
                indx = jmax
                new_arr[:, 0:indx, :] = arr
                for j in range(npoints):
                    new_arr[:, indx+j, :] = v + (j+1)*dv
            else:
                v = arr[:, 0, :]
                if jmax > 1:
                    dv = (arr[:, 1, :] - v) * delta
                else:
                    dv = normal * -delta
                indx = npoints
                new_arr[:, indx:, :] = arr
                indx -= 1
                for j in range(npoints):
                    new_arr[:, indx-j, :] = v + (j+1)*dv
        else:
            if delta > 0:
                v = arr[:, :, -1]
                if kmax > 1:
                    dv = (v - arr[:, :, -2]) * delta
                else:
                    dv = normal * delta
                indx = kmax
                new_arr[:, :, 0:indx] = arr
                for k in range(npoints):
                    new_arr[:, :, indx+k] = v + (k+1)*dv
            else:
                v = arr[:, :, 0]
                if kmax > 1:
                    dv = (arr[:, :, 1] - v) * delta
                else:
                    dv = normal * -delta
                indx = npoints
                new_arr[:, :, indx:] = arr
                indx -= 1
                for k in range(npoints):
                    new_arr[:, :, indx-k] = v + (k+1)*dv
        return new_arr

    def _extend_2d(self, axis, delta, npoints, normal):
        """ 2D (index space) extension. """
        imax, jmax = self.real_shape
        need_normal = False
        if axis == 'i':
            new_shape = (imax + npoints, jmax)
            if imax < 2:
                need_normal = True
        else:
            new_shape = (imax, jmax + npoints)
            if jmax < 2:
                need_normal = True

        # Select normal if necessary.
        if need_normal:
            extent = self.extent
            dims = len(extent) / 2
            if normal is None:
                if dims == 2:
                    min1, max1, min2, max2 = extent
                    delta1 = max1 - min1
                    delta2 = max2 - min2
                    if delta1 <= delta2:
                        normal = (1., 0.)
                    else:
                        normal = (0., 1.)
                else:
                    min1, max1, min2, max2, min3, max3 = self.extent()
                    delta1 = max1 - min1
                    delta2 = max2 - min2
                    delta3 = max3 - min3
                    if delta1 <= delta2:
                        if delta1 <= delta3:
                            normal = (1., 0., 0.)
                        else:
                            normal = (0., 0., 1.)
                    elif delta2 <= delta3:
                        normal = (0., 1., 0.)
                    else:
                        normal = (0., 0., 1.)
            elif len(normal) != dims:
                raise ValueError('normal needs %d elements' % dims)
        else:
            normal = (0., 0., 0.)

        # Extend.
        grid = GridCoordinates()
        if self.x is not None:
            grid.x = self._extrap_2d(axis, delta, npoints, self.x, new_shape,
                                     normal[0])
            grid.y = self._extrap_2d(axis, delta, npoints, self.y, new_shape,
                                     normal[1])
        else:
            grid.r = self._extrap_2d(axis, delta, npoints, self.r, new_shape,
                                     normal[0])
            grid.t = self._extrap_2d(axis, delta, npoints, self.t, new_shape,
                                     normal[1])
        if self.z is not None:
            grid.z = self._extrap_2d(axis, delta, npoints, self.z, new_shape,
                                     normal[2])
        grid.ghosts = copy.copy(self._ghosts)
        return grid

    @staticmethod
    def _extrap_2d(axis, delta, npoints, arr, new_shape, normal):
        """ Return extrapolated `arr`. """
        imax, jmax = arr.shape
        new_arr = numpy.zeros(new_shape)

        if axis == 'i':
            if delta > 0:
                v = arr[-1, :]
                if imax > 1:
                    dv = (v - arr[-2, :]) * delta
                else:
                    dv = normal * delta
                indx = imax
                new_arr[0:indx, :] = arr
                for i in range(npoints):
                    new_arr[indx+i, :] = v + (i+1)*dv
            else:
                v = arr[0, :]
                if imax > 1:
                    dv = (arr[1, :] - v) * delta
                else:
                    dv = normal * -delta
                indx = npoints
                new_arr[indx:, :] = arr
                indx -= 1
                for i in range(npoints):
                    new_arr[indx-i, :] = v + (i+1)*dv
        else:
            if delta > 0:
                v = arr[:, -1]
                if jmax > 1:
                    dv = (v - arr[:, -2]) * delta
                else:
                    dv = normal * delta
                indx = jmax
                new_arr[:, 0:indx] = arr
                for j in range(npoints):
                    new_arr[:, indx+j] = v + (j+1)*dv
            else:
                v = arr[:, 0]
                if jmax > 1:
                    dv = (arr[:, 1] - v) * delta
                else:
                    dv = normal * -delta
                indx = npoints
                new_arr[:, indx:] = arr
                indx -= 1
                for j in range(npoints):
                    new_arr[:, indx-j] = v + (j+1)*dv
        return new_arr

    def _extend_1d(self, delta, npoints, normal):
        """ 1D (index space) extension. """
        imax, = self.real_shape
        new_shape = (imax + npoints,)

        # If unspecified, normal is +x.
        normal = normal or (1., 0., 0.)

        # Extend.
        grid = GridCoordinates()
        if self.x is not None:
            grid.x = self._extrap_1d(delta, npoints, self.x, new_shape,
                                     normal[0])
            if self.y is not None:
                grid.y = self._extrap_1d(delta, npoints, self.y, new_shape,
                                         normal[1])
        else:
            grid.r = self._extrap_1d(delta, npoints, self.r, new_shape,
                                     normal[0])
            grid.t = self._extrap_1d(delta, npoints, self.t, new_shape,
                                     normal[1])
        if self.z is not None:
            grid.z = self._extrap_1d(delta, npoints, self.z, new_shape,
                                     normal[2])
        grid.ghosts = copy.copy(self._ghosts)
        return grid

    @staticmethod
    def _extrap_1d(delta, npoints, arr, new_shape, normal):
        """ Return extrapolated `arr`. """
        imax, = arr.shape
        new_arr = numpy.zeros(new_shape)

        if delta > 0:
            v = arr[-1]
            if imax > 1:
                dv = (v - arr[-2]) * delta
            else:
                dv = normal * delta
            indx = imax
            new_arr[0:indx] = arr
            for i in range(npoints):
                new_arr[indx+i:] = v + (i+1)*dv
        else:
            v = arr[0]
            if imax > 1:
                dv = (arr[1] - v) * delta
            else:
                dv = normal * -delta
            indx = npoints
            new_arr[indx:] = arr
            indx -= 1
            for i in range(npoints):
                new_arr[indx-i] = v + (i+1)*dv
        return new_arr

    def make_cartesian(self, axis='z'):
        """
        Convert to Cartesian coordinate system.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
            Only used for 3D data.
        """
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z' or self.z is None:
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
            Only used for 3D data.
        """
        self.r = self.x.copy()
        self.t = self.x.copy()
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z' or self.z is None:
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

