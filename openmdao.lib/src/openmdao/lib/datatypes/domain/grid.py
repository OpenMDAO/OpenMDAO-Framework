from math import atan2, cos, hypot, sin

from openmdao.lib.datatypes.domain.vector import Vector


class GridCoordinates(Vector):
    """
    Coordinate data for a :class:`Zone`.
    Currently limited to structured grids.
    1D spatial data must set 'x'.
    2D spatial data must set 'x', and 'y' or 'r' and 't'.
    3D spatial data must set 'x', 'y', and 'z' or 'r', 't', and 'z'.
    Note that index dimension is not the same as spatial dimension.
    I.e. a curve in 3D space is represented by 1D 'x', 'y', and 'z' arrays.
    It can also be represented by 2D or 3D arrays where only one index
    dimension varies.
    """

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
                      doc='Number of ghost/rind planes in each index direction.')

    @property
    def shape(self):
        """ Coordinate index limits, not including 'ghost/rind' planes. """
        ijk = super(GridCoordinates, self).shape
        i = len(ijk)
        if i < 1:
            return ijk
        ghosts = self._ghosts
        imax = ijk[0] - (ghosts[0] + ghosts[1])
        if i < 2:
            return (imax,)
        jmax = ijk[1] - (ghosts[2] + ghosts[3])
        if i < 3:
            return (imax, jmax)
        kmax = ijk[2] - (ghosts[4] + ghosts[5])
        return (imax, jmax, kmax)

    @property
    def extent(self):
        """
        Coordinate ranges, not including 'ghost/rind' planes.
        If cartesian ``(xmin,xmax,ymin,ymax,zmin,zmax)``.
        Otherwise ``(zmin,zmax,rmin,rmax,tmin,tmax)``.
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
        ijk = super(GridCoordinates, self).shape
        ghosts = self._ghosts

        imin = ghosts[0]
        jmin = ghosts[2]
        kmin = ghosts[4]

        imax = ijk[0] - ghosts[1]
        jmax = ijk[1] - ghosts[3]
        kmax = ijk[2] - ghosts[5]

        if self.x is not None:
            x = self.x[imin:imax, jmin:jmax, kmin:kmax]
            y = self.y[imin:imax, jmin:jmax, kmin:kmax]
            z = self.z[imin:imax, jmin:jmax, kmin:kmax]
            return (x.min(), x.max(), y.min(), y.max(), z.min(), z.max())
        else:
            r = self.r[imin:imax, jmin:jmax, kmin:kmax]
            t = self.t[imin:imax, jmin:jmax, kmin:kmax]
            z = self.z[imin:imax, jmin:jmax, kmin:kmax]
            return (z.min(), z.max(), r.min(), r.max(), t.min(), t.max())

    def _extent_2d(self):
        """ 2D (index space) coordinate ranges. """
        ijk = super(GridCoordinates, self).shape
        ghosts = self._ghosts

        imin = ghosts[0]
        jmin = ghosts[2]

        imax = ijk[0] - ghosts[1]
        jmax = ijk[1] - ghosts[3]

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
                return (z.min(), z.max(), r.min(), r.max(), t.min(), t.max())
            return (r.min(), r.max(), t.min(), t.max())

    def _extent_1d(self):
        """ 1D (index space) coordinate ranges. """
        ijk = super(GridCoordinates, self).shape
        ghosts = self._ghosts

        imin = ghosts[0]
        imax = ijk[0] - ghosts[1]

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
                return (z.min(), z.max(), r.min(), r.max(), t.min(), t.max())
            return (r.min(), r.max(), t.min(), t.max())

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`GridCoordinates`
            The grid to check against.

        logger: :class:`Logger` or None
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

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None):
        """
        Construct a new :class:`GridCoordinates` from data extracted from the
        specified region. Ghost data is copied.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract neglecting ghost/rind planes.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            Similarly, for 1D zones only specify imin and imax.
        """
        i = len(super(GridCoordinates, self).shape)
        if i == 3:
            if jmin is None or jmax is None or kmin is None or kmax is None:
                raise ValueError('3D extract requires jmin, jmax, kmin, and kmax')
            return self._extract_3d(imin, imax, jmin, jmax, kmin, kmax)
        elif i == 2:
            if kmin is not None or kmax is not None:
                raise ValueError('2D extract undefined for kmin or kmax')
            if jmin is None or jmax is None:
                raise ValueError('2D extract requires jmin and jmax')
            return self._extract_2d(imin, imax, jmin, jmax)
        else:
            if kmin is not None or kmax is not None or \
               jmin is not None or jmax is not None:
                raise ValueError('1D extract undefined for jmin, jmax, kmin, or kmax')
            return self._extract_1d(imin, imax)

    def _extract_3d(self, imin, imax, jmin, jmax, kmin, kmax):
        """ 3D (index space) extraction. """
        # Support end-relative indexing.
        grid_imax, grid_jmax, grid_kmax = super(GridCoordinates, self).shape
        if imin < 0:
            imin += grid_imax
        if imax < 0:
            imax += grid_imax
        if jmin < 0:
            jmin += grid_jmax
        if jmax < 0:
            jmax += grid_jmax
        if kmin < 0:
            kmin += grid_kmax
        if kmax < 0:
            kmax += grid_kmax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]
        imax += ghosts[0]
        jmin += ghosts[2]
        jmax += ghosts[2]
        kmin += ghosts[4]
        kmax += ghosts[4]

        grid = GridCoordinates()
        if self.x is not None:  # Cartesian
            grid.x = self.x[imin:imax+1, jmin:jmax+1, kmin:kmax+1]
            grid.y = self.y[imin:imax+1, jmin:jmax+1, kmin:kmax+1]
        else:  # Cylindrical
            grid.r = self.r[imin:imax+1, jmin:jmax+1, kmin:kmax+1]
            grid.t = self.t[imin:imax+1, jmin:jmax+1, kmin:kmax+1]
        grid.z = self.z[imin:imax+1, jmin:jmax+1, kmin:kmax+1]
        grid.ghosts = ghosts
        return grid

    def _extract_2d(self, imin, imax, jmin, jmax):
        """ 2D (index space) extraction. """
        # Support end-relative indexing.
        grid_imax, grid_jmax = super(GridCoordinates, self).shape
        if imin < 0:
            imin += grid_imax
        if imax < 0:
            imax += grid_imax
        if jmin < 0:
            jmin += grid_jmax
        if jmax < 0:
            jmax += grid_jmax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]
        imax += ghosts[0]
        jmin += ghosts[2]
        jmax += ghosts[2]

        grid = GridCoordinates()
        if self.x is not None:  # Cartesian
            grid.x = self.x[imin:imax+1, jmin:jmax+1]
            grid.y = self.y[imin:imax+1, jmin:jmax+1]
        else:  # Cylindrical
            grid.r = self.r[imin:imax+1, jmin:jmax+1]
            grid.t = self.t[imin:imax+1, jmin:jmax+1]
        if self.z is not None:
            grid.z = self.z[imin:imax+1, jmin:jmax+1]
        grid.ghosts = ghosts
        return grid

    def _extract_1d(self, imin, imax):
        """ 1D (index space) extraction. """
        # Support end-relative indexing.
        grid_imax = super(GridCoordinates, self).shape[0]
        if imin < 0:
            imin += grid_imax
        if imax < 0:
            imax += grid_imax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]

        grid = GridCoordinates()
        if self.x is not None:  # Cartesian
            grid.x = self.x[imin:imax+1]
            if self.y is not None:
                grid.y = self.y[imin:imax+1]
        else:  # Cylindrical
            grid.r = self.r[imin:imax+1]
            grid.t = self.t[imin:imax+1]
        if self.z is not None:
            grid.z = self.z[imin:imax+1]
        grid.ghosts = ghosts
        return grid

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

