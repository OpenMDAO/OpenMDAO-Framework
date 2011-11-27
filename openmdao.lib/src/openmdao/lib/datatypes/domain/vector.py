import copy
from math import atan2, cos, hypot, radians, sin
import numpy


class Vector(object):
    """
    Vector data for a :class:`FlowSolution`, also the base for
    :class:`GridCoordinates`.
    In Cartesian coordinates, array indexing order is x,y,z;
    so an 'i-face' is a y,z surface.
    In cylindrical coordinates, array indexing order is z,r,t;
    so an 'i-face' is an r,t surface.
    """

    def __init__(self):
        self.x = None
        self.y = None
        self.z = None
        self.r = None
        self.t = None
        self._ghosts = (0, 0, 0, 0, 0, 0)

    def _get_ghosts(self):
        return self._ghosts

    def _set_ghosts(self, ghosts):
        if len(ghosts) < 2*len(self.shape):
            raise ValueError('ghosts must be a %d-element array'
                             % (2*len(self.shape)))
        for i in ghosts:
            if i < 0:
                raise ValueError('All ghost values must be >= 0')
        self._ghosts = ghosts

    ghosts = property(_get_ghosts, _set_ghosts,
                      doc='Number of ghost/rind planes for each index direction.')

    @property
    def shape(self):
        """ Data index limits, not including 'ghost/rind' planes. """
        ijk = self.real_shape
        if len(ijk) < 1:
            return ()
        ghosts = self._ghosts
        imax = ijk[0] - (ghosts[0] + ghosts[1])
        if len(ijk) < 2:
            return (imax,)
        jmax = ijk[1] - (ghosts[2] + ghosts[3])
        if len(ijk) < 3:
            return (imax, jmax)
        kmax = ijk[2] - (ghosts[4] + ghosts[5])
        return (imax, jmax, kmax)

    @property
    def real_shape(self):
        """ Data index limits, including any 'ghost/rind' planes. """
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                return arr.shape
        return ()

    def is_equivalent(self, other, name, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`Vector`
            The vector to check against.

        name: string
            Name of this vector, used for reporting differences.

        logger: :class:`Logger` or None
            Used to log debug messages that will indicate what if anything is
            not equivalent.

        tolerance: float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, Vector):
            logger.debug('other is not a Vector object.')
            return False
        for component in ('x', 'y', 'z', 'r', 't'):
            if not self._check_equivalent(other, name, component, logger,
                                          tolerance):
                return False
        if other.ghosts != self.ghosts:
            logger.debug('ghost cell counts are not equal: %s vs. %s.',
                         other.ghosts, self.ghosts)
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
                    return False
            else:
                try:
                    if (other_arr != arr).any():
                        logger.debug('%s %s values are not equal.', name,
                                     component.upper())
                        return False
                except Exception as exc:
                    logger.debug('%s %s: %r vs. %r: %s', name, component.upper(),
                                 other_arr, arr, exc)
                    logger.debug('!=: %r', other_arr != arr)
                    return False
        return True

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None,
                ghosts=None):
        """
        Construct a new :class:`Vector` from data extracted from the
        specified region.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            For 1D zones omit jmin, jmax, kmin, and kmax.

        ghosts: int[]
            Numer of ghost/rind planes for the new zone.
            If ``None`` the existing specification is used.
        """
        ghosts = ghosts or self._ghosts
        i = len(self.shape)
        if i == 3:
            if kmin is None or kmax is None or jmin is None or jmax is None:
                raise ValueError('3D extract requires jmin, jmax, kmin, and kmax')
            return self._extract_3d(imin, imax, jmin, jmax, kmin, kmax, ghosts)
        elif i == 2:
            if kmin is not None or kmax is not None:
                raise ValueError('2D extract undefined for kmin or kmax')
            if jmin is None or jmax is None:
                raise ValueError('2D extract requires jmin and jmax')
            return self._extract_2d(imin, imax, jmin, jmax, ghosts)
        elif i == 1:
            if kmin is not None or kmax is not None:
                raise ValueError('1D extract undefined for jmin, jmax, kmin, or kmax')
            return self._extract_1d(imin, imax, ghosts)
        else:
            raise RuntimeError('Vector is empty!')

    def _extract_3d(self, imin, imax, jmin, jmax, kmin, kmax, new_ghosts):
        """ 3D (index space) extraction. """
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghost planes.
        vec_imax, vec_jmax, vec_kmax = self.shape
        if imin < 0:
            imin += vec_imax
        imin += ghosts[0]
        if imax < 0:
            imax += vec_imax
        imax += ghosts[0]
        if jmin < 0:
            jmin += vec_jmax
        jmin += ghosts[2]
        if jmax < 0:
            jmax += vec_jmax
        jmax += ghosts[2]
        if kmin < 0:
            kmin += vec_kmax
        kmin += ghosts[4]
        if kmax < 0:
            kmax += vec_kmax
        kmax += ghosts[4]

        # Adjust for new ghost/rind planes.
        imin -= new_ghosts[0]
        imax += new_ghosts[1]
        jmin -= new_ghosts[2]
        jmax += new_ghosts[3]
        kmin -= new_ghosts[4]
        kmax += new_ghosts[5]

        # Check limits.
        if imin < 0 or imax > vec_imax+ghosts[1] or \
           jmin < 0 or jmax > vec_jmax+ghosts[3] or \
           kmin < 0 or kmax > vec_kmax+ghosts[5]:
            region = (imin, imax, jmin, jmax, kmin, kmax)
            original = (0, vec_imax+ghosts[1], 0, vec_jmax+ghosts[3],
                        0, vec_kmax+ghosts[5])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                setattr(vec, component,
                        arr[imin:imax+1, jmin:jmax+1, kmin:kmax+1])
        return vec

    def _extract_2d(self, imin, imax, jmin, jmax, new_ghosts):
        """ 2D (index space) extraction. """
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghost planes.
        vec_imax, vec_jmax = self.shape
        if imin < 0:
            imin += vec_imax
        imin += ghosts[0]
        if imax < 0:
            imax += vec_imax
        imax += ghosts[0]
        if jmin < 0:
            jmin += vec_jmax
        jmin += ghosts[2]
        if jmax < 0:
            jmax += vec_jmax
        jmax += ghosts[2]

        # Check limits.
        if imin < 0 or imax > vec_imax+ghosts[1] or \
           jmin < 0 or jmax > vec_jmax+ghosts[3]:
            region = (imin, imax, jmin, jmax)
            original = (0, vec_imax+ghosts[1], 0, vec_jmax+ghosts[3])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                setattr(vec, component,
                        arr[imin:imax+1, jmin:jmax+1])
        return vec

    def _extract_1d(self, imin, imax, new_ghosts):
        """ 1D (index space) extraction. """
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghost planes.
        vec_imax, = self.shape
        if imin < 0:
            imin += vec_imax
        imin += ghosts[0]
        if imax < 0:
            imax += vec_imax
        imax += ghosts[0]

        # Check limits.
        if imin < 0 or imax > vec_imax+ghosts[1]:
            region = (imin, imax)
            original = (0, vec_imax+ghosts[1])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                setattr(vec, component, arr[imin:imax+1])
        return vec

    def extend(self, axis, delta, npoints):
        """
        Construct a new :class:`Vector` by replication.

        axis: 'i', 'j', or 'k'
            Index axis to extend.

        delta: float.
            Direction. A negative value adds points before the current
            zero-index of `axis`. 

        npoints: int > 0
            Number of points to add in `axis` dimension.
        """
        if not delta:
            raise ValueError('delta must be non-zero')
        if npoints < 1:
            raise ValueError('npoints must be >= 1')
        i = len(self.shape)
        if i == 3:
            if axis not in ('i', 'j', 'k'):
                raise ValueError('axis must be i, j, or k')
            return self._extend_3d(axis, delta, npoints)
        elif i == 2:
            if axis not in ('i', 'j'):
                raise ValueError('axis must be i or j')
            return self._extend_2d(axis, delta, npoints)
        elif i == 1:
            if axis != 'i':
                raise ValueError('axis must be i')
            return self._extend_1d(delta, npoints)
        else:
            raise RuntimeError('Vector is empty!')

    def _extend_3d(self, axis, delta, npoints):
        """ 3D (index space) extension. """
        imax, jmax, kmax = self.real_shape
        if axis == 'i':
            new_shape = (imax + npoints, jmax, kmax)
            indx = imax if delta > 0 else npoints
        elif axis == 'j':
            new_shape = (imax, jmax + npoints, kmax)
            indx = jmax if delta > 0 else npoints
        else:
            new_shape = (imax, jmax, kmax + npoints)
            indx = kmax if delta > 0 else npoints

        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                new_arr = numpy.zeros(new_shape)
                if axis == 'i':
                    if delta > 0:
                        new_arr[0:indx, :, :] = arr
                        for i in range(npoints):
                            new_arr[indx+i, :, :] = arr[-1, :, :]
                    else:
                        new_arr[indx:, :, :] = arr
                        for i in range(npoints):
                            new_arr[i, :, :] = arr[0, :, :]
                elif axis == 'j':
                    if delta > 0:
                        new_arr[:, 0:indx, :] = arr
                        for j in range(npoints):
                            new_arr[:, indx+j, :] = arr[:, -1, :]
                    else:
                        new_arr[:, indx:, :] = arr
                        for j in range(npoints):
                            new_arr[:, j, :] = arr[:, 0, :]
                else:
                    if delta > 0:
                        new_arr[:, :, 0:indx] = arr
                        for k in range(npoints):
                            new_arr[:, :, indx+k] = arr[:, :, -1]
                    else:
                        new_arr[:, :, indx:] = arr
                        for k in range(npoints):
                            new_arr[:, :, k] = arr[:, :, 0]
                setattr(vec, component, new_arr)
        vec.ghosts = copy.copy(self._ghosts)
        return vec

    def _extend_2d(self, axis, delta, npoints):
        """ 2D (index space) extension. """
        imax, jmax = self.real_shape
        if axis == 'i':
            new_shape = (imax + npoints, jmax)
            indx = imax if delta > 0 else npoints
        else:
            new_shape = (imax, jmax + npoints)
            indx = jmax if delta > 0 else npoints

        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                new_arr = numpy.zeros(new_shape)
                if axis == 'i':
                    if delta > 0:
                        new_arr[0:indx, :] = arr
                        for i in range(npoints):
                            new_arr[indx+i, :] = arr[-1, :]
                    else:
                        new_arr[indx:, :] = arr
                        for i in range(npoints):
                            new_arr[i, :] = arr[0, :]
                else:
                    if delta > 0:
                        new_arr[:, 0:indx] = arr
                        for j in range(npoints):
                            new_arr[:, indx+j] = arr[:, -1]
                    else:
                        new_arr[:, indx:] = arr
                        for j in range(npoints):
                            new_arr[:, j] = arr[:, 0]
                setattr(vec, component, new_arr)
        vec.ghosts = copy.copy(self._ghosts)
        return vec

    def _extend_1d(self, delta, npoints):
        """ 1D (index space) extension. """
        imax, = self.real_shape
        new_shape = (imax + npoints,)
        indx = imax if delta > 0 else npoints

        vec = Vector()
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                new_arr = numpy.zeros(new_shape)
                if delta > 0:
                    new_arr[0:indx] = arr
                    for i in range(npoints):
                        new_arr[indx+i] = arr[-1]
                else:
                    new_arr[indx:] = arr
                    for i in range(npoints):
                        new_arr[i] = arr[0]
                setattr(vec, component, new_arr)
        vec.ghosts = copy.copy(self._ghosts)
        return vec

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        if self.z is None:
            raise AttributeError('flip_z: no Z component')
        self.z *= -1.

    def make_cartesian(self, grid, axis='z'):
        """
        Convert to Cartesian coordinate system.

        grid: :class:`GridCoordinates`
            Must be in cylindrical form.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        if grid.shape != self.shape:
            raise NotImplementedError('make_cartesian: grid shape mismatch'
                                      ' not supported')
        gt_flat = grid.t.flat
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z' or self.z is None:
            self.x = self.r.copy()
            self.y = self.r.copy()
            x_flat = self.x.flat
            y_flat = self.y.flat
            for i in range(self.r.size):
                gt = gt_flat[i]
                sine = sin(gt)
                cosine = cos(gt)
                r = r_flat[i]
                t = t_flat[i]
                x_flat[i] = r*cosine - t*sine
                y_flat[i] = r*sine   + t*cosine
            self.r = None
            self.t = None

        elif axis == 'x':
            self.x = self.z
            self.y = self.r.copy()
            self.z = self.r.copy()
            y_flat = self.y.flat
            z_flat = self.z.flat
            for i in range(self.r.size):
                gt = gt_flat[i]
                sine = sin(gt)
                cosine = cos(gt)
                r = r_flat[i]
                t = t_flat[i]
                y_flat[i] = r*cosine - t*sine
                z_flat[i] = r*sine   + t*cosine
            self.r = None
            self.t = None

        else:
            raise ValueError("axis must be 'z' or 'x'")

    def make_cylindrical(self, grid, axis='z'):
        """
        Convert to cylindrical coordinate system.

        grid: :class:`GridCoordinates`
            Must be in cylindrical form.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        if grid.shape != self.shape:
            raise NotImplementedError('make_cylindrical: grid shape mismatch'
                                      ' not supported')
        gt_flat = grid.t.flat
        self.r = self.x.copy()
        self.t = self.x.copy()
        r_flat = self.r.flat
        t_flat = self.t.flat

        if axis == 'z' or self.z is None:
            x_flat = self.x.flat
            y_flat = self.y.flat
            for i in range(self.x.size):
                gt = gt_flat[i]
                x = x_flat[i]
                y = y_flat[i]
                magnitude = hypot(x, y)
                rel_theta = atan2(y, x) - gt
                r_flat[i] = magnitude * cos(rel_theta)
                t_flat[i] = magnitude * sin(rel_theta)
            self.x = None
            self.y = None

        elif axis == 'x':
            y_flat = self.y.flat
            z_flat = self.z.flat
            for i in range(self.y.size):
                gt = gt_flat[i]
                y = y_flat[i]
                z = z_flat[i]
                magnitude = hypot(y, z)
                rel_theta = atan2(z, y) - gt
                r_flat[i] = magnitude * cos(rel_theta)
                t_flat[i] = magnitude * sin(rel_theta)
            self.z = self.x
            self.x = None
            self.y = None

        else:
            raise ValueError("axis must be 'z' or 'x'")

    def rotate_about_x(self, deg):
        """
        Rotate about the X axis.

        deg: float (degrees)
           Amount of rotation.
        """
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
        """
        Rotate about the Y axis.

        deg: float (degrees)
           Amount of rotation.
        """
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
        """
        Rotate about the Z axis.

        deg: float (degrees)
           Amount of rotation.
        """
        if self.x is None:
            raise AttributeError('rotate_about_z: no X component')
        if self.y is None:
            raise AttributeError('rotate_about_z: no Y component')

        sine   = sin(radians(deg))
        cosine = cos(radians(deg))
        x_new  = self.x*cosine - self.y*sine
        self.y = self.y*cosine + self.x*sine
        self.x = x_new

    def promote(self):
        """ Promote from N-dimensional to N+1 dimensional index space. """
        shape = self.real_shape

        if len(shape) > 2:
            raise RuntimeError('Vector is 3D')

        elif len(shape) > 1:
            imax, jmax = shape
            if self.x is not None:  # x,y -> x,y,z
                new_arr = numpy.zeros((imax, jmax, 1))
                new_arr[:, :, 0] = self.x[:, :]
                self.x = new_arr
                new_arr = numpy.zeros((imax, jmax, 1))
                new_arr[:, :, 0] = self.y[:, :]
                self.y = new_arr
                if self.z is not None:
                    new_arr = numpy.zeros((1, imax, jmax))
                    new_arr[:, :, 0] = self.z[:, :]
                    self.z = new_arr
                else:
                    self.z = numpy.zeros((imax, jmax, 1))
            else:  # r,t -> z,r,t (note index order change!)
                new_arr = numpy.zeros((1, imax, jmax))
                new_arr[0, :, :] = self.r[:, :]
                self.r = new_arr
                new_arr = numpy.zeros((1, imax, jmax))
                new_arr[0, :, :] = self.t[:, :]
                self.t = new_arr
                if self.z is not None:
                    new_arr = numpy.zeros((1, imax, jmax))
                    new_arr[0, :, :] = self.z[:, :]
                    self.z = new_arr
                else:
                    self.z = numpy.zeros((1, imax, jmax))

        elif len(shape) > 0:
            imax = shape[0]
            if self.x is not None:  # x -> x,y[,z]
                new_arr = numpy.zeros((imax, 1))
                new_arr[:, 0] = self.x[:]
                self.x = new_arr
                if self.y is not None:
                    new_arr = numpy.zeros((imax, 1))
                    new_arr[:, 0] = self.y[:]
                    self.y = new_arr
                    if self.z is not None:
                        new_arr = numpy.zeros((imax, 1))
                        new_arr[:, 0] = self.z[:]
                        self.z = new_arr
                else:
                    self.y = numpy.zeros((imax, 1))
            else:  # r,t -> r,t[,z]
                new_arr = numpy.zeros((imax, 1))
                new_arr[:, 0] = self.r[:]
                self.r = new_arr
                new_arr = numpy.zeros((imax, 1))
                new_arr[:, 0] = self.t[:]
                self.t = new_arr
                if self.z is not None:
                    new_arr = numpy.zeros((imax, 1))
                    new_arr[:, 0] = self.z[:]
                    self.z = new_arr
        else:
            raise RuntimeError('Vector is empty!')

    def demote(self):
        """ Demote from N-dimensional to N-1 dimensional index space. """
        shape = self.real_shape
        ghosts = self._ghosts

        if len(shape) > 2:
            imax, jmax, kmax = shape
            imx = imax - (ghosts[0] + ghosts[1])
            jmx = jmax - (ghosts[2] + ghosts[3])
            kmx = kmax - (ghosts[4] + ghosts[5])
            if imx == 1:
                if self.x is not None:
                    new_arr = numpy.zeros((jmax, kmax))
                    new_arr[:, :] = self.x[0, :, :]
                    self.x = new_arr
                    new_arr = numpy.zeros((jmax, kmax))
                    new_arr[:, :] = self.y[0, :, :]
                    self.y = new_arr
                else:
                    new_arr = numpy.zeros((jmax, kmax))
                    new_arr[:, :] = self.r[0, :, :]
                    self.r = new_arr
                    new_arr = numpy.zeros((jmax, kmax))
                    new_arr[:, :] = self.t[0, :, :]
                    self.t = new_arr
                new_arr = numpy.zeros((jmax, kmax))
                new_arr[:, :] = self.z[0, :, :]
                self.z = new_arr
                self._ghosts = (ghosts[2], ghosts[3], ghosts[4], ghosts[5], 0, 0)
            elif jmx == 1:
                if self.x is not None:
                    new_arr = numpy.zeros((imax, kmax))
                    new_arr[:, :] = self.x[:, 0, :]
                    self.x = new_arr
                    new_arr = numpy.zeros((imax, kmax))
                    new_arr[:, :] = self.y[:, 0, :]
                    self.y = new_arr
                else:
                    new_arr = numpy.zeros((imax, kmax))
                    new_arr[:, :] = self.r[:, 0, :]
                    self.r = new_arr
                    new_arr = numpy.zeros((imax, kmax))
                    new_arr[:, :] = self.t[:, 0, :]
                    self.t = new_arr
                new_arr = numpy.zeros((imax, kmax))
                new_arr[:, :] = self.z[:, 0, :]
                self.z = new_arr
                self._ghosts = (ghosts[0], ghosts[1], ghosts[4], ghosts[5], 0, 0)
            elif kmx == 1:
                if self.x is not None:
                    new_arr = numpy.zeros((imax, jmax))
                    new_arr[:, :] = self.x[:, :, 0]
                    self.x = new_arr
                    new_arr = numpy.zeros((imax, jmax))
                    new_arr[:, :] = self.y[:, :, 0]
                    self.y = new_arr
                else:
                    new_arr = numpy.zeros((imax, jmax))
                    new_arr[:, :] = self.r[:, :, 0]
                    self.r = new_arr
                    new_arr = numpy.zeros((imax, jmax))
                    new_arr[:, :] = self.t[:, :, 0]
                    self.t = new_arr
                new_arr = numpy.zeros((imax, jmax))
                new_arr[:, :] = self.z[:, :, 0]
                self.z = new_arr
                self._ghosts = (ghosts[0], ghosts[1], ghosts[2], ghosts[3], 0, 0)
            else: 
                raise RuntimeError('No i, j, or k plane to collapse')

        elif len(shape) > 1:
            imax, jmax = shape
            imx = imax - (ghosts[0] + ghosts[1])
            jmx = jmax - (ghosts[2] + ghosts[3])
            if imx == 1:
                if self.x is not None:
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = self.x[0, :]
                    self.x = new_arr
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = self.y[0, :]
                    self.y = new_arr
                else:
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = self.r[0, :]
                    self.r = new_arr
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = self.t[0, :]
                    self.t = new_arr
                if self.z is not None:
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = self.z[0, :]
                    self.z = new_arr
                self._ghosts = (ghosts[2], ghosts[3], 0, 0, 0, 0)
            elif jmx == 1:
                if self.x is not None:
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = self.x[:, 0]
                    self.x = new_arr
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = self.y[:, 0]
                    self.y = new_arr
                else:
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = self.r[:, 0]
                    self.r = new_arr
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = self.t[:, 0]
                    self.t = new_arr
                if self.z is not None:
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = self.z[:, 0]
                    self.z = new_arr
                self._ghosts = (ghosts[0], ghosts[1], 0, 0, 0, 0)
            else:
                raise RuntimeError('No i or j plane to collapse')

        elif len(shape) > 0:
            raise RuntimeError('Vector is 1D')

        else:
            raise RuntimeError('Vector is empty!')

