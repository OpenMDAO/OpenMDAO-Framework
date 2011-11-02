from math import atan2, cos, hypot, radians, sin

import numpy


class Vector(object):
    """
    Vector data for a :class:`FlowSolution`.
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

    @property
    def shape(self):
        """ Tuple of index limits. """
        for component in ('x', 'y', 'z', 'r', 't'):
            arr = getattr(self, component)
            if arr is not None:
                return arr.shape
        return ()

    @property
    def extent(self):
        """
        Tuple of component ranges.
        If Cartesian ``(xmin,xmax,ymin,ymax,zmin,zmax)``.
        Otherwise ``(rmin,rmax,tmin,tmax,zmin,zmax)``.
        """
        if self.x is not None:
            if self.y is not None:
                if self.z is not None:
                    return (self.x.min(), self.x.max(),
                            self.y.min(), self.y.max(),
                            self.z.min(), self.z.max())
                return (self.x.min(), self.x.max(),
                        self.y.min(), self.y.max())
            return (self.x.min(), self.x.max())
        elif self.r is not None:
            if self.z is not None:
                return (self.r.min(), self.r.max(),
                        self.t.min(), self.t.max(),
                        self.z.min(), self.z.max())
            return (self.r.min(), self.r.max(),
                    self.t.min(), self.t.max())
        else:
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

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None):
        """
        Construct a new :class:`Vector` from data extracted from the
        specified region.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            For 1D zones omit jmin, jmax, kmin, and kmax.
        """
        i = len(self.shape)
        if i == 3:
            if kmin is None or kmax is None or jmin is None or jmax is None:
                raise ValueError('3D extract requires jmin, jmax, kmin, and kmax')
            return self._extract_3d(imin, imax, jmin, jmax, kmin, kmax)
        elif i == 2:
            if kmin is not None or kmax is not None:
                raise ValueError('2D extract undefined for kmin or kmax')
            if jmin is None or jmax is None:
                raise ValueError('2D extract requires jmin and jmax')
            return self._extract_2d(imin, imax, jmin, jmax)
        elif i == 1:
            if kmin is not None or kmax is not None:
                raise ValueError('1D extract undefined for jmin, jmax, kmin, or kmax')
            return self._extract_1d(imin, imax)
        else:
            raise RuntimeError('Vector is empty!')

    def _extract_3d(self, imin, imax, jmin, jmax, kmin, kmax):
        """ 3D (index space) extraction. """
        # Support end-relative indexing.
        vec_imax, vec_jmax, vec_kmax = self.shape
        if imin < 0:
            imin += vec_imax
        if imax < 0:
            imax += vec_imax
        if jmin < 0:
            jmin += vec_jmax
        if jmax < 0:
            jmax += vec_jmax
        if kmin < 0:
            kmin += vec_kmax
        if kmax < 0:
            kmax += vec_kmax

        # Check limits.
        if imin < 0 or imax > vec_imax or \
           jmin < 0 or jmax > vec_jmax or \
           kmin < 0 or kmax > vec_kmax:
            region = (imin, imax, jmin, jmax, kmin, kmax)
            original = (0, vec_imax, 0, vec_jmax, 0, vec_kmax)
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

    def _extract_2d(self, imin, imax, jmin, jmax):
        """ 2D (index space) extraction. """
        # Support end-relative indexing.
        vec_imax, vec_jmax = self.shape
        if imin < 0:
            imin += vec_imax
        if imax < 0:
            imax += vec_imax
        if jmin < 0:
            jmin += vec_jmax
        if jmax < 0:
            jmax += vec_jmax

        # Check limits.
        if imin < 0 or imax > vec_imax or jmin < 0 or jmax > vec_jmax:
            region = (imin, imax, jmin, jmax)
            original = (0, vec_imax, 0, vec_jmax)
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

    def _extract_1d(self, imin, imax):
        """ 1D (index space) extraction. """
        # Support end-relative indexing.
        vec_imax = self.shape[0]
        if imin < 0:
            imin += vec_imax
        if imax < 0:
            imax += vec_imax

        # Check limits.
        if imin < 0 or imax > vec_imax:
            region = (imin, imax)
            original = (0, vec_imax)
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
        shape = self.shape
        if axis == 'i':
            new_shape = (shape[0] + npoints, shape[1], shape[2])
            indx = shape[0] if delta > 0 else new_shape[0] - shape[0]
        elif axis == 'j':
            new_shape = (shape[0], shape[1] + npoints, shape[2])
            indx = shape[1] if delta > 0 else new_shape[1] - shape[1]
        else:
            new_shape = (shape[0], shape[1], shape[2] + npoints)
            indx = shape[2] if delta > 0 else new_shape[2] - shape[2]

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
        return vec

    def _extend_2d(self, axis, delta, npoints):
        """ 2D (index space) extension. """
        shape = self.shape
        if axis == 'i':
            new_shape = (shape[0] + npoints, shape[1])
            indx = shape[0] if delta > 0 else new_shape[0] - shape[0]
        else:
            new_shape = (shape[0], shape[1] + npoints)
            indx = shape[1] if delta > 0 else new_shape[1] - shape[1]

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
        return vec

    def _extend_1d(self, delta, npoints):
        """ 1D (index space) extension. """
        shape = self.shape
        new_shape = (shape[0] + npoints,)
        indx = shape[0] if delta > 0 else new_shape[0] - shape[0]

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
        shape = self.shape

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
        shape = self.shape

        if len(shape) > 2:
            imax, jmax, kmax = shape
            if imax == 1:
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
            elif jmax == 1:
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
            elif kmax == 1:
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
            else: 
                raise RuntimeError('No i, j, or k plane to collapse')

        elif len(shape) > 1:
            imax, jmax = shape
            if imax == 1:
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
            elif jmax == 1:
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
            else:
                raise RuntimeError('No i or j plane to collapse')
        else:
            raise RuntimeError('Vector is 1D')

