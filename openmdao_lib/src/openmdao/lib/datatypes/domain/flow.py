import copy
import numpy

VERTEX = 'Vertex'
CELL_CENTER = 'CellCenter'
_GRID_LOCATIONS = (VERTEX, CELL_CENTER)


class FlowSolution(object):
    """
    Contains solution variables for a :class:`Zone`.
    All variables have the same shape and grid location.
    """

    def __init__(self):
        self._grid_location = VERTEX
        self._ghosts = (0, 0, 0, 0, 0, 0)
        self._arrays = []
        self._vectors = []

    def _get_grid_location(self):
        return self._grid_location

    def _set_grid_location(self, loc):
        if loc not in _GRID_LOCATIONS:
            raise ValueError('%r is not a valid grid location' % loc)
        self._grid_location = loc

    grid_location = property(_get_grid_location, _set_grid_location,
                             doc='Position at which data is located;'
                                 ' must be one of %s' % (_GRID_LOCATIONS,))
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
        for vector in self._vectors:
            vector.ghosts = ghosts

    ghosts = property(_get_ghosts, _set_ghosts,
                      doc='Number of ghost cells for each index direction.')
    @property
    def arrays(self):
        """ List of scalar data arrays. """
        return self._arrays

    @property
    def vectors(self):
        """ List of vector data. """
        return self._vectors

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
        if self._vectors:
            return self._vectors[0].real_shape
        elif self._arrays:
            return self._arrays[0].shape
        else:
            return ()

    def add_array(self, name, array):
        """
        Add a :class:`numpy.ndarray` of scalar data and bind to `name`.
        Returns the added array.

        name: string
            Name for the added array.

        array: :class:`numpy.ndarray`
            Scalar data.
        """
        if hasattr(self, name):
            raise ValueError('name %r is already bound' % name)
        if self._arrays:
            ijk = self._arrays[0].shape
        elif self._vectors:
            ijk = self._vectors[0].shape
        else:
            ijk = ()
        if ijk and array.shape != ijk:
            raise ValueError('array shape %s != existing shape %s'
                             % (array.shape, ijk))
        setattr(self, name, array)
        self._arrays.append(array)
        return array

    def add_vector(self, name, vector):
        """
        Add a :class:`Vector` and bind to `name`.
        Returns the added vector.

        name: string
            Name for the added array.

        vector: :class:`Vector`
            Vector data.
        """
        if hasattr(self, name):
            raise ValueError('name %r is already bound' % name)
        shape = self.real_shape
        if shape and vector.real_shape != shape:
            raise ValueError('vector real shape %s != existing real shape %s'
                             % (vector.real_shape, shape))
        setattr(self, name, vector)
        self._vectors.append(vector)
        vector.ghosts = self.ghosts
        return vector

    def copy(self):
        """ Returns a deep copy of self. """
        return copy.deepcopy(self)

    def _copy_scalars(self, other):
        """ Copy scalars from `other` to self. """
        for name, val in other.__dict__.items():
            if not hasattr(self, name):
                setattr(self, name, val)

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`FlowSolution`
            The flowfield to check against.

        logger: :class:`Logger` or None
            Used to log debug messages that will indicate what, if anything, is
            not equivalent.

        tolerance: float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, FlowSolution):
            logger.debug('other is not a FlowSolution object.')
            return False

        if other.grid_location != self.grid_location:
            logger.debug('grid locations are not equal: %s vs. %s.',
                         other.grid_location, self.grid_location)
            return False

        if other.ghosts != self.ghosts:
            logger.debug('flow ghost cell counts are not equal: %s vs. %s.',
                         other.ghosts, self.ghosts)
            return False

        for arr in self._arrays:
            name = self.name_of_obj(arr)
            try:
                other_arr = getattr(other, name)
            except AttributeError:
                logger.debug('other is missing array %r', name)
                return False
            if tolerance > 0.:
                if not numpy.allclose(other_arr, arr, tolerance, tolerance):
                    logger.debug("%s values are not 'close'.", name)
                    return False
            else:
                if (other_arr != arr).any():
                    logger.debug('%s values are not equal.', name)
                    return False

        for vector in self._vectors:
            name = self.name_of_obj(vector)
            try:
                other_vector = getattr(other, name)
            except AttributeError:
                logger.debug('other is missing vector %r', name)
                return False
            if not vector.is_equivalent(other_vector, name, logger, tolerance):
                return False

# TODO: check scalars

        return True

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None,
                ghosts=None):
        """
        Construct a new :class:`FlowSolution` from data extracted from the
        specified region.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract neglecting ghost/rind planes.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
            For 1D zones omit jmin, jmax, kmin, and kmax.

        ghosts: int[]
            Number of ghost/rind planes for the new zone.
            If ``None`` the existing specification is used.
        """
        ghosts = ghosts or self._ghosts
        i = len(self.shape)
        if i == 3:
            if jmin is None or jmax is None or kmin is None or kmax is None:
                raise ValueError('3D extract requires jmin, jmax, kmin, and kmax')
            return self._extract_3d(imin, imax, jmin, jmax, kmin, kmax, ghosts)
        elif i == 2:
            if kmin is not None or kmax is not None:
                raise ValueError('2D extract undefined for kmin or kmax')
            if jmin is None or jmax is None:
                raise ValueError('2D extract requires jmin and jmax')
            return self._extract_2d(imin, imax, jmin, jmax, ghosts)
        elif i == 1:
            if kmin is not None or kmax is not None or \
               jmin is not None or jmax is not None:
                raise ValueError('1D extract undefined for jmin, jmax, kmin, or kmax')
            return self._extract_1d(imin, imax, ghosts)
        else:
            raise RuntimeError('FlowSolution is empty!')

    def _extract_3d(self, imin, imax, jmin, jmax, kmin, kmax, new_ghosts):
        """ 3D (index space) extraction. """
        imn, imx, jmn, jmx, kmn, kmx = imin, imax, jmin, jmax, kmin, kmax
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghostplanes.
        flow_imax, flow_jmax, flow_kmax = self.shape
        if imin < 0:
            imin += flow_imax
        imin += ghosts[0]
        if imax < 0:
            imax += flow_imax
        imax += ghosts[0]
        if jmin < 0:
            jmin += flow_jmax
        jmin += ghosts[2]
        if jmax < 0:
            jmax += flow_jmax
        jmax += ghosts[2]
        if kmin < 0:
            kmin += flow_kmax
        kmin += ghosts[4]
        if kmax < 0:
            kmax += flow_kmax
        kmax += ghosts[4]

        # Adjust for new ghost/rind planes.
        imin -= new_ghosts[0]
        imax += new_ghosts[1]
        jmin -= new_ghosts[2]
        jmax += new_ghosts[3]
        kmin -= new_ghosts[4]
        kmax += new_ghosts[5]

        # Check limits.
        if imin < 0 or imax > flow_imax+ghosts[1] or \
           jmin < 0 or jmax > flow_jmax+ghosts[3] or \
           kmin < 0 or kmax > flow_kmax+ghosts[5]:
            region = (imin, imax, jmin, jmax, kmin, kmax)
            original = (0, flow_imax+ghosts[1], 0, flow_jmax+ghosts[3],
                        0, flow_kmax+ghosts[5])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr),
                           arr[imin:imax+1, jmin:jmax+1, kmin:kmax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extract(imn, imx, jmn, jmx, kmn, kmx,
                                           ghosts=new_ghosts))
        flow.grid_location = self.grid_location
        flow.ghosts = new_ghosts
        flow._copy_scalars(self)
        return flow

    def _extract_2d(self, imin, imax, jmin, jmax, new_ghosts):
        """ 2D (index space) extraction. """
        imn, imx, jmn, jmx, = imin, imax, jmin, jmax
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghost planes.
        flow_imax, flow_jmax = self.shape
        if imin < 0:
            imin += flow_imax
        imin += ghosts[0]
        if imax < 0:
            imax += flow_imax
        imax += ghosts[0]
        if jmin < 0:
            jmin += flow_jmax
        jmin += ghosts[2]
        if jmax < 0:
            jmax += flow_jmax
        jmax += ghosts[2]

        # Adjust for new ghost/rind planes.
        imin -= new_ghosts[0]
        imax += new_ghosts[1]
        jmin -= new_ghosts[2]
        jmax += new_ghosts[3]

        # Check limits.
        if imin < 0 or imax > flow_imax+ghosts[1] or \
           jmin < 0 or jmax > flow_jmax+ghosts[3]:
            region = (imin, imax, jmin, jmax)
            original = (0, flow_imax+ghosts[1], 0, flow_jmax+ghosts[3])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr),
                           arr[imin:imax+1, jmin:jmax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extract(imn, imx, jmn, jmx,
                                           ghosts=new_ghosts))
        flow.grid_location = self.grid_location
        flow.ghosts = new_ghosts
        flow._copy_scalars(self)
        return flow

    def _extract_1d(self, imin, imax, new_ghosts):
        """ 1D (index space) extraction. """
        imn, imx = imin, imax
        ghosts = self._ghosts

        # Support end-relative indexing and adjust for existing ghost planes.
        flow_imax, = self.shape
        if imin < 0:
            imin += flow_imax
        imin += ghosts[0]
        if imax < 0:
            imax += flow_imax
        imax += ghosts[0]

        # Adjust for new ghost/rind planes.
        imin -= new_ghosts[0]
        imax += new_ghosts[1]

        # Check limits.
        if imin < 0 or imax > flow_imax+ghosts[1]:
            region = (imin, imax)
            original = (0, flow_imax+ghosts[1])
            raise ValueError('Extraction region %s exceeds original %s'
                             % (region, original))
        # Extract.
        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr), arr[imin:imax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extract(imn, imx, ghosts=new_ghosts))
        flow.grid_location = self.grid_location
        flow.ghosts = new_ghosts
        flow._copy_scalars(self)
        return flow

    def extend(self, axis, delta, npoints):
        """
        Construct a new :class:`FlowSolution` by replication.
        The existing ghosts/rind planes specification is retained.

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
            raise RuntimeError('FlowSolution is empty!')

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

        flow = FlowSolution()
        for arr in self._arrays:
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
            flow.add_array(self.name_of_obj(arr), new_arr)

        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extend(axis, delta, npoints))

        flow.grid_location = self.grid_location
        flow.ghosts = copy.copy(self._ghosts)
        flow._copy_scalars(self)
        return flow

    def _extend_2d(self, axis, delta, npoints):
        """ 2D (index space) extension. """
        imax, jmax = self.real_shape

        if axis == 'i':
            new_shape = (imax + npoints, jmax)
            indx = imax if delta > 0 else npoints
        else:
            new_shape = (imax, jmax + npoints)
            indx = jmax if delta > 0 else npoints

        flow = FlowSolution()
        for arr in self._arrays:
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
            flow.add_array(self.name_of_obj(arr), new_arr)

        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extend(axis, delta, npoints))

        flow.grid_location = self.grid_location
        flow.ghosts = copy.copy(self._ghosts)
        flow._copy_scalars(self)
        return flow

    def _extend_1d(self, delta, npoints):
        """ 1D (index space) extension. """
        imax, = self.real_shape
        new_shape = (imax + npoints,)
        indx = imax if delta > 0 else npoints

        flow = FlowSolution()
        for arr in self._arrays:
            new_arr = numpy.zeros(new_shape)
            if delta > 0:
                new_arr[0:indx] = arr
                for i in range(npoints):
                    new_arr[indx+i] = arr[-1]
            else:
                new_arr[indx:] = arr
                for i in range(npoints):
                    new_arr[i] = arr[0]
            flow.add_array(self.name_of_obj(arr), new_arr)

        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extend('i', delta, npoints))

        flow.grid_location = self.grid_location
        flow.ghosts = copy.copy(self._ghosts)
        flow._copy_scalars(self)
        return flow

    def name_of_obj(self, obj):
        """ Return name of object or None if not found. """
        for name, value in self.__dict__.items():
            if value is obj:
                return name
        return None

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        for vector in self._vectors:
            vector.flip_z()

    def make_cartesian(self, grid, axis='z'):
        """
        Convert to Cartesian coordinate system.

        grid: :class:`GridCoordinates`
            Must be in cylindrical form.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        for vector in self._vectors:
            vector.make_cartesian(grid, axis)

    def make_cylindrical(self, grid, axis='z'):
        """
        Convert to cylindrical coordinate system.

        grid: :class:`GridCoordinates`
            Must be in cylindrical form.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        for vector in self._vectors:
            vector.make_cylindrical(grid, axis)

    def rotate_about_x(self, deg):
        """
        Rotate about the X axis.

        deg: float (degrees)
            Amount of rotation.
        """
        for vector in self._vectors:
            vector.rotate_about_x(deg)

    def rotate_about_y(self, deg):
        """
        Rotate about the Y axis.

        deg: float (degrees)
            Amount of rotation.
        """
        for vector in self._vectors:
            vector.rotate_about_y(deg)

    def rotate_about_z(self, deg):
        """
        Rotate about the Z.

        deg: float (degrees)
            Amount of rotation.
        """
        for vector in self._vectors:
            vector.rotate_about_z(deg)

    def promote(self):
        """ Promote from N-dimensional to N+1 dimensional index space. """
        shape = self.real_shape
        if len(shape) > 2:
            raise RuntimeError('FlowSolution is 3D')

        elif len(shape) > 1:
            imax, jmax = shape
            for i, arr in enumerate(self._arrays):
                name = self.name_of_obj(arr)
                new_arr = numpy.zeros((imax, jmax, 1))
                new_arr[:, :, 0] = arr[:, :]
                setattr(self, name, new_arr)
                self._arrays[i] = new_arr
        elif len(shape) > 0:
            imax = shape[0]
            for i, arr in enumerate(self._arrays):
                name = self.name_of_obj(arr)
                new_arr = numpy.zeros((imax, 1))
                new_arr[:, 0] = arr[:]
                setattr(self, name, new_arr)
                self._arrays[i] = new_arr
        else:
            raise RuntimeError('FlowSolution is empty!')

        for vector in self._vectors:
            vector.promote()

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
                for i, arr in enumerate(self._arrays):
                    name = self.name_of_obj(arr)
                    new_arr = numpy.zeros((jmax, kmax))
                    new_arr[:, :] = arr[ghosts[0], :, :]
                    setattr(self, name, new_arr)
                    self._arrays[i] = new_arr
                self._ghosts = (ghosts[2], ghosts[3], ghosts[4], ghosts[5], 0, 0)
            elif jmx == 1:
                for i, arr in enumerate(self._arrays):
                    name = self.name_of_obj(arr)
                    new_arr = numpy.zeros((imax, kmax))
                    new_arr[:, :] = arr[:, ghosts[1], :]
                    setattr(self, name, new_arr)
                    self._arrays[i] = new_arr
                self._ghosts = (ghosts[0], ghosts[1], ghosts[4], ghosts[5], 0, 0)
            elif kmx == 1:
                for i, arr in enumerate(self._arrays):
                    name = self.name_of_obj(arr)
                    new_arr = numpy.zeros((imax, jmax))
                    new_arr[:, :] = arr[:, :, ghosts[2]]
                    setattr(self, name, new_arr)
                    self._arrays[i] = new_arr
                self._ghosts = (ghosts[0], ghosts[1], ghosts[2], ghosts[3], 0, 0)
            else:
                raise RuntimeError('No i, j, or k plane to collapse')

        elif len(shape) > 1:
            imax, jmax = shape
            imx = imax - (ghosts[0] + ghosts[1])
            jmx = jmax - (ghosts[2] + ghosts[3])
            if imx == 1:
                for i, arr in enumerate(self._arrays):
                    name = self.name_of_obj(arr)
                    new_arr = numpy.zeros((jmax,))
                    new_arr[:] = arr[ghosts[0], :]
                    setattr(self, name, new_arr)
                    self._arrays[i] = new_arr
                self._ghosts = (ghosts[2], ghosts[3], 0, 0, 0, 0)
            elif jmx == 1:
                for i, arr in enumerate(self._arrays):
                    name = self.name_of_obj(arr)
                    new_arr = numpy.zeros((imax,))
                    new_arr[:] = arr[:, ghosts[1]]
                    setattr(self, name, new_arr)
                    self._arrays[i] = new_arr
                self._ghosts = (ghosts[0], ghosts[1], 0, 0, 0, 0)
            else:
                raise RuntimeError('No i or j plane to collapse')

        elif len(shape) > 0:
            raise RuntimeError('FlowSolution is 1D')

        else:
            raise RuntimeError('FlowSolution is empty!')

        for vector in self._vectors:
            vector.demote()

