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
        self._ghosts = [0, 0, 0, 0, 0, 0]
        self._arrays = []
        self._vectors = []

    def _get_grid_location(self):
        return self._grid_location

    def _set_grid_location(self, loc):
        if loc not in _GRID_LOCATIONS:
            raise ValueError("'%s' is not a valid grid location" % loc)
        self._grid_location = loc

    grid_location = property(_get_grid_location, _set_grid_location,
                             doc='Position at which data is located.')
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
        if self._arrays:
            ijk = self._arrays[0].shape
        elif self._vectors:
            ijk = self._vectors[0].shape
        else:
            ijk = ()
        if len(ijk) < 1:
            return ijk
        ghosts = self._ghosts
        imax = ijk[0] - (ghosts[0] + ghosts[1])
        if len(ijk) < 2:
            return (imax,)
        jmax = ijk[1] - (ghosts[2] + ghosts[3])
        if len(ijk) < 3:
            return (imax, jmax)
        kmax = ijk[2] - (ghosts[4] + ghosts[5])
        return (imax, jmax, kmax)

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
            raise ValueError("name '%s' is already bound" % name)
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
            raise ValueError("name '%s' is already bound" % name)
        setattr(self, name, vector)
        self._vectors.append(vector)
        return vector

    def is_equivalent(self, other, logger, tolerance=0.):
        """
        Test if self and `other` are equivalent.

        other: :class:`FlowSolution`
            The flowfield to check against.

        logger: :class:`Logger` or None
            Used to log debug messages that will indicate what if anything is
            not equivalent.

        tolerance: float
            The maximum relative difference in array values to be considered
            equivalent.
        """
        if not isinstance(other, FlowSolution):
            logger.debug('other is not a FlowSolution object.')
            return False

        if self.grid_location != other.grid_location:
            logger.debug('grid locations are not equal.')
            return False

        if self.ghosts != other.ghosts:
            logger.debug('ghost cell counts are not equal.')
            return False

        for arr in self._arrays:
            name = self.name_of_obj(arr)
            try:
                other_arr = getattr(other, name)
            except AttributeError:
                logger.debug("other is missing array '%s'", name)
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
                logger.debug("other is missing vector '%s'", name)
                return False
            if not vector.is_equivalent(other_vector, name, logger, tolerance):
                return False

# TODO: check scalars

        return True

    def extract(self, imin, imax, jmin=None, jmax=None, kmin=None, kmax=None):
        """
        Construct a new :class:`FlowSolution` from data extracted from the
        specified region. Ghost data is copied.

        imin, imax, jmin, jmax, kmin, kmax: int
            Specifies the region to extract neglecting ghost/rind planes.
            Negative values are relative to the size in that dimension,
            so -1 refers to the last element. For 2D zones omit kmin and kmax.
        """
        i = len(self.shape)
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
        if self._arrays:
            flow_imax, flow_jmax, flow_kmax = self._arrays[0].shape
        elif self._vectors:
            flow_imax, flow_jmax, flow_kmax = self._vectors[0].shape
        else:
            flow_imax, flow_jmax, flow_kmax = (0, 0, 0)
        if imin < 0:
            imin += flow_imax
        if imax < 0:
            imax += flow_imax
        if jmin < 0:
            jmin += flow_jmax
        if jmax < 0:
            jmax += flow_jmax
        if kmin < 0:
            kmin += flow_kmax
        if kmax < 0:
            kmax += flow_kmax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]
        imax += ghosts[0]
        jmin += ghosts[2]
        jmax += ghosts[2]
        kmin += ghosts[4]
        kmax += ghosts[4]

        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr),
                           arr[imin:imax+1, jmin:jmax+1, kmin:kmax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extract(imin, imax, jmin, jmax, kmin, kmax))
        flow.grid_location = self.grid_location
        flow.ghosts = ghosts
        return flow

    def _extract_2d(self, imin, imax, jmin, jmax):
        """ 2D (index space) extraction. """
        # Support end-relative indexing.
        if self._arrays:
            flow_imax, flow_jmax = self._arrays[0].shape
        elif self._vectors:
            flow_imax, flow_jmax = self._vectors[0].shape
        else:
            flow_imax, flow_jmax = (0, 0)
        if imin < 0:
            imin += flow_imax
        if imax < 0:
            imax += flow_imax
        if jmin < 0:
            jmin += flow_jmax
        if jmax < 0:
            jmax += flow_jmax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]
        imax += ghosts[0]
        jmin += ghosts[2]
        jmax += ghosts[2]

        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr),
                           arr[imin:imax+1, jmin:jmax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector),
                            vector.extract(imin, imax, jmin, jmax))
        flow.grid_location = self.grid_location
        flow.ghosts = ghosts
        return flow

    def _extract_1d(self, imin, imax):
        """ 1D (index space) extraction. """
        # Support end-relative indexing.
        if self._arrays:
            flow_imax = self._arrays[0].shape[0]
        elif self._vectors:
            flow_imax = self._vectors[0].shape[0]
        else:
            flow_imax = 0
        if imin < 0:
            imin += flow_imax
        if imax < 0:
            imax += flow_imax

        # Adjust for ghost/rind planes.
        ghosts = self.ghosts
        imin += ghosts[0]
        imax += ghosts[0]

        flow = FlowSolution()
        for arr in self._arrays:
            flow.add_array(self.name_of_obj(arr), arr[imin:imax+1])
        for vector in self._vectors:
            flow.add_vector(self.name_of_obj(vector), vector.extract(imin, imax))
        flow.grid_location = self.grid_location
        flow.ghosts = ghosts
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

