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

    def add_array(self, name, array):
        """
        Add a :class:`numpy.ndarray` of scalar data and bind to `name`.
        Returns the added array.

        name: string
            Name for the added array.

        array: ndarray
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

        vector: Vector
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

        other: FlowSolution
            The flowfield to check against.

        logger: Logger or None
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
            if name is None:
                raise AttributeError('cannot find array!')
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
            if name is None:
                raise AttributeError('cannot find vector!')
            try:
                other_vector = getattr(other, name)
            except AttributeError:
                logger.debug("other is missing vector '%s'", name)
                return False
            if not vector.is_equivalent(other_vector, name, logger, tolerance):
                return False

# TODO: check scalars

        return True

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

        grid: GridCoordinates
            Must be in cylindrical form.

        axis: string
            Specifies which is the cylinder axis ('z' or 'x').
        """
        for vector in self._vectors:
            vector.make_cartesian(grid, axis)

    def make_cylindrical(self, grid, axis='z'):
        """
        Convert to cylindrical coordinate system.

        grid: GridCoordinates
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

