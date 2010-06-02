from openmdao.lib.traits.domain.coords import GridCoordinates


class Zone(object):
    """ One zone in a possibly multi-zone :class:`DomainObj`. """

    def __init__(self):
        self.coords = GridCoordinates()
        self.arrays = []
        self.vectors = []
        self.reference_state = None
        self.symmetry = None
        self.symmetry_axis = None
        self.symmetry_instances = 1

    @property
    def shape(self):
        """ Returns tuple of index limits. """
        return self.coords.shape

    @property
    def extent(self):
        """ Returns tuple of coordinate ranges. """
        return self.coords.extent

    def add_array(self, name, array):
        """ Add an array bound to `name`. Returns the added array. """
        if hasattr(self, name):
            raise ValueError("name '%s' is already bound" % name)
        setattr(self, name, array)
        self.arrays.append(array)
        return array

    def add_vector(self, name, vector):
        """ Add a :class:`Vector` bound to `name`. Returns the added vector. """
        if hasattr(self, name):
            raise ValueError("name '%s' is already bound" % name)
        setattr(self, name, vector)
        self.vectors.append(vector)
        return vector

    def is_equivalent(self, other, logger):
        """ Test if self and `other` are equivalent. """
        if not isinstance(other, Zone):
            logger.debug('other is not a Zone object.')
            return False

        if not self.coords.is_equivalent(other.coords, logger):
            return False

# TODO: check scalars.

        for arr in self.arrays:
            name = self.name_of_obj(arr)
            if name is None:
                raise AttributeError('cannot find array!')
            try:
                other_arr = getattr(other, name)
            except AttributeError:
                logger.debug("other is missing array '%s'", name)
                return False
            if (other_arr != arr).any():
                logger.debug('%s values are not equal.', name)
                return False

        for vector in self.vectors:
            name = self.name_of_obj(vector)
            if name is None:
                raise AttributeError('cannot find vector!')
            try:
                other_vector = getattr(other, name)
            except AttributeError:
                logger.debug("other is missing vector '%s'", name)
                return False
            if not vector.is_equivalent(other_vector, name, logger):
                return False

        return True

    def name_of_obj(self, obj):
        """ Return name of object. """
        for name, value in self.__dict__.items():
            if value is obj:
                return name
        return None

    def flip_z(self):
        """ Convert to other-handed coordinate system. """
        self.coords.flip_z()
        for vector in self.vectors:
            vector.flip_z()

    def make_cartesian(self):
        """ Convert to cartesian coordinate system. """
        self.coords.make_cartesian()
        for vector in self.vectors:
            vector.make_cartesian()

    def make_cylindrical(self):
        """ Convert to cylindrical coordinate system. """
        self.coords.make_cylindrical()
        for vector in self.vectors:
            vector.make_cylindrical()

    def translate(self, delta_x, delta_y, delta_z):
        """ Translate coordinates. """
        self.coords.translate(delta_x, delta_y, delta_z)

    def rotate_about_x(self, deg):
        """ Rotate about the X axis by `deg` degrees. """
        self.coords.rotate_about_x(deg)
        for vector in self.vectors:
            vector.rotate_about_x(deg)

    def rotate_about_y(self, deg):
        """ Rotate about the Y axis by `deg` degrees. """
        self.coords.rotate_about_y(deg)
        for vector in self.vectors:
            vector.rotate_about_y(deg)

    def rotate_about_z(self, deg):
        """ Rotate about the Z axis by `deg` degrees. """
        self.coords.rotate_about_z(deg)
        for vector in self.vectors:
            vector.rotate_about_z(deg)

