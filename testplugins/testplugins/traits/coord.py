
from enthought.traits.api import TraitType

class CoordinatesTrait(TraitType):

    def __init__(self, default_value = (0.,0.,0.), **metadata):
        super(Coordinates, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        if isinstance(value, tuple) and len(value) == 3 and \
           all([isinstance(val,float) or isinstance(val,int) for val in value]):
            return value
        else:
            self.error(object, name, value)
    
