
from enthought.traits.api import TraitType
from enthought.traits.trait_handlers import NoDefaultSpecified


class Variable(TraitType):
    """An OpenMDAO specific trait type that serves as a common base
    class for framework visible inputs and outputs.
    """
    def __init__ ( self, default_value = NoDefaultSpecified, **metadata ):
        if 'vartypename' not in metadata:
            metadata['vartypename'] = self.__class__.__name__
        super(Variable, self).__init__(default_value=default_value, **metadata)



