
from openmdao.main.variable import Variable
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IUncertainVariable

class UncertainDistVar(Variable):
    """A variable wrapper for an UncertainDistribution variable.
       """

    def __init__(self, default_value=None, iotype=None, desc=None, **metadata):

        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype

        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc

        super(UncertainDistVar, self).__init__(default_value=default_value, **metadata)

    def validate(self, obj, name, value):
        """ Use the Enthought trait's validate.
        """
        if not has_interface(value, IUncertainVariable):
            raise ValueError("'%s' does not implement the IUncertainVariable interface" %
                             name)
        return value
