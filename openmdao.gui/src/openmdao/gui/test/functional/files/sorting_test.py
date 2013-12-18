from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float


class SortingComp(Component):
    """ Used to test alphanumeric sorting of inputs & outputs. """

    stress_i1  = Float(iotype='in')
    stress_i2  = Float(iotype='in')
    stress_i10 = Float(iotype='in')

    stress_o1  = Float(iotype='out')
    stress_o2  = Float(iotype='out')
    stress_o10 = Float(iotype='out')


