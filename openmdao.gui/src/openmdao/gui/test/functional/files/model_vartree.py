""" This model is used by test_valueeditors to test expand/collapse on
the object and parameter panes.
"""

from openmdao.main.api import Component, Assembly, VariableTree
from openmdao.lib.datatypes.api import Float, Slot

class DumbVT3(VariableTree):
    def __init__(self):
        super(DumbVT3, self).__init__()
        self.add('a', Float(1., units='ft'))
        self.add('b', Float(12., units='inch'))


class DumbVT2(VariableTree):
    vt3 = Slot(DumbVT3, iotype='in')
    def __init__(self):
        super(DumbVT2, self).__init__()
        self.add('x', Float(-1.))
        self.add('y', Float(-2.))
        self.add('vt3', DumbVT3())


class DumbVT(VariableTree):
    vt2 = Slot(DumbVT2, iotype='in')
    def __init__(self):
        super(DumbVT, self).__init__()
        self.add('vt2', DumbVT2())
        self.add('v1', Float(1., desc='vv1'))
        self.add('v2', Float(2., desc='vv2'))


class SimpleComp(Component):
    cont_in = Slot(DumbVT, iotype='in')
    cont_out = Slot(DumbVT, iotype='out')

    def __init__(self):
        super(SimpleComp, self).__init__()
        self.add('cont_in', DumbVT())
        self.add('cont_out', DumbVT())

    
class Topp(Assembly):

    def configure(self):
        self.add('p1', SimpleComp())