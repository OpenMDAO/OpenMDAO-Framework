""" This model is used by test_valueeditors to test expand/collapse on
the object and parameter panes.
"""

from openmdao.main.api import Component, Assembly, VariableTree
from openmdao.lib.datatypes.api import Float, VarTree

class DumbVT3(VariableTree):
    a = Float(1., units='ft')
    b = Float(12., units='inch')

class DumbVT2(VariableTree):
    x = Float(-1.)
    y = Float(-2.)
    vt3 = VarTree(DumbVT3())

class DumbVT(VariableTree):
    v1 = Float(1., desc='vv1')
    v2 = Float(2., desc='vv2')
    vt2 = VarTree(DumbVT2(), iotype='in')

class SimpleComp(Component):
    cont_in = VarTree(DumbVT(), iotype='in')
    cont_out = VarTree(DumbVT(), iotype='out')


class Topp(Assembly):

    def configure(self):
        self.add('p1', SimpleComp())
