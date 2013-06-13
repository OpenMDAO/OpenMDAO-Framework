
import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='inch')
    d = Float(iotype='out', units='inch')
    arr = Array([1.,2.,3.], iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        

def _nested_model():
    top = set_as_top(Assembly())
    top.add('sub', Assembly())
    top.add('comp7', Simple())
    top.add('comp8', Simple())
    sub = top.sub
    sub.add('comp1', Simple())
    sub.add('comp2', Simple())
    sub.add('comp3', Simple())
    sub.add('comp4', Simple())
    sub.add('comp5', Simple())
    sub.add('comp6', Simple())

    top.driver.workflow.add(['comp7', 'sub', 'comp8'])
    sub.driver.workflow.add(['comp1','comp2','comp3',
                             'comp4','comp5','comp6'])

    sub.create_passthrough('comp1.a', 'a1')
    sub.create_passthrough('comp4.c', 'c4')
    
    return top


class PseudoCompTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def test_basic_units(self):
        asm = Assembly()
        asm.add("comp1", Simple())
        asm.add("comp2", Simple())
        asm.connect("comp1.c", "comp2.a")
        self.assertEqual(asm._depgraph.list_connections(show_pseudo=True),
                         [])

    def test_connect1(self):
        "comp1.c+comp2.d"
        



    
