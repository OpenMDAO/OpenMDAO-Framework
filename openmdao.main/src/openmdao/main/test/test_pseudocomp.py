
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
    # just hierarchy, no connections
    top = set_as_top(Assembly())
    top.add('comp1', Simple())
    top.add('asm', Assembly())
    top.add('comp2', Simple())
    asm = top.asm
    asm.add('comp1', Simple())
    asm.add('comp2', Simple())
    asm.add('comp3', Simple())

    top.driver.workflow.add(['comp1', 'asm', 'comp2'])
    asm.driver.workflow.add(['comp1','comp2','comp3'])

    asm.create_passthrough('comp1.a', 'a1')
    asm.create_passthrough('comp3.c', 'c3')
    
    return top


class PseudoCompTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def test_basic_units(self):
        top = _nested_model()
        top.asm.connect("comp1.c", "comp2.a")
        self.assertTrue('#1' in top.asm._depgraph)
        # self.assertEqual(asm._depgraph.list_connections(show_pseudo=True),
        #                  [])

    # connect('comp1.c*comp2.d', 'comp3.a')
    # disconnect() for a var in an expr
    # disconnect() for an exact expr (src and dest)
    # connect('a1+comp2.c', 'comp3.b')
    # disconnect() for a boundary var in an expr  
    # invlidation through a pseudocomp
    # listing connections with pseudocomps
    # listing connections without pseudocomps (will have multiple srcs connected to one dest)
    # expr with array index ref
       



    
