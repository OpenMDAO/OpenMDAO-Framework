
import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='inch')
    d = Float(iotype='out', units='inch')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

class SimpleNoUnits(Component):
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    arr = Array([1.,2.,3.], iotype='out')
    
    def __init__(self):
        super(SimpleNoUnits, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        

def _nested_model(units=True):
    if units:
        klass = Simple
    else:
        klass = SimpleNoUnits

    # just hierarchy, no connections
    top = set_as_top(Assembly())
    top.add('comp1', klass())
    top.add('asm', Assembly())
    top.add('comp2', klass())
    asm = top.asm
    asm.add('comp1', klass())
    asm.add('comp2', klass())
    asm.add('comp3', klass())

    top.driver.workflow.add(['comp1', 'asm', 'comp2'])
    asm.driver.workflow.add(['comp1','comp2','comp3'])

    asm.create_passthrough('comp1.a', 'a1')
    asm.create_passthrough('comp3.c', 'c3')
    
    return top

def _simple_model(units=True):
    if units:
        klass = Simple
    else:
        klass = SimpleNoUnits
    top = set_as_top(Assembly())
    top.add("comp1", klass())
    top.add("comp2", klass())
    top.driver.workflow.add(['comp1','comp2'])
    top.connect("comp1.c", "comp2.a")
    return top

class PseudoCompTestCase(unittest.TestCase):

    def setUp(self):
        self.fakes = ['@bin','@bout','@xin','@xout']

    def test_basic_nounits(self):
        top = _simple_model(units=False)
        self.assertEqual(set(top._depgraph._graph.nodes()),
                         set(['comp1','comp2','driver']+self.fakes))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('comp1.c', 'comp2.a')]))

    def test_basic_units(self):
        top = _simple_model()
        self.assertEqual(set(top._depgraph._graph.nodes()),
                         set(['comp1','comp2','_0','driver']+self.fakes))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_0.out0', 'comp2.a'), 
                              ('comp1.c', '_0.in0')]))
        self.assertEqual(top._0._eqn, 'out0 = in0')
        top.comp1.a = 7.
        top.comp1.b = 3.
        top.run()
        self.assertEqual(top.comp1.c, 10.)
        
        
    def test_multi_src(self):
        top = _simple_model()
        top.add("comp3", Simple())
        top.driver.workflow.add('comp3')
        top.connect("comp1.d+comp3.d", "comp2.b")

        self.assertEqual(set(top._depgraph._graph.nodes()),
                         set(['comp1','comp2','comp3', 
                              '_0', '_1', 'driver']+self.fakes))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('comp3.d', '_1.in0'), ('_1.out0', 'comp2.b'), 
                              ('_0.out0', 'comp2.a'), ('comp1.d', '_1.in1'), 
                              ('comp1.c', '_0.in0')]))

    # connect('comp1.c*comp2.d', 'comp3.a')
    # disconnect() for a var in an expr
    # disconnect() for an exact expr (src and dest)
    # connect('a1+comp2.c', 'comp3.b')
    # disconnect() for a boundary var in an expr  
    # invlidation through a pseudocomp
    # listing connections with pseudocomps
    # listing connections without pseudocomps (will have multiple srcs connected to one dest)
    # expr with array index ref
       


if __name__ == '__main__':
    unittest.main()

    
