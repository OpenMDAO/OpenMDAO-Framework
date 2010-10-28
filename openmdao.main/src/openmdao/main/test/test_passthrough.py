# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.datatypes.api import Int
from openmdao.main.depgraph import dump_graph

class Simple(Component):
    
    a = Int(iotype='in')
    b = Int(iotype='in')
    c = Int(iotype='out')
    d = Int(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def getvals(self):
        return (self.a, self.b, self.c, self.d)

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

class PassthroughTestCase(unittest.TestCase):

    def setUp(self):
        pass
    
    def tearDown(self):
        self.asm = None
        
    def _setup_simple(self):
        """
        top
            c1
            a1
                c2
            c3
        """
        top = self.asm = set_as_top(Assembly())
        top.add('c1', Simple())
        a1 = top.add('a1', Assembly())
        a1.add('c2', Simple())
        top.add('c3', Simple())
            
        # iteration hierarchy
        top.driver.workflow.add([top.c1,top.a1,top.c3])
        a1.driver.workflow.add([a1.c2])
        
        top.connect('c1.c', 'a1.c2.b')
        top.connect('a1.c2.d', 'c3.a')
        
    def test_simple_passthrough(self):
        varnames = ['a','b','c','d']
        self._setup_simple()
        self.assertEqual(set(self.asm.c1.list_outputs()), set(['c','d']))
        self.assertTrue('c2.b' in self.asm.a1.list_inputs())
        self.asm.run()
        self.assertEqual(self.asm.c3.getvals(), (-2,2,0,-4))
        self.assertEqual([self.asm.c3._valid_dict[n] for n in varnames],
                         [True,True,True,True])
        self.assertEqual([self.asm.a1.c2._valid_dict[n] for n in varnames],
                         [True,True,True,True])
        self.assertEqual([self.asm.c1._valid_dict[n] for n in varnames],
                         [True,True,True,True])
        self.asm.c1.a = 6
        self.assertEqual([self.asm.c3._valid_dict[n] for n in varnames],
                         [False,True,False,False])
        self.assertEqual([self.asm.a1.c2._valid_dict[n] for n in varnames],
                         [True,False,False,False])
        self.asm.run()
        self.assertEqual(self.asm.c3.getvals(), (-7,2,-5,-9))
        
    def test_real_passthrough(self):
        self._setup_simple()
        self.asm.a1.create_passthrough('c2.a')
        self.asm.a1.create_passthrough('c2.c')
        self.asm.connect('c1.d', 'a1.a')
        self.asm.connect('a1.c', 'c3.b')
        self.asm.run()
        self.assertEqual(self.asm.c3.getvals(), (-4,2,-2,-6))
        
    def test_basics(self):
        c = Simple()
        c.connect('parent.c1.foo', 'a')
        self.assertEqual(['a'], c._depgraph.get_connected_inputs())
        self.assertEqual([], c._depgraph.get_connected_outputs())
        self.assertTrue('parent.c1.foo' in c._depgraph._graph['@exin']['@bin']['link']._srcs)
        c.connect('c', 'parent.c2.a')
        self.assertTrue('parent.c2.a' in c._depgraph._graph['@bout']['@exout']['link']._dests)
        self.assertEqual(['a'], c._depgraph.get_connected_inputs())
        self.assertEqual(['c'], c._depgraph.get_connected_outputs())
        
        
        
if __name__ == "__main__":
    unittest.main()


