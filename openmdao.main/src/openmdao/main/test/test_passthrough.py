# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.api import Int

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
        self._setup_simple()
        self.assertEqual(set(self.asm.c1.list_outputs()), set(['c','d']))
        self.assertTrue('c2.b' in self.asm.a1.list_inputs())
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()


