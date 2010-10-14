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
        """
        top
            comp1
            nested
                comp1
                nested
                    comp1
            comp2
        """
        top = self.asm = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.add('comp2', Simple())
        nested = top.add('nested', Assembly())
        nested.add('comp1', Simple())
        nested2 = nested.add('nested', Assembly())
        nested2.add('comp1', Simple())
            
        # iteration hierarchy
        top.driver.workflow.add([top.comp1,top.nested,top.comp2])
        nested.driver.workflow.add([nested.comp1, nested.nested])
        nested2.driver.workflow.add([nested2.comp1])
        
    def tearDown(self):
        self.asm = None
        
    def test_nested_connect(self):
        self.asm.connect('comp1.c', 'nested.nested.comp1.a')
        self.asm.connect('nested.comp1.c', 'comp2.a')
        self.asm.nested.connect('nested.comp1.c', 'comp1.b')
        self.asm.run()
        
        
if __name__ == "__main__":
    unittest.main()


