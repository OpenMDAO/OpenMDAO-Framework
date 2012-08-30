import unittest

from openmdao.main.api import Component, VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Int, Slot

class TstContainer(VariableTree):
    dummy1 = Float(1.0) 

class TstComponent(Component):
    dummy_data = Slot(TstContainer, iotype='in')
    dummy_data_out = Slot(TstContainer, iotype='out')
    dummyin = Float(iotype='in')

    def __init__(self): 
        super(TstComponent,self).__init__()
        self.add('dummy_data',TstContainer())
        self.add('dummy_data_out',TstContainer())
        
    def execute(self):
        self.dummy_data_out = self.dummy_data.copy()

class TstAssembly(Assembly):

    def configure(self):
        self.add('comp',TstComponent())
        self.create_passthrough('comp.dummy_data.dummy1')
        self.create_passthrough('comp.dummy_data_out.dummy1', 'dummy1_out')
        self.driver.workflow.add('comp')

class TstAssembly2(Assembly):

    def configure(self):
        self.add('comp',TstComponent())
        self.create_passthrough('comp.dummy_data')
        self.create_passthrough('comp.dummy_data_out', 'dummy1_out')
        self.driver.workflow.add('comp')

class VarTreePassthroughTestCase(unittest.TestCase):

    def test_vartree_passthrough(self):
        
        # Tests that we can create a passthrough of one variable in a tree
        blah = set_as_top(TstAssembly())
        blah.dummy1 = 5.0
        self.assertEqual(blah.dummy1_out, 1.0)
        blah.run()
        self.assertEqual(blah.comp.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy1, 5.0)
        self.assertEqual(blah.dummy1_out, 5.0)
    
    def test_vartree_passthrough(self):
        
        # Tests that we can create a passthrough of an entire variable tree
        blah = set_as_top(TstAssembly2())
        blah.dummy_data.dummy1 = 5.0
        self.assertEqual(blah.dummy1_out.dummy1, 1.0)
        blah.run()
        self.assertEqual(blah.comp.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy1_out.dummy1, 5.0)
    
        
if __name__ == "__main__":
    unittest.main()



