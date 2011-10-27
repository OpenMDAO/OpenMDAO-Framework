import unittest

from openmdao.main.api import Component, VariableTree, Assembly, set_as_top
from openmdao.lib.datatypes.api import Float, Int, Slot

class TstContainer(VariableTree):
    dummy1 = Float(1.0) 

class TstComponent(Component):
    dummy_data = Slot(TstContainer(),iotype='in')
    dummy_data_out = Slot(TstContainer(),iotype='out')
    dummyin = Float(iotype='in')

    def __init__(self): 
        super(TstComponent,self).__init__()
        self.add('dummy_data',TstContainer())
        self.add('dummy_data_out',TstContainer())
        
    def execute(self):
        self.dummy_data_out.dummy1 = self.dummy_data.dummy1

class TstAssembly(Assembly):

    def __init__(self):
        super(TstAssembly,self).__init__()
        self.add('comp',TstComponent())
        self.create_passthrough('comp.dummy_data.dummy1')
        self.create_passthrough('comp.dummy_data_out.dummy1', 'dummy1_out')
        self.driver.workflow.add('comp')

class VarTreePassthroughTestCase(unittest.TestCase):

    def test_vartree_passthrough(self):
        blah = set_as_top(TstAssembly())
        blah.dummy1 = 5.0
        self.assertEqual(blah.dummy1_out, 1.0)
        blah.run()
        self.assertEqual(blah.comp.dummy_data.dummy1, 5.0)
        self.assertEqual(blah.dummy1, 5.0)
        self.assertEqual(blah.dummy1_out, 5.0)
    
        
if __name__ == "__main__":
    unittest.main()



