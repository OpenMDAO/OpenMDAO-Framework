
import unittest

from openmdao.main.api import VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, VarTree

class TstContainer(VariableTree):
    dummy1 = Float(1.0) 

class TstAssembly1(Assembly):
    dummy_data = VarTree(TstContainer(), iotype='in')
    dummy_data_out = VarTree(TstContainer(), iotype='out')

class TstAssembly2(Assembly):
    
    dummy_var1 = Float(1.0, iotype='in')

    def configure(self):
        self.add('assemb1', TstAssembly1())
        self.connect('dummy_var1', 'assemb1.dummy_data.dummy1')
        self.driver.workflow.add('assemb1')

class VarTreeConnectTestCase(unittest.TestCase):

    def test_vartree_connect(self):
        blah = set_as_top(TstAssembly2())
        blah.dummy_var1 = 5.0
        self.assertEqual(blah.assemb1.dummy_data.dummy1, 1.0)
        blah.run()
        self.assertEqual(blah.assemb1.dummy_data.dummy1, 5.0)
    
        
if __name__ == "__main__":
    unittest.main()


