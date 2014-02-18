
import unittest

from openmdao.main.api import VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, VarTree

class TstVtree(VariableTree):
    floatval = Float(1.0) 

class VTAssembly(Assembly):
    vtree_in = VarTree(TstVtree(), iotype='in')
    vtree_out = VarTree(TstVtree(), iotype='out')

class AsmWithSub(Assembly):
    
    floatvar = Float(1.0, iotype='in')

    def configure(self):
        self.add('subasm', VTAssembly())
        self.connect('floatvar', 'subasm.vtree_in.floatval')
        self.driver.workflow.add('subasm')

class VarTreeConnectTestCase(unittest.TestCase):

    def test_vartree_connect(self):
        top = set_as_top(AsmWithSub())
        top.floatvar = 5.0
        self.assertEqual(top.subasm.vtree_in.floatval, 1.0)
        top.run()
        self.assertEqual(top.subasm.vtree_in.floatval, 5.0)
    
        
if __name__ == "__main__":
    unittest.main()


