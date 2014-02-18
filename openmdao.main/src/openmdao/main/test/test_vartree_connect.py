
import unittest

from openmdao.main.api import VariableTree, Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, VarTree

class TstVtree(VariableTree):
    floatval = Float(1.0)
    floatval2 = Float(2.0)

class VTAssembly(Assembly):
    vtree_in = VarTree(TstVtree(), iotype='in')
    vtree_out = VarTree(TstVtree(), iotype='out')

class VTComp(Component):
    vtree_in = VarTree(TstVtree(), iotype='in')
    vtree_out = VarTree(TstVtree(), iotype='out')

    def execute(self):
        self.vtree_out.floatval = self.vtree_in.floatval
        self.vtree_out.floatval2 = self.vtree_in.floatval2

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

    def test_vartree_set_subvar_after_connecting_other_subvar(self):
        top = set_as_top(Assembly())
        top.add("comp1", VTComp())
        top.add("comp2", VTComp())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.vtree_out.floatval', 'comp2.vtree_in.floatval')
        top.comp1.vtree_in.floatval = 7.7
        top.comp2.vtree_in.floatval2 = -2.2
        top.run()
        self.assertEqual(7.7, top.comp2.vtree_out.floatval)
        self.assertEqual(-2.2, top.comp2.vtree_out.floatval2)
    
        
if __name__ == "__main__":
    unittest.main()


