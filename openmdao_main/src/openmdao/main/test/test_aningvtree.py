import unittest

from openmdao.main.api import VariableTree, Component, Assembly
from openmdao.main.datatypes.api import Float, VarTree


class VT(VariableTree):
    x = Float()


class C(Component):

    x = Float(iotype='in')
    out = Float(iotype='out')

    def execute(self):
        self.out = 2*self.x

class A(Assembly):

    vt = VarTree(VT(), iotype='in')

    def configure(self):
        self.add('c', C())
        self.driver.workflow.add(['c'])
        self.connect('vt.x', 'c.x')
        self.create_passthrough('c.out')

class TestCase(unittest.TestCase):
    def test_vtree(self):
        a = A()
        a.vt.x = 1.0
    
        a.run()
    
        self.assertEqual(a.out, 2.0)
        
if __name__ == '__main__':
    unittest.main()
