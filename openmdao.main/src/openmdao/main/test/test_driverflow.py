"""
Test of drivers in a workflow.

test_multidriver.py also tests various permutations of this.
"""

import unittest

from openmdao.main.api import Assembly, Component, Driver, Expression
from openmdao.lib.api import Float

class Dumbcomp(Component):
    x = Float(0., iotype='in')
    y = Float(0., iotype='out')
    
    def execute(self):
        self.y = self.x

class Dumbdriver(Driver):
    objective = Expression(iotype='in')
    design = Expression(iotype='out')
    val = Float(0., iotype='out')
    
    def continue_iteration(self):
        return False
    
    def pre_iteration(self):
        self.design.set(self.val)
    
    def post_iteration(self):
        self.val = self.objective.evaluate()

class DriverflowTestCase(unittest.TestCase):

    def test_comp_in_driverflow(self):
        asm = Assembly()
        try:
            asm.add_container('drv', Dumbcomp(), workflow='driverflow')
        except TypeError, err:
            self.assertTrue(str(err).startswith(": Workflow.add validation failed for type "))
        else:
            self.fail('expected TypeError')
        
    def test_drv_in_workflow(self):
        asm = Assembly()
        try:
            asm.add_container('comp', Dumbdriver(), workflow='workflow')
        except TypeError as err:
            self.assertTrue(str(err).startswith(": Dataflow.add validation failed for type "))
        else:
            self.fail('expected TypeError')
        
    def test_bad_workflow_name(self):
        asm = Assembly()
        try:
            asm.add_container('comp', Dumbcomp(), workflow='bogus')
        except NameError, err:
            self.assertEqual(str(err), ": 'bogus' is not a known workflow")
        else:
            self.fail('expected NameError')
        

if __name__ == '__main__':
    unittest.main()

