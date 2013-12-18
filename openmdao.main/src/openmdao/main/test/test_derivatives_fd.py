"""
Specific unit testing for finite difference.
"""

import unittest

import numpy as np

from openmdao.main.api import Component, VariableTree, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.util.testutil import assert_rel_error

class MyComp(Component):
    
    x1 = Float(1.0, iotype='in')
    x2 = Float(1.0, iotype='in', fd_step = .1)
    
    y = Float(3.3, iotype='out')
    
    def execute(self):
        ''' Simple eq '''
        
        self.y = 2.0*self.x1*self.x1 + 2.0*self.x2*self.x2
    

class MyCompDerivs(Component):
    
    x1 = Float(1.0, iotype='in')
    x2 = Float(1.0, iotype='in')
    
    y = Float(3.3, iotype='out')
    
    def execute(self):
        ''' Simple eq '''
        
        self.y = 2.0*self.x1*self.x1 + 2.0*self.x2*self.x2
    
    def linearize(self):
        ''' Simple eq '''
        
        self.J = np.array([[4.0*self.x1, 4.0*self.x2]])
        
    def provideJ(self):
        
        input_keys = ('x1', 'x2')
        output_keys = ('y', )
        return input_keys, output_keys, self.J
        

class TestFiniteDifference(unittest.TestCase): 

    def test_fd_step(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])
        
        model.x1 = 1.0
        model.x2 = 1.0
        model.run()
        
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        
        assert_rel_error(self, J[0, 0], 4.0, 0.0001)
        assert_rel_error(self, J[0, 1], 4.2, 0.0001)
        
    def test_force_fd(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyCompDerivs())
        model.driver.workflow.add(['comp'])
        
        model.x1 = 1.0
        model.x2 = 1.0
        model.run()
        
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count, 1)
        self.assertEqual(model.comp.derivative_exec_count, 1)
        
        model.comp.force_fd = True
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count, 3)
        self.assertEqual(model.comp.derivative_exec_count, 1)
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()