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
    x3 = Float(1.0, iotype='in', fd_step = .1, fd_form='central')
    x4 = Float(0.001, iotype='in', fd_step = .1, fd_step_type='relative')
    
    y = Float(3.3, iotype='out')
    
    def execute(self):
        ''' Simple eq '''
        
        self.y = 2.0*self.x1*self.x1 + 2.0*self.x2*self.x2 + \
                 2.0*self.x3*self.x3 + 2.0*self.x4*self.x4

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
        
        model.run()
        
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2', 'comp.x3', 'comp.x4'],
                                                outputs=['comp.y'])
        
        assert_rel_error(self, J[0, 0], 4.0, 0.0001)
        assert_rel_error(self, J[0, 1], 4.2, 0.0001)
        assert_rel_error(self, J[0, 2], 4.0, 0.0001)
        assert_rel_error(self, J[0, 3], 0.0042, 0.0001)
        
    def test_central(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])
        model.driver.gradient_options.fd_form = 'central'
        
        model.run()
        
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        
        assert_rel_error(self, J[0, 0], 4.0, 0.0001)
        # Central gets this right even with a bad step
        assert_rel_error(self, J[0, 1], 4.0, 0.0001)
        
    def test_fd_step_type(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])
        model.comp.x1 = 1e12
        model.run()
        
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1'],
                                                outputs=['comp.y'])
        
        assert_rel_error(self, J[0, 0], 0.0, 0.0001)
        
        model.driver.gradient_options.fd_step_type = 'relative'
        model.run()
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1'],
                                                outputs=['comp.y'])
        
        assert_rel_error(self, J[0, 0], 4.0e12, 0.0001)
        
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
        
        # Component-wise force FD
        model.comp.force_fd = True
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count, 3)
        self.assertEqual(model.comp.derivative_exec_count, 1)
        
        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'])
        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'], fd_form='central')
        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'], fd_step_type='relative')
        
        # Full model force FD
        model.comp.force_fd = False
        model.driver.gradient_options.force_fd = True
        old_count = model.comp.exec_count
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                                outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count - old_count, 2)
        self.assertEqual(model.comp.derivative_exec_count, 1)
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()