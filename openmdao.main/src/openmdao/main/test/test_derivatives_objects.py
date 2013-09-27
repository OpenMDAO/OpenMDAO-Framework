"""
Testing differentiation of user-defined datatypes.
"""

import unittest

import numpy as np

from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.lib.geometry.box import BoxParametricGeometry
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.interfaces import IParametricGeometry, implements, \
                                     IStaticGeometry
from openmdao.main.variable import Variable
from openmdao.util.testutil import assert_rel_error


class DataObject(object):
    
    def __init__(self):
        self.x = 0
        self.y = 0
        self.z = 0
        
    def set(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        
    def get(self):
        return self.x, self.y, self.z

class Comp_Send(Component):
    '''Passes a data object as output.'''
    
    p1 = Float(0.0, iotype='in')
    p2 = Float(0.0, iotype='in')
    
    data = Variable(DataObject(), iotype='out', data_shape=(3, ))
    dummy = Float(1.0, iotype='out')
    
    def execute(self):
        ''' Load computation result into self.data.'''
        
        p1 = self.p1
        p2 = self.p2
        
        x = p1*p1 + p2
        y = p2*p2 - p1
        z = 2.0*p1 + 3.0*p2
        
        self.data.set(x, y, z)
        
    def linearize(self):
        ''' Jacobian'''
        
        dxdp1 = 2.0*self.p1
        dxdp2 = 1.0
        dydp1 = -1.0
        dydp2 = 2.0*self.p2
        dzdp1 = 2.0
        dzdp2 = 3.0
        
        self.J = np.array([[dxdp1, dxdp2], [dydp1, dydp2], [dzdp1, dzdp2]])
        
    def apply_deriv(self, arg, result):
        
        if 'data' in result:
            
            result['data'] += self.J[:, 0]*arg['p1']
            result['data'] += self.J[:, 1]*arg['p2']
    
    def apply_derivT(self, arg, result):
        
        if 'data' in arg:
            
            result['p1'] += self.J.T[0, :].dot(arg['data'])
            result['p2'] += self.J.T[1, :].dot(arg['data'])
    
class Comp_Receive(Component):
    '''Takes a data object as input.'''
    
    data = Variable(iotype='in')
    
    q1 = Float(0.0, iotype='out')
    q2 = Float(0.0, iotype='out')
    q3 = Float(0.0, iotype='out')
    
    dummy = Float(0.0, iotype='in')
    
    def execute(self):
        
        x, y, z = self.data.get()
        
        self.q1 = -1.0*x
        self.q2 = 2.0*y
        self.q3 = 3.0*z
    
    def linearize(self):
        ''' Jacobian'''
        
        self.J = np.array([[-1.0, 0.0, 0.0], 
                           [0.0, 2.0, 0.0], 
                           [0.0, 0.0, 3.0]])
        
    def apply_deriv(self, arg, result):
        
        if 'data' in arg:
            
            result['q1'] += self.J[:, 0].dot(arg['data'])
            result['q2'] += self.J[:, 1].dot(arg['data'])
            result['q3'] += self.J[:, 2].dot(arg['data'])
    
    def apply_derivT(self, arg, result):
        
        if 'data' in result:
            
            result['data'] += self.J.T[0, :]*arg['q1']
            result['data'] += self.J.T[1, :]*arg['q2']
            result['data'] += self.J.T[2, :]*arg['q3']
    
class Testcase_deriv_obj(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass
    
    def test_provideJ(self):    
        
        top = set_as_top(Assembly())
        top.add('c1', Comp_Send())
        top.add('c2', Comp_Receive())
        top.connect('c1.data', 'c2.data')
        #top.connect('c1.dummy', 'c2.dummy')
        top.driver.workflow.add(['c1', 'c2'])
        
        top.c1.p1 = 3.0
        top.c1.p2 = 5.0
        top.run()
        
        inputs = ['c1.p1', 'c1.p2']
        outputs = ['c2.q1', 'c2.q2', 'c2.q3']
        J = top.driver.workflow.calc_gradient(inputs, outputs, fd=True)
        
        assert_rel_error(self, J[0, 0], -6.0, .00001)
        assert_rel_error(self, J[0, 1], -1.0, .00001)
        assert_rel_error(self, J[1, 0], -2.0, .00001)
        assert_rel_error(self, J[1, 1], 20.0, .00001)
        assert_rel_error(self, J[2, 0], 6.0, .00001)
        assert_rel_error(self, J[2, 1], 9.0, .00001)
        
        top.driver.workflow.config_changed()
        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='forward')
        
        assert_rel_error(self, J[0, 0], -6.0, .00001)
        assert_rel_error(self, J[0, 1], -1.0, .00001)
        assert_rel_error(self, J[1, 0], -2.0, .00001)
        assert_rel_error(self, J[1, 1], 20.0, .00001)
        assert_rel_error(self, J[2, 0], 6.0, .00001)
        assert_rel_error(self, J[2, 1], 9.0, .00001)
        
        top.driver.workflow.config_changed()
        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='adjoint')
        
        assert_rel_error(self, J[0, 0], -6.0, .00001)
        assert_rel_error(self, J[0, 1], -1.0, .00001)
        assert_rel_error(self, J[1, 0], -2.0, .00001)
        assert_rel_error(self, J[1, 1], 20.0, .00001)
        assert_rel_error(self, J[2, 0], 6.0, .00001)
        assert_rel_error(self, J[2, 1], 9.0, .00001)

class GeoWithDerivatives(BoxParametricGeometry): 
    '''Adds derivative functions to the famous box geometry.'''

    implements(IParametricGeometry, IStaticGeometry)

    def linearize(self):
        pass
    
    def apply_deriv(self, arg, result):
        pass
    
    def apply_derivT(self, arg, result):
        pass
    
    def provideJ(self):
        pass
        
class Testcase_geom_deriv(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass
    
    def test_basic_delegation(self):
        
        top = Assembly()
        top.add('geo', GeomComponent())
        
        # Function not there before we slot
        self.assertTrue(not hasattr(top.geo, 'linearize'))
        self.assertTrue(not hasattr(top.geo, 'apply_deriv'))
        self.assertTrue(not hasattr(top.geo, 'apply_derivT'))
        self.assertTrue(not hasattr(top.geo, 'provideJ'))
        
        top.geo.add('parametric_geometry', GeoWithDerivatives())
        
        # Now they should be there.
        self.assertTrue(hasattr(top.geo, 'linearize'))
        self.assertTrue(hasattr(top.geo, 'apply_deriv'))
        self.assertTrue(hasattr(top.geo, 'apply_derivT'))
        self.assertTrue(hasattr(top.geo, 'provideJ'))
        
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
