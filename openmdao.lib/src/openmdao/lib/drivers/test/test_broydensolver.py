"""
Test the broyden solver component.
"""

import os
import unittest
import numpy

from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import BroydenSolver, Float
from openmdao.util.testutil import assert_rel_error

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

class SellarBroyden(Assembly):
    """Solution of the sellar analytical problem using MDF.
    
    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
    """

    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        # pylint: disable-msg=E1101
        
        super(SellarBroyden, self).__init__()
        
        # create solver instance
        self.add('driver', BroydenSolver())
        
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        self.driver.workflow.add([self.dis1, self.dis2])

        self.connect('dis1.y1','dis2.y1')
        
        # solver connections
        self.driver.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.driver.add_eq_constraint('dis2.y2 - dis1.y2', '0.0')
        self.driver.itmax = 10
        self.driver.alpha = .4
        self.driver.tol = .000000001

class MIMOEquation(Component):
    """Equation with 2 inputs and 2 outputs"""
    
    # pylint: disable-msg=E1101
    x1 = Float(1.0, iotype='in', desc='Global Design Variable')
    x2 = Float(1.0, iotype='in', desc='Global Design Variable')
    x3 = Float(1.0, iotype='in', desc='Global Design Variable')
    x4 = Float(1.0, iotype='in', desc='Global Design Variable')
    x5 = Float(1.0, iotype='in', desc='Global Design Variable')

    f1 = Float(iotype='out', desc='Output of this Discipline')        
    f2 = Float(iotype='out', desc='Output of this Discipline')        
    f3 = Float(iotype='out', desc='Output of this Discipline')        
    f4 = Float(iotype='out', desc='Output of this Discipline')        
    f5 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Should converge to x=[0,0,0,0,0]"""
        
        xx = numpy.array([self.x1, self.x2, self.x3, self.x4, self.x5])
        
        d = numpy.array([3,2,1.5,1,0.5]) 
        c = 0.01
        
        ff = -d*numpy.array(xx) - c*numpy.array(xx)**3
        
        self.f1 = ff[0]
        self.f2 = ff[1]
        self.f3 = ff[2]
        self.f4 = ff[3]
        self.f5 = ff[4]
        
        
class MIMOBroyden(Assembly):
    """Solution of the MIMO problem using MDF.
    """

    def __init__(self):
        """ Creates a new Assembly with this problem
        root at (0,1)
        """
        
        # pylint: disable-msg=E1101
        
        super(MIMOBroyden, self).__init__()
        
        # create solver instance
        self.add('driver', BroydenSolver())
        
        self.add('dis1', MIMOEquation())
        self.driver.workflow.add([self.dis1])

        # solver connections
        self.driver.add_parameter('dis1.x1', low=-9.e99, high=9.e99)
        self.driver.add_parameter('dis1.x2', low=-9.e99, high=9.e99)
        self.driver.add_parameter('dis1.x3', low=-9.e99, high=9.e99)
        self.driver.add_parameter('dis1.x4', low=-9.e99, high=9.e99)
        self.driver.add_parameter('dis1.x5', low=-9.e99, high=9.e99)
        self.driver.add_eq_constraint('dis1.f1', '0.0')
        self.driver.add_eq_constraint('dis1.f2', '0.0')
        self.driver.add_eq_constraint('dis1.f3', '0.0')
        self.driver.add_eq_constraint('dis1.f4', '0.0')
        self.driver.add_eq_constraint('dis1.f5', '0.0')
        self.driver.itmax = 40
        self.driver.alpha = .8
        self.driver.tol = .000001

        

class TestCase(unittest.TestCase):
    """ Test the broyden solver. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass

    def test_Broyden2(self):
        """ Run Broyden2"""
    
        prob = SellarBroyden()
        set_as_top(prob)
    
        prob.dis1.z1_in = 5.0
        prob.dis1.z2_in = 2.0
        prob.dis1.x1 = 1.0
        prob.dis2.z1_in = 5.0
        prob.dis2.z2_in = 2.0
        prob.driver.algorithm = "broyden2"
        
        prob.run()

        assert_rel_error(self, prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, prob.dis2.y2, 0.904988, 0.0001)

    def test_Broyden3(self):
        """ Run Broyden3"""
    
        prob = SellarBroyden()
        set_as_top(prob)
    
        prob.dis1.z1_in = 5.0
        prob.dis1.z2_in = 2.0
        prob.dis1.x1 = 1.0
        prob.dis2.z1_in = 5.0
        prob.dis2.z2_in = 2.0
        prob.driver.algorithm = "broyden3"
        
        prob.run()

        assert_rel_error(self, prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, prob.dis2.y2, 0.904988, 0.0001)
        
    def test_ExcitingMixing(self):
        """ Run ExcitingMixing"""
    
        prob = SellarBroyden()
        set_as_top(prob)
    
        prob.dis1.z1_in = 5.0
        prob.dis1.z2_in = 2.0
        prob.dis1.x1 = 1.0
        prob.dis2.z1_in = 5.0
        prob.dis2.z2_in = 2.0
        prob.driver.algorithm = "excitingmixing"
        
        prob.run()

        assert_rel_error(self, prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, prob.dis2.y2, 0.904988, 0.0001)
     
    def test_MIMO_Broyden2(self):
        """ Testing Broyden on a 2 input 2 output case"""
    
        prob = MIMOBroyden()
        set_as_top(prob)
    
        prob.dis1.x1 = 1.0
        prob.dis1.x2 = 1.0
        prob.dis1.x3 = 1.0
        prob.dis1.x4 = 1.0
        prob.dis1.x5 = 1.0
        prob.driver.algorithm = "broyden2"
        
        prob.run()
        assert_rel_error(self, 1.0 - prob.dis1.x1, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x2, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x3, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x4, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x5, 1.0, 0.0001)
        
    def test_MIMO_Broyden3(self):
        """ Testing Broyden on a 2 input 2 output case"""
    
        prob = MIMOBroyden()
        set_as_top(prob)
    
        prob.dis1.x1 = 1.0
        prob.dis1.x2 = 1.0
        prob.dis1.x3 = 1.0
        prob.dis1.x4 = 1.0
        prob.dis1.x5 = 1.0
        prob.driver.algorithm = "broyden3"
        
        prob.run()
        assert_rel_error(self, 1.0 - prob.dis1.x1, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x2, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x3, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x4, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x5, 1.0, 0.0001)
        
    def test_MIMO_ExcitingMixing(self):
        """ Testing Broyden on a 2 input 2 output case"""
    
        prob = MIMOBroyden()
        set_as_top(prob)
    
        prob.dis1.x1 = 1.0
        prob.dis1.x2 = 1.0
        prob.dis1.x3 = 1.0
        prob.dis1.x4 = 1.0
        prob.dis1.x5 = 1.0
        prob.driver.algorithm = "excitingmixing"
        prob.driver.alpha = 0.1
        
        prob.run()
        assert_rel_error(self, 1.0 - prob.dis1.x1, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x2, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x3, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x4, 1.0, 0.0001)
        assert_rel_error(self, 1.0 - prob.dis1.x5, 1.0, 0.0001)
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
