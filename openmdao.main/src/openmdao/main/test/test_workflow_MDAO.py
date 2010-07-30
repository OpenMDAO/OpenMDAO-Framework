"""
Test run/step/stop aspects of a simple workflow.
"""

import os
import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.exceptions import RunStopped
from openmdao.lib.api import CONMINdriver, Iterate, Float
from openmdao.util.testutil import assert_rel_error

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

class Discipline1(Component):
    """Component containing Discipline 1"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable')
    y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y1 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = z1**2 + z2 + x1 - 0.2*y2"""
        
        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2
        
        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "Discipline 1 - %f, %f, %f, %f, %f!!" % (self.y1, self.y2, \
        #                                    self.z1, self.z2, self.x1)


class Discipline2(Component):
    """Component containing Discipline 2"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) + z1 + z2"""
        
        z1 = self.z1
        z2 = self.z2
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.y2 = y1**(.5) + z1 + z2
        #print "Discipline 2 - %f, %f, %f, %f!!" % (self.y1, self.y2, self.z1, \
        #                                          self.z2)


class Discipline2a(Component):
    """Component containing Discipline 2a"""
    
    # pylint: disable-msg=E1101
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    temp1 = Float(iotype='out', desc='Output of this Discipline')

        
    def execute(self):
        """Evaluates the equation  
        y1 = [y1**(.5)] + z1 + z2"""
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.temp1 = y1**(.5)

        
class Discipline2b(Component):
    """Component containing Discipline 2b"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    temp1 = Float(0.0, iotype='in', desc='Intermediate Variable')

    temp2 = Float(iotype='out', desc='Intermediate Variable')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) [+ z1] + z2"""
        
        z1 = self.z1
        
        self.temp2 = self.temp1 + z1

        
class Discipline2c(Component):
    """Component containing Discipline 2c"""
    
    # pylint: disable-msg=E1101
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    temp2 = Float(0.0, iotype='in', desc='Intermediate Variable')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) + z1 [+ z2]"""
        
        z2 = self.z2
        
        self.y2 = self.temp2 + z2
        
        
class Coupler(Component):
    """Component that holds some design variables.
    This is only needed because we can't hook an optimizer up to multiple
    locations of the same design variable"""
    
    # pylint: disable-msg=E1101
    z1_in = Float(0.0, iotype='in', desc='Global Design Variable')
    z2_in = Float(0.0, iotype='in', desc='Global Design Variable')
    x1_in = Float(0.0, iotype='in', desc='Local Design Variable for CO')
    y1_in = Float(0.0, iotype='in', desc='Coupling Variable')
    y2_in = Float(0.0, iotype='in', desc='Coupling Variable')
    z1 = Float(0.0, iotype='out', desc='Global Design Variable')
    z2 = Float(0.0, iotype='out', desc='Global Design Variable')
    x1 = Float(0.0, iotype='out', desc='Local Design Variable for CO')
    y1 = Float(0.0, iotype='out', desc='Coupling Variable')
    y2 = Float(0.0, iotype='out', desc='Coupling Variable')
    
    def execute(self):
        """ Pass everything through"""
        self.z1 = self.z1_in
        self.z2 = self.z2_in
        self.x1 = self.x1_in
        self.y1 = self.y1_in
        self.y2 = self.y2_in


class SellarMDF(Assembly):
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
        
        super(SellarMDF, self).__init__()

        # create Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Outer Loop - Global Optimization
        self.add('coupler', Coupler())
        self.add('iterate', Iterate())
        self.driver.workflow.add([self.coupler, self.iterate])

        # Inner Loop - Full Multidisciplinary Solve via "iterate"
        self.add('dis1', Discipline1())
        self.add('dis2', Discipline2())
        self.iterate.workflow.add([self.dis1, self.dis2])
        
        # Make all connections
        self.connect('coupler.z1','dis1.z1')
        self.connect('coupler.z1','dis2.z1')
        self.connect('coupler.z2','dis1.z2')
        self.connect('coupler.z2','dis2.z2')
        self.connect('dis1.y1','dis2.y1')

        # Iteration loop
        self.iterate.loop_end = 'dis2.y2'
        self.iterate.loop_start = 'dis1.y2'
        self.iterate.max_iteration = 1000
        self.iterate.tolerance = .0001

        # Optimization parameters
        self.driver.objective = '(dis1.x1)**2 + coupler.z2 + dis1.y1 + math.exp(-dis2.y2)'
        for param, low, high in zip(['coupler.z1_in', 'coupler.z2_in', 'dis1.x1'],
                                    [-10.0, 0.0, 0.0],
                                    [10.0, 10.0, 10.0]):
            self.driver.add_parameter(param, low=low, high=high)
        self.driver.constraints = ['3.16 - dis1.y1',
                                   'dis2.y2 - 24.0' ]
        self.driver.cons_is_linear = [1, 1, 1, 1, 1, 0, 0, 0]
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .001
        self.driver.fdchm = .001
        self.driver.delfun = .0001
        self.driver.dabfun = .000001
        self.driver.ct = -.01
        self.driver.ctlmin = 0.0001

class SellarIDF(Assembly):
    """Solution of the sellar analytical problem using IDF.
    
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
        
        super(SellarIDF, self).__init__()

        # create Optimizer instance
        self.add('driver', CONMINdriver())

        # Disciplines
        self.add('coupler', Coupler())
        self.add('dis1', Discipline1())
        self.add('dis2', Discipline2())
        
        # Driver process definition
        self.driver.workflow.add([self.coupler, self.dis1, self.dis2])
        
        # Make all connections
        self.connect('coupler.z1','dis1.z1')
        self.connect('coupler.z1','dis2.z1')
        self.connect('coupler.z2','dis1.z2')
        self.connect('coupler.z2','dis2.z2')

        # Optimization parameters
        self.driver.objective = '(dis1.x1)**2 + coupler.z2 + dis1.y1 + math.exp(-dis2.y2)'
        for param, low, high in zip(['coupler.z1_in', 'coupler.z2_in', 'dis1.x1', 'dis2.y1', 'dis1.y2'],
                                    [-10.0, 0.0, 0.0, 3.16, -10.0],
                                    [10.0, 10.0, 10.0, 10, 24.0]):
            self.driver.add_parameter(param, low=low, high=high)
        self.driver.constraints = ['dis2.y1-dis1.y1',
                                   'dis1.y1-dis2.y1',
                                   'dis2.y2-dis1.y2',
                                   'dis1.y2-dis2.y2']
        #self.driver.cons_is_linear = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.001
        self.driver.ctlmin = 0.001

class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO.
    
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
        super(SellarCO, self).__init__()
        
        # Global Optimization
        self.add('driver', CONMINdriver())
        self.add('coupler', Coupler())
        self.add('localopt1', CONMINdriver())
        self.add('localopt2', CONMINdriver())
        self.driver.workflow.add([self.coupler, self.localopt1, 
                                  self.localopt2])
        
        # Local Optimization 1
        self.add('dis1', Discipline1())
        self.localopt1.workflow.add(self.dis1)
        
        # Local Optimization 2
        self.add('dis2a', Discipline2a())
        self.add('dis2b', Discipline2b())
        self.add('dis2c', Discipline2c())
        self.connect('dis2a.temp1','dis2b.temp1')
        self.connect('dis2b.temp2','dis2c.temp2')
        self.localopt2.workflow.add([self.dis2a, self.dis2b, self.dis2c])
        
        #Parameters - Global Optimization
        self.driver.objective = '(coupler.x1)**2 + coupler.z2 + coupler.y1' + \
                                                '+ math.exp(-coupler.y2)'
        for param,low,high in zip(['coupler.z1_in', 'coupler.z2_in', 'coupler.x1_in',
                                   'coupler.y1_in', 'coupler.y2_in'],
                                  [-10.0, 0.0, 0.0, 3.16, -10.0],
                                  [10.0, 10.0, 10.0, 10, 24.0]):
            self.driver.add_parameter(param, low=low, high=high)

        self.driver.constraints = ['(coupler.z1-dis1.z1)**2 + (coupler.z2-dis1.z2)**2 + (coupler.x1-dis1.x1)**2 + '
                                   '(coupler.y1-dis1.y1)**2 + (coupler.y2-dis1.y2)**2',
                                   
                                   '(coupler.z1-dis2b.z1)**2 + (coupler.z2-dis2c.z2)**2 + (coupler.y1-dis2a.y1)**2 + '
                                   '(coupler.y2-dis2c.y2)**2' ]
        
        self.driver.printvars = ['dis1.y1','dis2c.y2']
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.001
        self.driver.ctlmin = 0.001

        #Parameters - Local Optimization 1
        self.localopt1.objective = '(coupler.z1-dis1.z1)**2 + ' + \
                                   '(coupler.z2-dis1.z2)**2 + ' + \
                                   '(coupler.x1-dis1.x1)**2 + ' + \
                                   '(coupler.y1-dis1.y1)**2 + ' + \
                                   '(coupler.y2-dis1.y2)**2'
        for param, low, high in zip(['dis1.z1', 'dis1.z2', 'dis1.x1', 'dis1.y2'],
                                    [-10.0, 0.0, 0.0, -10.0],
                                    [10.0, 10.0, 10.0, 24.0]):
            self.localopt1.add_parameter(param, low=low, high=high)
        #self.localopt1.lower_bounds = [-10.0, 0.0, 0.0, -10.0]
        #self.localopt1.upper_bounds = [10.0, 10.0, 10.0, 24.0]
        self.localopt1.iprint = 0
        self.localopt1.itmax = 100
        self.localopt1.fdch = .003
        self.localopt1.fdchm = .003
        self.localopt1.delfun = .001
        self.localopt1.dabfun = .00001
        
        #Parameters - Local Optimization 2
        self.localopt2.objective = '(coupler.z1-dis2b.z1)**2 + ' + \
                                   '(coupler.z2-dis2c.z2)**2 + ' + \
                                   '(coupler.y1-dis2a.y1)**2 + ' + \
                                   '(coupler.y2-dis2c.y2)**2'
        for param, low, high in zip(['dis2b.z1', 'dis2c.z2', 'dis2a.y1'],
                                    [-10.0, 0.0, 3.16],
                                    [10.0, 10.0, 10]):
            self.localopt2.add_parameter(param, low=low, high=high)
        self.localopt2.iprint = 0
        self.localopt2.itmax = 100
        self.localopt2.fdch = .003
        self.localopt2.fdchm = .003
        self.localopt2.delfun = .001
        self.localopt2.dabfun = .00001

        
class TestCase(unittest.TestCase):
    """ Test MDAO architectures implemented as OpenMDAO workflows. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass

    def test_MDF(self):
        """ Run MDF"""
        
        prob = SellarMDF()
        set_as_top(prob)
        prob.coupler.z1_in = 5.0
        prob.coupler.z2_in = 2.0
        prob.dis1.x1 = 1.0
    
        prob.run()
        assert_rel_error(self, prob.coupler.z1_in, 1.977, 0.01)
        assert_rel_error(self, 1.0-prob.coupler.z2_in, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)

    def test_IDF(self):
        """ Run IDF"""
        
        prob = SellarIDF()
        set_as_top(prob)
    
        prob.coupler.z1_in = 5.0
        prob.coupler.z2_in = 2.0
        prob.dis1.x1 = 1.0
        prob.dis2.y1 = 3.16
    
        prob.run()
        assert_rel_error(self, prob.coupler.z1_in, 1.977, 0.01)
        assert_rel_error(self, 1.0-prob.coupler.z2_in, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)
        
    def test_CO(self):
        """ Run CO"""
        
        prob = SellarCO()
        set_as_top(prob)
    
        # Set up initial conditions
    
        prob.coupler.z1_in = 5.0
        prob.dis1.z1 = 5.0
        prob.dis2b.z1 = 5.0
    
        prob.coupler.z2_in = 2.0
        prob.dis1.z2 = 2.0
        prob.dis2c.z2 = 2.0
    
        prob.coupler.x1_in = 1.0
        prob.dis1.x1 = 1.0
        
        prob.coupler.y1_in = 3.16
        prob.coupler.y2_in = 0.0
        prob.dis1.y2 = 0.0
        prob.dis2a.y1 = 3.16
        
        prob.run()

        assert_rel_error(self, prob.coupler.z1_in, 2.0, 0.1)
        assert_rel_error(self, 1.0-prob.coupler.z2_in, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.coupler.x1_in, 1.0, 0.1)

        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

