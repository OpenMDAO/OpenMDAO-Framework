"""
Test run/step/stop aspects of a simple workflow.
"""

import os
import unittest

from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.examples.mdao.sellar_MDF import SellarMDF
from openmdao.examples.mdao.sellar_IDF import SellarIDF
from openmdao.examples.mdao.sellar_CO import SellarCO
from openmdao.examples.mdao.sellar_BLISS import SellarBLISS

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.datatypes.api import Float
from openmdao.util.testutil import assert_rel_error

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"


class SellarDiscipline2a(Component):
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

        

class SellarDiscipline2b(Component):
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

        

class SellarDiscipline2c(Component):
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



class SellarCO_Multi(Assembly):
    """Solution of the sellar analytical problem using CO.
    
    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
    """

    z1 = Float()
    z2 = Float()
    x1 = Float()
    y1 = Float()
    y2 = Float()
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        # pylint: disable-msg=E1101
        super(SellarCO_Multi, self).__init__()
        
        # Global Optimization
        self.add('driver', CONMINdriver())
        self.add('localopt1', CONMINdriver())
        self.add('localopt2', CONMINdriver())
        self.driver.workflow.add(['localopt1', 'localopt2'])
        
        # Local Optimization 1
        self.add('dis1', SellarDiscipline1())
        self.localopt1.workflow.add('dis1')
        
        # Local Optimization 2
        self.add('dis2a', SellarDiscipline2a())
        self.add('dis2b', SellarDiscipline2b())
        self.add('dis2c', SellarDiscipline2c())
        self.connect('dis2a.temp1','dis2b.temp1')
        self.connect('dis2b.temp2','dis2c.temp2')
        self.localopt2.workflow.add(['dis2a', 'dis2b', 'dis2c'])
        
        #Parameters - Global Optimization
        # using group parameters to 'broadcast' same data 
        self.driver.add_objective('x1**2 + z2 + y1 + math.exp(-y2)')
        for param,low,high in zip([('z1','dis1.z1','dis2b.z1'), 
                                   ('z2','dis1.z2','dis2c.z2'), 
                                   ('x1','dis1.x1'), ('y1','dis2a.y1'), 
                                   ('y2','dis1.y2')],
                                  [-10.0, 0.0, 0.0, 3.16, -10.0],
                                  [10.0, 10.0, 10.0, 10, 24.0]):
            self.driver.add_parameter(param, low=low, high=high)

        map(self.driver.add_constraint, [
            '(z1-dis1.z1)**2 + (z2-dis1.z2)**2 + (x1-dis1.x1)**2 + '
            '(y1-dis1.y1)**2 + (y2-dis1.y2)**2 < 0',
            '(z1-dis2b.z1)**2 + (z2-dis2c.z2)**2 + (y1-dis2a.y1)**2 + '
            '(y2-dis2c.y2)**2 < 0' ])
        
        self.driver.printvars = ['dis1.y1','dis2c.y2']
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.0008
        self.driver.ctlmin = 0.0008

        #Parameters - Local Optimization 1
        self.localopt1.add_objective('(z1-dis1.z1)**2 + ' + \
                                   '(z2-dis1.z2)**2 + ' + \
                                   '(x1-dis1.x1)**2 + ' + \
                                   '(y1-dis1.y1)**2 + ' + \
                                   '(y2-dis1.y2)**2')
        for param, low, high in zip(['dis1.z1', 'dis1.z2', 'dis1.x1', 'dis1.y2'],
                                    [-10.0, 0.0, 0.0, -10.0],
                                    [10.0, 10.0, 10.0, 24.0]):
            self.localopt1.add_parameter(param, low=low, high=high)
        self.localopt1.iprint = 0
        self.localopt1.itmax = 100
        self.localopt1.fdch = .003
        self.localopt1.fdchm = .003
        self.localopt1.delfun = .0001
        self.localopt1.dabfun = .000001
        
        #Parameters - Local Optimization 2
        self.localopt2.add_objective('(z1-dis2b.z1)**2 + ' + \
                                   '(z2-dis2c.z2)**2 + ' + \
                                   '(y1-dis2a.y1)**2 + ' + \
                                   '(y2-dis2c.y2)**2')
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
        prob = SellarMDF()
        set_as_top(prob)
        prob.dis1.z1 = prob.dis2.z1 = 5.0
        prob.dis1.z2 = prob.dis2.z2 = 2.0
        prob.dis1.x1 = 1.0
    
        prob.run()
        assert_rel_error(self, prob.dis1.z1, 1.977, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)

    def test_IDF(self):
        prob = SellarIDF()
        set_as_top(prob)
    
        prob.dis1.z1 = prob.dis2.z1 = 5.0
        prob.dis1.z2 = prob.dis2.z2 = 2.0
        prob.dis1.x1 = 1.0
        prob.dis2.y1 = 3.16
    
        prob.run()
        assert_rel_error(self, prob.dis1.z1, 1.977, 0.04)
        assert_rel_error(self, 1.0-prob.dis1.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)
        
    def test_CO(self):
        prob = SellarCO()
        set_as_top(prob)
    
        # Set up initial conditions
    
        prob.dis1.z1 = 5.0
        prob.dis2.z1 = 5.0
    
        prob.dis1.z2 = 2.0
        prob.dis2.z2 = 2.0
    
        prob.dis1.x1 = 1.0
        
        prob.dis2.y1 = 3.16
        
        prob.dis1.y2 = 0.0
        
        prob.run()

        assert_rel_error(self, prob.global_des_var_targets[0], 2.0, 0.1)
        assert_rel_error(self, 1.0-prob.global_des_var_targets[1], 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.local_des_var_targets[0], 1.0, 0.1)

    def test_CO_Multi(self):
        prob = SellarCO_Multi()
        set_as_top(prob)
    
        # Set up initial conditions
    
        prob.z1 = 5.0
        prob.dis1.z1 = 5.0
        prob.dis2b.z1 = 5.0
    
        prob.z2 = 2.0
        prob.dis1.z2 = 2.0
        prob.dis2c.z2 = 2.0
    
        prob.x1 = 1.0
        prob.dis1.x1 = 1.0
        
        prob.y1 = 3.16
        prob.dis2a.y1 = 3.16
        
        prob.y2 = 0.0
        prob.dis1.y2 = 0.0
        
        prob.run()

        assert_rel_error(self, prob.z1, 2.0, 0.1)
        assert_rel_error(self, 1.0-prob.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.x1, 1.0, 0.1)

    def test_BLISS(self):
        prob = SellarBLISS()
        set_as_top(prob)
    
        prob.dis1.z1 = prob.dis2.z1 = prob.z_store[0] = 5.0
        prob.dis1.z2 = prob.dis2.z2 = prob.z_store[1] = 2.0
        prob.dis1.x1 = prob.x1_store = 1.0
    
        prob.run()
        assert_rel_error(self, prob.dis1.z1, 1.977, 0.04)
        assert_rel_error(self, 1.0-prob.dis1.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)
        

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

