
import unittest
from nose import SkipTest
from numpy import array

from openmdao.examples.mdao.sellar_MDF import SellarMDF
from openmdao.examples.mdao.sellar_MDF_solver import SellarMDF as SellarMDF_no_deriv
from openmdao.examples.mdao.sellar_IDF import SellarIDF
from openmdao.examples.mdao.sellar_CO import SellarCO
from openmdao.examples.mdao.sellar_BLISS import SellarBLISS
from openmdao.main.test.simpledriver import SimpleDriver

from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.lib.optproblems import sellar

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.depgraph import simple_node_iter

from openmdao.util.testutil import assert_rel_error

# pylint: disable=E1101,E1103
# "Instance of <class> has no <attr> member"


class SellarDiscipline2a(Component):
    """Component containing Discipline 2a"""

    # pylint: disable=E1101
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

    # pylint: disable=E1101
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

    # pylint: disable=E1101
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

    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based,
    Concurrent Subspace Optimization for Multidisciplinary System Design,"
    Proceedings References 79 of the 34th AIAA Aerospace Sciences Meeting and
    Exhibit, Reno, NV, January 1996.
    """

    z1 = Float(iotype='in')
    z2 = Float(iotype='in')
    x1 = Float(iotype='in')
    y1 = Float(iotype='in')
    y2 = Float(iotype='in')

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Global Optimization
        self.add('driver', SLSQPdriver())
        self.add('localopt1', SLSQPdriver())
        self.add('localopt2', SLSQPdriver())
        self.driver.workflow.add(['localopt1', 'localopt2'])

        # Local Optimization 1
        self.add('dis1', sellar.Discipline1())
        self.localopt1.workflow.add('dis1')

        # Local Optimization 2
        self.add('dis2a', SellarDiscipline2a())
        self.add('dis2b', SellarDiscipline2b())
        self.add('dis2c', SellarDiscipline2c())
        self.connect('dis2a.temp1', 'dis2b.temp1')
        self.connect('dis2b.temp2', 'dis2c.temp2')
        self.localopt2.workflow.add(['dis2a', 'dis2b', 'dis2c'])

        #Parameters - Global Optimization
        # using group parameters to 'broadcast' same data
        self.driver.add_objective('x1**2 + z2 + y1 + math.exp(-y2)')
        for param, low, high in zip(['z1', 'z2', 'x1', 'y1', 'y2'],
                                    [-10.0, 0.0, 0.0, 3.16, -10.0],
                                    [10.0, 10.0, 10.0, 10, 24.0]):
            self.driver.add_parameter(param, low=low, high=high)

        map(self.driver.add_constraint, [
            '(z1-dis1.z1)**2 + (z2-dis1.z2)**2 + (x1-dis1.x1)**2 + '
            '(y1-dis1.y1)**2 + (y2-dis1.y2)**2 < 0',
            '(z1-dis2b.z1)**2 + (z2-dis2c.z2)**2 + (y1-dis2a.y1)**2 + '
            '(y2-dis2c.y2)**2 < 0'])

        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.0008
        self.driver.ctlmin = 0.0008

        #Parameters - Local Optimization 1
        self.localopt1.add_objective('(z1-dis1.z1)**2 + '
                                     '(z2-dis1.z2)**2 + '
                                     '(x1-dis1.x1)**2 + '
                                     '(y1-dis1.y1)**2 + '
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
        self.localopt2.add_objective('(z1-dis2b.z1)**2 + '
                                     '(z2-dis2c.z2)**2 + '
                                     '(y1-dis2a.y1)**2 + '
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

    def test_MDF_no_deriv(self):
        prob = SellarMDF_no_deriv()
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

        # In the top workflow, the subdrivers should each become a PA.
        self.assertTrue(len(prob.driver.workflow._system.subsystems()) == 10)
        comp_list = prob.driver.workflow._system.subsystems()[5]._nodes
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue(('localopt2',) in comp_list)
        comp_list = prob.driver.workflow._system.subsystems()[6]._nodes
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue(('localopt1',) in comp_list)

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

        # In the top workflow, the subdrivers should each become a PA.
        self.assertTrue(len(prob.driver.workflow._system.subsystems()) == 10)
        comp_list = prob.driver.workflow._system.subsystems()[6]._nodes
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue(('localopt2',) in comp_list)
        comp_list = prob.driver.workflow._system.subsystems()[5]._nodes
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue(('localopt1',) in comp_list)

        assert_rel_error(self, prob.z1, 2.0, 0.1)
        assert_rel_error(self, 1.0-prob.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.x1, 1.0, 0.1)

    def test_BLISS(self):

        raise SkipTest("FIXME: this currently fails on 2 test platforms (ringtail and tahr)")

        prob = set_as_top(SellarBLISS())

        prob.dis1.z1 = prob.dis2.z1 = prob.dis12lin.z1 = prob.dis1pre.z1 = 5.0
        prob.dis1.z2 = prob.dis2.z2 = prob.dis12lin.z2 = prob.dis1pre.z2 = 2.0
        prob.dis1.x1 = prob.dis1lin.x1 = 1.0

        prob.run()
        assert_rel_error(self, prob.dis1.z1, 1.977, 0.04)
        assert_rel_error(self, 1.0-prob.dis1.z2, 1.0, 0.01)
        assert_rel_error(self, 1.0-prob.dis1.x1, 1.0, 0.1)


class TestCon(Component):
    """ test con constraint """

    x1 = Float(default_value=0,
              iotype='in', desc='test x')
    g1 = Float(iotype='out', desc='test g')

    def execute(self):
        self.g1 = self.x1


class SolverCO(Assembly):
    """ solver using assmebly """

    x = Float(default_value=0, iotype='in', desc='test x')

    def configure(self):
        """config"""

        self.add('driver', SLSQPdriver())
        self.add('testdrv', SLSQPdriver())

        self.driver.workflow.add(['testdrv'])
        self.add('con', TestCon())
        self.testdrv.workflow.add(['con'])

        self.driver.add_parameter('x', low=-10, high=10)
        self.driver.add_constraint('(con.x1-x)**2 < 1e-3')
        self.driver.add_objective('x**2')

        self.testdrv.add_parameter('con.x1', low=-10, high=10)
        self.testdrv.add_objective('(con.x1-x)**2')
        self.testdrv.add_constraint('con.g1>=0')


class SolverCO2(Assembly):

    x = Array(default_value=[0.0, 0.0], iotype='in', desc='test x')

    def configure(self):
        self.add('driver', SimpleDriver())
        self.driver.gradient_options.force_fd = True
        self.driver.add_parameter('x', low=array([-10, -10]), high=array([10, 10]))
        self.driver.add_objective('(x[0]-1)**2 + (x[1]-1)**2')

class SolverCO2scalar(Assembly):

    x = Float(iotype='in', desc='test x')

    def configure(self):
        self.add('driver', SimpleDriver())
        self.driver.gradient_options.force_fd = True
        self.driver.add_parameter('x', low=-10., high=10.)
        self.driver.add_objective('x**2')



class TestSubOptInclusion(unittest.TestCase):

    def test_basic_CO(self):
        # Our CO model failed if our submodels didn't have outputs in the graph.
        # This test covers the fix.

        sim = set_as_top(SolverCO())
        sim.run()

    def test_SolverCO2(self):
        raise SkipTest("FIXME: un-skip this after refactor of scattering")
        # Fix for a bug reported on the forum
        sim = set_as_top(SolverCO2())
        sim.x = [2.0, 2.0]       
        sim.run()
        J = sim.driver.calc_gradient()

        assert_rel_error(self, J[0, 0], 2.0, .001)
        assert_rel_error(self, J[0, 1], 2.0, .001)

    def test_SolverCO2scalar(self):
        sim = set_as_top(SolverCO2scalar())
        sim.x = 1.0
        sim.run()
        J = sim.driver.calc_gradient()

        assert_rel_error(self, J[0, 0], 2.0, .001)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
