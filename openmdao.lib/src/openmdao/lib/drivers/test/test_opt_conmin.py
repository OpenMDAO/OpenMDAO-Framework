"""
Test the CONMIN optimizer component
"""

import unittest
import numpy

from openmdao.lib.datatypes.api import TraitError

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Array, Str
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.util.testutil import assert_rel_error


class OptRosenSuzukiComponent(Component):
    """ From the CONMIN User's Manual:
    EXAMPLE 1 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.
    
         MINIMIZE OBJ = X(1)**2 - 5*X(1) + X(2)**2 - 5*X(2) +
                        2*X(3)**2 - 21*X(3) + X(4)**2 + 7*X(4) + 50
    
         Subject to:
    
              G(1) = X(1)**2 + X(1) + X(2)**2 - X(2) +
                     X(3)**2 + X(3) + X(4)**2 - X(4) - 8   .LE.0
    
              G(2) = X(1)**2 - X(1) + 2*X(2)**2 + X(3)**2 +
                     2*X(4)**2 - X(4) - 10                  .LE.0
    
              G(3) = 2*X(1)**2 + 2*X(1) + X(2)**2 - X(2) +
                     X(3)**2 - X(4) - 5                     .LE.0
                     
    This problem is solved beginning with an initial X-vector of
         X = (1.0, 1.0, 1.0, 1.0)
    The optimum design is known to be
         OBJ = 6.000
    and the corresponding X-vector is
         X = (0.0, 1.0, 2.0, -1.0)
    """
    
    x = Array(iotype='in', low=-10, high=99)
    result = Float(iotype='out')
    obj_string = Str(iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        super(OptRosenSuzukiComponent, self).__init__(doc)
        self.x = numpy.array([1., 1., 1., 1.], dtype=float)
        self.result = 0.
        
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x[0]**2 - 5.*self.x[0] + 
                       self.x[1]**2 - 5.*self.x[1] +
                       2.*self.x[2]**2 - 21.*self.x[2] + 
                       self.x[3]**2 + 7.*self.x[3] + 50)
        self.obj_string = "Bad"


class CONMINdriverTestCase(unittest.TestCase):
    """test CONMIN optimizer component"""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', CONMINdriver())
        self.top.add('comp', OptRosenSuzukiComponent())
        self.top.driver.workflow.add('comp')
        self.top.driver.iprint = 0
        self.top.driver.itmax = 30
        
    def tearDown(self):
        self.top = None
        
    def test_opt1(self):
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, 
            ['comp.x[0]', 'comp.x[1]','comp.x[2]', 'comp.x[3]'])
        
        # pylint: disable-msg=C0301
        map(self.top.driver.add_constraint,[
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])        
        self.top.run()
        # pylint: disable-msg=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2], 
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3], 
                               self.top.comp.x[3], places=1)

    def test_opt1_flippedconstraints(self):
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, 
            ['comp.x[0]', 'comp.x[1]','comp.x[2]', 'comp.x[3]'])
        
        # pylint: disable-msg=C0301
        map(self.top.driver.add_constraint,[
            '8 > comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3]',
            '10 > comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3]',
            '5 > 2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3]'])        
        self.top.run()
        # pylint: disable-msg=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2], 
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3], 
                               self.top.comp.x[3], places=1)


    def test_no_design_vars(self):
        self.top.driver.add_objective('comp.result')
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 
                "driver: no parameters specified")
        else:
            self.fail('RuntimeError expected')
    
    def test_no_objective(self):
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                             'comp.x[2]', 'comp.x[3]'])
        try:
            self.top.run()
        except Exception, err:
            self.assertEqual(str(err), "driver: no objective specified")
        else:
            self.fail('Exception expected')
            
    def test_get_objective(self):
        self.top.driver.add_objective('comp.result')
        self.assertEqual('comp.result', self.top.driver.list_objective())
    
    def test_update_objective(self):
        try:
            val = self.top.driver.eval_objective()
        except Exception, err:
            self.assertEqual(str(err), "driver: no objective specified")
        else:
            self.fail('Exception expected')
            
        self.top.comp.result = 88.
        self.top.driver.add_objective('comp.result')
        self.assertEqual(self.top.driver.eval_objective(), 88.)
        
    
    def test_bad_design_vars(self):
        try:
            self.top.driver.add_parameter('comp_bogus.x[0]')
            self.top.driver.add_parameter('comp.x[1]')
        except AttributeError, err:
            self.assertEqual(str(err), 
                "driver: Can't add parameter 'comp_bogus.x[0]' because it doesn't exist.")
        else:
            self.fail('TraitError expected')
    
    def test_scale_design_vector_size_mismatch(self):
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]'])
        self.top.driver.scal = [2,3,4]
        try:
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err),
                             "driver: size of scale factor array"+
                             " (3) does not match number of design vars (2)")
        else:
            self.fail('ValueError expected')

    
    def test_gradient_step_size_large(self):
        # Test that a larger value of fd step-size is less acurate
        
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                             'comp.x[2]', 'comp.x[3]'])
        
        # pylint: disable-msg=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8.',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10.',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5.'])        
        
        self.top.run()
        baseerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())
        
        self.top.driver.fdch = .3
        self.top.driver.fdchm = .3
        self.top.comp.x = numpy.array([1., 1., 1., 1.], dtype=float)
        self.top.run()
        newerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())

        # pylint: disable-msg=E1101
        if baseerror > newerror:
            self.fail("Coarsening CONMIN gradient step size did not make the objective worse.")
        
        
    def test_scaling(self):
        
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                             'comp.x[2]', 'comp.x[3]'])
        self.top.driver.scal = [10.0, 10.0, 10.0, 10.0]
        self.top.driver.nscal = -1
        
        # pylint: disable-msg=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8.',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10.',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5.'])
        
        self.top.run()
        
        # No test, just verifies that the syntax didn't fail.

    def test_linear_constraint_specification(self):
        # Note, just testing problem specification and setup
        
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, 
            ['comp.x[0]', 'comp.x[1]','comp.x[2]', 'comp.x[3]'])
        
        map(self.top.driver.add_constraint,['comp.x[1] + 3.0*comp.x[2] > 3.0',
                                            'comp.x[2] + comp.x[3] > 13.0',
                                            'comp.x[1] - 0.73*comp.x[3]*comp.x[2] > -12.0'])
        self.top.driver.cons_is_linear = [1,1,0]
        self.top.driver.itmax = 1

        self.top.run()
        
    def test_max_iteration(self):
        
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                             'comp.x[2]', 'comp.x[3]'])
        self.top.driver.scal = [10.0, 10.0, 10.0, 10.0]
        self.top.driver.nscal = -1
        
        self.top.driver.itmax = 2
        
        # pylint: disable-msg=C0301
        self.top.run()
        
        # pylint: disable-msg=E1101
        self.assertEqual(self.top.driver.iter_count,2)

    def test_input_minmax_violation(self):
        
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                             'comp.x[2]', 'comp.x[3]'])
        
        self.top.comp.x[0] = 100
        try:
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err),
                             "driver: maximum exceeded for initial value of: comp.x[0]")
        else:
            self.fail('ValueError expected')

        self.top.comp.x[0] = -50
        try:
            self.top.run()
        except ValueError, err:
            self.assertEqual(str(err),
                             "driver: minimum exceeded for initial value of: comp.x[0]")
        else:
            self.fail('ValueError expected')

        self.top.comp.x[0] = 99.0001
        self.top.driver.ctlmin = .001
        self.top.run()


if __name__ == "__main__":
    unittest.main()


