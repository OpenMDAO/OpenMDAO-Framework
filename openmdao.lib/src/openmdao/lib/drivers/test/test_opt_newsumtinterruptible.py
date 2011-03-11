"""
Test the NEWSUMT optimizer component using a variety of
problems and special cases.

Problem                     | Objective | Linear Constraints | Nonlinear Constraints
====================================================================================
Rosen Suzuki                | nonlinear |          0         |         3
NEWSUMT Manual #1           | linear    |          2         |         1
Constrained Betts           | nonlinear |          1         |         0 
Paraboloid                  | nonlinear |          0         |         0
Paraboloid w lin constraint | nonlinear |          1         |         0
Paraboloid w non-lin const  | nonlinear |          0         |         1

"""

import sys
import unittest

import numpy

# disable complaints about .__init__: Use super on an old style class
# pylint: disable-msg=E1002

# disable complaints about Module 'numpy' has no 'array' member
# pylint: disable-msg=E1101 

# Disable complaints about Too few public methods
# pylint: disable-msg=R0903

# Disable complaints Invalid name "setUp" (should match [a-z_][a-z0-9_]{2,30}$)
# pylint: disable-msg=C0103

# Disable complaints Comma not followed by a space
# pylint: disable-msg=C0324

# Disable complaints Used builtin function 'map'
# pylint: disable-msg=W0141

# Disable complaints Too many public methods
# pylint: disable-msg=R0904

# Disable complaints about not being able to import modules that Python
#     really can import
# pylint: disable-msg=F0401,E0611

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Array
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import NEWSUMTdriver
from openmdao.util.testutil import assert_rel_error

class OptRosenSuzukiComponent(Component):
    """ From the NEWSUMT User's Manual:
    EXAMPLE 2 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.
    
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
    
    x = Array(iotype='in')
    result = Float(iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        """Initialize"""
        
        super(OptRosenSuzukiComponent, self).__init__(doc)
        # Initial guess
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


class Example1FromManualComponent(Component):
    """ From the NEWSUMT User's Manual:

         EXAMPLE 1
    
         MINIMIZE OBJ = 10.0 * x(1) + X(2)
    
         Subject to:
    
              G(1) = 2.0 * X(1) - X(2) - 1.0 > 0
              G(2) = X(1) - 2.0 * X(2) + 1.0 > 0    
              G(3) = - X(1)**2 + 2.0 * ( X(1) + X(2) ) - 1.0 > 0
                     
    This problem is solved beginning with an initial X-vector of
         X = (2.0, 1.0)
    The optimum design is known to be
         OBJ = 5.5917
    and the corresponding X-vector is
         X = (0.5515, 0.1006)
    """
    
    x = Array(iotype='in')
    result = Float(iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        """Initialize"""
        
        super(Example1FromManualComponent, self).__init__(doc)
        # Initial guess
        self.x = numpy.array([2.0, 1.0], dtype=float)
        self.result = 0.0
        
        self.opt_objective = 5.5917
        self.opt_design_vars = [0.5515, 0.1006]

    def execute(self):
        """calculate the new objective value"""
        
        self.result = (10.0 * self.x[0] + self.x[1] )


class ParaboloidComponent(Component):
    """     
         MINIMIZE OBJ = ( X(1) - 2.0 ) ** 2 +  ( X(2) - 3.0 ) **2
    """
    
    x = Array(iotype='in')
    result = Float(iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        super(ParaboloidComponent, self).__init__(doc)
        self.x = numpy.array([10., 10.], dtype=float) # initial guess
        self.result = 0.
        
        self.opt_objective = 0.
        self.opt_design_vars = [2., 3.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x[0] - 2.0) ** 2 + (self.x[1] - 3.0) ** 2


class ConstrainedBettsComponent(Component):
    """     
         MINIMIZE OBJ = 0.01 * x(1) **2 + x(2) ** 2 - 100.0
    
         Subject to:

              2 <= x(1) <= 50
            -50 <= x(2) <= 50
    
              10 * x(1) - x(2) >= 10.0

                  or

              10.0 - 10.0 * x(1) + x(2) <= 0.0

                  or

              - 10.0 + 10.0 * x(1) - x(2) >= 0.0
                     
    This problem is solved beginning with an initial X-vector of
         X = (-1.0, - 1.0 )
    The optimum design is known to be
         OBJ = - 99.96 
    and the corresponding X-vector is
         X = (2.0, 0.0 )
    """
    
    x = Array(iotype='in')
    result = Float(iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        super(ConstrainedBettsComponent, self).__init__(doc)
        self.x = numpy.array([-1.0, -1.0], dtype=float) # initial guess
        self.result = 0.
        
        self.opt_objective = -99.96
        self.opt_design_vars = [2.0, 0.0]

    def execute(self):
        """calculate the new objective value"""

        self.result = 0.01 * self.x[0] ** 2 + self.x[1] ** 2 - 100.0


class NEWSUMTdriverParaboloidTestCase(unittest.TestCase):
    """test NEWSUMT optimizer component using an unconstrained
    paraboloid function"""

    def setUp(self):
        '''setup'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', ParaboloidComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 30
        self.top.driver.jprint = -1
        self.top.driver.lobj = 0
        
    def tearDown(self):
        '''tear down'''
        
        self.top = None

    def test_opt1(self):
        
        self.top.driver.add_objective('comp.result')

        self.top.driver.add_parameters( [
            ('comp.x[0]', -100.0, 100.0),
            ('comp.x[1]', -100.0, 100.0),
            ] )
        
        map(self.top.driver.add_constraint,[ ])    
        self.top.run()
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
    
    def test_infinite_bounds(self):
        
        self.top.driver.add_objective('comp.result')

        self.top.driver.add_parameters( [
            ('comp.x[0]', float( '-inf' ), float( 'inf' ) ),
            ('comp.x[1]', float( '-inf' ), float( 'inf' ) ),
            ] )
        
        map(self.top.driver.add_constraint,[ ])    
        self.top.run()
        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
    
class NEWSUMTdriverParaboloidWithLinearConstraintTestCase(unittest.TestCase):
    """test NEWSUMT optimizer component using a
    paraboloid function constrained by a linear constraint"""

    def setUp(self):
        '''setUp'''
        
        self.top = set_as_top(Assembly())
        self.top.add('comp', ParaboloidComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 30
        self.top.driver.jprint = -1
        self.top.driver.lobj = 0
        self.top.driver.ilin = numpy.array( [1], dtype=int )
        
    def tearDown(self):
        ''' tear down'''

        self.top = None

    def test_opt1(self):

        self.top.driver.add_objective( 'comp.result' )
        
        self.top.driver.add_parameters( [
            ('comp.x[0]', -100.0, 100.0),
            ('comp.x[1]', -100.0, 100.0),
            ] )

        map(self.top.driver.add_constraint,[ 'comp.x[0] - 4.0 > 0.0' ] )
        self.top.run()
        self.assertAlmostEqual(4.0, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(4.0,
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(3.0,
                               self.top.comp.x[1], places=2)
    
class NEWSUMTdriverParaboloidWithNonLinearConstraintTestCase(unittest.TestCase):
    """test NEWSUMT optimizer component using a
    paraboloid function constrained by a nonlinear constraint"""

    def setUp(self):
        '''setup test'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', ParaboloidComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 300
        self.top.driver.jprint = -1
        self.top.driver.lobj = 0
        
    def tearDown(self):
        '''tear down'''
        self.top = None

    def test_opt1(self):

        self.top.driver.add_objective( 'comp.result' )

        self.top.driver.add_parameters( [
            ('comp.x[0]', -100.0, 100.0),
            ('comp.x[1]', -100.0, 100.0),
            ] )

        map(self.top.driver.add_constraint,[ '- comp.x[0]**2 - ( comp.x[1] - 3.0 )**2 + 1.0 > 0.0' ] )
        self.top.run()
        self.assertAlmostEqual(1.0, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(1.0,
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(3.0,
                               self.top.comp.x[1], places=2)
    
class NEWSUMTdriverConstrainedBettsTestCase(unittest.TestCase):
    """test NEWSUMT optimizer component for the Constrained Betts problem
    
    """

    def setUp(self):
        '''setup test'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', ConstrainedBettsComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 100
        # use finite differences for gradients for objective
        #      and constraint functions
        # use the default values for the step size = 0.01
        self.top.driver.jprint = -1
        self.top.driver.epsrsf = 0.0000005
        
    def tearDown(self):
        '''tear down'''
        self.top = None

    def test_opt1(self):

        self.top.driver.add_objective( 'comp.result' )

        self.top.driver.add_parameters( [
            ('comp.x[0]', 2.0, 50.0),
            ('comp.x[1]', -50.0, 50.0),
            ] )
        
        map(self.top.driver.add_constraint,[ '-10.0 + 10.0 * comp.x[0] - comp.x[1] > 0.0' ] )
        self.top.driver.ilin = [1]

        self.top.run()
        
        assert_rel_error(self,
                         self.top.comp.opt_objective, 
                         self.top.driver.eval_objective(),
                         0.001)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
        
    def test_opt1_with_OpenMDAO_FD(self):

        self.top.driver.add_objective( 'comp.result' )

        self.top.driver.add_parameters( [
            ('comp.x[0]', 2.0, 50.0),
            ('comp.x[1]', -50.0, 50.0),
            ] )
        
        map(self.top.driver.add_constraint,[ '-10.0 + 10.0 * comp.x[0] - comp.x[1] > 0.0' ] )
        self.top.driver.ilin = [1]

        self.top.driver.differentiator = FiniteDifference(self.top.driver)
        
        self.top.run()
        
        assert_rel_error(self,
                         self.top.comp.opt_objective, 
                         self.top.driver.eval_objective(),
                         0.001)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)
        
class NEWSUMTdriverRosenSuzukiTestCase(unittest.TestCase):
    """test NEWSUMT optimizer component using the Rosen Suzuki problem"""

    def setUp(self):
        '''setup test'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', OptRosenSuzukiComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 30
        self.top.driver.jprint = -1
        self.top.driver.lobj = 0
        
    def tearDown(self):
        '''tear down'''
        self.top = None
        
    def test_opt1(self):

        self.top.driver.add_objective('comp.result')

        self.top.driver.add_parameters( [
            ('comp.x[0]', -10.0, 99.0),
            ('comp.x[1]', -10.0, 99.0),
            ('comp.x[2]', -10.0, 99.0),
            ('comp.x[3]', -10.0, 99.0),
            ] )

        map(self.top.driver.add_constraint,[
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])        
        self.top.run()

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

class NEWSUMTdriverExample1FromManualTestCase(unittest.TestCase):
    """
      Example 1 from the NEWSUMT manual
    """

    def setUp(self):
        '''setup test'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', Example1FromManualComponent())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 100
        self.top.driver.jprint = -1
        self.top.driver.lobj = 1
        self.top.driver.epsrsf = 0.0005
        self.top.driver.epsodm = 0.001
        self.top.driver.epsgsn = 0.001
        self.top.driver.default_fd_stepsize = 0.001
        self.top.driver.stepmx = 1e10
        self.top.driver.maxrsf = 30
        
    def tearDown(self):
        '''tear down'''
        self.top = None
        
    def test_opt1(self):

        self.top.driver.add_objective('comp.result')
        
        self.top.driver.add_parameters( [
            ('comp.x[0]', 0.0, 100.0),
            ('comp.x[1]', 0.0, 100.0),
            ] )

        map(self.top.driver.add_constraint,[
            '2.0 * comp.x[0] - comp.x[1] - 1.0 > 0.0',
            'comp.x[0] - 2.0 * comp.x[1] + 1.0 > 0.0',
            '- comp.x[0]**2 + 2.0 * ( comp.x[0] + comp.x[1]) - 1.0 > 0.0'
            ])    
        self.top.run()

        assert_rel_error(self, self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), .05)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x[0], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x[1], places=2)

    def test_bad_ilin(self):
        # Test when user specify an ilin of improper dimension
        
        self.top.driver.add_objective('comp.result')
        
        self.top.driver.add_parameters( [
            ('comp.x[0]', 0.0, 100.0),
            ('comp.x[1]', 0.0, 100.0),
            ] )

        map(self.top.driver.add_constraint,[
            '2.0 * comp.x[0] - comp.x[1] - 1.0 > 0.0',
            'comp.x[0] - 2.0 * comp.x[1] + 1.0 > 0.0',
            '- comp.x[0]**2 + 2.0 * ( comp.x[0] + comp.x[1]) - 1.0 > 0.0'
            ])    

        self.top.driver.ilin = numpy.array( [0, 0], dtype=int )

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: Dimension of NEWSUMT setting 'ilin' should be equal to " + \
                  "the number of constraints."            
            self.assertEqual(str(err), msg )
        else:
            self.fail('RuntimeError expected')
        
    def test_no_design_vars(self):
        # test to see if code responds correctly to no
        #   design vars
        
        self.top.driver.add_objective( 'comp.result' )
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 
                "driver: no parameters specified")
        else:
            self.fail('RuntimeError expected')
    
    def test_get_objective(self):
        # test getting the objective function
        self.top.driver.add_objective( 'comp.result' )
        self.assertEqual('comp.result', self.top.driver.list_objective())
    
    def test_update_objective(self):

        try:
            self.top.driver.eval_objective()
        except Exception, err:
            self.assertEqual(str(err), "driver: no objective specified")
        else:
            self.fail('Exception expected')
            
        self.top.comp.result = 88.
        self.top.driver.add_objective( 'comp.result' )
        self.assertEqual(self.top.driver.eval_objective(), 88.)
        
    
    def test_bad_design_vars(self):
        # test to see if the code handles bad design vars
        try:
            map(self.top.driver.add_parameter, 
                ['comp_bogus.x[0]', 'comp.x[1]'] )
        except AttributeError, err:
            self.assertEqual(str(err), 
                "driver: Can't add parameter 'comp_bogus.x[0]'" + \
                " because it doesn't exist.")
        else:
            self.fail('AttributeError expected')
    
    def test_bounds_swapped(self):
        # test for when lower and upper bounds are swapped
        self.top.driver.add_objective( 'comp.result' )


        try:
            self.top.driver.add_parameters( [
                ('comp.x[0]', -10.0, 99.0),
                ('comp.x[1]',  99.0, -10.0),
                ] )
        except ValueError, err:
            self.assertEqual(str(err),
                             "driver: Parameter 'comp.x[1]' " + \
                             "has a lower bound (99.0) that " + \
                             "exceeds its upper bound (-10.0)" )
        else:
            self.fail('ValueError expected')
        
    def test_max_iteration(self):
        # test to see if the driver really
        #   does stop after the max given to it

        self.top.driver.add_objective('comp.result' )
        
        self.top.driver.add_parameters( [
            ('comp.x[0]', -10.0, 99.0),
            ('comp.x[1]', -10.0, 99.0),
            ] )

        map(self.top.driver.add_constraint,[
            '2.0 * comp.x[0] - comp.x[1] - 1.0 > 0.0',
            'comp.x[0] - 2.0 * comp.x[1] + 1.0 > 0.0',
            '- comp.x[0]**2 + 2.0 * ( comp.x[0] + comp.x[1]) - 1.0 > 0.0'
            ] )

        self.top.driver.itmax = 2
        
        self.top.run()
        

        self.assertEqual(self.top.driver.iter_count,2)
        
    def test_gradient_step_size_large(self):
        # Test that a larger value of fd step-size is less accurate
        
        self.top.driver.add_objective( 'comp.result' )
        
        self.top.driver.add_parameters( [
            ('comp.x[0]', 0.0, 100.0),
            ('comp.x[1]', 0.0, 100.0),
            ] )

        map(self.top.driver.add_constraint,[
            '2.0 * comp.x[0] - comp.x[1] - 1.0 > 0.0',
            'comp.x[0] - 2.0 * comp.x[1] + 1.0 > 0.0',
            '- comp.x[0]**2 + 2.0 * ( comp.x[0] + comp.x[1]) - 1.0 > 0.0'
            ] )
        self.top.driver.default_fd_stepsize = 0.01
        
        self.top.run()
        baseerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())
        
        self.top.driver.default_fd_stepsize = 10.0
        self.top.comp.x = numpy.array([2.0, 1.0], dtype=float)
        self.top.run()
        newerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())


        if baseerror > newerror:
            self.fail("Coarsening CONMIN gradient step size did not make the objective worse.")

            
class OptRosenSuzukiComponent_Deriv(Component):
    """ From the NEWSUMT User's Manual:
    EXAMPLE 2 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.
    
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
    
    x1 = Float(1.0, iotype='in')
    x2 = Float(1.0, iotype='in')
    x3 = Float(1.0, iotype='in')
    x4 = Float(1.0, iotype='in')
    result = Float(0.0, iotype='out')
    
    # pylint: disable-msg=C0103
    def __init__(self, doc=None):
        """Initialize"""
        
        super(OptRosenSuzukiComponent_Deriv, self).__init__(doc)
        # Initial guess
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

        self.derivatives.declare_first_derivative(self, 'result', 'x1')
        self.derivatives.declare_first_derivative(self, 'result', 'x2')
        self.derivatives.declare_first_derivative(self, 'result', 'x3')
        self.derivatives.declare_first_derivative(self, 'result', 'x4')
        self.derivatives.declare_second_derivative(self, 'result', 'x1', 'x1')
        self.derivatives.declare_second_derivative(self, 'result', 'x1', 'x2')
        self.derivatives.declare_second_derivative(self, 'result', 'x1', 'x3')
        self.derivatives.declare_second_derivative(self, 'result', 'x1', 'x4')
        self.derivatives.declare_second_derivative(self, 'result', 'x2', 'x2')
        self.derivatives.declare_second_derivative(self, 'result', 'x2', 'x3')
        self.derivatives.declare_second_derivative(self, 'result', 'x2', 'x4')
        self.derivatives.declare_second_derivative(self, 'result', 'x3', 'x3')
        self.derivatives.declare_second_derivative(self, 'result', 'x3', 'x4')
        self.derivatives.declare_second_derivative(self, 'result', 'x4', 'x4')

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x1**2 - 5.*self.x1 + 
                       self.x2**2 - 5.*self.x2 +
                       2.*self.x3**2 - 21.*self.x3 + 
                       self.x4**2 + 7.*self.x4 + 50)

    def calculate_derivatives(self, first, second):
        """Analytical derivatives"""
        
        if first:
        
            df_dx1 = 2.0*self.x1 - 5.0
            df_dx2 = 2.0*self.x2 - 5.0
            df_dx3 = 4.0*self.x3 - 21.0
            df_dx4 = 2.0*self.x4 + 7.0
        
            self.derivatives.set_first_derivative('result', 'x1', df_dx1)
            self.derivatives.set_first_derivative('result', 'x2', df_dx2)
            self.derivatives.set_first_derivative('result', 'x3', df_dx3)
            self.derivatives.set_first_derivative('result', 'x4', df_dx4)
        
        if second:
        
            df_dx1dx1 = 2.0
            df_dx2dx2 = 2.0
            df_dx3dx3 = 4.0
            df_dx4dx4 = 2.0
            
            self.derivatives.set_second_derivative('result', 'x1', 'x1', df_dx1dx1)
            self.derivatives.set_second_derivative('result', 'x2', 'x2', df_dx2dx2)
            self.derivatives.set_second_derivative('result', 'x3', 'x3', df_dx3dx3)
            self.derivatives.set_second_derivative('result', 'x4', 'x4', df_dx4dx4)

class NEWSUMTdriverRosenSuzukiTestCaseDeriv(unittest.TestCase):
    """test NEWSUMT optimizer component using the Rosen Suzuki problem"""

    def setUp(self):
        '''setup test'''
        self.top = set_as_top(Assembly())
        self.top.add('comp', OptRosenSuzukiComponent_Deriv())
        self.top.add('driver', NEWSUMTdriver())
        self.top.driver.workflow.add('comp')
        self.top.driver.itmax = 30
        self.top.driver.jprint = -1
        self.top.driver.lobj = 0
        
    def tearDown(self):
        '''tear down'''
        self.top = None
        
    def test_opt1(self):

        self.top.driver.add_objective('comp.result')

        self.top.driver.add_parameter('comp.x1', -10.0, 99.0, fd_step=0.01)
        self.top.driver.add_parameter('comp.x2', -10.0, 99.0)
        self.top.driver.add_parameter('comp.x3', -10.0, 99.0)
        self.top.driver.add_parameter('comp.x4', -10.0, 99.0)

        map(self.top.driver.add_constraint,[
            'comp.x1**2+comp.x1+comp.x2**2-comp.x2+comp.x3**2+comp.x3+comp.x4**2-comp.x4 < 8',
            'comp.x1**2-comp.x1+2*comp.x2**2+comp.x3**2+2*comp.x4**2-comp.x4 < 10',
            '2*comp.x1**2+2*comp.x1+comp.x2**2-comp.x2+comp.x3**2-comp.x4 < 5'])
        
        self.top.driver.differentiator = FiniteDifference(self.top.driver)
        self.top.run()

        self.assertAlmostEqual(self.top.comp.opt_objective, 
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0], 
                               self.top.comp.x1, places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1], 
                               self.top.comp.x2, places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2], 
                               self.top.comp.x3, places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3], 
                               self.top.comp.x4, places=1)

if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(NEWSUMTdriverConstrainedBettsTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverRosenSuzukiTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverParaboloidTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverParaboloidWithLinearConstraintTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverParaboloidWithNonLinearConstraintTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverExample1FromManualTestCase))
    suite.addTest(unittest.makeSuite(NEWSUMTdriverRosenSuzukiTestCaseDeriv))

    results = unittest.TextTestRunner(verbosity=2).run(suite)

