# pylint: disable-msg=C0111,C0103

from nose import SkipTest

import unittest
from math import sqrt  # so expr can find it
from StringIO import StringIO

from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver
from openmdao.main.api import Assembly, Component, Driver, Workflow, \
                              set_as_top, dump_iteration_tree
from openmdao.main.datatypes.api import Float, Int, Str
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasparameters import HasParameters
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecCompWithDerivatives
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error
import openmdao.main.pseudocomp as pcompmod

exec_order = []


class Adder(Component):
    """Outputs the sum of its two inputs."""

    x1 = Float(0., iotype='in')
    x2 = Float(0., iotype='in')
    sum = Float(0., iotype='out')

    def execute(self):
        self.sum = self.x1 + self.x2

@add_delegate(HasObjective, HasParameters)
class Summer(Driver):
    """Sums the objective over some number of iterations, feeding
    its current sum back into the specified parameter."""

    max_iterations = Int(1, iotype='in')
    sum = Float(iotype='out')

    def __init__(self):
        super(Summer, self).__init__()
        self.itercount = 0

    def continue_iteration(self):
        return self.itercount < self.max_iterations

    def start_iteration(self):
        self.itercount = 0
        self.sum = 0.001

    def pre_iteration(self):
        self.set_parameters([self.sum])

    def post_iteration(self):
        self.sum += self.eval_objective()
        self.itercount += 1

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        super(Summer, self).execute()


class ExprComp(Component):
    """Evaluates an expression based on the input x and assigns it to f_x"""

    x = Float(iotype='in')
    f_x = Float(iotype='out')
    expr = Str('x', iotype='in')

    def __init__(self, expr='x'):
        super(ExprComp, self).__init__()
        self.expr = expr

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.f_x = eval(self.expr, globals(), self.__dict__)


class ExprComp2(Component):
    """Evaluates an expression based on the inputs x & y and assigns it to f_xy"""

    x = Float(iotype='in')
    y = Float(iotype='in')
    f_xy = Float(iotype='out')
    expr = Str('x', iotype='in')

    def __init__(self, expr='x'):
        super(ExprComp2, self).__init__()
        self.expr = expr

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.f_xy = eval(self.expr, globals(), self.__dict__)

class MultiDriverTestCase(unittest.TestCase):

    def setUp(self):
        global exec_order
        exec_order = []
        pcompmod._count = 0

    def tearDown(self):
        self.top = None

    def rosen_setUp(self):
        # Chop up the equations for the Rosen-Suzuki optimization problem
        # into 4 ExprComp components and some Adders so that our driver
        # will iterate over more than one compnent
        top = set_as_top(Assembly())
        self.top = top

        # create the first driver
        drv = top.add('driver1', CONMINdriver())

        top.add('comp1', ExprComp(expr='x**2 - 5.0*x'))
        top.add('comp2', ExprComp(expr='x**2 - 5.0*x'))
        top.add('comp3', ExprComp(expr='2.0*x**2 - 21.0*x'))
        top.add('comp4', ExprComp(expr='x**2 + 7.0*x'))

        top.add('adder1', Adder())
        top.add('adder2', Adder())
        top.add('adder3', Adder())

        top.connect('comp1.f_x', 'adder1.x1')
        top.connect('comp2.f_x', 'adder1.x2')
        top.connect('comp3.f_x', 'adder2.x1')
        top.connect('comp4.f_x', 'adder2.x2')
        top.connect('adder1.sum', 'adder3.x1')
        top.connect('adder2.sum', 'adder3.x2')

        top.driver.workflow.add('driver1')
        drv.workflow.add(['comp1','comp2','comp3','comp4',
                          'adder1', 'adder2', 'adder3'])

        drv.itmax = 30
        #drv.conmin_diff = True
        drv.add_objective('adder3.sum+50.')
        drv.add_parameter('comp1.x', -10., 99.)
        drv.add_parameter('comp2.x', -10., 99.)
        drv.add_parameter('comp3.x', -10., 99.)
        drv.add_parameter('comp4.x', -10., 99.)
        map(drv.add_constraint, [
            'comp1.x**2 + comp2.x**2 + comp3.x**2 + comp4.x**2 + comp1.x-comp2.x+comp3.x-comp4.x < 8.0',
            'comp1.x**2 + 2.*comp2.x**2 + comp3.x**2 + 2.*comp4.x**2 - comp1.x - comp4.x < 10.',
            '2.0*comp1.x**2 + comp2.x**2 + comp3.x**2 + 2.0*comp1.x - comp2.x - comp4.x < 5.0',
        ])
        # expected optimal values
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def test_var_depends(self):
        print "*** test_var_depends ***"
        self.rosen_setUp()
        self.top._setup()
        srcs, dests = self.top.driver.get_expr_var_depends(recurse=True)
        self.assertEqual(set(['comp1.x', 'comp2.x', 'comp3.x', 'comp4.x']), dests)
        self.assertEqual(set(['_pseudo_0.out0','_pseudo_1.out0','_pseudo_2.out0','_pseudo_3.out0']),
                         srcs)
        srcs, dests = self.top.driver.get_expr_var_depends(recurse=False)
        self.assertEqual(set(), srcs)
        self.assertEqual(set(), dests)
        self.top.driver1.remove_parameter('comp2.x')
        self.top._setup()
        srcs, dests = self.top.driver.get_expr_var_depends(recurse=True)
        self.assertEqual(set(['comp1.x', 'comp3.x', 'comp4.x']), dests)
        self.assertEqual(set(['_pseudo_0.out0','_pseudo_1.out0','_pseudo_2.out0','_pseudo_3.out0']), srcs)

    def test_one_driver(self):
        global exec_order
        print "*** test_one_driver ***"
        self.rosen_setUp()
        self.top.run()
        assert_rel_error(self, self.opt_objective,
                         self.top.driver1.eval_objective(), 0.01)
        self.assertAlmostEqual(self.opt_design_vars[0],
                               self.top.comp1.x, places=1)
        assert_rel_error(self, self.opt_design_vars[1], self.top.comp2.x, 0.01)
        assert_rel_error(self, self.opt_design_vars[2], self.top.comp3.x, 0.01)
        self.assertAlmostEqual(self.opt_design_vars[3],
                               self.top.comp4.x, places=1)
        runcount = self.top.adder3.exec_count

        # verify that driver will run if any of its referenced variables are invalid
        self.top.comp1.x = 99
        self.top.run()
        self.assertTrue(runcount+2 <= self.top.adder3.exec_count)

    def test_2_drivers(self):
        print "*** test_2_drivers ***"
        self.rosen_setUp()
        drv = self.top.add('driver1a', CONMINdriver())
        self.top.add('comp1a', ExprComp(expr='x**2'))
        self.top.add('comp2a', ExprComp(expr='x-5.0*sqrt(x)'))
        self.top.connect('comp1a.f_x', 'comp2a.x')

        self.top.driver.workflow.add('driver1a')
        drv.workflow.add(['comp1a', 'comp2a'])

        drv.itmax = 40
        # Note, this is a bad test for our gradient stuff. It has 2 local
        # minima, and pretty much requires forward or backward difference
        # to reach one of them if you start at 0.0. Still, it works. -- KTM
        drv.add_objective('comp2a.f_x')
        drv.add_parameter('comp1a.x', low=0, high=99)

        self.top.run()

        assert_rel_error(self, self.top.driver1.eval_objective(),
                         self.opt_objective, 0.01)
        self.assertAlmostEqual(self.opt_design_vars[0],
                               self.top.comp1.x, places=1)
        assert_rel_error(self, self.opt_design_vars[1], self.top.comp2.x, 0.01)
        assert_rel_error(self, self.opt_design_vars[2], self.top.comp3.x, 0.01)
        assert_rel_error(self, self.opt_design_vars[3], self.top.comp4.x, 0.01)
        self.assertAlmostEqual(-6.2498054387439232,
                               self.top.driver1a.eval_objective(),
                               places=2)
        self.assertAlmostEqual(2.4860514783551508,
                               self.top.comp1a.x, places=1)


    def test_2_nested_assemblies(self):
        print "*** test_2_nested_assemblies ***"
        #
        # Solve (x-3)^2 + xy + (y+4)^2 = 3
        # using two optimizers nested. The inner loop optimizes y
        # the outer loop takes care of x
        # Enough components created to assure that the optimizers don't "touch"
        #
        # Optimal solution: x = 6.6667; y = -7.3333
        self.top = set_as_top(Assembly())
        # create the outer driver
        outer_driver = self.top.add('driver', CONMINdriver())
        nested = self.top.add('nested', Assembly())
        # create the inner driver
        inner_driver = nested.add('driver', CONMINdriver())

        nested.add('comp1', ExprComp(expr='x-3'))
        nested.add('comp2', ExprComp(expr='-3'))
        nested.add('comp3', ExprComp2(expr='x*x + (x+3)*y + (y+4)**2'))
        nested.add('comp4', ExprComp2(expr='x+y'))
        nested.comp1.x = 50
        nested.comp3.y = -50

        # Hook stuff up
        nested.connect('comp1.f_x', 'comp3.x')
        nested.connect('comp3.f_xy', 'comp4.y')
        nested.connect('comp2.f_x', 'comp4.x')

        nested.create_passthrough('comp1.x')
        nested.create_passthrough('comp4.f_xy')

        outer_driver.workflow.add('nested')
        inner_driver.workflow.add(['comp1','comp2','comp3','comp4'])

        inner_driver.itmax = 30
        inner_driver.fdch = .000001
        inner_driver.fdchm = .000001
        #inner_driver.conmin_diff = True
        inner_driver.add_objective('comp3.f_xy')
        inner_driver.add_parameter('comp3.y', low=-50, high=50)

        outer_driver.itmax = 30
        outer_driver.fdch = .000001
        outer_driver.fdchm = .000001
        #outer_driver.conmin_diff = True
        outer_driver.add_objective('nested.f_xy')   # comp4.f_xy passthrough
        outer_driver.add_parameter('nested.x', low=-50, high=50)  # comp1.x passthrough

        self.top.run()

        # Notes: CONMIN does not quite reach the analytical minimum
        # In fact, it only gets to about 2 places of accuracy.
        # This is also the case for a single 2-var problem.
        self.assertAlmostEqual(nested.x, 6.66667, places=4)
        self.assertAlmostEqual(nested.comp3.y, -7.33333, places=4)

        # test dumping of iteration tree
        stream = StringIO()
        dump_iteration_tree(self.top, full=False, f=stream, tabsize=3)
        s = stream.getvalue()

        # Comp2 and Comp3 are ambiguous in the sort
        s = s.replace('comp2', 'comp2or3')
        s = s.replace('comp3', 'comp2or3')
        self.assertEqual(s,
            '\n   driver\n      nested\n         driver\n            '
            'comp1\n            comp2or3\n            comp2or3\n'
            '            comp4\n')

    def test_2_nested_drivers_same_assembly(self):
        print "*** test_2_nested_drivers_same_assembly ***"
        #
        # Solve (x-3)^2 + xy + (y+4)^2 = 3
        # using two optimizers nested. The inner loop optimizes y
        # the outer loop takes care of x
        #
        # Optimal solution: x = 6.6667; y = -7.3333
        top = set_as_top(Assembly())
        # create the outer driver
        outer_driver = top.add('driver', CONMINdriver())

        # create the inner driver
        inner_driver = top.add('driver1', CONMINdriver())

        top.add('comp1', ExprComp(expr='x-3'))
        top.add('comp2', ExprComp(expr='-3'))
        top.add('comp3', ExprComp2(expr='x*x + (x+3)*y + (y+4)**2'))
        top.add('comp4', ExprComp2(expr='x+y'))
        top.comp1.x = 50
        top.comp3.y = -50

        # Hook stuff up
        top.connect('comp1.f_x', 'comp3.x')
        top.connect('comp3.f_xy', 'comp4.y')
        top.connect('comp2.f_x', 'comp4.x')

        # Driver process definition
        outer_driver.workflow.add(['comp1', 'driver1', 'comp2', 'comp4'])
        inner_driver.workflow.add(['comp3'])

        inner_driver.itmax = 30
        inner_driver.fdch = .000001
        inner_driver.fdchm = .000001
        inner_driver.add_objective('comp3.f_xy')
        inner_driver.add_parameter('comp3.y', low=-50, high=50)

        outer_driver.itmax = 30
        outer_driver.fdch = .000001
        outer_driver.fdchm = .000001
        outer_driver.add_objective('comp4.f_xy')
        outer_driver.add_parameter('comp1.x', low=-50, high=50)

        top.run()
        # Notes: CONMIN does not quite reach the anlytical minimum
        # In fact, it only gets to about 2 places of accuracy.
        # This is also the case for a single 2-var problem.
        self.assertAlmostEqual(top.comp1.x, 6.66667, places=4)
        self.assertAlmostEqual(top.comp3.y, -7.33333, places=4)

        # test dumping of iteration tree
        stream = StringIO()
        dump_iteration_tree(top, full=False, f=stream, tabsize=3)
        s = stream.getvalue()
        self.assertEqual(s,
            '\n   driver\n      comp1\n      driver1\n         comp3\n      comp2\n'
            '      comp4\n')

    def test_2_nested_drivers_same_assembly_extra_comp(self):
        print "*** test_2_nested_drivers_same_assembly ***"
        #
        # Same as above, but one extra trailing component in outer
        # workflow.
        #
        # Optimal solution: x = 6.6667; y = -7.3333
        self.top = set_as_top(Assembly())
        top = self.top
        # create the outer driver
        outer_driver = top.add('driver', CONMINdriver())

        # create the inner driver
        inner_driver = top.add('driver1', CONMINdriver())

        top.add('comp1', ExprComp(expr='x-3'))
        top.add('comp2', ExprComp(expr='-3'))
        top.add('comp3', ExprComp2(expr='x*x + (x+3)*y + (y+4)**2'))
        top.add('comp4', ExprComp2(expr='x+y'))
        top.add('comp5', ExprComp(expr='x'))
        top.comp1.x = 50
        top.comp3.y = -50

        # Hook stuff up
        top.connect('comp1.f_x', 'comp3.x')
        top.connect('comp3.f_xy', 'comp4.y')
        top.connect('comp2.f_x', 'comp4.x')
        top.connect('comp4.f_xy', 'comp5.x')

        # Driver process definition
        outer_driver.workflow.add(['driver1', 'comp5'])
        inner_driver.workflow.add(['comp1','comp2','comp3','comp4'])

        inner_driver.itmax = 30
        inner_driver.fdch = .000001
        inner_driver.fdchm = .000001
        inner_driver.add_objective('comp3.f_xy')
        inner_driver.add_parameter('comp3.y', low=-50, high=50)

        outer_driver.itmax = 30
        outer_driver.fdch = .000001
        outer_driver.fdchm = .000001
        outer_driver.add_objective('comp5.f_x')
        outer_driver.add_parameter('comp1.x', low=-50, high=50)

        self.top.run()

        # Notes: CONMIN does not quite reach the anlytical minimum
        # In fact, it only gets to about 2 places of accuracy.
        # This is also the case for a single 2-var problem.
        self.assertAlmostEqual(top.comp1.x, 6.66667, places=4)
        self.assertAlmostEqual(top.comp3.y, -7.33333, places=4)

        # test dumping of iteration tree
        stream = StringIO()
        dump_iteration_tree(self.top, full=False, f=stream, tabsize=3)
        s = stream.getvalue()
        s = s.replace('comp2', 'comp2or3')
        s = s.replace('comp3', 'comp2or3')
        self.assertEqual(s,
            '\n   driver\n      driver1\n         comp1\n         comp2or3\n'
            '         comp2or3\n         comp4\n      comp5\n')

    def test_2drivers_same_iterset(self):
        #
        #  D1--->
        #  |    |
        #  |<---C1----->
        #       |      |
        #       |<-----D2
        #

        raise SkipTest("We currently don't allow a component instance in multiple workflows.")
        print "*** test_2drivers_same_iterset ***"
        global exec_order
        top = set_as_top(Assembly())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('D1', Summer())
        top.D1.add_objective('C1.f_x')
        top.D1.add_parameter('C1.x', low=-999, high=999)
        top.D1.max_iterations = 3
        top.add('D2', Summer())
        top.D2.add_objective('C1.f_x')
        top.D2.add_parameter('C1.x', low=-999, high=999)
        top.D2.max_iterations = 2

        top.driver.workflow.add(['D1', 'D2'])
        top.D1.workflow.add('C1')
        top.D2.workflow.add('C1')

        top.run()

        self.assertEqual(top.D2.exec_count, 1)
        self.assertEqual(top.D1.exec_count, 1)
        self.assertEqual(top.C1.exec_count,
                         top.D1.max_iterations+top.D2.max_iterations)
        self.assertEqual(exec_order,
                         ['D1', 'C1', 'C1', 'C1',
                          'D2', 'C1', 'C1'])

    def test_2drivers_discon_same_iterset(self):
        #
        #  D1--->
        #  |    |
        #  |    C1--------->|
        #  |                |
        #  |<----------C2   |
        #              |    |
        #              |<---D2
        #
        raise SkipTest("We currently don't allow a component instance in multiple workflows.")
        global exec_order
        print "*** test_2drivers_discon_same_iterset ***"
        top = set_as_top(Assembly())
        top.add('D1', Summer())
        top.add('D2', Summer())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('C2', ExprComp(expr='x+1'))
        top.D1.add_objective('C2.f_x')
        top.D1.add_parameter('C1.x', low=-999, high=999)
        top.D1.max_iterations = 2
        top.D2.add_objective('C1.f_x')
        top.D2.add_parameter('C2.x', low=-999, high=999)
        top.D2.max_iterations = 3

        top.driver.workflow.add(['D1', 'D2'])
        top.D1.workflow.add(['C1', 'C2'])
        top.D2.workflow.add(['C1', 'C2'])

        top.run()

        self.assertEqual(top.D2.exec_count, 1)
        self.assertEqual(top.D1.exec_count, 1)
        self.assertEqual(top.C1.exec_count,
                         top.D1.max_iterations+top.D2.max_iterations)
        self.assertEqual(top.C2.exec_count,
                         top.D1.max_iterations+top.D2.max_iterations)

        # since C1 and C2 are not dependent on each other, they could
        # execute in any order (depending on dict hash value which can differ per platform)
        # so need two possible exec orders
        order1 = ['D1', 'C1', 'C2', 'C1', 'C2', 'D2', 'C1', 'C2', 'C1', 'C2', 'C1', 'C2']
        order2 = ['D1', 'C2', 'C1', 'C2', 'C1', 'D2', 'C1', 'C2', 'C1', 'C2', 'C1', 'C2']
        self.assertTrue(exec_order==order1 or exec_order==order2)


    def test_2peer_drivers(self):
        #
        #  D1-->
        #  |   |
        #  |<--C1------>|
        #               |
        #          D2-->|
        #          |    |
        #          |<---C2
        global exec_order
        print "*** test_2peer_drivers ***"
        top = set_as_top(Assembly())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('C2', ExprComp2(expr='x+y'))

        top.connect('C1.f_x', 'C2.x')
        top.add('D1', Summer())
        top.D1.add_objective('C1.f_x')
        top.D1.add_parameter('C1.x', low=-999, high=999)
        top.D1.max_iterations = 2
        top.add('D2', Summer())
        top.D2.add_objective('C2.f_xy')
        top.D2.add_parameter('C2.y', low=-999, high=999)
        top.D2.max_iterations = 3

        top.driver.workflow.add(['D1', 'D2'])
        top.D1.workflow = Workflow(top.D1, members=['C1'])
        top.D2.workflow = Workflow(top.D2, members=['C2'])

        top.run()
        self.assertEqual(top.D2.exec_count, 1)
        self.assertEqual(top.D1.exec_count, 1)
        self.assertEqual(top.C1.exec_count, top.D1.max_iterations)
        self.assertEqual(top.C2.exec_count, top.D2.max_iterations)
        self.assertEqual(exec_order,
                         ['D1', 'C1', 'C1',
                          'D2', 'C2', 'C2', 'C2'])

        top.C1.exec_count = 0
        top.C2.exec_count = 0
        top.D1.exec_count = 0
        top.D2.exec_count = 0
        top.D1.set('max_iterations', 5)
        top.D2.set('max_iterations', 4)
        exec_order = []
        top.run()
        self.assertEqual(top.D2.exec_count, 1)
        self.assertEqual(top.D1.exec_count, 1)
        self.assertEqual(top.C1.exec_count, top.D1.max_iterations)
        self.assertEqual(top.C2.exec_count, top.D2.max_iterations)
        self.assertEqual(exec_order,
                         ['D1', 'C1', 'C1', 'C1', 'C1', 'C1',
                          'D2', 'C2', 'C2', 'C2', 'C2'])

    def test_cascade_opt(self):

        raise SkipTest("We currently don't allow a component instance in multiple workflows.")

        top = set_as_top(Assembly())

        eq = ['f = (x-3)**2 + x*y + (y+4)**2 - 3']
        deriv = ['df_dx = 2.0*x - 6.0 + y',
                 'df_dy = 2.0*y + 8.0 + x']
        top.add('comp', ExecCompWithDerivatives(eq, deriv))
        top.add('driver', SimpleDriver())
        top.add('opt1', SLSQPdriver())
        top.add('opt2', SLSQPdriver())

        top.opt1.workflow.add(['comp'])
        top.opt2.workflow.add(['comp'])
        top.driver.workflow.add(['opt1', 'opt2'])

        top.opt1.add_parameter('comp.x', low=-100, high=100)
        top.opt1.add_parameter('comp.y', low=-100, high=100)
        top.opt1.add_objective('comp.f')
        top.opt1.maxiter = 2
        top.opt2.add_parameter('comp.x', low=-100, high=100)
        top.opt2.add_parameter('comp.y', low=-100, high=100)
        top.opt2.add_objective('comp.f')
        top.opt2.maxiter = 50

        top.run()

        assert_rel_error(self, top.comp.x, 6.666309, 0.01)
        assert_rel_error(self, top.comp.y, -7.333026, 0.01)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                     outputs=['comp.f'])
        edges = top.driver.workflow._edges
        print edges
        self.assertEqual(set(edges['@in0']), set(['~opt1.comp|x', '~opt2.comp|x']))
        self.assertEqual(set(edges['@in1']), set(['~opt1.comp|y', '~opt2.comp|y']))
        self.assertEqual(set(edges['~opt1.comp|f']), set(['@out0']))
        self.assertEqual(set(edges['~opt2.comp|f']), set(['@out0']))


if __name__ == "__main__":

    #import cProfile
    #cProfile.run('unittest.main()', 'profout')

    #import pstats
    #p = pstats.Stats('profout')
    #p.strip_dirs()
    #p.sort_stats('time')
    #p.print_stats()
    #print '\n\n---------------------\n\n'
    #p.print_callers()
    #print '\n\n---------------------\n\n'
    #p.print_callees()

    unittest.main()
