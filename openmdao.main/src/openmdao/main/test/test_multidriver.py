# pylint: disable-msg=C0111,C0103

import unittest
import logging
from math import sqrt

from openmdao.main.api import Assembly, Component, Driver, Expression, \
                              Dataflow, SequentialWorkflow, set_as_top
from openmdao.lib.api import Float, Int, Str
from openmdao.lib.drivers.conmindriver import CONMINdriver

exec_order = []

class Adder(Component):
    """Outputs the sum of its two inputs."""
    
    x1 = Float(0., iotype='in')
    x2 = Float(0., iotype='in')
    sum = Float(0., iotype='out')
    
    def __init__(self):
        super(Adder, self).__init__()
        self.runcount = 0
        
    def execute(self):
        self.sum = self.x1 + self.x2
        self.runcount += 1

class Summer(Driver):
    """Sums the objective over some number of iterations, feeding
    its current sum back into the specified design variable."""
    
    objective = Expression(iotype='in')
    design = Expression(iotype='out')
    max_iterations = Int(1, iotype='in')
    sum = Float(iotype='out')
    
    def __init__(self):
        super(Summer, self).__init__()
        self.runcount = 0
        self.itercount = 0
    
    def continue_iteration(self):
        return self.itercount < self.max_iterations
    
    def start_iteration(self):
        self.itercount = 0
        self.sum = 0.
        
    def pre_iteration(self):
        self.design.set(self.sum)
    
    def post_iteration(self):
        self.sum += self.objective.evaluate()
        self.itercount += 1
    
    def execute(self):
        global exec_order
        exec_order.append(self.name)
        super(Summer, self).execute()
        self.runcount += 1

class ExprComp(Component):
    """Evaluates an expression based on the input x and assigns it to f_x"""
    
    x = Float(iotype='in')
    f_x = Float(iotype='out')
    expr = Str('x', iotype='in')

    def __init__(self, expr='x'):
        super(ExprComp, self).__init__()
        self.runcount = 0
        self.expr = expr
        
    def execute(self):
        global exec_order
        exec_order.append(self.name)
        x = self.x
        self.f_x = eval(self.expr)
        self.runcount += 1
    
class ExprComp2(Component):
    """Evaluates an expression based on the inputs x & y and assigns it to f_xy"""
    
    x = Float(iotype='in')
    y = Float(iotype='in')
    f_xy = Float(iotype='out')
    expr = Str('x', iotype='in')
    
    def __init__(self, expr='x'):
        super(ExprComp2, self).__init__()
        self.runcount = 0
        self.expr = expr

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        x = self.x
        y = self.y
        self.f_xy = eval(self.expr)
        self.runcount += 1
        
class MultiDriverTestCase(unittest.TestCase):

    def setUp(self):
        global exec_order
        exec_order = []

    def rosen_setUp(self):
        # Chop up the equations for the Rosen-Suzuki optimization problem
        # into 4 PolyOrder2 components and some Adders so that our driver
        # will iterate over more than one compnent
        top = set_as_top(Assembly())
        self.top = top
        
        # create the first driver
        drv = top.add('driver1', CONMINdriver())
        
        top.driver.workflow = Dataflow(top, members=[drv])

        top.add('comp1', ExprComp(expr='x**2 - 5.0*x'))
        top.add('comp2', ExprComp(expr='x**2 - 5.0*x'))
        top.add('comp3', ExprComp(expr='2.0*x**2 - 21.0*x'))
        top.add('comp4', ExprComp(expr='x**2 + 7.0*x'))
        
        top.add('adder1', Adder())
        top.connect('comp1.f_x', 'adder1.x1')
        top.connect('comp2.f_x', 'adder1.x2')
        
        top.add('adder2', Adder())
        top.connect('comp3.f_x', 'adder2.x1')
        top.connect('comp4.f_x', 'adder2.x2')
        
        top.add('adder3', Adder())
        top.connect('adder1.sum', 'adder3.x1')
        top.connect('adder2.sum', 'adder3.x2')
        
        drv.itmax = 30
        drv.objective = 'adder3.sum+50.'
        drv.design_vars = ['comp1.x', 'comp2.x', 'comp3.x', 'comp4.x']
        drv.lower_bounds = [-10, -10, -10, -10]
        drv.upper_bounds = [99, 99, 99, 99]
        drv.constraints = [
            'comp1.x**2 + comp2.x**2 + comp3.x**2 + comp4.x**2 + comp1.x-comp2.x+comp3.x-comp4.x-8.0',
            'comp1.x**2 + 2.*comp2.x**2 + comp3.x**2 + 2.*comp4.x**2 - comp1.x - comp4.x -10.',
            '2.0*comp1.x**2 + comp2.x**2 + comp3.x**2 + 2.0*comp1.x - comp2.x - comp4.x -5.0',
        ]
        # expected optimal values
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]
        

    def test_one_driver(self):
        global exec_order
        self.rosen_setUp()
        self.top.run()
        self.assertAlmostEqual(self.opt_objective, 
                               self.top.driver1.objective.evaluate(), places=2)
        self.assertAlmostEqual(self.opt_design_vars[0], 
                               self.top.comp1.x, places=1)
        self.assertAlmostEqual(self.opt_design_vars[1], 
                               self.top.comp2.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[2], 
                               self.top.comp3.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[3], 
                               self.top.comp4.x, places=1)
        runcount = self.top.adder3.runcount
        
        # verify that driver will run if any of its referenced variables are invalid
        self.top.comp1.x = 99
        self.top.run()
        self.assertTrue(runcount+2 <= self.top.adder3.runcount)
        
    def test_2_drivers(self):
        self.rosen_setUp()
        drv = self.top.add('driver1a', CONMINdriver())
        drv.workflow = Dataflow(self.top)
        self.top.add('comp1a', ExprComp(expr='x**2'), workflow=drv.workflow)
        self.top.add('comp2a', ExprComp(expr='x-5.0*sqrt(x)'), workflow=drv.workflow)
        self.top.connect('comp1a.f_x', 'comp2a.x')
        drv.workflow = Dataflow(self.top, 
                                members=[self.top.comp1a, self.top.comp2a])
        self.top.driver.workflow.add(drv)
        drv.itmax = 40
        drv.objective = 'comp2a.f_x'
        drv.design_vars = ['comp1a.x']
        drv.lower_bounds = [0]
        drv.upper_bounds = [99]
        self.top.run()
        
        self.assertAlmostEqual(self.opt_objective, 
                               self.top.driver1.objective.evaluate(), places=2)
        self.assertAlmostEqual(self.opt_design_vars[0], 
                               self.top.comp1.x, places=1)
        self.assertAlmostEqual(self.opt_design_vars[1], 
                               self.top.comp2.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[2], 
                               self.top.comp3.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[3], 
                               self.top.comp4.x, places=1)
        self.assertAlmostEqual(-6.2498054387439232, 
                               self.top.driver1a.objective.evaluate(), 
                               places=2)
        self.assertAlmostEqual(2.4860514783551508, 
                               self.top.comp1a.x, places=1)

        
    def test_2_nested_assemblies(self):
        #
        # Solve (x-3)^2 + xy + (y+4)^2 = 3
        # using two optimizers nested. The inner loop optimizes y
        # the outer loop takes care of x
        # Enough components created to assure that the optimizers don't "touch"
        # 
        # Optimal solution: x = 6.6667; y = -7.3333
        self.top = set_as_top(Assembly())
        # create the outer driver
        drv2 = self.top.add('driver', CONMINdriver())
        nested = self.top.add('nested', Assembly())
        # create the inner driver
        drv1 = nested.add('driver', CONMINdriver())
        
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

        ## create one driver for testing
        #drv1 = self.top.add('driver1', CONMINdriver())
        #drv1.itmax = 30
        #drv1.iprint = 1001
        #drv1.fdch = .000001
        #drv1.fdchm = .000001
        #drv1.objective = 'comp4.f_xy'
        #drv1.design_vars = ['comp1.x', 'comp3.y']
        #drv1.lower_bounds = [-50, -50]
        #drv1.upper_bounds = [50, 50]
        ##drv1.constraints = ['comp1.x**2 + comp3.y**2']
            
        drv1.itmax = 30
        drv1.fdch = .000001
        drv1.fdchm = .000001
        drv1.objective = 'comp3.f_xy'
        drv1.design_vars = ['comp3.y']
        drv1.lower_bounds = [-50]
        drv1.upper_bounds = [50]
        
        drv2.itmax = 30
        drv2.fdch = .000001
        drv2.fdchm = .000001
        drv2.objective = 'nested.f_xy'   # comp4.f_xy passthrough
        drv2.design_vars = ['nested.x']  # comp1.x passthrough
        drv2.lower_bounds = [-50]
        drv2.upper_bounds = [50]
        
        self.top.run()

        # Notes: CONMIN does not quite reach the anlytical minimum
        # In fact, it only gets to about 2 places of accuracy.
        # This is also the case for a single 2-var problem.
        self.assertAlmostEqual(nested.x, 6.66667, places=4)
        self.assertAlmostEqual(nested.comp3.y, -7.33333, places=4)

    def test_2_nested_drivers_same_assembly(self):
        #
        # Solve (x-3)^2 + xy + (y+4)^2 = 3
        # using two optimizers nested. The inner loop optimizes y
        # the outer loop takes care of x
        # 
        # Optimal solution: x = 6.6667; y = -7.3333
        self.top = set_as_top(Assembly())
        top = self.top
        # create the outer driver
        drv = self.top.add('driver', CONMINdriver())
        
        # create the inner driver
        drv1 = top.add('driver1', CONMINdriver())
        
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
        
        drv1.itmax = 30
        drv1.fdch = .000001
        drv1.fdchm = .000001
        drv1.objective = 'comp3.f_xy'
        drv1.design_vars = ['comp3.y']
        drv1.lower_bounds = [-50]
        drv1.upper_bounds = [50]
        
        drv.itmax = 30
        drv.fdch = .000001
        drv.fdchm = .000001
        drv.objective = 'comp4.f_xy'
        drv.design_vars = ['comp1.x']
        drv.lower_bounds = [-50]
        drv.upper_bounds = [50]
        
        drv1.workflow = Dataflow(top, members=[top.comp3])
        drv.workflow = Dataflow(top, members=[top.comp1, top.comp2, drv1, top.comp4])
        
        self.top.run()

        # Notes: CONMIN does not quite reach the anlytical minimum
        # In fact, it only gets to about 2 places of accuracy.
        # This is also the case for a single 2-var problem.
        self.assertAlmostEqual(top.comp1.x, 6.66667, places=4)
        self.assertAlmostEqual(top.comp3.y, -7.33333, places=4)
        
    def test_2drivers_same_iterset(self):
        #
        #  D1--->
        #  |    |
        #  |<---C1----->
        #       |      |
        #       |<-----D2
        #
        global exec_order
        top = set_as_top(Assembly())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('D1', Summer())
        top.D1.objective = 'C1.f_x'
        top.D1.design = 'C1.x'
        top.D1.max_iterations = 3
        top.add('D2', Summer())
        top.D2.objective = 'C1.f_x'
        top.D2.design = 'C1.x'
        top.D2.max_iterations = 2
        
        top.driver.workflow = Dataflow(top, members=[top.D1, top.D2])
        
        top.run()
        
        self.assertEqual(top.D2.runcount, 1)
        self.assertEqual(top.D1.runcount, 1)
        self.assertEqual(top.C1.runcount, 
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
        global exec_order
        top = set_as_top(Assembly())
        top.add('D1', Summer())
        top.add('D2', Summer())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('C2', ExprComp(expr='x+1'))
        top.D1.objective = 'C2.f_x'
        top.D1.design = 'C1.x'
        top.D1.max_iterations = 2
        top.D2.objective = 'C1.f_x'
        top.D2.design = 'C2.x'
        top.D2.max_iterations = 3

        top.driver.workflow = Dataflow(top, members=[top.D1, top.D2])
        top.run()
        
        self.assertEqual(top.D2.runcount, 1)
        self.assertEqual(top.D1.runcount, 1)
        self.assertEqual(top.C1.runcount, 
                         top.D1.max_iterations)
        self.assertEqual(top.C2.runcount, 
                         top.D2.max_iterations+1)
                         
        # since C1 and C2 are not dependent on each other, they could
        # execute in any order (depending on dict hash value which can differ per platform)
        # so need two possible exec orders
        order1 = ['D1', 'C1', 'C2', 'C1', 'D2', 'C2', 'C2', 'C2']
        order2 = ['D1', 'C2', 'C1', 'C1', 'D2', 'C2', 'C2', 'C2']
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
        top = set_as_top(Assembly())
        top.add('C1', ExprComp(expr='x+1'))
        top.add('C2', ExprComp2(expr='x+y'))
        
        top.connect('C1.f_x', 'C2.x')
        top.add('D1', Summer())
        top.D1.objective = 'C1.f_x'
        top.D1.design = 'C1.x'
        top.D1.max_iterations = 2
        top.add('D2', Summer())
        top.D2.objective = 'C2.f_xy'
        top.D2.design = 'C2.y'
        top.D2.max_iterations = 3
        
        top.driver.workflow = Dataflow(top, members=[top.D1, top.D2])
        top.D1.workflow = SequentialWorkflow(members=[top.C1])
        top.D2.workflow = SequentialWorkflow(members=[top.C2])
        
        top.run()
        self.assertEqual(top.D2.runcount, 1)
        self.assertEqual(top.D1.runcount, 1)
        self.assertEqual(top.C1.runcount, top.D1.max_iterations)
        self.assertEqual(top.C2.runcount, top.D2.max_iterations)
        self.assertEqual(exec_order,
                         ['D1', 'C1', 'C1',
                          'D2', 'C2', 'C2', 'C2'])
        
        top.C1.runcount = 0
        top.C2.runcount = 0
        top.D1.runcount = 0
        top.D2.runcount = 0
        top.D1.set('max_iterations', 5)
        top.D2.set('max_iterations', 4)
        exec_order = []
        top.run()
        self.assertEqual(top.D2.runcount, 1)
        self.assertEqual(top.D1.runcount, 1)
        self.assertEqual(top.C1.runcount, top.D1.max_iterations)
        self.assertEqual(top.C2.runcount, top.D2.max_iterations)
        self.assertEqual(exec_order,
                         ['D1', 'C1', 'C1', 'C1', 'C1', 'C1',
                          'D2', 'C2', 'C2', 'C2', 'C2'])
        
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


