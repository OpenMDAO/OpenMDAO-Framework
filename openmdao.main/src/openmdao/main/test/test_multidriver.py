# pylint: disable-msg=C0111,C0103

import unittest
import logging
from math import sqrt

from openmdao.main import Assembly, Component, Float, String
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers.conmindriver import CONMINdriver

class Adder(Component):
    """Outputs the sum of its two inputs."""
    def __init__(self, name, parent):
        super(Adder, self).__init__(name, parent)
        Float('x1', self, INPUT, default=0.0)
        Float('x2', self, INPUT, default=0.0)
        Float('sum', self, OUTPUT, default=0.0)
        self.runcount = 0
        
    def execute(self):
        self.sum = self.x1 + self.x2
        self.runcount += 1
        
class ExprComp(Component):
    """Evaluates an expression based on the input x and assigns it to f_x"""
    def __init__(self, name, parent, expr='x'):
        super(ExprComp, self).__init__(name, parent)
        Float('x', self, INPUT, default=0.0)
        Float('f_x', self, OUTPUT, default = 0.0)
        String('expr', self, INPUT, default = expr)
        self.runcount = 0
        
    def execute(self):
        x = self.x
        self.f_x = eval(self.expr)
        self.runcount += 1
    
class ExprComp2(Component):
    """Evaluates an expression based on the inputs x & y and assigns it to f_xy"""
    def __init__(self, name, parent, expr='x'):
        super(ExprComp2, self).__init__(name, parent)
        Float('x', self, INPUT, default=0.0)
        Float('y', self, INPUT, default=0.0)
        Float('f_xy', self, OUTPUT, default = 0.0)
        String('expr', self, INPUT, default = expr)
        self.runcount = 0
        
    def execute(self):
        x = self.x
        y = self.y
        self.f_xy = eval(self.expr)
        self.runcount += 1
    
        
class MultiDriverTestCase(unittest.TestCase):

    def setUp(self):
        # Chop up the equations for the Rosen-Suzuki optimization problem
        # into 4 PolyOrder2 components and some Adders so that our driver
        # will iterate over more than one compnent
        top = Assembly('top', None)
        self.top = top
        ExprComp('comp1',top, expr='x**2 - 5.0*x')
        ExprComp('comp2',top, expr='x**2 - 5.0*x')
        ExprComp('comp3',top, expr='2.0*x**2 - 21.0*x')
        ExprComp('comp4',top, expr='x**2 + 7.0*x')
        
        Adder('adder1', top)
        top.connect('comp1.f_x', 'adder1.x1')
        top.connect('comp2.f_x', 'adder1.x2')
        
        Adder('adder2', top)
        top.connect('comp3.f_x', 'adder2.x1')
        top.connect('comp4.f_x', 'adder2.x2')
        
        Adder('adder3', top)
        top.connect('adder1.sum', 'adder3.x1')
        top.connect('adder2.sum', 'adder3.x2')
        
        # create the first driver
        drv = CONMINdriver('driver1',top)
        drv.maxiters = 30
        drv.objective.value = 'adder3.sum+50.'
        drv.design_vars.value = ['comp1.x', 'comp2.x', 'comp3.x', 'comp4.x']
        drv.lower_bounds = [-10, -10, -10, -10]
        drv.upper_bounds = [99, 99, 99, 99]
        drv.constraints.value = [
            'comp1.x**2 + comp2.x**2 + comp3.x**2 + comp4.x**2 + comp1.x-comp2.x+comp3.x-comp4.x-8.0',
            'comp1.x**2 + 2.*comp2.x**2 + comp3.x**2 + 2.*comp4.x**2 - comp1.x - comp4.x -10.',
            '2.0*comp1.x**2 + comp2.x**2 + comp3.x**2 + 2.0*comp1.x - comp2.x - comp4.x -5.0',
        ]
        # expected optimal values
        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]
        

    def test_one_driver(self):
        self.assertEqual(['comp2', 'comp3', 'comp1', 'comp4', 'adder3'], 
                         self.top.driver1.sorted_components())
        self.top.run()
        self.assertAlmostEqual(self.opt_objective, 
                               self.top.driver1.objective.refvalue, places=2)
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
        self.top.set('comp1.x', 99)
        self.top.run()
        self.assertTrue(runcount+2 <= self.top.adder3.runcount)
        
    def test_2_drivers(self):
        ExprComp('comp1a',self.top, expr='x**2')
        ExprComp('comp2a',self.top, expr='x-5.0*sqrt(x)')
        self.top.connect('comp1a.f_x', 'comp2a.x')
        drv = CONMINdriver('driver1a',self.top)
        drv.maxiters = 40
        drv.objective.value = 'comp2a.f_x'
        drv.design_vars.value = ['comp1a.x']
        drv.lower_bounds = [0]
        drv.upper_bounds = [99]
        drv.constraints.value = ['driver1.objective.refvalue'] # this just forces driver1 to run first
        self.top.run()
        self.assertAlmostEqual(self.opt_objective, 
                               self.top.driver1.objective.refvalue, places=2)
        self.assertAlmostEqual(self.opt_design_vars[0], 
                               self.top.comp1.x, places=1)
        self.assertAlmostEqual(self.opt_design_vars[1], 
                               self.top.comp2.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[2], 
                               self.top.comp3.x, places=2)
        self.assertAlmostEqual(self.opt_design_vars[3], 
                               self.top.comp4.x, places=1)
        self.assertAlmostEqual(-6.2498054387439232, 
                               self.top.driver1a.objective.refvalue, places=5)
        self.assertAlmostEqual(2.4860514783551508, 
                               self.top.comp1a.x, places=5)

        
    #def test_2_nested_drivers(self):
        ##
        ## Solve (x-3)^2 + xy + (y+4)^2 = 3
        ## using two optimizers nested. The inner loop optimizes y
        ## the outer loop takes care of x
        ## Enough components created to assure that the optimizers don't "touch"
        ## 
        ## Optimal solution: x = 6.6667; y = -7.3333
        
        #ExprComp('comp1',self.top, expr='x-3')
        #ExprComp('comp2',self.top, expr='-3')
        #ExprComp2('comp3',self.top, expr='x*x + (x+3)*y + (y+4)**2')
        #ExprComp2('comp4',self.top, expr='x+y')
        #self.top.comp1.set('x', 50)
        #self.top.comp3.set('y', 50)
        
        ## Get rid of junk we don't need
        #self.top.remove_child('adder1')
        #self.top.remove_child('adder2')
        #self.top.remove_child('adder3')

        ## Hook stuff up
        #self.top.connect('comp1.f_x', 'comp3.x')
        #self.top.connect('comp3.f_xy', 'comp4.y')
        #self.top.connect('comp2.f_x', 'comp4.x')

        ## create the inner driver
        #drv1 = CONMINdriver('driver1',self.top)
        #drv1.maxiters = 30
        #drv1.objective.value = 'comp3.f_xy'
        #drv1.design_vars.value = ['comp3.y']
        #drv1.lower_bounds = [-50]
        #drv1.upper_bounds = [50]
        
        ## create the outer driver
        #drv2 = CONMINdriver('driver2',self.top)
        #drv2.maxiters = 30
        #drv2.objective.value = 'comp4.f_xy'
        #drv2.design_vars.value = ['comp1.x']
        #drv2.lower_bounds = [-50]
        #drv2.upper_bounds = [50]
        
        #self.top.run()

        #self.assertAlmostEqual(self.top.comp1.x, 6.6667, places=4)
        #self.assertAlmostEqual(self.top.comp3.y, -7.3333, places=4)
        
        
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


