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
        
    def execute(self):
        self.sum = self.x1 + self.x2
        
class ExprComp(Component):
    """Evaluates an expression based on the input x and assigns it to f_x"""
    def __init__(self, name, parent, expr='x'):
        super(ExprComp, self).__init__(name, parent)
        Float('x', self, INPUT, default=0.0)
        Float('f_x', self, OUTPUT, default = 0.0)
        String('expr', self, INPUT, default = expr)
        
    def execute(self):
        x = self.x
        self.f_x = eval(self.expr)
    
        
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


