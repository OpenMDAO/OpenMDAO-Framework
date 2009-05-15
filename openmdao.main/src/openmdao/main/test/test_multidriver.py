# pylint: disable-msg=C0111,C0103

import unittest
import logging

from openmdao.main import Model, Assembly, Component, Float, String
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers.conmindriver import CONMINdriver

class PolyOrder2(Component):
    """Calculates a result given polynomial coefficients 
    for x**0, x**1, and x**2.
    """
    def __init__(self, name, parent):
        super(PolyOrder2, self).__init__(name, parent)
        Float('x', self, INPUT, default=0.)
        Float('c2', self, INPUT, default=1.0)
        Float('c1', self, INPUT, default=1.0)
        Float('c0', self, INPUT, default=0.0)
        Float('f_x', self, OUTPUT, default=0., doc='result of c2*x**2+c1*x+c0')
        
    def execute(self):
        self.f_x = self.x*(self.c2*self.x + self.c1) + self.c0
               
class Adder(Component):
    """Outputs the sum of its two inputs."""
    def __init__(self, name, parent):
        super(Adder, self).__init__(name, parent)
        Float('x1', self, INPUT, default=0.0)
        Float('x2', self, INPUT, default=0.0)
        Float('sum', self, OUTPUT, default=0.0)
        
    def execute(self):
        self.sum = self.x1 + self.x2
        
class TwoVarComp(Component):
    """For inputs a and b, outputs 7a^2+b^4-4ab-10a-8b"""
    def __init__(self, name, parent):
        super(TwoVarComp, self).__init__(name, parent)
        Float('a', self, INPUT, default=0.0)
        Float('b', self, INPUT, default=0.0)
        Float('f_x', self, OUTPUT, default = 0.0)
        
    def execute(self):
        self.f_x = (7.*self.a*self.a+self.b**4-4.*self.a*self.b-
                    10.*self.a-8.*self.b)
        
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
        PolyOrder2('comp1',top)
        PolyOrder2('comp2',top)
        PolyOrder2('comp3',top)
        PolyOrder2('comp4',top)
        
        # set up polynomials
        top.comp1.c1 = -5.0  # comp1 -->  x**2 - 5.0*x
        top.comp2.c1 = -5.0  # comp2 -->  x**2 - 5.0*x
        top.comp3.c2 = 2.0   # comp3 -->  2.0*x**2 - 21.0*x
        top.comp3.c1 = -21.0
        top.comp4.c1 = 7.0   # comp4 -->  x**2 + 7.0*x
        
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
        """
        Test problem 9 (Hock and Schittkowski 100)
          objective = (x(1) - 10.0)**2 + 5.0*(x(2) - 12.0)**2 + x(3)**4 + 3.0*(x(4)  &
               - 11.0)**2 + 10.0*x(5)**6 + 7.0*x(6)**2 + x(7)**4 - 4.0*x(6)*x(7) &
               - 10.0*x(6) - 8.0*x(7)
          con(1) = 127.0 - 2.0*x(1)**2 - 3.0*x(2)**4 - x(3) - 4.0*x(4)**2 - 5.0*x(5)
          con(2) = 282.0 - 7.0*x(1) - 3.0*x(2) - 10.0*x(3)**2 - x(4) + x(5)
          con(3) = 196.0 - 23.0*x(1) - x(2)**2 - 6.0*x(6)**2 + 8.0*x(7)
          con(4) = -4.0*x(1)**2 - x(2)**2 + 3.0*x(1)*x(2) - 2.0*x(3)**2 - 5.0*x(6) + 11.0*x(7)
        """
        TwoVarComp('comp67', self.top)
        ExprComp('compa1', self.top, expr='(x-10.)**2')
        ExprComp('compa2', self.top, expr='5.0*(x - 12.0)**2')
        ExprComp('compa3', self.top, expr='x**4')
        ExprComp('compa4', self.top, expr='3.0*(x- 11.0)**2')
        ExprComp('compa5', self.top, expr='10.0*x**6')
        drv = CONMINdriver('driver2',self.top)
        drv.maxiters = 130
        drv.objective.value = 'compa1.f_x + compa2.f_x + compa3.f_x + compa4.f_x + compa5.f_x + comp67.f_x'
        drv.design_vars.value = ['compa1.x', 'compa2.x', 'compa3.x', 'compa4.x', 
                                 'compa5.x', 'comp67.a', 'comp67.b']
        drv.lower_bounds = [-10]*7
        drv.upper_bounds = [99]*7
        drv.constraints.value = [
          '127.0 - 2.0*compa1.x**2 - 3.0*compa2.x**4 - compa3.x - 4.0*compa4.x**2 - 5.0*compa5.x',
          '282.0 - 7.0*compa1.x - 3.0*compa2.x - 10.0*compa3.x**2 - compa4.x + compa5.x',
          '196.0 - 23.0*compa1.x - compa2.x**2 - 6.0*comp67.a**2 + 8.0*comp67.b',
          '-4.0*compa1.x**2 - compa2.x**2 + 3.0*compa1.x*compa2.x - 2.0*compa3.x**2 - 5.0*comp67.a + 11.0*comp67.b'
        ]
        # expected optimal values
        self.opt_objective = 680.630
        self.opt_design_vars = [2.33050, 1.95137, -0.477538, 4.36573, 
                                -0.624488, 1.03813, 1.59423]
        self.top.run()
        self.assertEqual(self.opt_design_vars, drv.design_vals)
        self.assertAlmostEqual(self.opt_objective, 
                               self.top.driver2.objective.refvalue, places=2)
        self.assertAlmostEqual(self.opt_design_vars[0], 
                               self.top.compa1.x, places=3)
    
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


