# pylint: disable-msg=C0111,C0103

import unittest
import logging
from math import sqrt

from openmdao.main import Assembly, Component, Driver, Float, Int, String
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.refvariable import RefVariable
from openmdao.lib.drivers.conmindriver import CONMINdriver

class Summer(Driver):
    """Sums the objective over some number of iterations, feeding
    its current sum back into the specified design variable."""
    def __init__(self, name, parent):
        super(Summer, self).__init__(name, parent)
        RefVariable('objective', self, INPUT)
        RefVariable('design', self, OUTPUT)
        Int('max_iterations', self, INPUT, default=1)
        Float('sum', self, OUTPUT, default=0.0)
        self.runcount = 0
        self.itercount = 0
    
    def continue_iteration(self):
        return self.itercount <= self.max_iterations
    
    def start_iteration(self):
        self.itercount = 0
        self.sum = 0.
        
    def pre_iteration(self):
        self.design.refvalue = self.sum
    
    def post_iteration(self):
        self.sum += self.objective.refvalue
    
    def execute(self):
        super(Summer, self).execute()
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
    
        
class NestedDriverTestCase(unittest.TestCase):

    def test_2drivers_same_iterset(self):
        top = Assembly('top', None)
        ExprComp('comp1', top, expr='x+1')
        Summer('driver1', top)
        top.driver1.objective.value = 'comp1.f_x'
        top.driver1.design.value = 'comp1.x'
        Summer('driver2', top)
        top.driver2.objective.value = 'comp1.f_x'
        top.driver2.design.value = 'comp1.x'
        try:
            top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 
                "Drivers top.driver1 and top.driver2 iterate over"+
                " the same set of components (['comp1']), so their order"+
                " cannot be determined")
        else:
            self.fail('RuntimeError expected')
            
    def test_2drivers_overlapping_iterset(self):
        top = Assembly('top', None)
        ExprComp('comp1', top, expr='x+1')
        ExprComp('comp2', top, expr='x+1')
        ExprComp('comp3', top, expr='x+1')
        top.connect('comp1.f_x', 'comp2.x')
        top.connect('comp2.f_x', 'comp3.x')
        Summer('driver1', top)
        top.driver1.objective.value = 'comp2.f_x'
        top.driver1.design.value = 'comp1.x'
        Summer('driver2', top)
        top.driver2.design.value = 'comp2.x'
        top.driver2.objective.value = 'comp3.f_x'
        try:
            top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 
                "Drivers top.driver2 and top.driver1 have overlap"+
                " (['comp2']) in their iteration sets, so their order"+
                " cannot be determined")
        else:
            self.fail('RuntimeError expected')
            
        
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


