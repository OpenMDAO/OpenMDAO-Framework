"""
Test the genetic optimizer driver
"""


import logging
import pkg_resources
import sys
import unittest
import numpy

from enthought.traits.api import TraitError

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, Array
from openmdao.lib.drivers import genetic
from openmdao.main.eggchecker import check_save_load

# pylint: disable-msg=E1101

class SphereFunction(Component):
    total = Float(0., iotype='out')
    x = Float(0, low=-5.12,high=5.13, iotype="in")
    y = Float(0, low=-5.12,high=5.13, iotype="in")
    z = Float(0, low=-5.12,high=5.13, iotype="in")
    
    
    def __init__(self, desc=None):
        super(SphereFunction, self).__init__(desc)
            
    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x**2+self.y**2+self.z**2

class SphereFunctionArray(Component):
    total = Float(0., iotype='out')
    x = Array(value=[0.0,0.0,0.0], iotype="in")
    
    
    
    def __init__(self, desc=None):
        super(SphereFunctionArray, self).__init__(desc)
            
    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x[0]**2+self.x[1]**2+self.x[2]**2
        
class TestCase(unittest.TestCase):
    """ test case for the genetic driver"""         
    
    def setUp(self):
        self.top = set_as_top(Assembly())
        
        self.top.add_container('driver', 
                               genetic.Genetic())

    def tearDown(self):
        self.top = None
        
    def test_optimizeSphere(self):
        self.top.add_container('comp', SphereFunction())
        self.top.driver.objective = "comp.total" 

        self.top.driver.add_des_var('comp.x')
        self.top.driver.add_des_var('comp.y')
        self.top.driver.add_des_var('comp.z')
        
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"
        
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               4.0742,places = 4)
        x,y,z = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x[0], -0.0516, places = 4)
        self.assertAlmostEqual(x[1], 2.0118, places = 4)
        self.assertAlmostEqual(x[2], .1559, places = 4)
        
    def test_optimizeSpherearray_nolowhigh(self):
        self.top.add_container('comp', SphereFunctionArray())
        self.top.driver.objective = "comp.total" 

        try:        
            self.top.driver.add_des_var('comp.x[0]')
        except TypeError,err: 
            self.assertEqual(str(err),"driver: values for 'high' and 'low' arguments are required when specifying an "
                             "Array element as a design variable. They were not given for 'comp.x[0]'")
        else: 
            self.fail('TypeError expected')
    
    def test_optimizeSpherearray(self):
        self.top.add_container('comp', SphereFunctionArray())
        self.top.driver.objective = "comp.total" 

        self.top.driver.add_des_var('comp.x[0]', low=-5.12,high=5.13)
        self.top.driver.add_des_var('comp.x[1]', low=-5.12,high=5.13)
        self.top.driver.add_des_var('comp.x[2]', low=-5.12,high=5.13)
        
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"
        
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               4.0742,places = 4)
        x,y,z = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x, -0.0516, places = 4)
        self.assertAlmostEqual(y, 2.0118, places = 4)
        self.assertAlmostEqual(z, .1559, places = 4)     