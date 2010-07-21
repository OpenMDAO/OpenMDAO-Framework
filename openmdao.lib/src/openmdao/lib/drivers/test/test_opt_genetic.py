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
from openmdao.lib.api import Float, Array, Enum, Int, Str
from openmdao.lib.api import Genetic
from openmdao.main.eggchecker import check_save_load

# pylint: disable-msg=E1101

class SphereFunction(Component):
    total = Float(0., iotype='out')
    x = Float(0, low=-5.12,high=5.13, iotype="in")
    y = Enum([-10,0,1,2,3,4,5], iotype="in")
    z = Int(0, low=-5,high=5, iotype="in")

    def __init__(self, desc=None):
        super(SphereFunction, self).__init__(desc)

    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x**2+self.y**2+self.z**2

class SphereFunctionArray(Component):
    total = Float(0., iotype='out')
    x = Array([0.0,0,0], iotype="in")

    def __init__(self, desc=None):
        super(SphereFunctionArray, self).__init__(desc)

    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x[0]**2+self.x[1]**2+self.x[2]**2

class TestCase(unittest.TestCase):
    """ test case for the genetic driver"""         

    def setUp(self):
        self.top = set_as_top(Assembly())

        self.top.add('driver', Genetic())

    def tearDown(self):
        self.top = None

    def test_optimizeSphere_set_high_low(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add(self.top.comp)
        self.top.driver.objective = "comp.total" 

        self.top.driver.add_parameter('comp.x',high=5.13,low=-5.12)
        self.top.driver.add_parameter('comp.y')
        self.top.driver.add_parameter('comp.z',high=5,low=-5)

        self.top.driver.seed = 123

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"


        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               .1920,places = 4)
        x,y,z = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x, -0.4381, places = 4)
        self.assertEqual(y, 0)
        self.assertEqual(z, 0)


    def test_optimizeSphere(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add(self.top.comp)
        self.top.driver.objective = "comp.total" 

        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_parameter('comp.y')
        self.top.driver.add_parameter('comp.z')

        self.top.driver.seed = 123

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"


        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               .1920,places = 4)
        x,y,z = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x, -0.4381, places = 4)
        self.assertEqual(y, 0)
        self.assertEqual(z, 0)

    def test_optimizeSpherearray_nolowhigh(self):
        self.top.add('comp', SphereFunctionArray())
        self.top.driver.workflow.add(self.top.comp)
        self.top.driver.objective = "comp.total" 

        try:        
            self.top.driver.add_parameter('comp.x[0]')
        except TypeError,err: 
            self.assertEqual(str(err),"driver: values for 'high' and 'low' arguments are required when specifying an "
                             "Array element as a parameter. They were not given for 'comp.x[0]'")
        else: 
            self.fail('TypeError expected')

    def test_optimizeSpherearray(self):
        self.top.add('comp', SphereFunctionArray())
        self.top.driver.workflow.add(self.top.comp)
        self.top.driver.objective = "comp.total" 

        self.top.driver.add_parameter('comp.x[0]', low=-5.12,high=5.13)
        self.top.driver.add_parameter('comp.x[1]', low=-5.12,high=5.13)
        self.top.driver.add_parameter('comp.x[2]', low=-5.12,high=5.13)

        self.top.driver.seed = 123

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"
        self.top.driver.selection_method="tournament"


        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               2.6925,places = 4)
        x,y,z = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x, -1.1610, places = 4)
        self.assertAlmostEqual(y, 0.2189, places = 4)
        self.assertAlmostEqual(z, -1.1387, places = 4)  


    def test_list_remove_clear_params(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add(self.top.comp)
        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_parameter('comp.y')

        params = self.top.driver.list_parameters()
        self.assertEqual(params,['comp.x','comp.y'])

        self.top.driver.remove_parameter('comp.x')
        params = self.top.driver.list_parameters()
        self.assertEqual(params,['comp.y'])  

        try: 
            self.top.driver.remove_parameter('xyz')
        except RuntimeError,err: 
            self.assertEqual(str(err),"driver: Trying to remove parameter 'xyz', but it is not in the genetic driver")
        else: 
            self.fail('RuntimeError Expected')


        self.top.driver.add_parameter('comp.x')
        self.top.driver.clear_parameters()
        params = self.top.driver.list_parameters()
        self.assertEqual(params,[])

        self.top.driver.add_parameter('comp.y')
        try: 
            self.top.driver.add_parameter('comp.y')
        except RuntimeError,err: 
            self.assertEqual(str(err),"driver: Trying to add 'comp.y' to the genetic driver, but it is already in the driver")
        else: 
            self.fail('RuntimeError expected')

    def test_0_low_high(self): 
        from openmdao.main.api import Assembly,Component, set_as_top
        from openmdao.lib.api import Genetic
        from openmdao.lib.api import Float,Int,Enum

        class SomeComp(Component):
            """Arbitrary component with a few public variables, but which does not really do 
            any calculations"""

            w = Float(0.0,low=-10,high=0.0,iotype="in")

            x = Float(0.0,low=0.0,high=100.0,iotype="in")
            y = Int(10,low=10,high=100,iotype="in")
            z = Enum([-10,-5,0,7],iotype="in")

        class Simulation(Assembly):
            """Top Level Assembly used for simulation"""

            def __init__(self):
                """Adds the Genetic driver to the assembly"""

                super(Simulation,self).__init__()

                opt = self.add('optimizer',Genetic())
                self.add('comp',SomeComp())
                opt.workflow.add(self.comp)

                self.optimizer.add_parameter('comp.x')
                self.optimizer.add_parameter('comp.y')
                self.optimizer.add_parameter('comp.z')
        s = Simulation()
    
    def test_improper_parameter_type(self): 
        class SomeComp(Component):
            """Arbitrary component with a few public variables, but which does not really do 
            any calculations"""
            z = Str("test",iotype="in")

        class Simulation(Assembly):
            """Top Level Assembly used for simulation"""

            def __init__(self):
                """Adds the Genetic driver to the assembly"""

                super(Simulation,self).__init__()

                self.add('driver',Genetic())
                self.add('comp',SomeComp())
                self.driver.workflow.add(self.comp)
                
                self.driver.add_parameter('comp.z')
        
        try:         
            s = Simulation()    
        except ValueError,err:
            self.assertEqual(str(err),"driver: Improper parameter type. Must be Float,Int, or an element of an Array.")
        else: 
            self.fail("ValueError expected")

            
if __name__ == "__main__":
    unittest.main()
