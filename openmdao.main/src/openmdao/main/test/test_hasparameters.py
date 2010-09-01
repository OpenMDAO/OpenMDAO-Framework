# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import Event

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.api import Int
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters
from openmdao.test.execcomp import ExecComp

@add_delegate(HasParameters)
class MyDriver(Driver):
    def start_iteration(self):
        self.iter_count = 0
        
    def post_iteration(self):
        self.iter_count += 1
        
    def continue_iteration(self):
        return self.iter_count < 3

class MyComp(Component):
        
    def execute(self):
        pass

class HasParametersTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())
        self.top.add('comp', ExecComp(exprs=['c=x+y','d=x-y']))
        self.top.driver.workflow.add(self.top.comp)
        
    def test_set_params(self):
        self.top.driver.add_parameters([('comp.x', 0., 1.e99), 
                                        ('comp.y', 0., 1.e99)])
        self.top.driver.set_parameters([22., 33.])
        self.assertEqual(self.top.comp.x, 22.)
        self.assertEqual(self.top.comp.y, 33.)
        
        ## try setting outside of bounds
        #try:
            #self.top.driver.set_parameters([-1., 3.])
        #except ValueError as err:
            #self.assertEqual(str(err), "parameter value (-1.0) is outside of allowed range [0.0 to 1e+99]")
    
    def test_list_add_remove_clear_params(self):
        self.top.driver.add_parameter('comp.x', low=0., high=1.e99)
        self.top.driver.add_parameter('comp.y', low=0., high=1.e99)

        params = self.top.driver.list_parameters()
        self.assertEqual(params,['comp.x','comp.y'])

        self.top.driver.remove_parameter('comp.x')
        params = self.top.driver.list_parameters()
        self.assertEqual(params,['comp.y'])  

        try: 
            self.top.driver.remove_parameter('comp.foo')
        except AttributeError,err: 
            self.assertEqual(str(err),"driver: Trying to remove parameter 'comp.foo' that is not in this driver.")
        else: 
            self.fail('RuntimeError Expected')


        self.top.driver.add_parameter('comp.x', low=0., high=1.e99)
        self.top.driver.clear_parameters()
        params = self.top.driver.list_parameters()
        self.assertEqual(params,[])

        self.top.driver.add_parameter('comp.y', low=0., high=1.e99)
        try: 
            self.top.driver.add_parameter('comp.y')
        except AttributeError,err: 
            self.assertEqual(str(err),"driver: Trying to add parameter 'comp.y' to driver, but it's already there")
        else: 
            self.fail('RuntimeError expected')
        

if __name__ == "__main__":
    unittest.main()


