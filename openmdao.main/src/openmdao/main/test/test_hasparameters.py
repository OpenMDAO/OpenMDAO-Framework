# pylint: disable-msg=C0111,C0103
import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.datatypes.api import Int, Event, Float
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters
from openmdao.test.execcomp import ExecComp

class DummyFloat(Component): 
    x = Float(0.0,low=-10,high=10)
    y = Float(0.0,low=0,high=10)
    
class DummyInt(Component): 
    x = Int(0,low=-10,high=10)
    y = Int(0,low=0,high=10)    

@add_delegate(HasParameters)
class MyDriver(Driver):
    def start_iteration(self):
        self.iter_count = 0
        
    def post_iteration(self):
        self.iter_count += 1
        
    def continue_iteration(self):
        return self.iter_count < 3


class HasParametersTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())
        self.top.add('comp', ExecComp(exprs=['c=x+y','d=x-y']))
        self.top.driver.workflow.add('comp')
        
    def test_set_params(self):
        self.top.driver.add_parameter('comp.x', 0., 1.e99, None) 
        self.top.driver.add_parameter('comp.y', 0., 1.e99, None)
        self.top.driver.set_parameters([22., 33.])
        self.assertEqual(self.top.comp.x, 22.)
        self.assertEqual(self.top.comp.y, 33.)
        
        ## try setting outside of bounds
        #try:
            #self.top.driver.set_parameters([-1., 3.])
        #except ValueError as err:
            #self.assertEqual(str(err), "parameter value (-1.0) is outside of allowed range [0.0 to 1e+99]")
            
    def test_set_broadcast_params(self): 
        self.top.driver.add_parameter(('comp.x','comp.y'), low=0.,high=1e99)
        self.top.driver.set_parameters([22.,])
        self.assertEqual(self.top.comp.x, 22.)
        self.assertEqual(self.top.comp.y, 22.)
        
    def test_add_incompatible_params(self): 
        self.top.add('d_float',DummyFloat())
        self.top.add('d_int',DummyInt())
        
        try: 
            self.top.driver.add_parameter(('d_float.x','d_float.y'), low=-1,high=10)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Trying to add parameter 'd_float.y', but the lower limit "
                             "supplied (-1) exceeds the built-in lower limit (0.0).")
        else: 
            self.fail("Exception Expected")
        
        try: 
            self.top.driver.add_parameter(('d_float.x','d_int.x'), low=-1,high=10)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Can not add parameter ('d_float.x', 'd_int.x') because "
                             "d_float.x and d_int.x are not all of the same type")
        else: 
            self.fail("Exception Expected")
        
    def test_add_broadcast_params(self): 
        
        self.top.driver.add_parameter(('comp.x','comp.y'), low=0.,high=1e99)
        
        try: 
            self.top.driver.add_parameter(('comp.x','comp.y'), low=0.,high=1e99)
        except Exception as err: 
            self.assertEqual(str(err),"driver: ['comp.y', 'comp.x'] are already Parameter targets")
        else: 
            self.fail("Exception expected")
            
        
        targets = self.top.driver.list_param_targets()
        self.assertEqual(frozenset(targets),frozenset(['comp.x','comp.y']))
        
        try: 
            self.top.driver.remove_parameter('comp.foo')
        except AttributeError,err: 
            self.assertEqual(str(err),"driver: Trying to remove parameter 'comp.foo' that is not in this driver.")
        else: 
            self.fail('RuntimeError Expected')
            
            
        try: 
            self.top.driver.remove_parameter(('comp.x','comp.foo'))
        except AttributeError,err: 
            self.assertEqual(str(err),"driver: Trying to remove parameter '('comp.x', 'comp.foo')' "
                             "that is not in this driver.")
        else: 
            self.fail('RuntimeError Expected')    
            
        self.top.driver.remove_parameter(('comp.x','comp.y'))
        self.assertEqual([],self.top.driver.list_param_targets())
        
        try:
            self.top.driver.add_parameter(('comp.x+comp.y','comp.x'), low=0, high=1.e99)
        except Exception, err:
            self.assertEqual(str(err), "driver: Can't add parameter: 'comp.x+comp.y' is not a valid parameter expression")
        else:
            self.fail("Exception expected")
        
    
    def test_list_add_remove_clear_params(self):
        try: 
            self.top.driver.add_parameter('comp.z', low=0, high=1.e99)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Can't add parameter 'comp.z' because it doesn't exist.")
            
        try: 
            self.top.driver.add_parameter('comp.x[0]', low=0, high=1.e99)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Can't add parameter because I can't evaluate 'comp.x[0]'.")    
        
        self.top.driver.add_parameter('comp.x', low=0., high=1.e99)
        self.top.driver.add_parameter('comp.y', low=0., high=1.e99)
        
        try:
            self.top.driver.add_parameter(('comp.x','comp.y'), low=0, high=1.e99)
        except Exception, err:
            self.assertEqual(str(err), "driver: ['comp.x'] are already Parameter targets")
        else:
            self.fail("Exception expected")
        
        try:
            self.top.driver.add_parameter('comp.x+comp.y', low=0, high=1.e99)
        except Exception, err:
            self.assertEqual(str(err), "driver: Can't add parameter: 'comp.x+comp.y' is not a valid parameter expression")
        else:
            self.fail("Exception expected")

        targets = self.top.driver.list_param_targets()
        self.assertEqual(targets,['comp.x','comp.y'])

        self.top.driver.remove_parameter('comp.x')
        targets = self.top.driver.list_param_targets()
        self.assertEqual(targets,['comp.y'])

        try: 
            self.top.driver.remove_parameter('comp.foo')
        except AttributeError,err: 
            self.assertEqual(str(err),"driver: Trying to remove parameter 'comp.foo' that is not in this driver.")
        else: 
            self.fail('RuntimeError Expected')


        self.top.driver.add_parameter('comp.x', low=0., high=1.e99)
        self.top.driver.clear_parameters()
        targets = self.top.driver.list_param_targets()
        self.assertEqual(targets,[])

        self.top.driver.add_parameter('comp.y', low=0., high=1.e99)
        try: 
            self.top.driver.add_parameter('comp.y')
        except ValueError,err: 
            self.assertEqual(str(err),"driver: ['comp.y'] are already Parameter targets")
        else: 
            self.fail('RuntimeError expected')
        
    def test_metadata(self):
        
            self.top.driver.add_parameter('comp.x', low=0., high=100, fd_step=.001)
            self.top.driver.add_parameter('comp.y', low=0., high=100)
            
            param = self.top.driver.get_parameters().values()
            print param
            
            self.assertEqual(param[0].low, 0.0)
            self.assertEqual(param[0].high, 100.0)
            self.assertEqual(param[0].fd_step, 0.001)
            self.assertEqual(param[1].fd_step, None)
            
            
if __name__ == "__main__":
    unittest.main()


