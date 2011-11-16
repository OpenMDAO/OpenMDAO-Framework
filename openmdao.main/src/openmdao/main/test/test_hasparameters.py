# pylint: disable-msg=C0111,C0103
import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.datatypes.api import Int, Event, Float, Array, Enum, Str
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters, Parameter, ParameterGroup
from openmdao.test.execcomp import ExecComp

class Dummy(Component): 
    x = Float(0.0,low=-10,high=10, iotype='in')
    y = Float(0.0,low=0,high=10, iotype='in')
    arr = Array([1,2,3,4,5], iotype='in')
    i = Int(0,low=-10,high=10, iotype='in')
    j = Int(0,low=0,high=10, iotype='in')
    enum_i = Enum(values=(1,5,8), iotype='in')
    enum_f = Enum(values=(1.1,5.5,8.8), iotype='in')
    
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
    
    def test_param_output(self):
        try:
            p = Parameter('comp.c', self.top, low=0, high=1e99, scope=self.top)
        except Exception as err:
            self.assertEqual(str(err), 
                             ": Can't add parameter 'comp.c' because 'comp.c' is an output.")
        else:
            self.fail("Exception expected")
        
    def test_add_parameter_param_target(self): 
        p = Parameter('comp.x', self.top, low=0, high=1e99, scope=self.top)
        p2 = Parameter('comp.y', self.top, low=0, high=1e99, scope=self.top)
        
        self.top.driver.add_parameter(p)
        self.assertEqual({'comp.x':p},self.top.driver.get_parameters())
        
        self.top.driver.remove_parameter('comp.x')
        
        self.top.driver.add_parameter(p,low=10.0)
        self.assertEqual({'comp.x':p},self.top.driver.get_parameters())
        self.assertEqual(10.0,self.top.driver.get_parameters()['comp.x'].low)
        self.top.driver.remove_parameter('comp.x')
        
        pg = ParameterGroup([p,p2])
        self.top.driver.add_parameter(pg)
        self.assertEqual({'comp.x':pg},dict(self.top.driver.get_parameters()))
        
        self.top.driver.remove_parameter('comp.x')
        
        pg = ParameterGroup([p,p2])
        self.top.driver.add_parameter(pg,low=10.0)
        self.assertEqual(10.0,self.top.driver.get_parameters()['comp.x'].low)
        
        
    def test_single_bogus(self):
        
        try:
            self.top.driver.add_parameter('comp.bogus', 0., 1.e99) 
        except AttributeError, err:
            self.assertEqual(str(err), "driver: Can't add parameter 'comp.bogus' because it doesn't exist.")
        else: 
            self.fail("Exception Expected")

        try:
            self.top.driver.add_parameter('zr()', 0., 1.e99) 
        except ValueError, err:
            self.assertEqual(str(err), "driver: Can't add parameter: 'zr()' is not a valid parameter expression")
        else: 
            self.fail("Exception Expected")

        try:
            self.top.driver.add_parameter('comp.x', 0., 1.e99, scope='bogus') 
        except TypeError, err:
            self.assertEqual(str(err), "driver: Can't add parameter: cannot create weak reference to 'str' object")
        else: 
            self.fail("Exception Expected")
            
        self.top.comp.add_trait('vstr', Str('Hey', iotype='in'))
        try:
            self.top.driver.add_parameter('comp.vstr', 0., 1.e99) 
        except ValueError, err:
            self.assertEqual(str(err), "driver: The value of parameter 'comp.vstr' must be a real or integral type, but its type is 'str'.")
        else: 
            self.fail("Exception Expected")
            
    def test_single_get_referenced_compnames(self): 
        self.top.driver.add_parameter('comp.x', 0., 1.e99) 
        self.assertEqual(set(["comp"]),self.top.driver.get_parameters()['comp.x'].get_referenced_compnames())
        
    def test_group_get_referenced_compnames(self): 
        self.top.driver.add_parameter(('comp.x','comp.y'), 0., 1.e99) 
        self.assertEqual(set(["comp"]),self.top.driver.get_parameters()[('comp.x','comp.y')].get_referenced_compnames())    
        
    def test_set_params(self):
        self.top.driver.add_parameter('comp.x', 0., 1.e99) 
        self.top.driver.add_parameter('comp.y', 0., 1.e99)
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
        self.top.add('dummy',Dummy())
        
        try: 
            self.top.driver.add_parameter(('dummy.x','dummy.y'), low=-1,high=10)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Trying to add parameter 'dummy.y', but the lower limit "
                             "supplied (-1) exceeds the built-in lower limit (0.0).")
        else: 
            self.fail("Exception Expected")
        
        try: 
            self.top.driver.add_parameter(('dummy.x','dummy.i'), low=-1,high=10)
        except Exception as err: 
            self.assertEqual(str(err),"driver: Can't add parameter ('dummy.x', 'dummy.i') because "
                             "dummy.x and dummy.i are not all of the same type")
        else: 
            self.fail("Exception Expected")
        
    def test_add_broadcast_params(self): 
        
        self.top.driver.add_parameter(('comp.x','comp.y'), low=0.,high=1e99)
        
        try: 
            self.top.driver.add_parameter(('comp.x','comp.y'), low=0.,high=1e99)
        except Exception as err: 
            self.assertEqual(str(err),"driver: ['comp.x', 'comp.y'] are already Parameter targets")
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
            self.assertEqual(str(err), "driver: ['comp.x', 'comp.y'] are already Parameter targets")
        else:
            self.fail("Exception expected")
        
        try:
            self.top.driver.add_parameter('comp.x+comp.y', low=0, high=1.e99)
        except Exception, err:
            self.assertEqual(str(err), "driver: Can't add parameter: 'comp.x+comp.y' is not a valid parameter expression")
        else:
            self.fail("Exception expected")

        targets = self.top.driver.list_param_targets()
        self.assertEqual(set(targets),set(['comp.x','comp.y']))

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
            self.assertEqual(str(err),"driver: 'comp.y' is already a Parameter target")
        else: 
            self.fail('RuntimeError expected')
        
    def test_named_params(self):
        self.top.add('comp', Dummy())
        self.top.driver.add_parameter('comp.arr[1]', low=0., high=1.e99, name='foo')
        self.top.driver.add_parameter('comp.arr[3]', low=0., high=1.e99, name='bar')
        
        try:
            self.top.driver.add_parameter('comp.x', name='foo')
        except Exception as err:
            self.assertEqual(str(err), "driver: foo is already a Parameter")
        
        try:
            self.top.driver.add_parameter('comp.arr[3]', name='blah')
        except Exception as err:
            self.assertEqual(str(err), "driver: 'comp.arr[3]' is already a Parameter target")
        
        targets = self.top.driver.list_param_targets()
        self.assertEqual(set(targets),set(['comp.arr[1]','comp.arr[3]']))

        self.top.driver.remove_parameter('bar')
        targets = self.top.driver.list_param_targets()
        self.assertEqual(targets,['comp.arr[1]'])
        
    def test_metadata(self):
        
        self.top.driver.add_parameter('comp.x', low=0., high=100, fd_step=.001)
        self.top.driver.add_parameter('comp.y', low=0., high=100)
        
        param = self.top.driver.get_parameters().values()
        
        self.assertEqual(param[0].low, 0.0)
        self.assertEqual(param[0].high, 100.0)
        self.assertEqual(param[0].fd_step, 0.001)
        self.assertEqual(param[1].fd_step, None)
        


class ParametersTestCase(unittest.TestCase):
    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())
        self.top.add('driver2', MyDriver())
        self.top.add('comp', ExecComp(exprs=['z=a+b+c+d']))
        self.top.driver.workflow.add('comp')
        
    def test_transform(self):

        # Vars with no bounds params with bounds
        self.top.driver.add_parameter('comp.a', low=8.6, high=9.4, scaler=1.5, adder=1.)
        self.top.driver2.add_parameter('comp.a', low=-6., high=10., scaler=4., adder=-2.)
        
        self.top.comp.a = 15.
        
        params = self.top.driver.get_parameters()
        params2 = self.top.driver2.get_parameters()
        
        d1val = params['comp.a'].evaluate()
        d2val = params2['comp.a'].evaluate()
        
        self.assertEqual(d1val, 9.0)
        self.assertEqual(d2val, 5.75)
        
        params['comp.a'].set(d1val)
        self.assertEqual(self.top.comp.a, 15.)
        
        params2['comp.a'].set(d2val)
        self.assertEqual(self.top.comp.a, 15.)
        
    def test_group_get_referenced_vars_by_compname(self):
        self.top.driver.add_parameter(('comp.a','comp.b'),0,1e99)
        self.top.driver.add_parameter(('comp.c','comp.d'),0,1e99)
        params = self.top.driver.get_parameters()
        
        data = params[('comp.a','comp.b')].get_referenced_vars_by_compname()
        self.assertEqual(['comp',],data.keys())
        self.assertEqual(set([param.target for param in data['comp']]),set(['comp.a','comp.b']))    
        
        data = params[('comp.c','comp.d')].get_referenced_vars_by_compname()
        self.assertEqual(['comp',],data.keys())
        self.assertEqual(set([param.target for param in data['comp']]),set(('comp.c','comp.d')))  

        # Vars with bounds, params with no bounds
        self.top.comp.add_trait('v1', Float(0.0, low=0.0, high=10.0, iotype='in'))
        self.top.comp.v1 = 5.0
        self.top.driver.add_parameter('comp.v1', scaler=0.5, adder=-2.0)

        params = self.top.driver.get_parameters()
        self.assertEqual(params['comp.v1'].evaluate(), 12.0)
        self.assertEqual(params['comp.v1'].high, 22.0)
        self.assertEqual(params['comp.v1'].low, 2.0)

        # Vars with bounds, params with bounds
        self.top.comp.add_trait('v2', Float(0.0, low=0.0, high=10.0, iotype='in'))
        self.top.comp.v1 = 5.0
        self.top.driver.clear_parameters()
        self.top.driver.add_parameter('comp.v2', scaler=0.5, adder=-2.0, low=4.0, high=18.0)
        
        params = self.top.driver.get_parameters()
        self.assertEqual(params['comp.v2'].high, 18.0)
        self.assertEqual(params['comp.v2'].low, 4.0)

    def test_transform_just_scale_or_add(self):

        self.top.comp.add_trait('v1', Float(0.0, low=-100.0, high=100.0, iotype='in'))
        self.top.driver.add_parameter('comp.v1', high=12.0, scaler=1.5)
        self.top.driver2.add_parameter('comp.v1', low=-6.0, adder=-2.)
        
        self.top.comp.v1 = 15.
        
        params = self.top.driver.get_parameters()
        params2 = self.top.driver2.get_parameters()
        
        d1val = params['comp.v1'].evaluate()
        d2val = params2['comp.v1'].evaluate()
        
        self.assertEqual(d1val, 10.0)
        self.assertEqual(d2val, 17.0)
        
    def test_transform_bad_wolf(self):
        
        try:
            self.top.driver.add_parameter('comp.a', low=8.6, high=9.4, scaler="bad", adder=1.0)
        except ValueError, err:
            self.assertEqual(str(err), "driver: Bad value given for parameter's 'scaler' attribute")
        else: 
            self.fail("Exception Expected")
            
        try:
            self.top.driver.add_parameter('comp.a', low=8.6, high=9.4, scaler=1.0, adder="wolf")
        except ValueError, err:
            self.assertEqual(str(err), "driver: Bad value given for parameter's 'adder' attribute")
        else: 
            self.fail("Exception Expected")
            


if __name__ == "__main__":
    unittest.main()


