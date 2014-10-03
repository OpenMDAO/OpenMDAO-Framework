# pylint: disable-msg=C0111,C0103
import unittest

from numpy import array

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Array, Int, Float, List, Enum, Str
from openmdao.main.hasparameters import HasParameters, Parameter, ParameterGroup
from openmdao.main.interfaces import implements, IHasParameters
from openmdao.main.test.test_derivatives import SimpleDriver
from openmdao.test.execcomp import ExecComp
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_raises


class Dummy(Component):

    x = Float(0.0, low=-10, high=10, iotype='in')
    y = Float(0.0, low=0, high=10, iotype='in')
    lst = List([1, 2, 3, 4, 5], iotype='in')
    i = Int(0, low=-10, high=10, iotype='in')
    j = Int(0, low=0, high=10, iotype='in')
    enum_i = Enum(values=(1, 5, 8), iotype='in')
    enum_f = Enum(values=(1.1, 5.5, 8.8), iotype='in')


@add_delegate(HasParameters)
class MyDriver(Driver):

    implements(IHasParameters)

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
        self.top.add('comp', ExecComp(exprs=['c=x+y', 'd=x-y']))
        self.top.driver.workflow.add('comp')

    def test_param_output(self):
        code = "Parameter('comp.c', low=0, high=1e99, scope=self.top)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't add parameter 'comp.c' because 'comp.c'"
                      " is an output.")

    def test_add_parameter_param_target(self):
        p = Parameter('comp.x', low=0, high=1e99, scope=self.top)
        p2 = Parameter('comp.y', low=0, high=1e99, scope=self.top)

        self.top.run()
        self.top.driver.add_parameter(p)
        self.assertEqual({'comp.x':p}, self.top.driver.get_parameters())

        self.top.run()
        self.top.driver.remove_parameter('comp.x')

        self.top.driver.add_parameter(p, low=10.0)
        self.assertEqual({'comp.x':p}, self.top.driver.get_parameters())
        self.assertEqual(10.0, self.top.driver.get_parameters()['comp.x'].low)
        self.top.driver.remove_parameter('comp.x')

        pg = ParameterGroup([p, p2])
        self.top.driver.add_parameter(pg)
        self.assertEqual({'comp.x':pg}, dict(self.top.driver.get_parameters()))

        self.top.driver.remove_parameter('comp.x')

        pg = ParameterGroup([p, p2])
        self.top.driver.add_parameter(pg, low=10.0)
        self.assertEqual(10.0, self.top.driver.get_parameters()['comp.x'].low)

    def test_single_bogus(self):
        code = "self.top.driver.add_parameter('comp.bogus', 0., 1.e99)"
        assert_raises(self, code, globals(), locals(), AttributeError,
                      "driver: Can't add parameter 'comp.bogus' because"
                      " it doesn't exist.")

        code = "self.top.driver.add_parameter('zr()', 0., 1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Can't add parameter: 'zr()' is not a"
                      " valid parameter expression")

        code = "self.top.driver.add_parameter('comp.x', 0., 1.e99, scope='bogus')"
        assert_raises(self, code, globals(), locals(), TypeError,
                      "driver: Can't add parameter: cannot create weak"
                      " reference to 'str' object")

        self.top.comp.add_trait('vstr', Str('Hey', iotype='in'))
        code = "self.top.driver.add_parameter('comp.vstr', 0., 1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: The value of parameter 'comp.vstr' must"
                      " be a real or integral type, but its type is 'str'.")

    def test_single_get_referenced_compnames(self):
        self.top.driver.add_parameter('comp.x', 0., 1.e99)
        self.assertEqual(set(["comp"]),
                         self.top.driver.get_parameters()['comp.x'].get_referenced_compnames())

    def test_group_get_referenced_compnames(self):
        self.top.driver.add_parameter(('comp.x', 'comp.y'), 0., 1.e99)
        self.assertEqual(set(["comp"]),
                         self.top.driver.get_parameters()[('comp.x', 'comp.y')].get_referenced_compnames())

    def test_set_params(self):
        self.top.driver.add_parameter('comp.x', 0., 1.e99)
        self.top.driver.add_parameter('comp.y', 0., 1.e99)
        self.top.run()
        self.top.driver.set_parameters([22., 33.])

        self.assertEqual(self.top._system.vec['u']['comp.x'][0], 22.)
        self.assertEqual(self.top._system.vec['u']['comp.y'][0], 33.)

        ## try setting outside of bounds
        #try:
            #self.top.driver.set_parameters([-1., 3.])
        #except ValueError as err:
            #self.assertEqual(str(err), "parameter value (-1.0) is outside of allowed range [0.0 to 1e+99]")

    def test_add_connected_param(self):
        self.top.create_passthrough('comp.x')
        self.top.driver.add_parameter('comp.x', 0., 1.e99)
        assert_raises(self, "self.top.run()", globals(), locals(), RuntimeError,
                      ": The following parameters collide with connected inputs: comp.x in driver")

    def test_set_param_by_name(self):
        self.top.driver.add_parameter('comp.x', 0., 1.e99, name='abc')
        self.top.driver.add_parameter('comp.y', 0., 1.e99, name='def')
        self.top.run()
        self.top.driver.set_parameter_by_name('abc', 22.0)
        self.top.driver.set_parameter_by_name('def', 33.0)

        self.assertEqual(self.top._system.vec['u']['comp.x'][0], 22.)
        self.assertEqual(self.top._system.vec['u']['comp.y'][0], 33.)

    def test_set_broadcast_params(self):
        self.top.driver.add_parameter(('comp.x', 'comp.y'), low=0., high=1e99)
        self.top.run()
        self.top.driver.set_parameters([22.])
        self.assertEqual(self.top._system.vec['u']['comp.x'][0], 22.)
        self.assertEqual(self.top._system.vec['u']['comp.y'][0], 22.)

    def test_set_boundary_params(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())
        self.top.add('comp', ExecComp(exprs=['c=x+y', 'd=x-y']))
        self.top.driver.workflow.add('comp')
        self.top.create_passthrough('comp.x')
        self.top.create_passthrough('comp.c')
        self.top.connect('x', 'comp.y')

        self.top.driver.add_parameter(('x'), low=0., high=1e99)
        self.top.x = 22.0
        self.top.run()
        self.assertEqual(self.top.x, 22.)
        self.assertEqual(self.top.comp.y, 22.)

    def test_set_boundary_params_nest(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())
        self.top.add('nest', Assembly())
        self.top.nest.add('comp', ExecComp(exprs=['c=x+y', 'd=x-y']))
        self.top.driver.workflow.add('nest')
        self.top.nest.driver.workflow.add('comp')
        self.top.nest.create_passthrough('comp.x')
        self.top.nest.create_passthrough('comp.c')
        self.top.nest.connect('x', 'comp.y')

        self.top.driver.add_parameter(('nest.x'), low=0., high=1e99)
        self.top.nest.x = 22.0
        self.top.run()
        self.assertEqual(self.top.nest.x, 22.)
        self.assertEqual(self.top.nest.comp.y, 22.)

    def test_add_incompatible_params(self):
        self.top.add('dummy', Dummy())

        code = "self.top.driver.add_parameter(('dummy.x', 'dummy.y'), low=-1, high=10)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Trying to add parameter 'dummy.y', but"
                      " the lower limit supplied (-1) exceeds the"
                      " built-in lower limit (0.0).")

        code = "self.top.driver.add_parameter(('dummy.x', 'dummy.i'), low=-1, high=10)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Can't add parameter ('dummy.x',"
                      " 'dummy.i') because dummy.x and dummy.i are not"
                      " all of the same type")

    def test_add_broadcast_params(self):
        driver = self.top.driver
        driver.add_parameter(('comp.x', 'comp.y'), low=0., high=1e99)

        code = "driver.add_parameter(('comp.x', 'comp.y'), low=0., high=1e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: ['comp.x', 'comp.y'] are already"
                      " Parameter targets")

        targets = driver.list_param_targets()
        self.assertEqual(frozenset(targets), frozenset(['comp.x', 'comp.y']))

        code = "driver.remove_parameter('comp.foo')"
        assert_raises(self, code, globals(), locals(), AttributeError,
                      "driver: Trying to remove parameter 'comp.foo'"
                      " that is not in this driver.")

        code = "driver.remove_parameter(('comp.x', 'comp.foo'))"
        assert_raises(self, code, globals(), locals(), AttributeError,
                      "driver: Trying to remove parameter '('comp.x',"
                      " 'comp.foo')' that is not in this driver.")

        driver.remove_parameter(('comp.x', 'comp.y'))
        self.assertEqual([], driver.list_param_targets())

        code = "driver.add_parameter(('comp.x+comp.y', 'comp.x'), low=0, high=1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Can't add parameter: 'comp.x+comp.y' is"
                      " not a valid parameter expression")

    def test_list_add_remove_clear_params(self):
        driver = self.top.driver

        code = "driver.add_parameter('comp.z', low=0, high=1.e99)"
        assert_raises(self, code, globals(), locals(), AttributeError,
                      "driver: Can't add parameter 'comp.z' because it"
                      " doesn't exist.")

        code = "driver.add_parameter('comp.x[0]', low=0, high=1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Can't add parameter because I can't"
                      " evaluate 'comp.x[0]'.")

        driver.add_parameter('comp.x', low=0., high=1.e99)
        driver.add_parameter('comp.y', low=0., high=1.e99)

        code = "driver.add_parameter(('comp.x', 'comp.y'), low=0, high=1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: ['comp.x', 'comp.y'] are already"
                      " Parameter targets")

        code = "driver.add_parameter('comp.x+comp.y', low=0, high=1.e99)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Can't add parameter: 'comp.x+comp.y' is"
                      " not a valid parameter expression")

        targets = driver.list_param_targets()
        self.assertEqual(set(targets), set(['comp.x', 'comp.y']))

        driver.remove_parameter('comp.x')
        targets = driver.list_param_targets()
        self.assertEqual(targets, ['comp.y'])

        code = "driver.remove_parameter('comp.foo')"
        assert_raises(self, code, globals(), locals(), AttributeError,
                      "driver: Trying to remove parameter 'comp.foo'"
                      " that is not in this driver.")

        driver.add_parameter('comp.x', low=0., high=1.e99)
        driver.clear_parameters()
        targets = driver.list_param_targets()
        self.assertEqual(targets, [])

        driver.add_parameter('comp.y', low=0., high=1.e99)
        code = "driver.add_parameter('comp.y')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: ['comp.y'] are already Parameter targets")

    def test_named_params(self):
        driver = self.top.driver
        self.top.add('comp', Dummy())
        driver.add_parameter('comp.lst[1]', low=0., high=1.e99, name='foo')
        driver.add_parameter('comp.lst[3]', low=0., high=1.e99, name='bar')

        code = "driver.add_parameter('comp.x', name='foo')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: foo is already a Parameter")

        code = "driver.add_parameter('comp.lst[3]', name='blah')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: ['comp.lst[3]'] are already Parameter targets")

        targets = driver.list_param_targets()
        self.assertEqual(set(targets), set(['comp.lst[1]', 'comp.lst[3]']))

        driver.remove_parameter('bar')
        targets = driver.list_param_targets()
        self.assertEqual(targets, ['comp.lst[1]'])

    def test_metadata(self):
        driver = self.top.driver
        driver.add_parameter('comp.x', low=0., high=100, fd_step=.001)
        driver.add_parameter('comp.y', low=0., high=100)

        param = driver.get_parameters().values()

        self.assertEqual(param[0].low, 0.0)
        self.assertEqual(param[0].high, 100.0)
        self.assertEqual(param[0].fd_step, 0.001)
        self.assertEqual(param[1].fd_step, None)

    def test_get_metadata(self):
        p = Parameter('comp.x', low=0, high=1e99, scope=self.top)

        p.get_metadata()
        self.assertEqual(p.get_metadata(),
                         ('comp.x', {'high': None, 'iotype': 'in',
                                     'type': 'trait', 'low': None,
                                     'vartypename': 'Float',
                                     'assumed_default':False}))

        p2 = Parameter('comp.y', low=0, high=1e99, scope=self.top)
        pg = ParameterGroup([p, p2])
        self.assertEqual(pg.get_metadata(),
            [('comp.x', {'fd_step': None, 'name': 'comp.x', 'scaler': 1.0,
                         'high': 9.9999999999999997e+98, 'start': None,
                         'low': 0, 'adder': 0.0}),
             ('comp.y', {'fd_step': None, 'name': 'comp.x', 'scaler': 1.0,
                         'high': 9.9999999999999997e+98, 'start': None,
                         'low': 0, 'adder': 0.0})])

    def test_connected_input_as_parameter(self):
        self.top.add('comp2', ExecComp(exprs=['c=x+y', 'd=x-y']))
        self.top.driver.add_parameter('comp2.x', low=-99.0, high=99.9)

        self.top.connect('comp.c', 'comp2.x')
        assert_raises(self, "self.top.run()", globals(), locals(), RuntimeError,
                      ": The following parameters collide with connected inputs: comp2.x in driver")

        self.top.disconnect('comp.c', 'comp2.x')

        # try with parameter group
        self.top.driver.clear_parameters()
        self.top.driver.add_parameter(('comp2.x', 'comp2.y'),
                                       low=-99.0, high=99.9)
        self.top.connect('comp.c', 'comp2.x')
        assert_raises(self, "self.top.run()", globals(), locals(), RuntimeError,
                      ": The following parameters collide with connected inputs: comp2.x in driver")

    def test_transform(self):
        self.top.comp.x = 15.0
        self.top.comp.y = 7.0

        # Vars with no bounds params with bounds
        self.top.driver.add_parameter('comp.x', low=8.6, high=9.4, scaler=1.5, adder=1.)
        self.top.driver.add_parameter('comp.y', low=-6., high=10., scaler=4., adder=-2.)

        params = self.top.driver.get_parameters()

        d1val = params['comp.x'].evaluate()[0]
        d2val = params['comp.y'].evaluate()[0]
        self.assertEqual(d1val, 9.0)
        self.assertEqual(d2val, 3.75)

        self.top.run()

        uvec = self.top._system.vec['u']

        self.assertEqual(uvec['comp.x'][0], 15.)
        self.assertEqual(uvec['comp.y'][0], 7.0)

        params['comp.x'].set(9.0, self.top.driver)
        params['comp.y'].set(3.75, self.top.driver)

        self.assertEqual(uvec['comp.x'][0], 15.)
        self.assertEqual(uvec['comp.y'][0], 7.)

        self.top.driver.set_parameters([1.0, 3.0])

        self.assertEqual(uvec['comp.x'][0], 3.)
        self.assertEqual(uvec['comp.y'][0], 4.)

        self.top.driver.set_parameters([9.0, 3.75])

        self.assertEqual(uvec['comp.x'][0], 15.)
        self.assertEqual(uvec['comp.y'][0], 7.)


class ParametersTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', SimpleDriver())
        self.top.add('driver1', MyDriver())
        self.top.add('driver2', MyDriver())
        self.top.add('comp', ExecComp(exprs=['z=a+b+c+d']))
        self.top.driver.workflow.add(['driver1', 'driver2'])

    def test_group_get_referenced_vars_by_compname(self):
        self.top.driver.add_parameter(('comp.a', 'comp.b'), 0, 1e99)
        self.top.driver.add_parameter(('comp.c', 'comp.d'), 0, 1e99)
        params = self.top.driver.get_parameters()

        data = params[('comp.a', 'comp.b')].get_referenced_vars_by_compname()
        self.assertEqual(['comp'], data.keys())
        self.assertEqual(set([param.target for param in data['comp']]),
                         set(['comp.a', 'comp.b']))

        data = params[('comp.c', 'comp.d')].get_referenced_vars_by_compname()
        self.assertEqual(['comp'], data.keys())
        self.assertEqual(set([param.target for param in data['comp']]),
                         set(('comp.c', 'comp.d')))

        # Vars with bounds, params with no bounds
        self.top.comp.add_trait('v1', Float(0.0, low=0.0, high=10.0, iotype='in'))
        self.top.comp.v1 = 5.0
        self.top.driver.add_parameter('comp.v1', scaler=0.5, adder=-2.0)

        params = self.top.driver.get_parameters()
        self.assertEqual(params['comp.v1'].evaluate()[0], 12.0)
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
        self.top.comp.v1 = 15.

        self.top.comp.add_trait('v1', Float(0.0, low=-100.0, high=100.0, iotype='in'))
        self.top.driver.add_parameter('comp.v1', high=12.0, scaler=1.5)
        self.top.driver2.add_parameter('comp.v1', low=-6.0, adder=-2.)

        params = self.top.driver.get_parameters()
        params2 = self.top.driver2.get_parameters()

        d1val = params['comp.v1'].evaluate()[0]
        d2val = params2['comp.v1'].evaluate()[0]

        self.assertEqual(d1val, 10.0)
        self.assertEqual(d2val, 17.0)

    def test_transform_bad_wolf(self):
        code = "self.top.driver.add_parameter('comp.a', low=8.6, high=9.4, scaler='bad', adder=1.0)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Bad value given for parameter's 'scaler'"
                      " attribute.")

        code = "self.top.driver.add_parameter('comp.a', low=8.6, high=9.4, scaler=1.0, adder='wolf')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Bad value given for parameter's 'adder'"
                      " attribute.")


class ArrayComp(Component):

    x1d = Array(dtype=float, shape=(None,), iotype='in')
    x2d = Array(dtype=float, shape=(None, None), iotype='in')
    fx1d = Float(iotype='out')
    fx2d = Array(dtype=float, shape=(None,), iotype='out')

    def execute(self):
        self.fx1d = sum(self.x1d)
        self.fx2d = array([sum(x) for x in self.x2d[:]])


class ArrayTest(unittest.TestCase):

    def setUp(self):
        self.top = Assembly()
        self.top.add('driver', MyDriver())
        self.top.add('comp', ArrayComp())
        self.top.driver.workflow.add('comp')

    def test_basic(self):
        top = self.top
        comp = top.comp
        driver = top.driver

        comp.x1d = [1, 2, 3]
        comp.x2d = [[1, 2, 3], [4, 5, 6]]
        top.run()
        self.assertEqual(comp.fx1d, 6.)
        self.assertEqual(list(comp.fx2d), [6., 15.])

        code = "driver.add_parameter('comp.x1d')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Trying to add parameter 'comp.x1d', but no"
                      " lower limit was found and no 'low' argument was given."
                      " One or the other must be specified.")

        code = "driver.add_parameter('comp.x1d', low=-10)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Trying to add parameter 'comp.x1d', but no"
                      " upper limit was found and no 'high' argument was given."
                      " One or the other must be specified.")

        driver.add_parameter('comp.x1d', low=-10, high=10, start=1)

        code = "driver.add_parameter('comp.x2d')"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Trying to add parameter 'comp.x2d', but no"
                      " lower limit was found and no 'low' argument was given."
                      " One or the other must be specified.")

        code = "driver.add_parameter('comp.x2d', low=-10)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "driver: Trying to add parameter 'comp.x2d', but no"
                      " upper limit was found and no 'high' argument was given."
                      " One or the other must be specified.")

        driver.add_parameter('comp.x2d', low=-10, high=10, start=2)
        top.run()

        comp.x1d = [1, 2, 3]
        comp.x2d = [[1, 2, 3], [4, 5, 6]]

        targets = driver.list_param_targets()
        self.assertEqual(targets, ['comp.x1d', 'comp.x2d'])

        targets = driver.list_param_group_targets()
        self.assertEqual(targets, [('comp.x1d',), ('comp.x2d',)])

        self.assertEqual(list(comp.x1d), [1, 2, 3])
        self.assertEqual([list(row) for row in comp.x2d[:]],
                         [[1, 2, 3], [4, 5, 6]])

        driver.init_parameters()
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [1, 1, 1])
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [2, 2, 2, 2, 2, 2])

        driver.set_parameter_by_name('comp.x1d', 3)
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [3, 3, 3])

        driver.set_parameter_by_name('comp.x1d', array([4, 5, 6]))
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [4, 5, 6])

        driver.set_parameter_by_name('comp.x2d', 4)
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [4, 4, 4, 4, 4, 4])

        driver.set_parameter_by_name('comp.x2d', array([[5, 6, 7], [8, 9, 0]]))
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [5, 6, 7, 8, 9, 0])

        driver.set_parameters([7, 8, 9, 1, 2, 3, 4, 5, 6])
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [7, 8, 9])
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [1, 2, 3, 4, 5, 6])

        self.assertEqual(driver._hasparameters.get_expr_depends(),
                         [('driver', 'comp')])
        self.assertEqual(driver._hasparameters.get_referenced_compnames(),
                         set(['comp']))
        self.assertEqual(driver._hasparameters.get_referenced_varpaths(),
                         set(['comp.x1d', 'comp.x2d']))

        # Still have last set_parameters() values.
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [7, 8, 9])
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [1, 2, 3, 4, 5, 6])

        top.run()
        # Now have init_parameters() values.
        var = driver.workflow._system.vec['u']['comp.x1d']
        self.assertEqual(list(var), [1, 1, 1])
        var = driver.workflow._system.vec['u']['comp.x2d']
        self.assertEqual(list(var), [2, 2, 2, 2, 2, 2])

    def test_mixed_use(self):
        # Connect to one element of array and use another element as parameter.
        self.top.comp.x1d = [1., 2., 3.]
        self.top.add('exec_comp', ExecComp(exprs=['c=x+y', 'd=x-y']))
        self.top.driver.workflow.add('exec_comp')
        self.top.connect('exec_comp.c', 'comp.x1d[0]')
        self.top.driver.add_parameter('comp.x1d[1]', low=-10, high=10, start=1.)
        self.top.run()

    def test_set_boundary_params(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', MyDriver())

        class ArrayComp2(Component):

            x = Array([0.0, 0.0], iotype='in')
            y = Array([0.0, 0.0], iotype='in')
            c = Array([0.0, 0.0], iotype='out')

            def execute(self):
                self.c = self.x + self.y

        self.top.add('comp', ArrayComp2())
        self.top.driver.workflow.add('comp')
        self.top.create_passthrough('comp.x')
        self.top.create_passthrough('comp.c')
        self.top.connect('x', 'comp.y')

        self.top.driver.add_parameter(('x'), low=0., high=1e99)
        self.top.x = [22.0, 31.1]
        self.top.run()
        self.assertEqual(self.top.x[0], 22.)
        self.assertEqual(self.top.x[1], 31.1)
        self.assertEqual(self.top.comp.y[0], 22.)
        self.assertEqual(self.top.comp.y[1], 31.1)


if __name__ == "__main__":
    unittest.main()

