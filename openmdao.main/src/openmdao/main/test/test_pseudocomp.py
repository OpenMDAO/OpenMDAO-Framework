
import unittest

import ast

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.pseudocomp import unit_xform
from openmdao.units.units import PhysicalQuantity
from openmdao.main.printexpr import print_node
from openmdao.util.testutil import assert_rel_error

class Simple(Component):
    a = Float(iotype='in', units='inch')
    b = Float(iotype='in', units='inch')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')
    dist = Float(iotype='out', units='ft')
    time = Float(iotype='out', units='s')
    speed = Float(iotype='in', units='inch/s')
    arr = Array([1.,2.,3.], iotype='out', units='ft')

    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = PhysicalQuantity(self.a + self.b, 'inch').in_units_of('ft').value
        self.d = PhysicalQuantity(self.a - self.b, 'inch').in_units_of('ft').value

class SimpleNoUnits(Component):
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    arr = Array([1.,2.,3.], iotype='out')

    def __init__(self):
        super(SimpleNoUnits, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

class MuComp(Component):
    mu = Float(1.81206e-5, iotype='in', units='kg/(m*s)')
    out = Float(iotype='out')

    def execute(self):
        self.out = self.mu

class MuAsm(Assembly):
    mu = Float(1.81206e-5, iotype='in', units='kg/m/s')


def _simple_model(units=True):
    if units:
        klass = Simple
    else:
        klass = SimpleNoUnits
    top = set_as_top(Assembly())
    top.add("comp1", klass())
    top.add("comp2", klass())
    top.driver.workflow.add(['comp1','comp2'])
    top.connect("comp1.c", "comp2.a")
    return top

class PseudoCompTestCase(unittest.TestCase):

    def test_unnecessary_pcomp(self):
        top = set_as_top(MuAsm())
        top.add('comp', MuComp())
        top._setup()
        nodes = set(top._depgraph.nodes())
        top.connect("mu", "comp.mu") # connect two vars with same units but diff unit strings
        self.assertEqual(set(), set(top._depgraph.nodes())-nodes)

    def test_basic_nounits(self):
        top = _simple_model(units=False)
        top._setup()
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', 'driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('comp1.c', 'comp2.a')]))

    def test_basic_units(self):
        top = _simple_model()
        top._setup()
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2','_pseudo_0', 'driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_0.out0', 'comp2.a'),
                              ('comp1.c', '_pseudo_0.in0')]))

        self.assertEqual(top._pseudo_0._orig_expr, "comp1.c 'ft' -> comp2.a 'inch'")
        top.comp1.a = 12.
        top.comp1.b = 24.
        top.run()
        self.assertAlmostEqual(top.comp1.c, 3.)
        self.assertAlmostEqual(top.comp2.a, 36.)

    def test_multi_src(self):
        top = _simple_model()  # comp1.c --> comp2.a
        top.connect('comp1.dist/comp1.time', 'comp2.speed')
        top.comp1.dist = 10.
        top.comp1.time = 5.
        # dist/time = 2 ft/sec
        top.run()
        #top._system.dump()
        self.assertAlmostEqual(top.comp2.speed, 24.) # speed = 24 inch/s

        self.assertTrue(hasattr(top, '_pseudo_0'))
        self.assertTrue(hasattr(top, '_pseudo_1'))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.dist/comp1.time', 'comp2.speed'),
                              ('comp1.c', 'comp2.a')]))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', 'driver',
                              '_pseudo_0', '_pseudo_1']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_0.out0', 'comp2.a'), ('comp1.c', '_pseudo_0.in0'),
                              ('comp1.dist', '_pseudo_1.in0'), ('comp1.time', '_pseudo_1.in1'),
                              ('_pseudo_1.out0', 'comp2.speed')]))

        # disconnect two linked expressions
        top.disconnect('comp1.dist/comp1.time')
        top._setup() # this results in new pseudos for unit connection and expr connection
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_2.out0', 'comp2.a'), ('comp1.c', '_pseudo_2.in0')]))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', '_pseudo_2','driver']))
        self.assertTrue(hasattr(top, '_pseudo_2'))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.c', 'comp2.a')]))

        top.connect('comp1.dist/comp1.time', 'comp2.speed')
        top._setup()
        self.assertTrue(hasattr(top, '_pseudo_3'))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', '_pseudo_3', '_pseudo_4','driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_3.out0', 'comp2.a'), ('comp1.c', '_pseudo_3.in0'),
                              ('comp1.dist', '_pseudo_4.in0'), ('comp1.time', '_pseudo_4.in1'),
                              ('_pseudo_4.out0', 'comp2.speed')]))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.dist/comp1.time', 'comp2.speed'),
                              ('comp1.c', 'comp2.a')]))

        # disconnect a single variable
        top.disconnect('comp1.dist')
        top._setup()
        self.assertFalse(hasattr(top, '_pseudo_4'))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', '_pseudo_5','driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_5.out0', 'comp2.a'), ('comp1.c', '_pseudo_5.in0')]))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.c', 'comp2.a')]))

        top.connect('comp1.dist/comp1.time', 'comp2.speed')
        top._setup()
        self.assertTrue(hasattr(top, '_pseudo_7'))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', '_pseudo_6', '_pseudo_7','driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_6.out0', 'comp2.a'), ('comp1.c', '_pseudo_6.in0'),
                              ('comp1.dist', '_pseudo_7.in0'), ('comp1.time', '_pseudo_7.in1'),
                              ('_pseudo_7.out0', 'comp2.speed')]))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.dist/comp1.time', 'comp2.speed'),
                              ('comp1.c', 'comp2.a')]))

        # disconnect a whole component
        top.disconnect('comp2')
        top._setup()
        self.assertFalse(hasattr(top, '_pseudo_7'))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['driver', 'comp2', 'comp1']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([]))
        self.assertEqual(set(top.list_connections()),
                         set())

    def test_multi_src_arr(self):
        top = _simple_model()  # comp1.c --> comp2.a
        top.connect('comp1.arr[1]/comp1.time', 'comp2.speed')
        top.comp1.arr[1] = 10.
        top.comp1.time = 5.
        # arr[1]/time = 2 ft/sec
        top.run()
        self.assertAlmostEqual(top.comp2.speed, 24.) # speed = 24 inch/s

        self.assertTrue(hasattr(top, '_pseudo_0'))
        self.assertTrue(hasattr(top, '_pseudo_1'))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.arr[1]/comp1.time', 'comp2.speed'),
                              ('comp1.c', 'comp2.a')]))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1','comp2', 'driver',
                              '_pseudo_0', '_pseudo_1']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_0.out0', 'comp2.a'), ('comp1.c', '_pseudo_0.in0'),
                              ('comp1.arr[1]', '_pseudo_1.in0'), ('comp1.time', '_pseudo_1.in1'),
                              ('_pseudo_1.out0', 'comp2.speed')]))

        # disconnect a single variable
        top.disconnect('comp1.arr[1]')
        top._setup()
        self.assertFalse(hasattr(top, '_pseudo_0'))
        self.assertFalse(hasattr(top, '_pseudo_1'))
        self.assertTrue(hasattr(top, '_pseudo_2'))
        self.assertEqual(set(top._depgraph.component_graph().nodes()),
                         set(['comp1', 'comp2',  '_pseudo_2', 'driver']))
        self.assertEqual(set(top._depgraph.list_connections()),
                         set([('_pseudo_2.out0', 'comp2.a'), ('comp1.c', '_pseudo_2.in0')]))
        self.assertEqual(set(top.list_connections()),
                         set([('comp1.c', 'comp2.a')]))

    def test_multi_src_boundary_var(self):
        top = _simple_model()  # comp1.c --> comp2.a
        top.add('arr', Array([1.,2.,3.,4.], iotype='in', units='ft'))
        top.add('spd_out', Float(0., iotype='out', units='inch/s'))

        top.connect('arr[1]/comp1.time', 'spd_out')
        top.arr[1] = 10.
        top.comp1.time = 5.
        # arr[1]/time = 2 ft/sec
        top.run()
        self.assertAlmostEqual(top.spd_out, 24.) # spd_out = 24 inch/s



    # disconnect() for a boundary var in an expr


class Comp1(Component):

    a = Array([0,0], iotype="in")
    b = Float(iotype="out")

    def execute(self):

        self.b = (self.a[0]+self.a[1]-1)**2

class SubAsmb(Assembly):

    x = Array([1.,1.], iotype="in")
    y = Float(2, iotype="in")

    z = Float(iotype="out")

    def configure(self):

        self.add('comp', Comp1())
        self.connect('x+y', 'comp.a')
        self.connect('comp.b', 'z')
        self.driver.workflow.add('comp')

class Test_Pseudo_Deriv(unittest.TestCase):

    def test_scaler_array_expression(self):

        model = Assembly()
        model.add('sub', SubAsmb())
        model.driver.workflow.add('sub')
        model.run()
        J = model.driver.calc_gradient(inputs=['sub.x', 'sub.y'],
                                       outputs=['sub.z'])

        assert_rel_error(self, J[0,0], 10.0, .001)
        assert_rel_error(self, J[0,1], 10.0, .001)
        assert_rel_error(self, J[0,2], 20.0, .001)

class UnitXformerTestCase(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_simple_conversion(self):
        node = ast.parse('a')
        cnv = unit_xform(node, 'ft', 'inch')
        newexpr = print_node(cnv)
        self.assertEqual(newexpr, 'a*12.0')

    def test_scaler_adder_conversion(self):
        node = ast.parse('a')
        cnv = unit_xform(node, 'degC', 'degF')
        newexpr = print_node(cnv)
        self.assertEqual(newexpr, '(a+17.7777777778)*1.8')



if __name__ == '__main__':
    unittest.main()


