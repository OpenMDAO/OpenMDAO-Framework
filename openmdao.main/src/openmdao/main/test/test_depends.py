# pylint: disable-msg=C0111,C0103

import unittest
import math

from nose import SkipTest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error
from openmdao.main.depgraph import gsort

exec_order = []

@add_delegate(HasObjectives, HasParameters, HasConstraints)
class DumbDriver(Driver):
    def __init__(self):
        self.oldval = 11.
        super(DumbDriver, self).__init__()

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.oldval += 1.

        self.set_parameters([self.oldval]*len(self.get_parameters()))
        super(DumbDriver, self).execute()


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')

    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.c = self.a + self.b
        self.d = self.a - self.b
        #print "%s: a=%s, b=%s, c=%s, d=%s" % (self.get_pathname(),self.a,self.b,self.c,self.d)


allcomps = ['sub.comp1','sub.comp2','sub.comp3','sub.comp4','sub.comp5','sub.comp6',
            'comp7','comp8']

topouts = ['sub.c2', 'sub.c4', 'sub.d1', 'sub.d3','sub.d5'
           'comp7.c', 'comp7.d','comp8.c', 'comp8.d']

topins = ['sub.a1', 'sub.a3', 'sub.b2', 'sub.b4','sub.b6'
          'comp7.a', 'comp7.b','comp8.a', 'comp8.b']

subins = ['comp1.a', 'comp1.b',
          'comp2.a', 'comp2.b',
          'comp3.a', 'comp3.b',
          'comp4.a', 'comp4.b',
          'comp5.a', 'comp5.b',
          'comp6.a', 'comp6.b',]

subouts = ['comp1.c', 'comp1.d',
           'comp2.c', 'comp2.d',
           'comp3.c', 'comp3.d',
           'comp4.c', 'comp4.d',
           'comp5.c', 'comp5.d',
           'comp6.c', 'comp6.d',]


subvars = subins+subouts

class DbgAssembly(Assembly):
    def execute(self):
        # for name in ['a1','a3','b2','b4','b6']:
        #     print ", %s=%s" % (name, getattr(self,name)),
        # print ""
        super(DbgAssembly, self).execute()
        # print self.get_pathname(),
        # for name in ['c2','d5','d1','c4','d3']:
        #     print ", %s=%s" % (name, getattr(self,name)),
        # print ""

def fullvnames(cname, vnames):
    return ['.'.join([cname,n]) for n in vnames]

def _nested_model():
    global exec_order
    exec_order = []
    top = set_as_top(Assembly())
    top.add('sub', DbgAssembly())
    top.add('comp7', Simple())
    top.add('comp8', Simple())
    sub = top.sub
    sub.add('comp1', Simple())
    sub.add('comp2', Simple())
    sub.add('comp3', Simple())
    sub.add('comp4', Simple())
    sub.add('comp5', Simple())
    sub.add('comp6', Simple())

    top.driver.workflow.add(['comp7', 'sub', 'comp8'])
    sub.driver.workflow.add(['comp1','comp2','comp3',
                             'comp4','comp5','comp6'])

    sub.create_passthrough('comp1.a', 'a1')
    sub.create_passthrough('comp2.b', 'b2')
    sub.create_passthrough('comp3.a', 'a3')
    sub.create_passthrough('comp3.d', 'd3')
    sub.create_passthrough('comp4.b', 'b4')
    sub.create_passthrough('comp4.c', 'c4')
    sub.create_passthrough('comp6.b', 'b6')
    sub.create_passthrough('comp2.c', 'c2')
    sub.create_passthrough('comp1.d', 'd1')
    sub.create_passthrough('comp5.d', 'd5')

    return top

class DependsTestCase(unittest.TestCase):


    def setUp(self):
        top = self.top = _nested_model()
        sub = top.sub
        sub.connect('comp1.c', 'comp4.a')
        sub.connect('comp5.c', 'comp1.b')
        sub.connect('comp2.d', 'comp5.b')
        sub.connect('comp3.c', 'comp5.a')
        sub.connect('comp4.d', 'comp6.a')

        top.connect('sub.c4', 'comp8.a')
        top.connect('comp7.c', 'sub.a3')
        top.connect('sub.d3', 'comp8.b')

    def test_simple(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.driver.workflow.add('comp1')
        top.run()
        self.assertEqual(top.comp1.c, 3)
        self.assertEqual(top.comp1.d, -1)
        top.set('comp1.a', 5)
        top.run()
        self.assertEqual(top.comp1.c, 7)
        self.assertEqual(top.comp1.d, 3)
        top.run()

        # now add another comp and connect them
        top.add('comp2', Simple())
        top.driver.workflow.add('comp2')
        top.connect('comp1.c', 'comp2.a')
        self.assertEqual(top.comp2.c, 3)
        self.assertEqual(top.comp2.d, -1)
        top.run()
        self.assertEqual(top.comp2.c, 9)
        self.assertEqual(top.comp2.d, 5)

    def test_disconnect(self):
        self.top.disconnect('comp7.c', 'sub.comp3.a')
        self.top.sub.disconnect('c4')
        self.top.disconnect('comp8')

    def test_disconnect2(self):
        self.top.run()
        self.assertEqual(set(self.top._depgraph.list_outputs('sub', connected=True)),
                         set(['sub.d3','sub.c4']))
        self.top.disconnect('comp8')
        self.top._setup()
        self.assertEqual(self.top._depgraph.list_outputs('sub', connected=True),
                         [])

    def test_lazy1(self):
        self.top.run()
        exec_counts = [self.top.get(x).exec_count for x in allcomps]
        self.assertEqual([1, 1, 1, 1, 1, 1, 1, 1], exec_counts)

        cvars = ('a','b','c','d')

        expected = [
            ('comp7', cvars, (1.0, 2.0, 3.0, -1.0)),
            ('sub', ('a3','b2','a1','b4','b6'), (3.0, 2.0, 1.0, 2.0, 2.0)),
            ('sub.comp2', cvars, (1.0, 2.0, 3.0, -1.0)),
            ('sub.comp3', cvars, (3.0, 2.0, 5.0, 1.0)),
            ('sub.comp5', cvars, (5.0, -1.0, 4.0, 6.0)),
            ('sub.comp1', cvars, (1.0, 4.0, 5.0, -3.0)),
            ('sub.comp4', cvars, (5.0, 2.0, 7.0, 3.0)),
            ('sub.comp6', cvars, (3.0, 2.0, 5.0, 1.0)),
            ('sub', ('c2','d5','d1','c4','d3'), (3.0, 6.0, -3.0, 7.0, 1.0)),
            ('comp8', cvars, (7.0, 1.0, 8.0, 6.0)),
        ]

        for cname, vnames, vals in expected:
            comp = self.top.get(cname)
            for name, val in zip(vnames, vals):
                self.assertEqual(getattr(comp, name), val)

    def test_lazy2(self):
        self.top.run()
        self.top.sub.b6 = 3
        self.top.run()
        outs = [(5,-3),(3,-1),(5,1),(7,3),(4,6),(6,0),(3,-1),(8,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]),
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))

    def test_lazy3(self):
        self.top.run()
        self.top.comp7.a = 3
        self.top.run()
        outs = [(7,-5),(3,-1),(7,3),(9,5),(6,8),(7,3),(5,1),(12,6)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]),
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))

    def test_lazy4(self):
        self.top.run()
        self.top.sub.set('b2', 5)
        self.top.run()
        outs = [(2,0),(6,-4),(5,1),(4,0),(1,9),(2,-2),(3,-1),(5,3)]
        for comp,vals in zip(allcomps,outs):
            self.assertEqual((comp,vals[0],vals[1]),
                             (comp,self.top.get(comp+'.c'),self.top.get(comp+'.d')))

    def test_sequential(self):
        # verify that if components aren't connected they should execute in the
        # order that they were added to the workflow instead of hash order
        global exec_order
        top = set_as_top(Assembly())
        top.add('c2', Simple())
        top.add('c1', Simple())
        top.add('c3', Simple())
        top.add('c4', Simple())
        top.driver.workflow.add(['c1','c2','c3','c4'])
        top.run()
        self.assertEqual(exec_order, ['c1','c2','c3','c4'])
        top.connect('c4.c', 'c3.a')  # now make c3 depend on c4
        exec_order = []
        #top.c4.a = 2  # makes c4 run again
        top.run()
        self.assertEqual(exec_order, ['c1','c2','c4','c3'])

    def test_expr_deps(self):
        raise SkipTest("FIXME when manual execution out of data order becomes a priority")
        top = set_as_top(Assembly())
        top.add('driver1', DumbDriver())
        top.add('driver2', DumbDriver())
        top.add('c1', Simple())
        top.add('c2', Simple())
        top.add('c3', Simple())

        top.driver.workflow.add(['driver1','driver2','c3'])
        top.driver1.workflow.add('c2')
        top.driver2.workflow.add('c1')

        top.connect('c1.c', 'c2.a')
        top.driver1.add_objective("c2.c*c2.d")
        top.driver2.add_objective("c1.c")
        #from openmdao.util.dotgraph import plot_graph
        #plot_graph(top._depgraph)
        top.run()
        # FIXME: without lazy evaluation, c1 runs in the wrong order
        self.assertEqual(exec_order, ['driver1','c2','driver2','c1','c3'])

    def test_force_with_input_updates(self):
        top = set_as_top(Assembly())
        top.add('c2', Simple())
        top.add('c1', Simple())
        top.connect('c1.c', 'c2.a')
        top.driver.workflow.add(['c1','c2'])
        top.run()
        self.assertEqual(top.c2.a, 3)
        top.c1.a = 2
        top.run()
        self.assertEqual(top.c2.a, 4)

    def test_get_required_compnames(self):
        sub = self.top.sub
        sub.add('driver', DumbDriver())
        sub.driver.add_objective('comp6.c')
        sub.driver.add_objective('comp5.d')
        self.top._setup()
        self.assertEqual(sub.driver._get_required_compnames(),
                         set(['comp5', 'comp6', '_pseudo_0', '_pseudo_1']))
        sub.driver.add_parameter('comp2.a', low=0.0, high=10.0)
        self.assertEqual(sub.driver._get_required_compnames(),
                         set(['comp2', 'comp5', 'comp1', 'comp4', 'comp6', '_pseudo_0', '_pseudo_1']))
        sub.driver.add_parameter('comp3.b', low=0.0, high=10.0)
        self.assertEqual(sub.driver._get_required_compnames(),
                         set(['comp6','comp5','comp1','comp4','comp3', 'comp2', '_pseudo_0', '_pseudo_1']))

    def test_auto_workflow(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.add('comp2', Simple())
        top.add('comp3', Simple())
        top.add('driver', DumbDriver())
        top.driver.add_parameter('comp2.a',low=-99,high=99)
        top.driver.add_objective('comp3.c')
        top.connect('comp1.c', 'comp2.b')
        top.connect('comp2.c', 'comp3.a')

        self.assertEqual(top.comp1.exec_count, 0)
        self.assertEqual(top.comp2.exec_count, 0)
        self.assertEqual(top.comp3.exec_count, 0)
        top.run()
        self.assertEqual(top.comp1.exec_count, 1)
        self.assertEqual(top.comp2.exec_count, 1)
        self.assertEqual(top.comp3.exec_count, 1)
        top.run()
        self.assertEqual(top.comp1.exec_count, 2)
        self.assertEqual(top.comp2.exec_count, 2)
        self.assertEqual(top.comp3.exec_count, 2)

class ArrSimple(Component):
    ain  = Array([0.,1.,2.,3.], iotype='in')
    aout = Array([0.,1.,2.,3.], iotype='out')
    ain2  = Array([0.,1.,2.,3.], iotype='in')
    aout2 = Array([0.,1.,2.,3.], iotype='out')


    def __init__(self):
        super(ArrSimple, self).__init__()

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.aout = self.ain * 2.0
        self.aout2 = self.ain2 * 0.5


class SimplePTAsm(Assembly):
    def configure(self):
        self.add('c2', Simple())
        self.add('c1', Simple())

        self.driver.workflow.add(['c1','c2'])

        self.connect('c1.c', 'c2.a')
        self.connect('c1.d', 'c2.b')

        self.create_passthrough('c1.a', 'a1')
        self.create_passthrough('c2.d', 'd2')


class DependsTestCase2(unittest.TestCase):

    def setUp(self):
        global exec_order
        self.top = set_as_top(Assembly())
        self.top.add('c2', Simple())
        self.top.add('c1', Simple())
        self.top.driver.workflow.add(['c1','c2'])
        self.top.run()

    def test_connected_vars(self):
        self.assertEqual(self.top._depgraph.list_outputs('c1', connected=True), [])
        self.assertEqual(self.top._depgraph.list_outputs('c2', connected=True), [])
        self.top.connect('c1.c', 'c2.a')
        self.top._setup()
        self.assertEqual(self.top._depgraph.list_outputs('c1', connected=True), ['c1.c'])
        self.assertEqual(self.top._depgraph.list_inputs('c2', connected=True), ['c2.a'])
        self.top.connect('c1.d', 'c2.b')
        self.top._setup()
        self.assertEqual(set(self.top._depgraph.list_outputs('c1', connected=True)), set(['c1.c', 'c1.d']))
        self.assertEqual(set(self.top._depgraph.list_inputs('c2', connected=True)), set(['c2.a', 'c2.b']))
        self.top.disconnect('c1.d', 'c2.b')
        self.top._setup()
        self.assertEqual(self.top._depgraph.list_outputs('c1', connected=True), ['c1.c'])
        self.assertEqual(self.top._depgraph.list_inputs('c2', connected=True), ['c2.a'])

    def test_unconnected_vars(self):
        c1extras = set(['.'.join(('c1',n)) for n in self.top.c1.list_vars()])-set(['c1.a','c1.b','c1.c','c1.d'])
        c2extras = set(['.'.join(('c2',n)) for n in self.top.c2.list_vars()])-set(['c2.a','c2.b','c2.c','c2.d'])
        self.assertEqual(set(self.top._depgraph.list_outputs('c1', connected=False))-c1extras, set(['c1.c', 'c1.d']))
        self.assertEqual(set(self.top._depgraph.list_inputs('c2', connected=False))-c2extras, set(['c2.a', 'c2.b']))
        self.top.connect('c1.c', 'c2.a')
        self.top._setup()
        self.assertEqual(set(self.top._depgraph.list_outputs('c1', connected=False))-c1extras, set(['c1.d']))
        self.assertEqual(set(self.top._depgraph.list_inputs('c2', connected=False))-c2extras, set(['c2.b']))
        self.top.connect('c1.d', 'c2.b')
        self.top._setup()
        self.assertEqual(set(self.top._depgraph.list_outputs('c1', connected=False))-c1extras, set())
        self.assertEqual(set(self.top._depgraph.list_inputs('c2', connected=False))-c2extras, set())
        self.top.disconnect('c1.d', 'c2.b')
        self.top._setup()
        self.assertEqual(set(self.top._depgraph.list_outputs('c1', connected=False))-c1extras, set(['c1.d']))
        self.assertEqual(set(self.top._depgraph.list_inputs('c2', connected=False))-c2extras, set(['c2.b']))

    def test_simple_run(self):
        self.top.connect('c1.c', 'c2.a')
        self.top.connect('c1.d', 'c2.b')
        self.top.run()
        self.assertEqual(self.top.c1.a, 1)
        self.assertEqual(self.top.c1.b, 2)
        self.assertEqual(self.top.c1.c, 3)
        self.assertEqual(self.top.c1.d, -1)
        self.assertEqual(self.top.c2.a, 3)
        self.assertEqual(self.top.c2.b, -1)
        self.assertEqual(self.top.c2.c, 2)
        self.assertEqual(self.top.c2.d, 4)

        self.top.c1.a = 2
        self.top.run()
        self.assertEqual(self.top.c1.a, 2)
        self.assertEqual(self.top.c1.b, 2)
        self.assertEqual(self.top.c1.c, 4)
        self.assertEqual(self.top.c1.d, 0)
        self.assertEqual(self.top.c2.a, 4)
        self.assertEqual(self.top.c2.b, 0)
        self.assertEqual(self.top.c2.c, 4)
        self.assertEqual(self.top.c2.d, 4)

    def test_simple_passthrough(self):
        self.top.add('model', SimplePTAsm())
        self.top.driver.workflow.add(['model'])
        self.top.connect('c1.c', 'model.a1')
        self.top.connect('model.d2', 'c2.a')

        self.top.run()


    def test_array_expr(self):
        class Dummy(Component):

            x = Array([[-1., 1.],[-2., 2.]],iotype="in",shape=(2,2), dtype='f')
            y = Array([[-1., 1.],[-2., 2.]],iotype="out",shape=(2,2), dtype='f')

            def execute(self):
                self.y = self.x

        class Stuff(Assembly):

            def configure(self):
                self.add('d1',Dummy())
                self.add('d2',Dummy())

                self.connect('d1.y[0][0]','d2.x[1][0]')
                self.connect('d1.y[1][0]','d2.x[0][0]')

                self.driver.workflow.add(['d1','d2'])

        s = set_as_top(Stuff())
        s.d1.x = [[-5,-6], [-7,-8]]
        s.run()
        self.assertEqual(s.d2.x[0,0], -7)
        self.assertEqual(s.d2.x[1,0], -5)
        self.assertEqual(s.d2.x[0,1], 1)
        self.assertEqual(s.d2.x[1,1], 2)

    def test_array2(self):
        top = set_as_top(Assembly())
        top.add('c1', ArrSimple())
        top.add('c3', ArrSimple())
        top.driver.workflow.add(['c1','c3'])
        top.connect('c1.aout[1]', 'c3.ain[2]')

        top.run()

        top.c1.ain = [55.,44.,33.]

        top.run()
        self.assertEqual(top.c3.ain[2], 88.)

    def test_array3(self):
        top = set_as_top(Assembly())
        top.add('c1', ArrSimple())
        top.add('sub',Assembly())
        top.sub.add('c2',ArrSimple())
        top.sub.create_passthrough('c2.ain')
        top.sub.create_passthrough('c2.ain2')
        top.sub.create_passthrough('c2.aout')
        top.sub.create_passthrough('c2.aout2')
        top.add('c3', ArrSimple())
        top.driver.workflow.add(['c1','sub', 'c3'])
        top.sub.driver.workflow.add('c2')
        top.connect('c1.aout[1]', 'sub.ain[1]')
        top.connect('sub.aout[1]', 'c3.ain[1]')

        top.c1.ain = [55.,44.,33.]

        top.run()

        self.assertEqual(top.c1.aout[1], 88.)
        self.assertEqual(top.sub.ain[1], 88.)
        self.assertEqual(top.sub.c2.ain[1], 88.)
        self.assertEqual(top.sub.aout[1], 176.)
        self.assertEqual(top.c3.ain[1], 176.)


    def test_units(self):
        top = self.top
        top.c2.add("velocity", Float(3.0, iotype='in', units='inch/s'))
        top.c1.add("length", Float(9.0, iotype='out', units='inch'))

        top.connect('c1.c', 'c2.velocity')
        try:
            top._setup()
        except Exception as err:
            self.assertEqual(str(err),
                             ": Can't connect 'c1.c' to 'c2.velocity': : Incompatible units for 'c1.c' and 'c2.velocity': units 'ft' are incompatible with assigning units of 'inch/s'")
        else:
            self.fail("Exception expected")

        top.disconnect('c1.c', 'c2.velocity')

        top.c1.a = 1.
        top.c1.b = 2.
        top.c1.length = 24.
        top.connect('c1.length', 'c2.a')
        top.run()
        assert_rel_error(self, top.c2.a, 2., 0.0001)


class DependsTestCase3(unittest.TestCase):

    def test_input_pseudocomp(self):
        top = set_as_top(Assembly())
        top.add('comp', ArrayComp())
        top.add('driver', DumbDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.a[0]', low=-100, high=100)
        top.driver.add_constraint('comp.a[0] < 100')

        # The first time it runs, the pcomp inputs update
        top.run()
        self.assertEqual(top.comp.a[0], top._pseudo_0.in0)

        # The second time it runs, the pcomp inputs no longer update
        top.run()
        self.assertEqual(top.comp.a[0], top._pseudo_0.in0)

class ArrayComp(Component):
    a = Array([1.,2.,3.,4.,5.], dtype=float, iotype="in")
    b = Array([1.,2.,3.,4.,5.], dtype=float, iotype='in')
    c = Array([2.,4.,6.,8.,10.], dtype=float, iotype='out')
    d = Array([0.,0.,0.,0.,0.], dtype=float, iotype='out')

    def execute(self):
        global exec_order
        exec_order.append(self.name)
        self.c = self.a + self.b
        self.d = self.a - self.b

class ExprDependsTestCase(unittest.TestCase):

    def setUp(self):
        global exec_order
        exec_order = []
        self.top = set_as_top(Assembly())
        self.top.add('c2', ArrayComp())
        self.top.add('c1', ArrayComp())
        self.top.driver.workflow.add(['c1','c2'])

    def test_basic(self):
        self.top.connect('c1.c', 'c2.a')
        self.top.connect('c1.d', 'c2.b')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [2,4,6,8,10])
        self.assertEqual(list(self.top.c2.b), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c2.d), [2,4,6,8,10])

    def test_entry_connect(self):
        self.top.connect('c1.c[2]', 'c2.a[3]')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [1,2,3,6,5])
        self.assertEqual(list(self.top.c2.b), [1,2,3,4,5])
        self.assertEqual(list(self.top.c2.c), [2,4,6,10,10])
        self.assertEqual(list(self.top.c2.d), [0,0,0,2,0])

        # now see if we can connect to another entry on c2.a
        self.top.connect('c1.d[2]', 'c2.a[1]')
        self.top.run()
        self.assertEqual(list(self.top.c1.c), [2,4,6,8,10])
        self.assertEqual(list(self.top.c1.d), [0,0,0,0,0])
        self.assertEqual(list(self.top.c2.a), [1,0,3,6,5])
        self.assertEqual(list(self.top.c2.b), [1,2,3,4,5])
        self.assertEqual(list(self.top.c2.c), [2,2,6,10,10])
        self.assertEqual(list(self.top.c2.d), [0,-2,0,2,0])

        # make sure only one connection allowed to a particular array entry
        try:
            self.top.connect('c1.d[1]', 'c2.a[1]')
        except Exception as err:
            self.assertEqual(str(err), ": Can't connect 'c1.d[1]' to 'c2.a[1]': : 'c2.a[1]' is already connected to source 'c1.d[2]'")


    def test_invalidation(self):
        self.top.run()
        self.top.connect('c1.c[2]', 'c2.a[3]')
        self.top.run()
        self.top.c1.a = [9,9,9,9,9]
        self.top.run()
        self.assertEqual(list(self.top.c2.a), [1,2,3,12,5])

    def test_simple_src_expr(self):
        # simple c1.c+c2.c -> c3.a
        top = set_as_top(Assembly())
        top.add('c1', Simple())
        top.add('c2', Simple())
        top.add('c3', Simple())
        top.driver.workflow.add(['c1','c2','c3'])
        top.connect('c1.c+c2.c', 'c3.a')
        top.run()
        self.assertEqual(top.c1.c+top.c2.c, 6)
        self.assertEqual(top.c3.a, 6)

    def test_simple_src_expr_sub(self):
        # simple c1.c+c2.c -> c3.a in a subassembly
        top = set_as_top(Assembly())
        sub = top.add('sub', Assembly())
        top.driver.workflow.add('sub')

        sub.add('c1', Simple())
        sub.add('c2', Simple())
        sub.add('c3', Simple())
        sub.driver.workflow.add(['c1','c2','c3'])
        sub.connect('c1.c+c2.c', 'c3.a')
        top.run()
        self.assertEqual(sub.c1.c+sub.c2.c, 6)
        self.assertEqual(sub.c3.a, 6)

    def test_src_exprs(self):
        top = _nested_model()
        top.run()

        total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        top.sub.connect('comp1.c+comp2.c+comp3.c', 'comp4.a')
        top.run()
        self.assertEqual(total, top.sub.comp4.a)

        top.sub.comp2.a = 99
        top.sub.run()
        total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        self.assertEqual(total, top.sub.comp4.a)
        top.sub.comp2.a = 88
        top.comp7.a = 11
        top.sub.run()
        total = top.sub.comp1.c+top.sub.comp2.c+top.sub.comp3.c
        self.assertEqual(total, top.sub.comp4.a)

    def test_float_exprs(self):
        top = _nested_model()
        top.run()

        total = math.sin(3.14)*top.sub.comp2.c
        top.sub.connect('sin(3.14)*comp2.c', 'comp4.a')
        top.run()
        self.assertEqual(total, top.sub.comp4.a)

        top.sub.disconnect('sin(3.14)*comp2.c', 'comp4.a')
        total = 3.0*top.sub.comp1.c
        top.sub.connect('3.0*comp1.c', 'comp4.a')
        top.run()
        self.assertEqual(total, top.sub.comp4.a)

    def test_slice_exprs(self):
        top = self.top
        top.run()
        total = top.c1.c[3:]
        top.connect('c1.c[3:]', 'c2.a[0:2]')
        top.run()
        self.assertEqual(list(total), list(top.c2.a[0:2]))

    def _all_nested_connections(self, obj):
        """Return a list of all connections all the way down."""
        visited = set()
        connection_set = set()
        objstack = [obj]
        while objstack:
            obj = objstack.pop()
            if obj not in visited:
                visited.add(obj)
                if isinstance(obj, Assembly):
                    connection_set.update(obj.list_connections())
                    for name in obj.list_containers():
                        comp = getattr(obj, name)
                        if isinstance(comp, Assembly):
                            if isinstance(comp, Assembly):
                                objstack.append(comp)
        return connection_set

    def test_connection_cleanup(self):
        global exec_order
        top = _nested_model()
        top.run()
        initial_connections = set(top.sub.list_connections())
        top.sub.connect('comp1.c', 'comp3.b')
        self.assertEqual(set(top.sub.list_connections())-initial_connections,
                         set([('comp1.c','comp3.b')]))
        top.sub.disconnect('comp1')
        self.assertEqual(set(top.sub.list_connections())-initial_connections, set())
        for u,v in self._all_nested_connections(top.sub):
            self.assertTrue('comp1.' not in u and 'comp1.' not in v)

    def test_connection_cleanup2(self):
        top = _nested_model()
        initial_connections = set(top.sub.list_connections())
        top.run()
        top.sub.connect('comp1.c*3.0', 'comp4.a')
        top.sub.connect('comp1.c', 'comp3.b')
        top.sub.disconnect('comp1.c','comp3.b')
        top._setup()
        self.assertEqual(set(top.sub.list_connections())-initial_connections,
                         set([('comp1.c*3.0', 'comp4.a')]))
        self.assertEqual(initial_connections-set(top.sub.list_connections()),
                         set())
        self.assertEqual(set(top.sub.list_connections())-initial_connections,
                         set([('comp1.c*3.0', 'comp4.a')]))
        for u,v in self._all_nested_connections(top.sub):
            self.assertTrue(not ('comp1.c' in u and 'comp3.b' in v))

    def test_bad_exprs(self):
        top = _nested_model()
        try:
            top.sub.connect('comp1.c', 'comp4.a+comp4.b')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a+comp4.b': : bad connected expression 'comp4.a+comp4.b' must reference exactly one variable")
        else:
            self.fail("Exception expected")

        try:
            top.sub.connect('comp1.c', 'comp4.a[foo]')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a[foo]': : bad destination expression 'comp4.a[foo]': only constant indices are allowed for arrays and slices")
        else:
            self.fail("Exception expected")

        try:
            top.sub.connect('comp1.c', 'comp4.a(5)')
        except Exception as err:
            self.assertEqual(str(err), "sub: Can't connect 'comp1.c' to 'comp4.a(5)': : bad destination expression 'comp4.a(5)': not assignable")
        else:
            self.fail("Exception expected")


class SortingTestCase(unittest.TestCase):
    def test_sort(self):
        top = set_as_top(Assembly())
        for i in range(11):
            top.add('c%d'%i, Simple())

        # connect two independent strings of comps to test sorting
        top.connect('c1.c', 'c2.a')
        top.connect('c2.c', 'c3.a')
        top.connect('c3.c', 'c4.a')
        top.connect('c4.c', 'c5.a')

        top.connect('c6.c', 'c7.a')
        top.connect('c7.c', 'c8.a')
        top.connect('c8.c', 'c9.a')
        top.connect('c9.c', 'c10.a')

        wfnames = ['c10','c5', 'c9', 'c4', 'c8', 'c3', 'c7', 'c2', 'c6', 'c1']
        top.driver.workflow.add(wfnames)

        top._setup()

        names = gsort(top.driver.workflow._reduced_graph.component_graph(), wfnames)

        self.assertEqual(names, ['c6', 'c7', 'c8', 'c9', 'c10', 'c1', 'c2', 'c3', 'c4', 'c5'])

    def test_sort2(self):
        top = set_as_top(Assembly())
        for i in range(11):
            top.add('c%d'%i, Simple())

        # connect two independent strings of comps to test sorting
        top.connect('c1.c', 'c2.a')
        top.connect('c2.c', 'c3.a')
        top.connect('c3.c', 'c4.a')
        top.connect('c4.c', 'c5.a')

        top.connect('c6.c', 'c7.a')
        top.connect('c7.c', 'c8.a')
        top.connect('c8.c', 'c9.a')
        top.connect('c9.c', 'c10.a')

        wfnames = ['c10', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9']
        top.driver.workflow.add(wfnames)

        top._setup()

        names = gsort(top.driver.workflow._reduced_graph.component_graph(), wfnames)

        self.assertEqual(names, ['c6', 'c7', 'c8', 'c9', 'c10', 'c1', 'c2', 'c3', 'c4', 'c5'])

    def test_sort_keep_orig_order2(self):
        top = set_as_top(Assembly())
        for i in range(11):
            top.add('c%d'%i, Simple())

        # connect two independent strings of comps to test sorting
        top.connect('c1.c', 'c2.a')
        top.connect('c2.c', 'c3.a')
        top.connect('c3.c', 'c4.a')
        top.connect('c4.c', 'c5.a')

        top.connect('c6.c', 'c7.a')
        top.connect('c7.c', 'c8.a')
        top.connect('c8.c', 'c9.a')
        top.connect('c9.c', 'c10.a')

        wfnames = ['c6', 'c7', 'c8', 'c9', 'c10', 'c1', 'c2', 'c3', 'c4', 'c5']
        top.driver.workflow.add(wfnames)

        top._setup()

        names = gsort(top.driver.workflow._reduced_graph.component_graph(), wfnames)

        self.assertEqual(names, wfnames)

        top.driver.workflow.clear()
        wfnames = ['c6', 'c1', 'c7', 'c2', 'c8', 'c3', 'c9', 'c4', 'c10', 'c5']
        top.driver.workflow.add(wfnames)

        top._setup()

        names = gsort(top.driver.workflow._reduced_graph.component_graph(), wfnames)

        self.assertEqual(names, wfnames)


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
