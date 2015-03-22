# pylint: disable-msg=C0111,C0103

import cStringIO
import os
import shutil
import unittest
import logging

from openmdao.main.api import Assembly, Component, Driver, Workflow, \
                              set_as_top, SimulationRoot, VariableTree
from openmdao.main.datatypes.api import Float, Instance, Int, Str, List, Array, VarTree
from openmdao.util.log import enable_trace, disable_trace
from openmdao.util.fileutil import onerror
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasobjective import HasObjective


@add_delegate(HasParameters, HasConstraints, HasObjective)
class DumbDriver(Driver):
    pass


class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')

    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 7.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult


class Simple(Component):

    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')

    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b


class SimpleUnits(Component):

    a = Float(iotype='in', units='inch')
    b = Float(iotype='in')
    kin = Float(iotype='in', units='K')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out')
    kout = Float(iotype='out', units='degK')

    def __init__(self):
        super(SimpleUnits, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b


class SimpleListComp(Component):

    a = List(Int, iotype='in')
    b = List(Int, iotype='in')
    c = List(Int, iotype='out')
    d = List(Int, iotype='out')

    def __init__(self):
        super(SimpleListComp, self).__init__()
        self.a = [1, 2, 3]
        self.b = [4, 5, 6]
        self.c = [5, 7, 9]
        self.d = [-3, -3, -3]

    def execute(self):
        self.c = [a+b for a, b in zip(self.a, self.b)]
        self.d = [a-b for a, b in zip(self.a, self.b)]


class DummyComp(Component):

    r = Float(iotype='in')
    r2 = Float(iotype='in')
    r3 = Float(iotype='in', desc="some random variable",
               low=-1.0, high=1.0, other_meta_data="test")
    s = Str(iotype='in')
    rout = Float(iotype='out', units='ft')
    r2out = Float(iotype='out')
    sout = Str(iotype='out')
    slistout = List(Str, iotype='out')

    dummy_in = Instance(Component, iotype='in')
    dummy_out = Instance(Component, iotype='out')
    dummy_out_no_copy = Instance(Component, iotype='out', copy=None)

    def __init__(self):
        super(DummyComp, self).__init__()
        self.r = 1.0
        self.r2 = -1.0
        self.rout = 0.0
        self.r2out = 0.0
        self.s = 'a string'
        self.sout = ''

        # make a nested container with input and output ContainerVars
        self.add('dummy', Multiplier())
        self.dummy_in = self.dummy
        self.dummy_out = self.dummy

    def execute(self):
        self.rout = self.r * 1.5
        self.r2out = self.r2 + 10.0
        self.sout = self.s[::-1]
        # pylint: disable-msg=E1101
        self.dummy.execute()


class Wrapper(Assembly):
    """
    Define a single-component Assembly so we explicitly control
    what variables are visible via passthrough traits.
    """

    def configure(self):
        self.add('comp', ComponentProxy())
        self.driver.workflow.add('comp')

        # define passthrough conections
        for path in ('x', 'y', 'z'):
            val = self.get('comp.' + path)
            self.create_passthrough('comp.'+path)

    def run(self):
        self._logger.debug('Wrapper.run() %r %r (%r %r)',
                           self.x, self.y, self.comp.x, self.comp.y)
        super(Wrapper, self).run()
        self._logger.debug('    complete')
        self._logger.debug('        %r (%r)', self.z, self.comp.z)


class FloatProxy(Float):
    """ Example of a 'proxy' trait. """

    def __init__(self, remote, **metadata):
        super(FloatProxy, self).__init__(**metadata)
        self._metadata['type'] = 'property'  # Just to show correct type.
        self._remote = remote

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_remote'] = None
        return state

    def restore(self, remote):
        self._remote = remote

    def get(self, obj, name):
        return self._remote.get(name, 0.)

    def set(self, obj, name, value):
        old = self.get(obj, name)
        if value != old:
            self._remote[name] = value
            obj.trait_property_changed(name, old, value)


class ComponentProxy(Component):
    """
    Example of a 'proxy' component. Normally variables in the `_vals` dictionary
    here would actually be somewhere else in a wrapped code.
    """

    def __init__(self):
        super(ComponentProxy, self).__init__()
        self._vals = {}
        self._remote = self._vals
        self.add('x', FloatProxy(self._remote, iotype='in'))
        self.add('y', FloatProxy(self._remote, iotype='in'))
        self.add('z', FloatProxy(self._remote, iotype='out'))

    def __getstate__(self):
        state = super(ComponentProxy, self).__getstate__()
        state['_remote'] = None
        return state

    def post_load(self):
        self._remote = self._vals
        for name, trait in self._alltraits().items():
            typ = trait.trait_type
            if isinstance(typ, FloatProxy):
                typ.restore(self._remote)
        super(ComponentProxy, self).post_load()

    def execute(self):
        self._logger.debug('execute')
        self._logger.debug('    %r %r', self.x, self.y)
        self.z = self.x * self.y
        self._logger.debug('    done')


class TracedAssembly(Assembly):
    """ Records `itername` in trace buffer. """

    def __init__(self, trace_buf):
        super(TracedAssembly, self).__init__()
        self.trace_buf = trace_buf

    def execute(self):
        msg = '%s: %s' % (self.get_pathname(), self.get_itername())
        self.trace_buf.append(msg)
        super(TracedAssembly, self).execute()


class TracedIterator(Driver):
    """ Records `itername` in trace buffer. """

    def __init__(self, trace_buf, count):
        super(TracedIterator, self).__init__()
        self.trace_buf = trace_buf
        self.max_iterations = count

    def execute(self):
        msg = '%s: %s' % (self.get_pathname(), self.get_itername())
        self.trace_buf.append(msg)
        for i in range(self.max_iterations):
            super(TracedIterator, self).execute()


class TracedComponent(Component):
    """ Records `itername` in trace buffer. """

    def __init__(self, trace_buf):
        super(TracedComponent, self).__init__()
        self.trace_buf = trace_buf

    def execute(self):
        msg = '%s: %s' % (self.get_pathname(), self.get_itername())
        self.trace_buf.append(msg)


class Dummy(Component):
    """ Just defines an empty execute. """
    def execute(self):
        pass


class AssemblyTestCase(unittest.TestCase):

    def setUp(self):
        """
        top
            comp1
            nested
                comp1
            comp2
            comp3
        """
        SimulationRoot.chroot(os.getcwd())
        top = self.asm = set_as_top(Assembly())
        top.add('comp1', DummyComp())
        nested = top.add('nested', Assembly())
        nested.add('comp1', DummyComp())
        for name in ['comp2', 'comp3']:
            top.add(name, DummyComp())

        top.driver.workflow.add(['comp1', 'nested', 'comp2', 'comp3'])
        nested.driver.workflow.add('comp1')

    def test_data_passing(self):
        comp1 = self.asm.comp1
        comp2 = self.asm.comp2
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp1.sout', 'comp2.s')
        self.asm.comp1.r = 3.0
        self.asm.comp1.s = 'once upon a time'
        self.assertEqual(comp1.get('r'), 3.0)
        self.assertEqual(comp1.get('s'), 'once upon a time')
        self.assertEqual(comp1.r, 3.0)
        self.assertEqual(comp1.s, 'once upon a time')

        self.asm.run()

        self.assertEqual(comp1.get('rout'), 4.5)
        self.assertEqual(comp1.get('sout'), 'emit a nopu ecno')
        self.assertEqual(comp1.rout, 4.5)
        self.assertEqual(comp1.sout, 'emit a nopu ecno')
        self.assertEqual(comp2.get('r'), 4.5)
        self.assertEqual(comp2.get('rout'), 6.75)
        self.assertEqual(comp2.r, 4.5)
        self.assertEqual(comp2.rout, 6.75)
        self.assertEqual(comp2.s, 'emit a nopu ecno')
        self.assertEqual(comp2.sout, 'once upon a time')

        # now test removal of the error callback when a connected input is disconnected
        self.asm.disconnect('comp1.rout', 'comp2.r')
        self.asm.comp2.r = 33
        self.assertEqual(33, self.asm.comp2.r)

    def test_direct_set_of_connected_input(self):
        comp1 = self.asm.comp1
        comp2 = self.asm.comp2
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp1.sout', 'comp2.s')

        # now test removal of the error callback when a connected input is disconnected
        self.asm.disconnect('comp1.rout', 'comp2.r')
        self.asm.comp2.r = 33
        self.assertEqual(33, self.asm.comp2.r)

    def test_connect_containers(self):
        self.asm.set('comp1.dummy_in.rval_in', 75.4)
        self.asm.connect('comp1.dummy_out', 'comp2.dummy_in')
        self.asm.connect('comp1.dummy_out_no_copy', 'comp3.dummy_in')
        self.asm.run()
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_in'), 75.4)
        self.assertEqual(self.asm.get('comp2.dummy_in.rval_out'), 75.4*1.5)
        self.assertFalse(self.asm.comp1.dummy_out is self.asm.comp2.dummy_in)
        self.assertTrue(self.asm.comp1.dummy_out_no_copy is self.asm.comp3.dummy_in)

    def test_create_passthrough(self):
        self.asm.set('comp3.r', 75.4)
        self.asm.create_passthrough('comp3.rout')
        self.assertEqual(self.asm.comp3.r, 75.4)
        self.assertEqual(self.asm.rout, 0.0)
        self.asm.run()
        self.assertEqual(self.asm.comp3.rout, 75.4*1.5)
        self.assertEqual(self.asm.rout, 75.4*1.5)

        self.asm.create_passthrough('comp3.r3')
        metadata = self.asm.get_metadata('r3')
        self.assertEqual(metadata['iotype'], 'in')
        self.assertEqual(metadata['desc'], 'some random variable')
        self.assertEqual(metadata['low'], -1.0)
        self.assertEqual(metadata['high'], 1.0)
        self.assertEqual(metadata['other_meta_data'], 'test')

    def test_create_passthrough_already_exists(self):
        self.asm.create_passthrough('comp3.rout')
        try:
            self.asm.create_passthrough('comp3.rout')
        except Exception, err:
            # for some reason, KeyError turns 'rout' into \'rout\', so
            # test against str(KeyError(msg)) instead of just msg  :(
            self.assertEqual(str(err), str(KeyError(": 'rout' already exists")))
        else:
            self.fail('expected Exception')

    def test_create_passthrough_alias(self):
        self.asm.nested.set('comp1.r', 75.4)
        self.asm.nested.create_passthrough('comp1.r', 'foobar')
        self.assertEqual(self.asm.nested.get('foobar'), 75.4)
        self.asm.run()
        self.assertEqual(self.asm.nested.get('foobar'), 75.4)

    def test_passthrough_already_connected(self):
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp1.sout', 'comp2.s')
        # this should fail since we're creating a second connection
        # to an input
        try:
            self.asm.create_passthrough('comp2.r')
            self.asm._setup()
        except RuntimeError, err:
            self.assertEqual(str(err),
                             ": Can't connect 'r' to 'comp2.r': : 'comp2.r'"
                             " is already connected to 'comp1.rout'")
            self.asm.disconnect('r', 'comp2.r')
        else:
            self.fail('RuntimeError expected')
        self.asm.set('comp1.s', 'some new string')
        # this one should be OK since outputs can have multiple connections
        self.asm.create_passthrough('comp1.sout')
        self.asm.run()
        self.assertEqual(self.asm.get('sout'), 'some new string'[::-1])

    def test_container_passthrough(self):
        self.asm.set('comp1.dummy_out.rval_in', 75.4)
        self.asm.create_passthrough('comp1.dummy_out', 'dummy_out_passthrough')
        self.asm.run()
        self.assertEqual(self.asm.get('dummy_out_passthrough.rval_out'),
                         75.4*1.5)

#    def test_discon_reconnect_passthrough(self):
#        self.fail('unfinished test')

    def test_invalid_connect(self):
        try:
            self.asm.connect('comp1.rout', 'comp2.rout')
        except RuntimeError, err:
            self.assertEqual(": Can't connect 'comp1.rout' to 'comp2.rout': :"
                             " comp2: 'rout' must be an input variable",
                             str(err))
        else:
            self.fail('exception expected')

    def test_self_connect(self):
        try:
            self.asm.connect('comp1.rout', 'comp1.r')
        except Exception, err:
            self.assertEqual(": Can't connect 'comp1.rout' to 'comp1.r': :"
                             " 'comp1.rout' and 'comp1.r' refer to the same"
                             " component.", str(err))
        else:
            self.fail('exception expected')

    def test_get_metadata(self):
        units = self.asm.comp1.get_metadata('rout', 'units')
        self.assertEqual(units, 'ft')

        meta = self.asm.comp1.get_metadata('rout')
        self.assertEqual(set(meta.keys()),
                         set(['assumed_default', 'vartypename', 'units',
                              'high', 'iotype', 'type', 'low']))
        self.assertEqual(meta['vartypename'], 'Float')
        self.assertEqual(self.asm.comp1.get_metadata('slistout', 'vartypename'),
                         'List')

    def test_missing_metadata(self):
        foo = self.asm.comp1.get_metadata('rout', 'foo')
        self.assertEqual(foo, None)

        try:
            self.asm.comp1.get_metadata('bogus', 'bar')
        except Exception as err:
            self.assertEqual(str(err),
                             "comp1: Couldn't find metadata for trait bogus")
        else:
            self.fail("Exception expected")

    def test_list_components(self):
        asm = Assembly()
        asm.add('a', Simple())
        asm.add('b', Simple())
        self.assertEqual(set(asm.list_components()),
                         set(['a', 'b', 'driver']))

    def test_circular_dependency(self):

        self.asm.connect('comp1.rout', 'comp2.r')

        # Cyclic graphs are permitted in declaration.
        self.asm.connect('comp2.rout', 'comp1.r')

        # duplicate entries in workflow should cause exception.
        asm = Assembly()
        asm.add('a', Simple())
        asm.add('b', Simple())
        dup1 = asm.add('dup1', Simple())
        dup2 = asm.add('dup2', Simple())
        self.assertEqual(dup1.exec_count, 0)
        self.assertEqual(dup2.exec_count, 0)
        sequence = ['dup1', 'a', 'dup2', 'dup1', 'b', 'dup1', 'dup2']
        asm.driver.workflow.add(sequence)
        try:
            asm.run()
        except RuntimeError as err:
            self.assertEqual(str(err),
                "driver workflow has duplicate entries: ['dup1', 'dup2']")


    def test_disconnect(self):
        # first, run connected
        comp2 = self.asm.get('comp2')
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 1.5)
        self.asm.comp1.r = 3.0
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)

        # now disconnect
        self.asm.comp1.r = 6.0
        self.asm.disconnect('comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 4.5)

        # now reconnect
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.run()
        self.assertEqual(comp2.r, 9.0)

        self.asm.disconnect('comp2.r')
        self.asm.connect('3.0*comp1.rout', 'comp2.r')
        self.asm.disconnect('3.0*comp1.rout', 'comp2.r')

    def test_disconnect_boundaries(self):

        # This test is from a user-reported bug where an exception was raised
        # if you tried to disconnect a boundary-var variable tree connection.

        class VTINputs(VariableTree):

            Lp = Float( units='m')

        class Top(Assembly):

            Src = VarTree(VTINputs(), iotype='in')
            Targ = VarTree(VTINputs(), iotype='out')

            def configure(self):
                pass

        top = set_as_top(Top())
        top.connect('Src.Lp', 'Targ.Lp')
        conn = top.list_connections()
        self.assertTrue(len(conn) == 1)
        top.disconnect('Src.Lp', 'Targ.Lp')
        conn = top.list_connections()
        self.assertTrue(len(conn) == 0)

    def test_input_passthrough_to_2_inputs(self):
        asm = set_as_top(Assembly())
        asm.add('nested', Assembly())
        comp1 = asm.nested.add('comp1', Simple())
        comp2 = asm.nested.add('comp2', Simple())

        asm.driver.workflow.add('nested')
        asm.nested.driver.workflow.add(['comp1', 'comp2'])

        asm.nested.create_passthrough('comp1.a')
        asm.nested.connect('a', 'comp2.b')
        self.assertEqual(asm.nested.comp1.a, 4.)
        self.assertEqual(asm.nested.comp2.b, 5.)
        asm.nested.a = 0.5
        # until we run, the values of comp1.a and comp2.b won't change
        self.assertEqual(asm.nested.comp1.a, 4.)
        self.assertEqual(asm.nested.comp2.b, 5.)
        asm.run()
        self.assertEqual(asm.nested.comp1.a, 0.5)
        self.assertEqual(asm.nested.comp2.b, 0.5)
        asm.nested.a = 999.
        self.assertEqual(asm.nested.comp1.a, 0.5)
        self.assertEqual(asm.nested.comp2.b, 0.5)
        asm.run()
        self.assertEqual(asm.nested.comp1.a, 999.)
        self.assertEqual(asm.nested.comp2.b, 999.)

    def test_connect_2_outs_to_passthrough(self):
        asm = set_as_top(Assembly())
        asm.add('nested', Assembly())
        asm.nested.add('comp1', Simple())
        asm.nested.add('comp2', Simple())
        asm.nested.create_passthrough('comp1.c')
        try:
            asm.nested.connect('comp2.d', 'c')
            asm._setup()
        except RuntimeError as err:
            self.assertEqual(str(err),
                             "nested: Can't connect 'comp2.d' to 'c': : 'c' is already connected to 'comp1.c'")
        else:
            self.fail('RuntimeError expected')

    def test_discon_not_connected(self):
        self.asm.connect('comp1.rout', 'comp2.r')

        # disconnecting something that isn't connected is ok and shouldn't
        # raise an exception
        self.asm.disconnect('comp2.s')

    def test_listcon_with_deleted_objs(self):
        self.asm.add('comp3', DummyComp())
        self.asm.connect('comp1.rout', 'comp2.r')
        self.asm.connect('comp3.sout', 'comp2.s')
        conns = self.asm.list_connections()
        self.assertEqual(set(conns), set([('comp1.rout', 'comp2.r'),
                                 ('comp3.sout', 'comp2.s')]))
        self.asm.remove('comp3')
        conns = self.asm.list_connections()
        self.assertEqual(conns, [('comp1.rout', 'comp2.r')])
        self.asm.run()

    def test_assembly_connect_init(self):
        class MyComp(Component):
            ModulesInstallPath = Str('', desc='', iotype='in')

            def execute(self):
                pass

        class MyAsm(Assembly):
            ModulesInstallPath = \
                Str('C:/work/IMOO2/imoo/modules', desc='', iotype='in')

            def configure(self):
                self.add('propulsion', MyComp())
                self.driver.workflow.add('propulsion')
                self.connect('ModulesInstallPath',
                             'propulsion.ModulesInstallPath')

        asm = set_as_top(MyAsm())
        asm.run()
        self.assertEqual(asm.ModulesInstallPath, 'C:/work/IMOO2/imoo/modules')
        self.assertEqual(asm.propulsion.ModulesInstallPath,
                         'C:/work/IMOO2/imoo/modules')

    def test_wrapper(self):
        # Test that wrapping via passthroughs to proxy traits works.
        top = set_as_top(Wrapper())

        expected = [
            '%s.FloatProxy' % __name__,
            'openmdao.main.datatypes.float.Float',
            'openmdao.main.variable.Variable',
            'traits.trait_handlers.TraitType',
            'traits.trait_handlers.BaseTraitHandler',
            '__builtin__.object'
        ]
        self.assertEqual(top.get_trait_typenames('x'), expected)

        for varname in ('x', 'comp.x', 'y', 'comp.y', 'z', 'comp.z'):
            self.assertEqual(top.get(varname), 0.)

        top.set('x', 6)
        top.set('y', 7)
        top.run()
        self.assertEqual(top.get('x'), 6.)
        self.assertEqual(top.get('comp.x'), 6.)
        self.assertEqual(top.get('y'), 7.)
        self.assertEqual(top.get('comp.y'), 7.)
        self.assertEqual(top.get('z'), 42.)
        self.assertEqual(top.get('comp.z'), 42.)

        top.set('x', 7)
        top.set('y', 8)
        top.run()
        self.assertEqual(top.get('x'), 7.)
        self.assertEqual(top.get('comp.x'), 7.)
        self.assertEqual(top.get('y'), 8.)
        self.assertEqual(top.get('comp.y'), 8.)
        self.assertEqual(top.get('z'), 56.)
        self.assertEqual(top.get('comp.z'), 56.)

        egg_info = top.save_to_egg('Top', 'v1')
        try:
            egg = Component.load_from_eggfile(egg_info[0])
            self.assertEqual(egg.get('x'), 7.)
            self.assertEqual(egg.get('comp.x'), 7.)
            self.assertEqual(egg.get('y'), 8.)
            self.assertEqual(egg.get('comp.y'), 8.)
            self.assertEqual(egg.get('z'), 56.)
            self.assertEqual(egg.get('comp.z'), 56.)

            egg.set('x', 11)
            egg.set('y', 3)
            egg.run()
            self.assertEqual(egg.get('x'), 11.)
            self.assertEqual(egg.get('comp.x'), 11.)
            self.assertEqual(egg.get('y'), 3)
            self.assertEqual(egg.get('comp.y'), 3.)
            self.assertEqual(egg.get('z'), 33.)
            self.assertEqual(egg.get('comp.z'), 33.)
        finally:
            os.remove(egg_info[0])
            shutil.rmtree('Top', onerror=onerror)

    def test_multiconnect(self):
        top = Assembly()
        for name in ('m1', 'm2', 'm3'):
            top.add(name, Multiplier())
            top.driver.workflow.add(name)
        top.connect('m1.rval_out', ('m2.mult', 'm3.mult'))
        top.m1.rval_in = 1.
        top.m2.rval_in = 3.
        top.m3.rval_in = 4.
        top.run()
        self.assertEqual(top.m2.rval_out, 4.5)
        self.assertEqual(top.m3.rval_out, 6.)

    def test_remove(self):
        top = Assembly()
        top._setup()
        g = top._depgraph.component_graph()
        comps = [name for name in g]
        self.assertEqual(comps, ['driver'])

        top.add('comp', Component())
        top._setup()

        g = top._depgraph.component_graph()
        comps = [name for name in g]
        self.assertEqual(set(comps), set(['driver', 'comp']))

        top.remove('comp')
        top._setup()

        g = top._depgraph.component_graph()
        comps = [name for name in g]
        self.assertEqual(comps, ['driver'])

    def test_itername(self):
        # top
        #     comp1
        #     driverA
        #         comp1
        #         comp2
        #     driverB
        #         comp2
        #         subassy
        #             comp3
        trace_buf = []
        top = set_as_top(TracedAssembly(trace_buf))
        top.add('driver', TracedIterator(trace_buf, 2))
        top.add('comp1', TracedComponent(trace_buf))
        top.add('driverA', TracedIterator(trace_buf, 3))
        top.add('comp2', TracedComponent(trace_buf))
        top.add('driverB', TracedIterator(trace_buf, 2))

        sub = top.add('subassy', TracedAssembly(trace_buf))
        sub.add('driver', TracedIterator(trace_buf, 2))
        sub.add('comp3', TracedComponent(trace_buf))
        sub.driver.workflow.add('comp3')

        # Default didn't execute comp1 first.
        top.driver.workflow = Workflow(top.driver)
        top.driver.workflow.add(('comp1', 'driverA', 'driverB'))
        top.driverA.workflow.add(('comp1', 'comp2'))
        top.driverB.workflow.add(('comp2', 'subassy'))

        top.run()
        top.set_itername('ReRun')
        top.run()

        expected = """\
:
driver:
comp1: 1-comp1
driverA: 1-driverA
comp1: 1-driverA.1-comp1
comp2: 1-driverA.1-comp2
comp1: 1-driverA.2-comp1
comp2: 1-driverA.2-comp2
comp1: 1-driverA.3-comp1
comp2: 1-driverA.3-comp2
driverB: 1-driverB
comp2: 1-driverB.1-comp2
subassy: 1-driverB.1-subassy
subassy.driver: 1-driverB.1-subassy
subassy.comp3: 1-driverB.1-subassy.1-comp3
subassy.comp3: 1-driverB.1-subassy.2-comp3
comp2: 1-driverB.2-comp2
subassy: 1-driverB.2-subassy
subassy.driver: 1-driverB.2-subassy
subassy.comp3: 1-driverB.2-subassy.1-comp3
subassy.comp3: 1-driverB.2-subassy.2-comp3
comp1: 2-comp1
driverA: 2-driverA
comp1: 2-driverA.1-comp1
comp2: 2-driverA.1-comp2
comp1: 2-driverA.2-comp1
comp2: 2-driverA.2-comp2
comp1: 2-driverA.3-comp1
comp2: 2-driverA.3-comp2
driverB: 2-driverB
comp2: 2-driverB.1-comp2
subassy: 2-driverB.1-subassy
subassy.driver: 2-driverB.1-subassy
subassy.comp3: 2-driverB.1-subassy.1-comp3
subassy.comp3: 2-driverB.1-subassy.2-comp3
comp2: 2-driverB.2-comp2
subassy: 2-driverB.2-subassy
subassy.driver: 2-driverB.2-subassy
subassy.comp3: 2-driverB.2-subassy.1-comp3
subassy.comp3: 2-driverB.2-subassy.2-comp3
: ReRun
driver: ReRun
comp1: ReRun.1-comp1
driverA: ReRun.1-driverA
comp1: ReRun.1-driverA.1-comp1
comp2: ReRun.1-driverA.1-comp2
comp1: ReRun.1-driverA.2-comp1
comp2: ReRun.1-driverA.2-comp2
comp1: ReRun.1-driverA.3-comp1
comp2: ReRun.1-driverA.3-comp2
driverB: ReRun.1-driverB
comp2: ReRun.1-driverB.1-comp2
subassy: ReRun.1-driverB.1-subassy
subassy.driver: ReRun.1-driverB.1-subassy
subassy.comp3: ReRun.1-driverB.1-subassy.1-comp3
subassy.comp3: ReRun.1-driverB.1-subassy.2-comp3
comp2: ReRun.1-driverB.2-comp2
subassy: ReRun.1-driverB.2-subassy
subassy.driver: ReRun.1-driverB.2-subassy
subassy.comp3: ReRun.1-driverB.2-subassy.1-comp3
subassy.comp3: ReRun.1-driverB.2-subassy.2-comp3
comp1: ReRun.2-comp1
driverA: ReRun.2-driverA
comp1: ReRun.2-driverA.1-comp1
comp2: ReRun.2-driverA.1-comp2
comp1: ReRun.2-driverA.2-comp1
comp2: ReRun.2-driverA.2-comp2
comp1: ReRun.2-driverA.3-comp1
comp2: ReRun.2-driverA.3-comp2
driverB: ReRun.2-driverB
comp2: ReRun.2-driverB.1-comp2
subassy: ReRun.2-driverB.1-subassy
subassy.driver: ReRun.2-driverB.1-subassy
subassy.comp3: ReRun.2-driverB.1-subassy.1-comp3
subassy.comp3: ReRun.2-driverB.1-subassy.2-comp3
comp2: ReRun.2-driverB.2-comp2
subassy: ReRun.2-driverB.2-subassy
subassy.driver: ReRun.2-driverB.2-subassy
subassy.comp3: ReRun.2-driverB.2-subassy.1-comp3
subassy.comp3: ReRun.2-driverB.2-subassy.2-comp3"""
        expected = expected.split('\n')
        errors = 0

        for i, line in enumerate(trace_buf):
            if line.strip() != expected[i].strip():
                logging.error('%d: expected %r, got %r', i, expected[i], line)
                errors += 1
        self.assertEqual(errors, 0)
        self.assertEqual(len(trace_buf), len(expected))

    def test_expr(self):
        class Dummy(Component):

            z = Array([[0], [0], [0]], iotype="in", shape=(3, 1))

            def execute(self):
                pass

        class TestA(Assembly):

            x = Float(0.0, iotype="in")

            def configure(self):
                self.add('d', Dummy())
                self.connect('x', 'd.z[0][0]')

        set_as_top(TestA())

    def test_tracing(self):
        # Check tracing of iteration coordinates.
        top = Assembly()
        comp = top.add('comp1', Dummy())
        top.add('driverA', Driver())
        comp = top.add('comp2', Dummy())
        top.add('driverB', Driver())

        sub = top.add('subassy', Assembly())
        comp = sub.add('comp3', Dummy())
        sub.driver.workflow.add('comp3')

        top.driver.workflow.add(('comp1', 'driverA', 'driverB'))
        top.driverA.workflow.add(('comp1', 'comp2'))
        top.driverB.workflow.add(('comp2', 'subassy'))

        trace_out = cStringIO.StringIO()
        enable_trace(trace_out)
        top.run()
        expected = """\
1-comp1
1-driverA.1-comp1
1-driverA.1-comp2
1-driverB.1-comp2
1-driverB.1-subassy.1-comp3
"""
        self.assertEqual(trace_out.getvalue(), expected)

        disable_trace()
        top.run()
        self.assertEqual(trace_out.getvalue(), expected)

    def _setup_move_rename(self):
        asm = set_as_top(Assembly())
        asm.add('sub', Assembly())
        asm.add('comp1', Simple())
        asm.sub.add('comp2', Simple())
        asm.sub.add('comp3', Simple())
        asm.add('comp4', Simple())
        asm.driver.workflow.add(['comp1', 'sub', 'comp4'])
        asm.sub.driver.workflow.add(['comp2', 'comp3'])
        asm.sub.add('a2', Float(iotype='in'))
        asm.sub.add('c3', Float(iotype='out'))
        asm.connect('comp1.c', 'sub.a2')
        asm.connect('sub.c3', 'comp4.a')
        asm.sub.connect('a2', 'comp2.a')
        asm.sub.connect('comp2.c', 'comp3.a')
        asm.sub.connect('comp3.c', 'c3')
        #asm.connect('comp1.d', 'sub.comp2.b')  # autopassthrough
        #asm.connect('sub.comp3.d', 'comp4.b')  # autopassthrough
        connections = asm.list_connections()
        self.assertEqual(set(connections),
                         set([('comp1.c', 'sub.a2'),
                              # ('comp1.d', 'sub.comp2.b'),
                              # ('sub.comp3.d', 'comp4.b'),
                              ('sub.c3', 'comp4.a')]))
        sub_connections = asm.sub.list_connections()
        self.assertEqual(set(sub_connections),
                         set([('comp3.c', 'c3'), ('a2', 'comp2.a'),
                              ('comp2.c', 'comp3.a')]))
        asm._setup()
        self.assertEqual([c.name for c in asm.driver.workflow],
                         ['comp1', 'sub', 'comp4'])
        self.assertEqual([c.name for c in asm.sub.driver.workflow],
                         ['comp2', 'comp3'])
        return asm

    def test_rename_asm(self):
        asm = self._setup_move_rename()
        asm.rename('sub', 'nested')
        connections = asm.list_connections()
        self.assertEqual(set(connections),
                         set([('comp1.c', 'nested.a2'),
                              ('nested.c3', 'comp4.a')]))
        sub_connections = asm.nested.list_connections()
        self.assertEqual(set(sub_connections),
                         set([('comp3.c', 'c3'), ('a2', 'comp2.a'),
                              ('comp2.c', 'comp3.a')]))
        asm._setup()
        self.assertEqual([c.name for c in asm.driver.workflow],
                         ['comp1', 'nested', 'comp4'])
        self.assertEqual([c.name for c in asm.nested.driver.workflow],
                         ['comp2', 'comp3'])
        self.assertEqual(asm.nested.name, 'nested')
        self.assertFalse(hasattr(asm, 'sub'))

    def test_rename_child(self):
        asm = self._setup_move_rename()
        asm.sub.rename('comp2', 'newcomp2')
        asm.sub.rename('comp3', 'newcomp3')
        asm._setup()

        self.assertEqual(asm.sub.newcomp2.name, 'newcomp2')
        self.assertEqual(asm.sub.newcomp3.name, 'newcomp3')
        self.assertFalse(hasattr(asm.sub, 'comp2'))
        self.assertFalse(hasattr(asm.sub, 'comp3'))

        connections = asm.list_connections()
        self.assertEqual(set(connections),
                         set([('comp1.c', 'sub.a2'),
                              ('sub.c3', 'comp4.a')]))
        sub_connections = asm.sub.list_connections()
        self.assertEqual(set(sub_connections),
                         set([('newcomp3.c', 'c3'), ('a2', 'newcomp2.a'),
                              ('newcomp2.c', 'newcomp3.a')]))
        self.assertEqual([c.name for c in asm.driver.workflow],
                         ['comp1', 'sub', 'comp4'])
        self.assertEqual([c.name for c in asm.sub.driver.workflow],
                         ['newcomp2', 'newcomp3'])


def pseudo_edges(index, num_inputs):
    pname = '_pseudo_%d' % index
    edges = [(pname, pname+'.out0')]
    for i in range(num_inputs):
        edges.append(('%s.in%d' % (pname, i), pname))
    return edges


class AssemblyTestCase2(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        self.top.add('driver', DumbDriver())
        top.add('C1', SimpleUnits())
        top.add('C2', SimpleUnits())
        top.add('C3', SimpleUnits())

    def test_cleanup(self):
        top = self.top
        clean_edges = set(top.list_connections())
        top._setup()
        clean_dep_edges = set(top._depgraph.edges())

        # first, a no units connection
        top.connect('C1.d', 'C2.b')
        self.assertEqual(set(top.list_connections()) - clean_edges,
                         set([('C1.d', 'C2.b')]))

        top.disconnect('C1')
        self.assertEqual(set(top.list_connections()) - clean_edges, set())

        # now a connection between two edges that have different aliases for the same unit
        # (should result in no pseudocomps being created)
        top.connect('C1.kout', 'C2.kin')
        self.assertEqual(set(top.list_connections()) - clean_edges,
                         set([('C1.kout', 'C2.kin')]))

        top.disconnect('C1')
        self.assertEqual(set(top.list_connections()) - clean_edges, set())

        # no units but a multi-comp source expression
        top.connect('C1.d+C2.d', 'C3.b')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges,
                         set([('_pseudo_0.out0', 'C3.b'),
                              ('C1.d', '_pseudo_0.in0'),
                              ('C2.d', '_pseudo_0.in1')]+pseudo_edges(0, 2)))

        # disconnecting one source comp from a mult-comp source expression
        top.disconnect('C1')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges, set())

        # replace the multi-comp connection (makes a new pseudocomp)
        top.connect('C1.d+C2.d', 'C3.b')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges,
                         set([('_pseudo_1.out0', 'C3.b'),
                              ('C1.d', '_pseudo_1.in0'),
                              ('C2.d', '_pseudo_1.in1')]+pseudo_edges(1, 2)))

        # disconnecting dest comp from a mult-comp source expression
        top.disconnect('C3')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges, set())

        # units conversion connection
        top.connect('C1.c', 'C3.a')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges,
                         set([('C1.c', '_pseudo_2.in0'),
                              ('_pseudo_2.out0', 'C3.a')]+pseudo_edges(2, 1)))

        # disconnect a units conversion connection by disconnecting a comp
        top.disconnect('C1')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges, set())

        # units conversion connection
        top.connect('C1.c', 'C3.a')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges,
                         set([('C1.c', '_pseudo_3.in0'),
                              ('_pseudo_3.out0', 'C3.a')]+pseudo_edges(3, 1)))

        top.disconnect('C1.c', 'C3.a')
        top._setup()
        self.assertEqual(set(top._depgraph.edges()) - clean_dep_edges, set())

if __name__ == "__main__":
    unittest.main()
