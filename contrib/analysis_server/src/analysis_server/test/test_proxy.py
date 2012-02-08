import glob
import logging
import nose
import os.path
import pkg_resources
import shutil
import socket
import sys
import unittest

from numpy.testing import assert_array_equal

from openmdao.main.api import Assembly, Component, FileRef, set_as_top
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.lib.datatypes.api import File, Float, Str
from openmdao.util.testutil import assert_raises

import analysis_server
from analysis_server.proxy import EnumProxy

ORIG_DIR = os.getcwd()


class Model(Assembly):
    """ Run AnalysisServer component inside an Assembly. """

    def configure(self):
        self.add('source', Source())
        self.add('as_comp', factory.create('ASTestComp'))
        self.add('sink', Sink())
        self.connect('source.x', 'as_comp.x')
        self.connect('source.y', 'as_comp.y')
        self.connect('source.tof', 'as_comp.obj_input.tof')
        self.connect('source.sof', 'as_comp.obj_input.subobj.sof')
        self.connect('source.output', 'as_comp.in_file')
        self.connect('as_comp.z', 'sink.z')
        self.connect('as_comp.obj_output.tof', 'sink.tof')
        self.connect('as_comp.obj_output.subobj.sof', 'sink.sof')
        self.connect('as_comp.out_file', 'sink.input')
        self.driver.workflow.add(['source', 'as_comp', 'sink'])

class Source(Component):
    """ Source data for test component. """

    x = Float(iotype='out')
    y = Float(iotype='out')
    tof = Float(iotype='out')
    sof = Float(iotype='out')
    output = File(path='output', iotype='out')

    def execute(self):
        self.x = 6
        self.y = 7
        self.tof = 2.781828
        self.sof = 3.14159
        with open(self.output.path, 'w') as out:
            out.write('Hello world!')

class Sink(Component):
    """ Sink data from test component. """

    z = Float(iotype='in')
    tof = Float(iotype='in')
    sof = Float(iotype='in')
    input = File(iotype='in')
    data = Str(iotype='out')

    def execute(self):
        with self.input.open() as inp:
            self.data = inp.read()


class TestCase(unittest.TestCase):
    """ Test AnalysisServer emulation. """

    directory = os.path.realpath(
        pkg_resources.resource_filename('analysis_server', 'test'))

    def setUp(self):
        """ Called before each test. """
        os.chdir(TestCase.directory)
        self.server, port = \
            analysis_server.start_server(port=0, resources='', debug=True)
        self.factory = analysis_server.ASFactory(port=port)

    def tearDown(self):
        """ Called after each test. """
        self.factory.shutdown()
        analysis_server.stop_server(self.server)
        os.remove('hosts.allow')
        for egg in glob.glob('*.egg'):
            os.remove(egg)
        for dirname in ('ASTestComp', 'ASTestComp2', 'logs'):
            if os.path.exists(dirname):
                shutil.rmtree(dirname)
        try:
            os.remove('as-0.out')
        except WindowsError:
            pass  # Still in use by server...
        os.chdir(ORIG_DIR)

    def get_state(self, container, path, client):
        """ Return remote state (variable values) as dictionary. """
        state = {}
        for prop, typ, access in client.list_properties(path):
            rpath = '.'.join([path, prop])
            if typ == 'PHXGroup':
                obj = getattr(container, prop)
                state.update(self.get_state(obj, rpath, client))
            elif access == 'in':  # Can't restore remote outputs.
                state[rpath] = client.get(rpath)
        return state

    def compare_states(self, before, after):
        """ Compare `before` to `after`. """
        for key in sorted(before.keys()):
            bval = before[key]
            aval = after[key]
            if aval != bval:
                logging.error('before[%s] %r', key, bval)
                logging.error('after[%s] %r', key, aval)
                self.fail('after[%s] != before[%s]' % (key, key))

    def test_factory(self):
        logging.debug('')
        logging.debug('test_factory')
        self.assertEqual(self.factory.get_available_types(),
                         [('ASTestComp', '0.1'),
                          ('ASTestComp', '0.2'),
                          ('ASTestComp2', '0.1')])
        self.assertEqual(self.factory.get_available_types('no-such-group'), [])
        self.assertEqual(self.factory.create('no-such-type'), None)

    def test_component(self):
        logging.debug('')
        logging.debug('test_component')
        comp = set_as_top(self.factory.create('ASTestComp'))
        comp.set('x', 6)
        comp.set('y', 7)
        comp.set('obj_input.tof', 2.781828)
        comp.set('obj_input.subobj.sof', 3.14159)
        path = 'output'
        with comp.dir_context:
            with open(path, 'w') as out:
                out.write('Hello world!')
        comp.set('in_file', FileRef(path, comp))
        with comp.dir_context:
            os.remove(path)
        comp.run()
        self.assertEqual(comp.get('z'), 42.)
        self.assertEqual(comp.get('obj_output.tof'), 2.781828)
        self.assertEqual(comp.get('obj_output.subobj.sof'), 3.14159)
        with comp.get('out_file').open() as inp:
            data = inp.read()
        self.assertEqual(data, 'Hello world!')

        result_b = comp.float_method()
        before = self.get_state(comp, 'the_obj', comp._client)
        state_file = 'state.pickle'
        try:
            comp.save(state_file)
            restored = Component.load(state_file)
            result_a = restored.float_method()
            after = self.get_state(restored, 'the_obj', restored._client)
            restored.pre_delete()
            self.assertEqual(result_a, result_b)
            self.compare_states(before, after)
        finally:
            os.remove(state_file)
            os.remove('AS-the_obj.in_file.dat')
            os.remove('AS-the_obj.out_file.dat')
            comp.pre_delete()

    def test_model(self):
        logging.debug('')
        logging.debug('test_model')
        model = set_as_top(Model(self.factory))
        self.assertEqual(model.sink.z, 0)
        self.assertEqual(model.sink.tof, 0)
        self.assertEqual(model.sink.sof, 0)
        self.assertEqual(model.sink.data, '')
        model.run()
        with model.source.dir_context:
            os.remove(model.source.output.path)
            os.remove('AS-the_obj.out_file.dat')
        os.remove('AS-the_obj.in_file.dat')
        self.assertEqual(model.sink.z, 42)
        self.assertEqual(model.sink.tof, 2.781828)
        self.assertEqual(model.sink.sof, 3.14159)
        self.assertEqual(model.sink.data, 'Hello world!')
        model.pre_delete()

    def test_bool(self):
        logging.debug('')
        logging.debug('test_bool')
        comp = self.factory.create('ASTestComp')
        self.assertTrue(comp.get('sub_group.b'))
        comp.set('sub_group.b', False)
        self.assertFalse(comp.sub_group.b)
        comp.sub_group.b = False
        self.assertFalse(comp.get('sub_group.b'))
        trait = comp.sub_group.get_trait('b')
        self.assertEqual(trait.desc, 'A boolean')
        comp.pre_delete()

    def test_enum(self):
        logging.debug('')
        logging.debug('test_enum')
        comp = self.factory.create('ASTestComp')

        self.assertEqual(comp.get('sub_group.ie'), 9)
        comp.set('sub_group.ie', 1)
        self.assertEqual(comp.sub_group.ie, 1)
        comp.sub_group.ie = 8
        self.assertEqual(comp.get('sub_group.ie'), 8)
        trait = comp.sub_group.get_trait('ie')
        self.assertEqual(trait.desc, 'Int enum')
        self.assertEqual(trait.values, [9, 8, 7, 1])

        self.assertEqual(comp.get('sub_group.fe'), 2.781828)
        comp.set('sub_group.fe', 3.14159)
        self.assertEqual(comp.sub_group.fe, 3.14159)
        comp.sub_group.fe = 2.781828
        self.assertEqual(comp.get('sub_group.fe'), 2.781828)
        trait = comp.sub_group.get_trait('fe')
        self.assertEqual(trait.desc, 'Float enum')
        self.assertEqual(trait.values, [2.781828, 3.14159])
        self.assertEqual(trait.aliases, ['e', 'pi'])

        self.assertEqual(comp.get('sub_group.se'), 'cold')
        comp.set('sub_group.se', 'hot')
        self.assertEqual(comp.sub_group.se, 'hot')
        comp.sub_group.se = 'nice'
        self.assertEqual(comp.get('sub_group.se'), 'nice')
        trait = comp.sub_group.get_trait('se')
        self.assertEqual(trait.desc, 'Str enum')
        self.assertEqual(trait.values, ['cold', 'hot', 'nice'])

        code = "enum = EnumProxy('in', comp._client, 'the_obj.x', 'PHXNone', '')"
        assert_raises(self, code, globals(), locals(), NotImplementedError,
                      "EnumProxy for 'PHXNone'", use_exec=True)

        comp.pre_delete()

    def test_float(self):
        logging.debug('')
        logging.debug('test_float')
        comp = self.factory.create('ASTestComp')

        self.assertEqual(comp.get('sub_group.f'), 0.5)
        comp.set('sub_group.f', 42)
        self.assertEqual(comp.sub_group.f, 42)
        comp.sub_group.f = -0.5
        self.assertEqual(comp.get('sub_group.f'), -0.5)
        trait = comp.sub_group.get_trait('f')
        self.assertEqual(trait.desc, 'A float')

        trait = comp.get_trait('y')
        self.assertEqual(trait.desc, 'Y input')
        self.assertEqual(trait.low, -10)
        self.assertEqual(trait.high, 10)
        self.assertEqual(trait.units, 'ft')

        comp.pre_delete()

    def test_floatND(self):
        logging.debug('')
        logging.debug('test_floatND')
        comp = self.factory.create('ASTestComp')
        self.assertEqual(comp.get('sub_group.f'), 0.5)

        assert_array_equal(comp.get('sub_group.f1d'),
                           [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5])
        comp.set('sub_group.f1d', [4.5, 3.5, 2.5, 1.5])
        assert_array_equal(comp.sub_group.f1d, [4.5, 3.5, 2.5, 1.5])
        comp.sub_group.f1d = [-5.5, -6.5, -7.5]
        assert_array_equal(comp.get('sub_group.f1d'), [-5.5, -6.5, -7.5])
        trait = comp.sub_group.get_trait('f1d')
        self.assertEqual(trait.desc, '1D float array')

        assert_array_equal(comp.get('sub_group.f2d'),
                           [ [1.5, 2.5, 3.5, 4.5], [5.5, 6.5, 7.5, 8.5] ])
        comp.set('sub_group.f2d', [ [4.5, 3.5], [2.5, 1.5] ])
        assert_array_equal(comp.sub_group.f2d, [ [4.5, 3.5], [2.5, 1.5] ])
        comp.sub_group.f2d = [ [-5.5, -6.5], [-7.5, -8.5] ]
        assert_array_equal(comp.get('sub_group.f2d'), [ [-5.5, -6.5], [-7.5, -8.5] ])
        trait = comp.sub_group.get_trait('f2d')
        self.assertEqual(trait.desc, '2D float array')

        assert_array_equal(comp.get('sub_group.f3d'),
            [ [ [1.5, 2.5, 3.5], [4.5, 5.5, 6.5], [7.5, 8.5, 9.5] ],
              [ [10.5, 20.5, 30.5], [40.5, 50.5, 60.5], [70.5, 80.5, 90.5] ] ])
        comp.set('sub_group.f3d', [ [ [4.5, 3.5], [2.5, 1.5] ] ])
        assert_array_equal(comp.sub_group.f3d, [ [ [4.5, 3.5], [2.5, 1.5] ] ])
        comp.sub_group.f3d = [ [ [-5.5, -6.5, -7.5] ] ]
        assert_array_equal(comp.get('sub_group.f3d'), [ [ [-5.5, -6.5, -7.5] ] ])
        trait = comp.sub_group.get_trait('f3d')
        self.assertEqual(trait.desc, '3D float array')

        comp.pre_delete()

    def test_int(self):
        logging.debug('')
        logging.debug('test_int')
        comp = self.factory.create('ASTestComp')
        self.assertEqual(comp.get('sub_group.i'), 7)
        comp.set('sub_group.i', 42)
        self.assertEqual(comp.sub_group.i, 42)
        comp.sub_group.i = -5
        self.assertEqual(comp.get('sub_group.i'), -5)
        trait = comp.sub_group.get_trait('i')
        self.assertEqual(trait.desc, 'An int')
        comp.pre_delete()

    def test_int1D(self):
        logging.debug('')
        logging.debug('test_int1D')
        comp = self.factory.create('ASTestComp')
        assert_array_equal(comp.get('sub_group.i1d'),
                           [1, 2, 3, 4, 5, 6, 7, 8, 9])
        comp.set('sub_group.i1d', [4, 3, 2, 1])
        assert_array_equal(comp.sub_group.i1d, [4, 3, 2, 1])
        comp.sub_group.i1d = [-5, -6, -7]
        assert_array_equal(comp.get('sub_group.i1d'), [-5, -6, -7])
        trait = comp.sub_group.get_trait('i1d')
        self.assertEqual(trait.desc, '1D int array')
        comp.pre_delete()

    def test_str(self):
        logging.debug('')
        logging.debug('test_str')
        comp = self.factory.create('ASTestComp')
        self.assertEqual(comp.get('sub_group.s'), 'Hello World!  ( & < > )')
        comp.set('sub_group.s', 'xyzzy')
        self.assertEqual(comp.sub_group.s, 'xyzzy')
        comp.sub_group.s = 'froboz'
        self.assertEqual(comp.get('sub_group.s'), 'froboz')
        trait = comp.sub_group.get_trait('s')
        self.assertEqual(trait.desc, 'A string')
        comp.pre_delete()

    def test_str1D(self):
        logging.debug('')
        logging.debug('test_str1D')
        comp = self.factory.create('ASTestComp')
        self.assertEqual(comp.get('sub_group.s1d'),
                         ['Hello', 'from', 'TestComponent.SubGroup'])
        comp.set('sub_group.s1d', ['froboz', 'xyzzy'])
        self.assertEqual(comp.sub_group.s1d, ['froboz', 'xyzzy'])
        comp.sub_group.s1d = ['lkjhlk', '654', '#$%^']
        self.assertEqual(comp.get('sub_group.s1d'), ['lkjhlk', '654', '#$%^'])
        trait = comp.sub_group.get_trait('s1d')
        self.assertEqual(trait.desc, '1D string array')
        comp.pre_delete()

    def test_units(self):
        logging.debug('')
        logging.debug('test_units')

        self.assertFalse(analysis_server.have_translation('FroBoz'))
        self.assertTrue(analysis_server.have_translation('Btu/hr'))

        self.assertEqual(analysis_server.get_translation('Btu/hr'), 'Btu/h')
        self.assertEqual(analysis_server.get_translation('FroBoz'), 'FroBoz')

        analysis_server.set_translation('FroBoz', 'degF')
        self.assertTrue(analysis_server.have_translation('FroBoz'))
        self.assertEqual(analysis_server.get_translation('FroBoz'), 'degF')


if __name__ == '__main__':
    sys.argv.append('--cover-package=analysis_server')
    sys.argv.append('--cover-erase')

    # Avoid having any user-defined resources causing problems during testing.
    RAM.configure('')

    nose.runmodule()

