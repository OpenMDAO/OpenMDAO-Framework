"""
Test distributed simulation.
"""

import glob
import logging
from math import pi
from multiprocessing import AuthenticationError
from multiprocessing.managers import RemoteError
import os
import shutil
import sys
import unittest
import nose

from enthought.traits.api import TraitError

from openmdao.main.api import Assembly, Case, Component, Container, Driver, \
                              set_as_top
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.main.interfaces import IComponent
from openmdao.main.mp_support import has_interface, is_instance, \
                                     read_server_config, _SHA1
from openmdao.main.objserverfactory import connect, start_server
from openmdao.main.rbac import Credentials, get_credentials, set_credentials, \
                               AccessController, RoleError, rbac

from openmdao.lib.api import Float, Int
from openmdao.lib.caserecorders.listcaserecorder import ListCaseRecorder

from openmdao.test.execcomp import ExecComp

from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_raises, assert_rel_error


# Used for naming classes we want to create instances of.
_MODULE = 'openmdao.main.test.test_distsim'

# Used for naming server directories.
_SERVER_ID = 0


class Box(ExecComp):
    """ Simple component for testing. """

    pid = Int(iotype='out')

    def __init__(self):
        super(Box, self).__init__([
            'surface_area = (width*(height+depth) + depth*height)*2',
            'volume = width*height*depth'])
        self.pid = os.getpid()

    def execute(self):
        print 'Box.execute(), %f %f %f on %d' \
              % (self.width, self.height, self.depth, self.pid)
        super(Box, self).execute()

    def no_rbac(self):
        pass

    @rbac('owner')
    def cause_parent_error1(self):
        return self.parent.no_such_variable

    @rbac('owner')
    def cause_parent_error2(self):
        return self.parent.get_proxy()


class HollowSphere(Component):
    """ Simple component for testing. """

    radius = Float(1.0, low=0., exclude_low=True, iotype='in', units='cm')
    thickness = Float(0.05, iotype='in', units='cm')

    inner_volume = Float(iotype='out', units='cm**3')
    volume = Float(iotype='out', units='cm**3')
    solid_volume = Float(iotype='out', units='cm**3')
    surface_area = Float(iotype='out', units='cm**2')
    pid = Int(iotype='out')

    def __init__(self, doc=None, directory=''):
        super(HollowSphere, self).__init__(doc, directory) 
        self.pid = os.getpid()

    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume


@add_delegate(HasParameters)
@add_delegate(HasObjectives)
class BoxDriver(Driver):
    """ Just drives :class:`Box` inputs and records results. """

    def __init__(self):
        super(BoxDriver, self).__init__()
        self.recorder = ListCaseRecorder()

    def execute(self):
        """ Runs with various box parameter values. """
        for width in range(1, 2):
            for height in range(1, 3):
                for depth in range(1, 4):
                    self._logger.debug('w,h,d %s, %s, %s', width, height, depth)
                    self.set_parameters((width, height, depth))
                    self.workflow.run()
                    volume, area = self.eval_objectives()
                    self._logger.debug('    v,a %s, %s', volume, area)

                    case = Case()
                    case.inputs = [('width', None, width),
                                   ('height', None, height),
                                   ('depth', None, depth)]
                    case.outputs = [('volume', None, volume),
                                    ('area', None, area),
                                    ('pid', None, self.parent.box.pid)]
                                   # Just to show access to remote from driver.
                    self.recorder.record(case)


class BoxSource(ExecComp):
    """ Just a pass-through for :class:`BoxDriver` input values. """

    def __init__(self):
        super(BoxSource, self).__init__(['width_out  = width_in',
                                         'height_out = height_in',
                                         'depth_out  = depth_in'])

class BoxSink(ExecComp):
    """ Just a pass-through for :class:`BoxDriver` result values. """

    def __init__(self):
        super(BoxSink, self).__init__(['volume_out = volume_in',
                                       'area_out   = area_in'])


class Model(Assembly):
    """ Drive a remote :class:`Box` via connections to local components. """

    def __init__(self, box):
        super(Model, self).__init__()
        self.add('driver', BoxDriver())
        self.driver.workflow.add(self.add('source', BoxSource()))
        self.driver.workflow.add(self.add('box', box))
        self.driver.workflow.add(self.add('sink', BoxSink()))

        self.driver.add_parameter('source.width_in',  low=1e-99, high=1e99)
        self.driver.add_parameter('source.height_in', low=1e-99, high=1e99)
        self.driver.add_parameter('source.depth_in',  low=1e-99, high=1e99)

        self.connect('source.width_out',  'box.width')
        self.connect('source.height_out', 'box.height')
        self.connect('source.depth_out',  'box.depth')

        self.connect('box.volume',       'sink.volume_in')
        self.connect('box.surface_area', 'sink.area_in')

        self.driver.add_objective('sink.volume_out')
        self.driver.add_objective('sink.area_out')


class Protector(AccessController):
    """ Special :class:`AccessController` to protect secrets. """

    def __init__(self):
        set_credentials(Credentials())  # Ensure something is current.
        super(Protector, self).__init__()
        self.owner = Credentials()
        self.owner.user = 'xyzzy@spooks-r-us.com'

    def check_access(self, role, methodname, obj, attr):
        if not role:
            raise RoleError('No access by null role')
        if role == 'owner':
            return
        if methodname != '__delattr__' and self.user_attribute(obj, attr):
            return
        raise RoleError("No %s access to '%s' by role '%s'"
                        % (methodname, attr, role))

    @staticmethod
    def user_attribute(obj, attr):
        if attr in obj.keys(iotype='in') or \
           attr in obj.keys(iotype='out') or \
           attr in ('parent', 'name'):
            return True
        return False

PROTECTOR = Protector()


class ProtectedBox(Box):
    """ Box which can be used but the innards are hidden. """

    secret = Int()

    def __init__(self):
        super(ProtectedBox, self).__init__()

    @rbac('owner')
    def proprietary_method(self):
        pass

    def get_access_controller(self):
        return PROTECTOR

    @rbac(('owner', 'user'))
    def get(self, path, index=None):
        if PROTECTOR.user_attribute(self, path):
            return super(ProtectedBox, self).get(path, index)
        raise RoleError("No get access to '%s' by role '%s'" % (attr, role))

    @rbac(('owner', 'user'))
    def get_dyn_trait(self, name, iotype=None):
        if PROTECTOR.user_attribute(self, name):
            return super(ProtectedBox, self).get_dyn_trait(name, iotype)
        raise RoleError("No get access to '%s' by role '%s'" % (attr, role))

    @rbac(('owner', 'user'))
    def get_wrapped_attr(self, name):
        if PROTECTOR.user_attribute(self, name):
            return super(ProtectedBox, self).get_wrapped_attr(name)
        raise RoleError("No get_wrapped_attr access to '%s' by role '%s'"
                        % (attr, role))

    @rbac(('owner', 'user'))
    def set(self, path, value, index=None, srcname=None, force=False):
        if PROTECTOR.user_attribute(self, path):
            return super(ProtectedBox, self).set(path, value, index, srcname, force)
        raise RoleError("No set access to '%s' by role '%s'"
                        % (attr, role))


class TestCase(unittest.TestCase):
    """ Test distributed simulation. """

    def setUp(self):
        """ Start server process. """
        global _SERVER_ID

        # Start each server process in a unique directory.
        _SERVER_ID += 1
        server_dir = 'Factory_%d' % _SERVER_ID
        if os.path.exists(server_dir):
            shutil.rmtree(server_dir)
        os.mkdir(server_dir)
        os.chdir(server_dir)
        self.server = None
        try:
            logging.debug('')
            logging.debug('tester pid: %s', os.getpid())
            logging.debug('starting server...')
            self.server = start_server()
            self.address, self.port, self.key = read_server_config('server.cfg')
            logging.debug('server pid: %s', self.server.pid)
            logging.debug('server address: %s', self.address)
            logging.debug('server port: %s', self.port)
            logging.debug('server key: %s', self.key)
        finally:
            os.chdir('..')

        # Force a key generation.
        if _SERVER_ID == 1:
            if sys.platform == 'win32':
                home = os.environ['HOMEDRIVE'] + os.environ['HOMEPATH']
            else:
                home = os.environ['HOME']
            key_dir = os.path.join(home, '.openmdao')
            key_file = os.path.join(key_dir, 'keys')
            if os.path.exists(key_file):
                os.remove(key_file)

        set_credentials(Credentials())
        self.factory = connect(self.address, self.port, pubkey=self.key)
        logging.debug('factory: %r', self.factory)

    def tearDown(self):
        """ Shut down server process. """
        if self.factory is not None:
            self.factory.cleanup()
        if self.server is not None:
            logging.debug('terminating server pid %s', self.server.pid)
            self.server.terminate(timeout=10)
            self.server = None
        keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
        if not keep_dirs:
            for path in glob.glob('Factory_*'):
                shutil.rmtree(path)

    def test_1_client(self):
        logging.debug('')
        logging.debug('test_client')

        # List available types.
        types = self.factory.get_available_types()
        logging.debug('Available types:')
        for typname, version in types:
            logging.debug('   %s %s', typname, version)

        # First a HollowSphere, accessed via get()/set().
        obj = self.factory.create(_MODULE+'.HollowSphere')
        sphere_pid = obj.get('pid')
        self.assertNotEqual(sphere_pid, os.getpid())

        radius = obj.get('radius')
        self.assertEqual(radius, 1.)
        radius += 1
        obj.set('radius', radius)
        new_radius = obj.get('radius')
        self.assertEqual(new_radius, 2.)
        self.assertEqual(obj.get('inner_volume'), 0.)
        self.assertEqual(obj.get('volume'), 0.)
        self.assertEqual(obj.get('solid_volume'), 0.)
        self.assertEqual(obj.get('surface_area'), 0.)
        obj.run()
        assert_rel_error(self, obj.get('inner_volume'), 33.510321638, 0.000001)
        assert_rel_error(self, obj.get('volume'),       36.086951213, 0.000001)
        assert_rel_error(self, obj.get('solid_volume'), 2.5766295747, 0.000001)
        assert_rel_error(self, obj.get('surface_area'), 50.265482457, 0.000001)

        msg = ": Trait 'radius' must be a float in the range (0.0, "
        assert_raises(self, "obj.set('radius', -1)", globals(), locals(),
                      TraitError, msg)

        # Now a Box, accessed via attribute methods.
        obj = self.factory.create(_MODULE+'.Box')
        box_pid = obj.get('pid')
        self.assertNotEqual(box_pid, os.getpid())
        self.assertNotEqual(box_pid, sphere_pid)

        obj.width  += 2
        obj.height += 2
        obj.depth  += 2
        self.assertEqual(obj.width, 2.)
        self.assertEqual(obj.height, 2.)
        self.assertEqual(obj.depth, 2.)
        self.assertEqual(obj.volume, 0.)
        self.assertEqual(obj.surface_area, 0.)
        obj.run()
        self.assertEqual(obj.volume, 8.0)
        self.assertEqual(obj.surface_area, 24.0)

        try:
            obj.no_rbac()
        except RemoteError as exc:
            msg = "AttributeError: method 'no_rbac' of"
            logging.debug('msg: %s', msg)
            logging.debug('exc: %s', exc)
            self.assertTrue(msg in str(exc))
        else:
            self.fail('Expected RemoteError')

    def test_2_model(self):
        logging.debug('')
        logging.debug('test_model')

        # Create model and run it.
        box = self.factory.create(_MODULE+'.Box')
        model = set_as_top(Model(box))
        model.run()

        # Check results.
        for width in range(1, 2):
            for height in range(1, 3):
                for depth in range(1, 4):
                    case = model.driver.recorder.cases.pop(0)
                    self.assertEqual(case.outputs[0][2], width*height*depth)

        self.assertTrue(is_instance(model.box.parent, Assembly))
        self.assertTrue(has_interface(model.box.parent, IComponent))

        # Upcall to use parent to resolve sibling.
        # At one time this caused proxy problems.
        source = model.box.parent.source
        self.assertEqual(source.width_in, 1.)

        # Cause server-side errors we can see.

        try:
            box.cause_parent_error1()
        except RemoteError as exc:
            msg = "AttributeError: attribute 'no_such_variable' of"
            logging.debug('msg: %s', msg)
            logging.debug('exc: %s', exc)
            self.assertTrue(msg in str(exc))
        else:
            self.fail('Expected RemoteError')

        try:
            box.cause_parent_error2()
        except RemoteError as exc:
            msg = "AttributeError: method 'get_proxy' of"
            logging.debug('msg: %s', msg)
            logging.debug('exc: %s', exc)
            self.assertTrue(msg in str(exc))
        else:
            self.fail('Expected RemoteError')

    def test_3_access(self):
        logging.debug('')
        logging.debug('test_access')

        # Create model and run it.
        box = self.factory.create(_MODULE+'.ProtectedBox')
        model = set_as_top(Model(box))
        model.run()

        # Check results.
        for width in range(1, 2):
            for height in range(1, 3):
                for depth in range(1, 4):
                    case = model.driver.recorder.cases.pop(0)
                    self.assertEqual(case.outputs[0][2], width*height*depth)

        # Check access protections.
        try:
            i = model.box.secret
        except RemoteError as exc:
            msg = "RoleError: No __getattribute__ access to 'secret' by role 'user'"
            logging.debug('msg: %s', msg)
            logging.debug('exc: %s', exc)
            self.assertTrue(msg in str(exc))
        else:
            self.fail('Expected RemoteError')

        try:
            model.box.proprietary_method()
        except RemoteError as exc:
            msg = "RoleError: proprietary_method(): No access for role 'user'"
            logging.debug('msg: %s', msg)
            logging.debug('exc: %s', exc)
            self.assertTrue(msg in str(exc))
        else:
            self.fail('Expected RemoteError')

        spook = Credentials()
        spook.user = 'xyzzy@spooks-r-us.com'
        saved = get_credentials()
        set_credentials(spook)
        i = model.box.secret
        model.box.proprietary_method()

        # Reset credentials to allow factory shutdown.
        set_credentials(saved)

    def test_4_authkey(self):
        logging.debug('')
        logging.debug('test_authkey')

        # Start server in non-public-key mode.
        # Connections must have matching authkey,
        # but data is sent in the clear!?
        # This is standard multiprocessing behaviour.
        authkey = 'password'
        server_dir = 'Factory_authkey'
        if os.path.exists(server_dir):
            shutil.rmtree(server_dir)
        os.mkdir(server_dir)
        os.chdir(server_dir)
        try:
            logging.debug('starting server (authkey %s)...', authkey)
            server = start_server(authkey=authkey, timeout=30)
            address, port, key = read_server_config('server.cfg')
            logging.debug('server address: %s', address)
            logging.debug('server port: %s', port)
            logging.debug('server key: %s', key)
        finally:
            os.chdir('..')

        factory = None
        try:
            set_credentials(Credentials())
            assert_raises(self, 'connect(address, port, pubkey=key)',
                          globals(), locals(), AuthenticationError,
                          'digest sent was rejected')

            factory = connect(address, port, authkey=authkey)
            logging.debug('factory: %r', factory)

            # Create model and run it.
            box = factory.create(_MODULE+'.Box')
            model = set_as_top(Model(box))
            model.run()

            # Check results.
            for width in range(1, 2):
                for height in range(1, 3):
                    for depth in range(1, 4):
                        case = model.driver.recorder.cases.pop(0)
                        self.assertEqual(case.outputs[0][2], width*height*depth)
        finally:
            if factory is not None:
                factory.cleanup()
            logging.debug('terminating server (authkey %s) pid %s',
                          authkey, server.pid)
            server.terminate(timeout=10)
            server = None

    def test_5_misc(self):
        logging.debug('')
        logging.debug('test_misc')

        # Try releasing a server twice.
        server = self.factory.create('')
        self.factory.release(server)
        assert_raises(self, 'self.factory.release(server)', globals(), locals(),
                      ValueError, "can't identify server ")

        # Check false return of has_interface().
        self.assertFalse(has_interface(self.factory, HasObjectives))

        # Check that credentials are required.
        credentials = get_credentials()
        set_credentials(None)
        msg = 'No credentials for PublicKey authentication of get_available_types'
        assert_raises(self, 'self.factory.get_available_types()',
                      globals(), locals(), RuntimeError, msg)
        set_credentials(credentials)

        # Try to connect to wrong port (assuming port+1 isn't being used!)
        port = self.port + 1
        assert_raises(self, 'connect(self.address, port, pubkey=self.key)',
                      globals(), locals(), RuntimeError, "can't connect to ")

        # Try to read config from non-existent file.
        assert_raises(self, "read_server_config('no-such-file')",
                      globals(), locals(), IOError,
                      "No such file 'no-such-file'")

        # Exercise SHA wrapper.
        sha1 = _SHA1.new('abc')
        self.assertEqual(sha1.digest(),
                         '\xa9\x99>6G\x06\x81j\xba>%qxP\xc2l\x9c\xd0\xd8\x9d')

        # Unpickleable argument.
        code = compile('3 + 4', '<string>', 'eval')
        assert_raises(self, 'self.factory.echo(code)', globals(), locals(),
                      TypeError, "can't pickle code objects")


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

