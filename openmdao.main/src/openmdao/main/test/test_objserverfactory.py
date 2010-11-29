"""
Test object service sub-objects for better test coverage.
Most functionality is exercised by test_distsim.py or test_extcode.py,
but in an unobservable manner as far as test coverage is concerned.
"""

import logging
import os.path
import shutil
import sys
import time
import unittest
import nose

from openmdao.main.objserverfactory import ObjServerFactory, ObjServer, \
                                           start_server
from openmdao.main.rbac import Credentials, set_credentials
from openmdao.util.testutil import assert_raises


class TestCase(unittest.TestCase):
    """ Test object service sub-objects for better test coverage. """

    def test_factory(self):
        logging.debug('')
        logging.debug('test_factory')

        testdir = 'test_factory'
        if os.path.exists(testdir):
            shutil.rmtree(testdir)
        os.mkdir(testdir)
        os.chdir(testdir)

        factory = None
        try:
            # Create a factory.
            set_credentials(Credentials())
            factory = ObjServerFactory()

            # Echo some arguments.
            args = factory.echo('Hello', 'world!')
            self.assertEqual(args[0], 'Hello')
            self.assertEqual(args[1], 'world!')

            # List types.
            types = factory.get_available_types()
            names = [name for name, version in types]
            self.assertTrue('openmdao.test.ExecComp' in names)

            # Create a component.
            exec_comp = factory.create('openmdao.test.ExecComp')
            exec_comp.run()

            # Force failed factory server startup via invalid port.
            assert_raises(self, "start_server(port='xyzzy')",
                          globals(), locals(), RuntimeError,
                          'Server startup failed')
            # Try to release a server that doesn't exist (release takes an
            # OpenMDAO_Proxy as argument, string here is easier).
            assert_raises(self, "factory.release('xyzzy')",
                          globals(), locals(), ValueError,
                          "can't identify server at 'not-a-proxy'")
        finally:
            if factory is not None:
                factory.cleanup()
            os.chdir('..')
            if sys.platform == 'win32':
                time.sleep(2)  # Wait for process shutdown.
            keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
            if not keep_dirs:
                shutil.rmtree(testdir)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')
        
        testdir = 'test_server'
        if os.path.exists(testdir):
            shutil.rmtree(testdir)
        os.mkdir(testdir)
        os.chdir(testdir)

        try:
            # Create a server.
            server = ObjServer()

            # Create a component.
            exec_comp = server.create('openmdao.test.ExecComp')
            exec_comp.run()
            egg_info = exec_comp.save_to_egg('exec_comp', '0')

            # Echo some arguments.
            args = server.echo('Hello', 'world!')
            self.assertEqual(args[0], 'Hello')
            self.assertEqual(args[1], 'world!')

            # Execute a command.
            cmd = 'dir' if sys.platform == 'win32' else 'ls'
            return_code, error_msg = \
                server.execute_command(cmd, None, 'cmd.out', None, None, 0, 10)
            self.assertEqual(return_code, 0)

            # Non-zero return code.
            return_code, error_msg = \
                server.execute_command('no-such-command',
                                       None, 'stdout1', 'stderr1', None, 0, 10)
            self.assertNotEqual(return_code, 0)

            # Exception creating process.
# FIXME: despite the files being closed, Windows thinks they're in use :-(
            if sys.platform != 'win32':
                try:
                    server.execute_command(['no-such-command'],
                                           None, 'stdout2', 'stderr2', None, 0, 10)
                except OSError as exc:
                    msg = '[Errno 2] No such file or directory'
                    self.assertEqual(str(exc), msg)
                else:
                    self.fail('Expected OSError')

            # Load a model.
            obj = server.load_model(egg_info[0])
            obj.run()

            assert_raises(self, "server.load_model('no-such-egg')",
                          globals(), locals(), ValueError,
                          "'no-such-egg' not found.")

            # Bogus file accesses.
            assert_raises(self, "server.open('../xyzzy', 'r')", globals(), locals(),
                          RuntimeError, "Can't open '../xyzzy', not within root ")
            assert_raises(self, "server.open('xyzzy', 'r')", globals(), locals(),
                          IOError, "[Errno 2] No such file or directory: 'xyzzy'")

            # Create a file using context.
            with server.open('xyzzy', 'w') as out:
                 out.write('Hello world!\n')

            # Create another file using file proxy.
            out = server.open('fred', 'w')
            out.write('Hello fred!\n')
            out.flush()  # Not really necessary, just for coverage.
            out.close()

            # Zip it.
            server.pack_zipfile(['xyzzy', 'fred'], 'zipped')

            # Make it read-only.
            server.chmod('zipped', 0400)

            if sys.platform == 'win32':
                msg = '[Error 2] The system cannot find the file specified'
            else:
                msg = "[Errno 2] No such file or directory: 'no-such-file'"
            assert_raises(self, "server.chmod('no-such-file', 0400)",
                          globals(), locals(), OSError, msg) 

            # Get stats.
            info = server.stat('zipped')

            assert_raises(self, "server.stat('no-such-file')",
                          globals(), locals(), OSError, msg)

            # Remove zipped contents.
            server.remove('xyzzy')
            server.remove('fred')

            if sys.platform == 'win32':
                msg = '[Error 2] The system cannot find the file specified'
            else:
                msg = "[Errno 2] No such file or directory: 'xyzzy'"
            assert_raises(self, "server.remove('xyzzy')",
                          globals(), locals(), OSError, msg)

            # Unpack.
            server.unpack_zipfile('zipped')
            server.chmod('zipped', 0600)  # Ensure we can remove under Windows.

            # Verify contents.
            with server.open('xyzzy', 'r') as inp:
                data = inp.read()
                self.assertEqual(data, 'Hello world!\n')

            inp = server.open('fred', 'r')
            try:
                data = inp.read(1000)
                self.assertEqual(data, 'Hello fred!\n')
            finally:
                inp.close()

        finally:
            os.chdir('..')
            shutil.rmtree(testdir)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

