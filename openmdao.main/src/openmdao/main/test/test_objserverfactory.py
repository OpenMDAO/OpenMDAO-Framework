"""
Test object service sub-objects for better test coverage.
Most functionality is exercised by test_distsim.py or test_extcode.py,
but in an unobservable manner as far as test coverage is concerned.
"""

import logging
import os.path
import shutil
import socket
import sys
import time
import unittest
import nose

from openmdao.main.component import SimulationRoot
from openmdao.main.objserverfactory import ObjServerFactory, ObjServer, \
                                           start_server, stop_server, \
                                           connect_to_server, _PROXIES
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.util.testutil import assert_raises
from openmdao.util.fileutil import onerror


class TestCase(unittest.TestCase):
    """ Test object service sub-objects for better test coverage. """

    def test_factory(self):
        logging.debug('')
        logging.debug('test_factory')

        testdir = 'test_factory'
        if os.path.exists(testdir):
            shutil.rmtree(testdir, onerror=onerror)
        os.mkdir(testdir)
        os.chdir(testdir)

        factory = None
        try:
            # Create a factory.
            factory = ObjServerFactory()

            # Echo some arguments.
            args = factory.echo('Hello', 'world!')
            self.assertEqual(args[0], 'Hello')
            self.assertEqual(args[1], 'world!')

            # List types.
            types = factory.get_available_types()
            names = [name for name, version in types]
            self.assertTrue('openmdao.test.execcomp.ExecComp' in names)

            # Create a component.
            exec_comp = factory.create('openmdao.test.execcomp.ExecComp')
            exec_comp.run()
            directory = 'Server_1'+os.sep
            if sys.platform == 'win32':
                directory = directory.lower()
            self.assertTrue(exec_comp.get_abs_directory().endswith(directory))

            # Create another in specified directory.
            exec_comp = factory.create('openmdao.test.execcomp.ExecComp',
                                       res_desc={'working_directory': 'floyd'})
            exec_comp.run()
            directory = 'floyd'+os.sep
            if sys.platform == 'win32':
                directory = directory.lower()
            self.assertTrue(exec_comp.get_abs_directory().endswith(directory))

            # Start server, connect, stop.
            server, cfg = start_server()
            proxy = connect_to_server(cfg)
            shutil.copy(cfg, 'saved_cfg')
            stop_server(server, cfg)

            _PROXIES.clear()
            assert_raises(self, "connect_to_server('saved_cfg')",
                          globals(), locals(), RuntimeError,
                          "Can't connect to server at ")

            # Force failed factory server startup via invalid port.
            address = socket.gethostbyname(socket.gethostname())
            code = "start_server(address=address, port='xyzzy'," \
                                 " allow_shell=True, tunnel=True, resources='')"
            assert_raises(self, code, globals(), locals(), RuntimeError,
                          'Server startup failed')

            # Try to release a server that doesn't exist (release takes an
            # OpenMDAO_Proxy as argument, string here is easier).
            assert_raises(self, "factory.release('xyzzy')",
                          globals(), locals(), ValueError,
                          "can't identify server at 'not-a-proxy'")

            # get_ram() is used by RAM.add_remotes().
            ram = factory.get_ram()
            self.assertTrue(ram is RAM._get_instance())

        finally:
            if factory is not None:
                factory.cleanup()
            SimulationRoot.chroot('..')
            if sys.platform == 'win32':
                time.sleep(2)  # Wait for process shutdown.
            keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
            if not keep_dirs:
                shutil.rmtree(testdir, onerror=onerror)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')

        testdir = 'test_server'
        if os.path.exists(testdir):
            shutil.rmtree(testdir, onerror=onerror)
        os.mkdir(testdir)
        os.chdir(testdir)

        try:
            # Create a server.
            server = ObjServer()

            # Create a component.
            exec_comp = server.create('openmdao.test.execcomp.ExecComp')
            exec_comp.run()
            egg_info = exec_comp.save_to_egg('exec_comp', '0')

            # Echo some arguments.
            args = server.echo('Hello', 'world!')
            self.assertEqual(args[0], 'Hello')
            self.assertEqual(args[1], 'world!')

            # Try to execute a command.
            cmd = 'dir' if sys.platform == 'win32' else 'ls'
            rdesc = {'remote_command': cmd,
                     'output_path': 'cmd.out',
                     'hard_runtime_limit': 10}
            code = 'server.execute_command(rdesc)'
            assert_raises(self, code, globals(), locals(), RuntimeError,
                          'shell access is not allowed by this server')

            # Try to load a model.
            assert_raises(self, 'server.load_model(egg_info[0])',
                          globals(), locals(), RuntimeError,
                          'shell access is not allowed by this server')

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

            # Try to create a process.
            args = 'dir' if sys.platform == 'win32' else 'ls'
            try:
                proc = server.create('subprocess.Popen', args=args, shell=True)
                proc.wait()
            except TypeError as exc:
                msg = "'subprocess.Popen' is not an allowed type"
                self.assertEqual(str(exc), msg)
            else:
                self.fail('Expected TypeError')

            # isdir().
            self.assertTrue(server.isdir('.'))

            # listdir().
            self.assertEqual(sorted(server.listdir('.')),
                             [egg_info[0], 'fred', 'xyzzy', 'zipped'])
            if sys.platform == 'win32':
                msg = "[Error 3] The system cannot find the path specified: '42/*.*'"
            else:
                msg = "[Errno 2] No such file or directory: '42'"
            assert_raises(self, "server.listdir('42')",
                          globals(), locals(), OSError, msg)
        finally:
            SimulationRoot.chroot('..')
            shutil.rmtree(testdir, onerror=onerror)

    def test_shell(self):
        logging.debug('')
        logging.debug('test_shell')

        testdir = 'test_shell'
        if os.path.exists(testdir):
            shutil.rmtree(testdir, onerror=onerror)
        os.mkdir(testdir)
        os.chdir(testdir)

        try:
            # Create a server.
            server = ObjServer(allow_shell=True)

            # Execute a command.
            if sys.platform == 'win32':
                cmd = 'cmd'
                args = ['/c', 'dir']
            else:
                cmd = 'ls'
                args = []
            rdesc = {'remote_command': cmd,
                     'args': args,
                     'output_path': 'cmd.out'}
            return_code, error_msg = server.execute_command(rdesc)
            self.assertEqual(return_code, 0)

            # Bad command, specify lots of resources.
            with open('stdin1', 'w') as out:
                out.write('z')
            rdesc = {'remote_command': 'no-such-command',
                     'args': ['a', 'b', 'c'],
                     'input_path': 'stdin1',
                     'error_path': 'stderr1',
                     'wallclock_time': 10}
            if sys.platform == 'win32':
                msg = '[Error 2] The system cannot find the file specified'
            else:
                msg = '[Errno 2] No such file or directory'
            assert_raises(self, 'server.execute_command(rdesc)',
                          globals(), locals(), OSError, msg)

            # Load a model.
            exec_comp = server.create('openmdao.test.execcomp.ExecComp')
            exec_comp.run()
            egg_info = exec_comp.save_to_egg('exec_comp', '0')
            obj = server.load_model(egg_info[0])
            obj.run()

            assert_raises(self, "server.load_model('no-such-egg')",
                          globals(), locals(), ValueError,
                          "'no-such-egg' not found.")
        finally:
            SimulationRoot.chroot('..')
            shutil.rmtree(testdir, onerror=onerror)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()
