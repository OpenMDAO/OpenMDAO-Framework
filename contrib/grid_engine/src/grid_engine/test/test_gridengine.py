import logging
import nose
import os.path
import pkg_resources
import sys
import unittest

from grid_engine import GridEngineAllocator, GridEngineServer


class TestCase(unittest.TestCase):

    directory = os.path.realpath(
        pkg_resources.resource_filename('grid_engine', 'test'))

    def setUp(self):
        # Force use of fake 'qsub'.
        self.orig_qsub = GridEngineServer._QSUB
        GridEngineServer._QSUB = os.path.join(TestCase.directory, 'qsub')

        # Force use of fake 'qhost'.
        self.orig_qhost = GridEngineAllocator._QHOST
        GridEngineAllocator._QHOST = os.path.join(TestCase.directory, 'qhost')

    def tearDown(self):
        GridEngineServer._QSUB = self.orig_qsub
        GridEngineAllocator._QHOST = self.orig_qhost
        for name in ('echo.stdout', 'qsub.out'):
            if os.path.exists(name):
                os.remove(name)

    def test_allocator(self):
        logging.debug('')
        logging.debug('test_allocator')
        allocator = GridEngineAllocator()
        nhosts = allocator.max_servers({})
        self.assertEqual(nhosts, 19*48)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')
        server = GridEngineServer()
        server.execute_command(dict(remote_command='echo',
                                    args=['hello', 'world']))

        with open('echo.stdout', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(lines, ['hello world\n'])

        with open('qsub.out', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(''.join(lines), """\
-V -sync yes -cwd -i /dev/null -o echo.stdout -j yes echo hello world
-V
-sync arg yes
-cwd
-i stdin /dev/null
-o stdout echo.stdout
-j join yes
+ '[' 1 -eq 1 ']'
+ echo hello world
""")


if __name__ == '__main__':
    sys.argv.append('--cover-package=grid_engine.')
    sys.argv.append('--cover-erase')
    nose.runmodule()

