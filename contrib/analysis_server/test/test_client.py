import getpass
import glob
import os.path
import pkg_resources
import platform
import shutil
import socket
import sys
import time
import unittest
import nose

from openmdao.util.testutil import assert_raises

import analysis_server

ORIG_DIR = os.getcwd()


class TestCase(unittest.TestCase):
    """ Test AnalysisServer emulation. """

    directory = os.path.realpath(
        pkg_resources.resource_filename('analysis_server', 'test'))

    def setUp(self):
        """ Called before each test. """
        os.chdir(TestCase.directory)
        self.server, self.port = analysis_server.start_server(port=0)
        self.client = analysis_server.Client(port=self.port)

    def tearDown(self):
        """ Called after each test. """
        self.client.quit()
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

    def test_add_proxy_clients(self):
        self.client.add_proxy_clients('clientHost1', 'clientHost2')

    def test_describe(self):
        expected = {
            'Version': '0.2',
            'Author': 'anonymous  ( & < > )',
            'hasIcon': 'false',
            'Description': 'Component for testing AnalysisServer functionality.\nAn additional description line.  ( & < > )',
            'Help URL': '',
            'Keywords': '',
            'Driver': 'false',
            'Time Stamp': '',
            'Requirements': '',
            'HasVersionInfo': 'true',
            'Checksum': '0',
        }
        expected['Time Stamp'] = \
            time.ctime(os.path.getmtime('ASTestComp-0.2.cfg'))
        result = self.client.describe('ASTestComp')
        self.assertEqual(result, expected)

    def test_end(self):
        self.client.start('ASTestComp', 'comp')
        self.client.end('comp')

    def test_execute(self):
        self.client.start('ASTestComp', 'comp')
        self.client.set('comp.in_file', 'Hello world!')
        self.client.execute('comp')
        self.client.execute('comp', background=True)

    def test_get(self):
        self.client.start('ASTestComp', 'comp')
        result = self.client.get('comp.x')
        self.assertEqual(result, '2')

    def test_get_branches(self):
        result = self.client.get_branches_and_tags()
        self.assertEqual(result, '')

    def test_get_direct(self):
        result = self.client.get_direct_transfer()
        self.assertFalse(result)

    def test_get_icon(self):
        code = "self.client.get_icon('ASTestComp')"
        assert_raises(self, code, globals(), locals(), RuntimeError, '')

    def test_get_license(self):
        expected = 'Use at your own risk!'
        result = self.client.get_license()
        self.assertEqual(result, expected)

    def test_get_status(self):
        expected = {'comp': 'ready'}
        self.client.start('ASTestComp', 'comp')
        result = self.client.get_status()
        self.assertEqual(result, expected)

    def test_get_sys_info(self):
        expected = {
            'version': '5.01',
            'build': '331',
            'num clients': '1',
            'num components': '3',
            'os name': platform.system(),
            'os arch': platform.processor(),
            'os version': platform.release(),
            'python version': platform.python_version(),
            'user name': getpass.getuser(),
        }
        result = self.client.get_sys_info()
        self.assertEqual(result, expected)

    def test_get_version(self):
        expected = """\
OpenMDAO Analysis Server 0.1
Use at your own risk!
Attempting to support Phoenix Integration, Inc.
version: 5.01, build: 331"""
        result = self.client.get_version()
        self.assertEqual(result, expected)

    def test_heartbeat(self):
        self.client.heartbeat(True)
        self.client.heartbeat(False)

    def test_help(self):
        expected = [
            'Available Commands:',
            'listComponents,lc [category]',
            'listCategories,la [category]',
            'describe,d <category/component> [-xml]',
            'start <category/component> <instanceName>',
            'end <object>',
            'execute,x <objectName>',
            'listProperties,list,ls,l [object]',
            'listGlobals,lg',
            'listValues,lv <object>',
            'listArrayValues,lav <object> (NOT IMPLEMENTED)',
            'get <object.property>',
            'set <object.property> = <value>',
            'move,rename,mv,rn <from> <to> (NOT IMPLEMENTED)',
            'getIcon <analysisComponent> (NOT IMPLEMENTED)',
            'getVersion',
            'getLicense',
            'getStatus',
            'help,h',
            'quit',
            'getSysInfo',
            'invoke <object.method()> [full]',
            'listMethods,lm <object> [full]',
            'addProxyClients <clientHost1>,<clientHost2>',
            'monitor start <object.property>, monitor stop <id>',
            'versions,v category/component',
            'ps <object>',
            'listMonitors,lo <objectName>',
            'heartbeat,hb [start|stop]',
            'listValuesURL,lvu <object>',
            'getDirectTransfer',
            'getByUrl <object.property> <url> (NOT IMPLEMENTED)',
            'setByUrl <object.property> = <url> (NOT IMPLEMENTED)',
            'setDictionary <xml dictionary string> (NOT IMPLEMENTED)',
            'getHierarchy <object.property>',
            'setHierarchy <object.property> <xml>',
            'deleteRunShare <key> (NOT IMPLEMENTED)',
            'getBranchesAndTags',
        ]
        result = self.client.help()
        self.assertEqual(result, expected)

    def test_invoke(self):
        self.client.start('ASTestComp', 'comp')
        result = self.client.invoke('comp.float_method')
        self.assertEqual(result, '0')
        result = self.client.invoke('comp.null_method')
        self.assertEqual(result, '')
        result = self.client.invoke('comp.str_method')
        self.assertEqual(result,
                         'current state: x 2.0, y 3.0, z 0.0, exe_count 0')

    def test_list_array_values(self):
        self.client.start('ASTestComp', 'comp')
        code = "self.client.list_array_values('comp')"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Exception: NotImplementedError('listArrayValues',)")

    def test_list_categories(self):
        result = self.client.list_categories('/')
        self.assertEqual(result, [])

    def test_list_components(self):
        result = self.client.list_components('/')
        self.assertEqual(result, ['ASTestComp', 'ASTestComp2'])

    def test_list_globals(self):
        result = self.client.list_globals()
        self.assertEqual(result, [])

    def test_list_methods(self):
        self.client.start('ASTestComp', 'comp')
        result = self.client.list_methods('comp')
        self.assertEqual(result, ['cause_exception',
                                  'float_method',
                                  'null_method',
                                  'str_method'])

        result = self.client.list_methods('comp', full=True)
        self.assertEqual(result, [('cause_exception', 'cause_exception'),
                                  ('float_method', 'float_method'),
                                  ('null_method', 'null_method'),
                                  ('str_method', 'str_method')])

    def test_list_monitors(self):
        self.client.start('ASTestComp', 'comp')
        result = self.client.list_monitors('comp')
        expected = [
            'ASTestComp.py',
            'ASTestComp_loader.py',
            '__init__.py',
        ]
        self.assertEqual(result, expected)

    def test_list_properties(self):
        self.client.start('ASTestComp', 'comp')
        result = self.client.list_properties()
        self.assertEqual(result, ['comp'])

        expected = [
            ('exe_count', 'PHXLong', 'out'),
            ('in_file', 'PHXRawFile', 'in'),
            ('out_file', 'PHXRawFile', 'out'),
            ('sub_group', 'PHXGroup', 'in'),
            ('x', 'PHXDouble', 'in'),
            ('y', 'PHXDouble', 'in'),
            ('z', 'PHXDouble', 'out'),
        ]
        result = self.client.list_properties('comp')
        self.assertEqual(result, expected)

    def test_monitor(self):
        self.client.start('ASTestComp', 'comp')
        result, monitor_id = self.client.start_monitor('comp.ASTestComp_loader.py')
        expected = """\
import os
import sys
if not '.' in sys.path:
    sys.path.append('.')

try:
    from openmdao.main.api import Component, SAVE_CPICKLE
except ImportError:
    print 'No OpenMDAO distribution available.'
    if __name__ != '__main__':
        print 'You can unzip the egg to access the enclosed files.'
        print 'To get OpenMDAO, please visit openmdao.org'
    sys.exit(1)

def load(**kwargs):
    '''Create object(s) from state file.'''
    return Component.load('ASTestComp.pickle', SAVE_CPICKLE, **kwargs)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
"""
        self.assertEqual(result[:len(expected)], expected)

        self.client.stop_monitor(monitor_id)

    def test_move(self):
        code = "self.client.move('from', 'to')"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Exception: NotImplementedError('move',)")

    def test_ps(self):
        expected = [{
            'PID': 0,
            'ParentPID': 0,
            'PercentCPU': 0.,
            'Memory': 0,
            'Time': 0.,
            'WallTime': 0.,
            'Command': os.path.basename(sys.executable),
        }]
        self.client.start('ASTestComp', 'comp')
        process_info = self.client.ps('comp')
        self.assertEqual(process_info, expected)

    def test_publish(self):
        result = self.client.list_components('/')
        self.assertFalse('Published' in result)
        try:
            analysis_server.publish_class('Published', '1', 'Publishing test',
                                          'ASTestComp.py', 'TestComponent',
                                          port=self.port)
            result = self.client.list_components('/')
            self.assertTrue('Published' in result)
            self.client.start('Published', 'comp')
#            self.client.set('comp.in_file', 'Hello world!')
#            self.client.execute('comp')
        finally:
            for path in glob.glob('Published*'):
                if os.path.isdir(path):
                    shutil.rmtree(path)
                else:
                    os.remove(path)

        code = "analysis_server.publish_class('Published', '1'," \
                                            " 'Publishing test'," \
                                            " 'NoComp.py', 'TestComponent'," \
                                            " port=self.port)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't import 'NoComp': ")

        code = "analysis_server.publish_class('Published', '1'," \
                                            " 'Publishing test'," \
                                            " 'ASTestComp.py', 'NoClass'," \
                                            " port=self.port)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't get class 'NoClass' in 'ASTestComp': ")

        code = "analysis_server.publish_class('Published', '1'," \
                                            " 'Publishing test'," \
                                            " 'ASTestComp.py', 'Bogus'," \
                                            " port=self.port)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't instantiate ASTestComp.Bogus: ")

        code = "analysis_server.publish_class('Published', '1'," \
                                            " 'Publishing test'," \
                                            " 'ASTestComp.py', 'TestComponent'," \
                                            " host='NoSuchHost', port=self.port)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't connect to NoSuchHost:%d: " % self.port)

    def test_quit(self):
        self.client.quit()

    def test_set(self):
        self.client.start('ASTestComp', 'comp')
        self.client.set('comp.x', '42')

    def test_set_mode(self):
        self.client.set_mode_raw()
        result = self.client.list_components()
        self.assertEqual(result, ['ASTestComp', 'ASTestComp2'])

        self.assertTrue(self.client._stream.raw)

        code = "self.client._stream.raw = False"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "Can only transition from 'cooked' to 'raw'",
                      use_exec=True)

    def test_start(self):
        self.client.start('ASTestComp', 'comp')

    def test_versions(self):
        result = self.client.versions('ASTestComp')
        self.assertEqual(result, ['0.1', '0.2'])


if __name__ == '__main__':
    sys.argv.append('--cover-package=analysis_server')
    sys.argv.append('--cover-erase')
    nose.runmodule()

