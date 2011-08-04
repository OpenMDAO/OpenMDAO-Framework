"""
Most server functionality is tested by test_client.py and test_proxy.py, but it
would be in the wrong process to be observed for test coverage purposes.
"""

import ConfigParser
import getpass
import glob
import os.path
import pkg_resources
import platform
import shutil
import sys
import time
import unittest
import nose

from openmdao.util.testutil import assert_raises

from analysis_server.server import Server, _Handler
from analysis_server.monitor import BaseMonitor
from analysis_server.wrapper import lookup

ORIG_DIR = os.getcwd()


class DummySocket(object):
    """ Something to provide requests and accept replies. """

    count = 0

    def __init__(self):
        self.request_data = []
        self.replies = []
        DummySocket.count += 1
        self.port = DummySocket.count

    def set_command(self, cmd, raw=False):
        """ Set command to be 'sent' to the server. """
        if raw:
            self.request_data = [chunk for chunk in cmd]
        else:
            if isinstance(cmd, basestring):
                self.request_data = ['%s\n' % cmd]
            else:
                self.request_data = ['%s\n' % line for line in cmd]
        self.replies = []

    def get_replies(self):
        """ Get list of server replies. """
        return self.replies

    def getpeername(self):
        """ Return name of 'socket' peer. """
        return ('ClientHost', self.port)

    def getsockname(self):
        """ Return name of 'socket'. """
        return ('ServerHost', 1835)

    def recv(self, maxlen):
        """ Return 'received' request. """
        try:
            return self.request_data.pop(0)
        except Exception:
            return ''

    def sendall(self, reply):
        """ 'Send' reply. """
        self.replies.append(reply)


class TestCase(unittest.TestCase):
    """ Test AnalysisServer emulation. """

    directory = os.path.realpath(
        pkg_resources.resource_filename('analysis_server', 'test'))

    def setUp(self):
        """ Called before each test. """
        os.chdir(TestCase.directory)
        if not os.path.exists('logs'):
            os.mkdir('logs')
        self.client = DummySocket()
        self.server = Server(port=0)
        self.server.per_client_loggers = False  # Avoid cleanup issues.
        self.handler = _Handler(self.client, self.client.getpeername(),
                                self.server)
        self.handler.setup()
        self.handler._server_per_obj = False

    def tearDown(self):
        """ Called after each test. """
        self.server.socket.close()
        del self.handler
        del self.server
        del self.client
        for egg in glob.glob('*.egg'):
            os.remove(egg)
        for filename in ('in_file.dat', 'inFile.dat', 'outFile.dat'):
            if os.path.exists(filename):
                os.remove(filename)
        for dirname in ('ASTestComp', 'logs'):
            if os.path.exists(dirname):
                shutil.rmtree(dirname)
        os.chdir(ORIG_DIR)

    def send_recv(self, cmd, raw=False, count=None):
        """ Set request, process, return replies. """
        self.client.set_command(cmd, raw)
        self.handler.handle()
        replies = self.client.get_replies()
        if count is not None:
            retries = 0
            while len(replies) < count:
                retries += 1
                if retries >= 100:
                    break
                time.sleep(0.1)
            replies = self.client.get_replies()
        return replies

    def compare(self, reply, expected):
        reply_lines = reply.split('\n')
        expected_lines = expected.split('\n')
        for i, reply_line in enumerate(reply_lines):
            if i >= len(expected_lines):
                self.fail('%d reply lines, %d expected lines'
                      % (len(reply_lines), len(expected_lines)))
            expected_line = expected_lines[i]
            if reply_line != expected_line:
                self.fail('Line %d: %r vs. %r'
                           % (i+1, reply_line, expected_line))
        if len(reply_lines) != len(expected_lines):
            self.fail('%d reply lines, %d expected lines'
                      % (len(reply_lines), len(expected_lines)))

    def test_host_filtering(self):
        self.assertTrue(self.server.verify_request(self.client,
                                                   ('127.0.0.1', 1234)))
        self.assertFalse(self.server.verify_request(self.client,
                                                    ('192.168.1.1', 1234)))
    def test_invalid(self):
        self.assertEqual(lookup(['no-such-type']), None)

        replies = self.send_recv('no-such-command')
        self.assertEqual(replies[-1],
                         'ERROR: command <no-such-command> not recognized\r\n>')

        replies = self.send_recv('describe NoSuchComp')
        self.assertEqual(replies[-1],
                         'ERROR: component </NoSuchComp>'
                         ' does not match a known component\r\n>')
        replies = self.send_recv('describe NoSuchComp?1')
        self.assertEqual(replies[-1],
                         'ERROR: component </NoSuchComp?1>'
                         ' does not match a known component\r\n>')

        replies = self.send_recv('execute froboz')
        self.assertEqual(replies[-1],
                         'ERROR: no such object: <froboz>\r\n>')

        self.assertEqual(self.server.config_errors, 0)

        code = "self.server._read_config('no-such-file.cfg'," \
                                       " self.handler._logger)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't read 'no-such-file.cfg'")

        config = ConfigParser.SafeConfigParser()
        code = "self.server._process_config(config, 'test_invalid.cfg'," \
                                          " self.handler._logger)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "No Python section in 'test_invalid.cfg'")

        config.add_section('Description')
        config.add_section('Python')
        config.add_section('Inputs')
        config.add_section('Outputs')
        config.add_section('Methods')
        assert_raises(self, code, globals(), locals(), ValueError,
                      "No version in .cfg file or .cfg filename")

        config.set('Description', 'version', '0.1')
        config.set('Python', 'filename', 'no-such-dir/no-such-file.py')
        config.set('Python', 'classname', 'no-such-class')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't import 'no-such-file'")

        config.set('Python', 'filename', 'ASTestComp.py')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't get class 'no-such-class' in 'ASTestComp'")

        config.set('Python', 'classname', 'Bogus')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Can't instantiate ASTestComp.Bogus")

        config.set('Python', 'classname', 'TestComponent')
        config.set('Methods', 'ext_name', 'no_such_method')
        code = "self.server._process_config(config, 'ASTestComp-0.1.cfg'," \
                                          " self.handler._logger)"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Bad configuration in 'ASTestComp-0.1.cfg':"
                      " 'no_such_method' is not a valid method")

        config.set('Methods', '*', 'float_method')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Bad configuration in 'ASTestComp-0.1.cfg':"
                      " internal name must be '*' if the external name is '*'")

        config.set('Inputs', 'ext_path', 'z')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Bad configuration in 'ASTestComp-0.1.cfg':"
                      " 'z' is not a valid 'in' variable")

        config.set('Inputs', '*', 'x')
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Bad configuration in 'ASTestComp-0.1.cfg':"
                      " internal path must be '*' if the external path is '*'")

    def test_add_proxy_clients(self):
        replies = self.send_recv('addProxyClients clientHost1, clientHost2')
        self.assertEqual(replies[-1], 'Client hosts added.\r\n>')

        replies = self.send_recv('addProxyClients')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'addProxyClients <clientHost1>, ...\r\n>')

    def test_describe(self):
        timestamp = time.ctime(os.path.getmtime('ASTestComp-0.2.cfg'))
        reply = """\
Version: 0.2
Author: anonymous  ( & < > )
hasIcon: false
Description: Component for testing AnalysisServer functionality.
An additional description line.  ( & < > )
Help URL: 
Keywords: 
Driver: false
Time Stamp: %s
Requirements: 
HasVersionInfo: %s
Checksum: 0"""
        reply = reply.replace('\n', '\r\n') + '\r\n>'

        expected = reply % (timestamp, 'true')
        replies = self.send_recv('describe ASTestComp')
        self.compare(replies[-1], expected)
        replies = self.send_recv('d ASTestComp')
        self.compare(replies[-1], expected)

        expected = reply % (timestamp, 'false')
        replies = self.send_recv('describe ASTestComp?0.2')
        self.compare(replies[-1], expected)

        reply = """\
<Description>
 <Version>0.2</Version>
 <Author>anonymous  ( &amp; &lt; &gt; )</Author>
 <Description>Component for testing AnalysisServer functionality.
An additional description line.  ( &amp; &lt; &gt; )</Description>
 <HelpURL></HelpURL>
 <Keywords></Keywords>
 <TimeStamp>%s</TimeStamp>
 <Checksum>0</Checksum>
 <Requirements></Requirements>
 <hasIcon>false</hasIcon>
 <HasVersionInfo>%s</HasVersionInfo>
</Description>"""
        reply = reply.replace('\n', '\r\n') + '\r\n>'

        expected = reply % (timestamp, 'true')
        replies = self.send_recv('describe ASTestComp -xml')
        self.compare(replies[-1], expected)
        replies = self.send_recv('d ASTestComp -xml')
        self.compare(replies[-1], expected)

        expected = reply % (timestamp, 'false')
        replies = self.send_recv('describe ASTestComp?0.2 -xml')
        self.compare(replies[-1], expected)

        replies = self.send_recv('describe')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'describe,d <category/component> [-xml]\r\n>')

    def test_end(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'end comp'])
        self.assertEqual(replies[-1],
                         'comp completed.\r\nObject comp ended.\r\n>')

        replies = self.send_recv('end froboz')
        self.assertEqual(replies[-1],
                         'ERROR: no such object: <froboz>\r\n>')

        replies = self.send_recv('end')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'end <object>\r\n>')

    def test_execute(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'execute comp'], count=4)
        self.assertEqual(replies[-1], 'comp completed.\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'x comp'], count=4)
        self.assertEqual(replies[-1], 'comp completed.\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'execute comp&'], count=4)
        self.assertEqual(replies[-1], 'comp completed.\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'execute comp &'], count=4)
        self.assertEqual(replies[-1], 'comp completed.\r\n>')

        replies = self.send_recv('execute')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'execute,x <objectName>[&]\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'set comp.x = -1',
                                  'execute comp'], count=5)
        self.assertEqual(replies[-1],
                        "ERROR: Exception: WrapperError('x -1.0 is < 0',)\r\n>")

    def test_get(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.x'], count=3)
        self.assertEqual(replies[-1], '2\r\n>')

        replies = self.send_recv('get')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'get <object.property>\r\n>')

    def test_get_branches(self):
        replies = self.send_recv('getBranchesAndTags')
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv('getBranchesAndTags oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getBranchesAndTags\r\n>')

    def test_get_direct(self):
        replies = self.send_recv('getDirectTransfer')
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv('getDirectTransfer oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getDirectTransfer\r\n>')

    def test_get_hierarchy(self):
        expected = """\
<?xml version='1.0' encoding='utf-8'?>
<Group>
<Variable name="exe_count" type="long" io="output" format="" description="Execution count">0</Variable>
<Variable name="in_file" type="file" io="input" description="Input file" isBinary="false" fileName=""></Variable>
<Variable name="out_file" type="file" io="output" description="Output file" isBinary="false" fileName=""></Variable>
<Group name="sub_group">
<Variable name="b" type="boolean" io="input" format="" description="A boolean">true</Variable>
<Variable name="f" type="double" io="input" format="" description="A float" units="">0.5</Variable>
<Variable name="f1d" type="double[]" io="input" format="" description="1D float array" units="cm">1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5</Variable>
<Variable name="f2d" type="double[]" io="input" format="" description="2D float array" units="mm">bounds[2, 4] {1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5}</Variable>
<Variable name="f3d" type="double[]" io="input" format="" description="3D float array" units="">bounds[2, 3, 3] {1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 20.5, 30.5, 40.5, 50.5, 60.5, 70.5, 80.5, 90.5}</Variable>
<Variable name="fe" type="double" io="input" format="" description="Float enum" units="m">2.781828</Variable>
<Variable name="i" type="long" io="input" format="" description="An int">7</Variable>
<Variable name="i1d" type="long[]" io="input" format="" description="1D int array" units="">1, 2, 3, 4, 5, 6, 7, 8, 9</Variable>
<Variable name="ie" type="long" io="input" format="" description="Int enum" units="">9</Variable>
<Variable name="s" type="string" io="input" format="" description="A string">Hello World!  ( &amp; &lt; &gt; )</Variable>
<Variable name="s1d" type="string[]" io="input" format="" description="1D string array" units="">"Hello", "from", "TestComponent.SubGroup"</Variable>
<Variable name="se" type="string" io="input" format="" description="Str enum" units="">cold</Variable>
</Group>
<Variable name="x" type="double" io="input" format="" description="X input" units="">2</Variable>
<Variable name="y" type="double" io="input" format="" description="Y input" units="ft">3</Variable>
<Variable name="z" type="double" io="output" format="" description="Z output" units="ft">0</Variable>
</Group>"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'getHierarchy comp'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('getHierarchy')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getHierarchy <object>\r\n>')

    def test_get_icon(self):
        replies = self.send_recv('getIcon ASTestComp')
        self.assertEqual(replies[-1],
                         "ERROR: Exception: NotImplementedError('getIcon',)\r\n>")

        replies = self.send_recv('getIcon')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getIcon <analysisComponent>\r\n>')

        replies = self.send_recv('getIcon NoSuchComp')
        self.assertEqual(replies[-1],
                         'ERROR: component </NoSuchComp>'
                         ' does not match a known component\r\n>')

    def test_get_license(self):
        replies = self.send_recv('getLicense')
        self.assertEqual(replies[-1], 'Use at your own risk!\r\n>')

        replies = self.send_recv('getLicense oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getLicense\r\n>')

    def test_get_status(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'getStatus'])
        self.assertEqual(replies[-1], 'comp: ready\r\n>')

        replies = self.send_recv('getStatus oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getStatus\r\n>')

    def test_get_sys_info(self):
        expected = """\
version: 5.01
build: 331
num clients: 0
num components: 3
os name: %s
os arch: %s
os version: %s
python version: %s
user name: %s""" % (platform.system(), platform.processor(),
                    platform.release(), platform.python_version(),
                    getpass.getuser())
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv('getSysInfo')
        self.compare(replies[-1], expected)

        replies = self.send_recv('getSysInfo oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getSysInfo\r\n>')

    def test_get_version(self):
        expected = """\
OpenMDAO Analysis Server 0.1
Use at your own risk!
Attempting to support Phoenix Integration, Inc.
version: 5.01, build: 331"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv('getVersion')
        self.compare(replies[-1], expected)

        replies = self.send_recv('getVersion oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'getVersion\r\n>')

    def test_heartbeat(self):
        replies = self.send_recv('heartbeat start')
        self.assertEqual(replies[-1], 'Heartbeating started\r\n>')
        replies = self.send_recv('heartbeat stop')
        self.assertEqual(replies[-1], 'Heartbeating stopped\r\n>')

        replies = self.send_recv('hb start')
        self.assertEqual(replies[-1], 'Heartbeating started\r\n>')
        replies = self.send_recv('hb stop')
        self.assertEqual(replies[-1], 'Heartbeating stopped\r\n>')

        replies = self.send_recv('heartbeat')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'heartbeat,hb [start|stop]\r\n>')

    def test_help(self):
        expected = """\
Available Commands:
   listComponents,lc [category]
   listCategories,la [category]
   describe,d <category/component> [-xml]
   start <category/component> <instanceName>
   end <object>
   execute,x <objectName>
   listProperties,list,ls,l [object]
   listGlobals,lg
   listValues,lv <object>
   listArrayValues,lav <object> (NOT IMPLEMENTED)
   get <object.property>
   set <object.property> = <value>
   move,rename,mv,rn <from> <to> (NOT IMPLEMENTED)
   getIcon <analysisComponent> (NOT IMPLEMENTED)
   getVersion
   getLicense
   getStatus
   help,h
   quit
   getSysInfo
   invoke <object.method()> [full]
   listMethods,lm <object> [full]
   addProxyClients <clientHost1>,<clientHost2>
   monitor start <object.property>, monitor stop <id>
   versions,v category/component
   ps <object>
   listMonitors,lo <objectName>
   heartbeat,hb [start|stop]
   listValuesURL,lvu <object>
   getDirectTransfer
   getByUrl <object.property> <url> (NOT IMPLEMENTED)
   setByUrl <object.property> = <url> (NOT IMPLEMENTED)
   setDictionary <xml dictionary string> (NOT IMPLEMENTED)
   getHierarchy <object.property>
   setHierarchy <object.property> <xml>
   deleteRunShare <key> (NOT IMPLEMENTED)
   getBranchesAndTags"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv('help')
        self.compare(replies[-1], expected)
        replies = self.send_recv('h')
        self.compare(replies[-1], expected)

        replies = self.send_recv('help oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'help,h\r\n>')

    def test_invoke(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'invoke comp.float_method()'], count=3)
        self.assertEqual(replies[-1], '0\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'invoke comp.null_method()'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'invoke comp.str_method()'], count=3)
        self.assertEqual(replies[-1],
                         'current state: x 2.0, y 3.0, z 0.0, exe_count 0\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'invoke comp.cause_exception()'], count=3)
        self.assertEqual(replies[-1],
            'ERROR: Exception: RuntimeError("comp: It\'s your own fault...",)\r\n>')

        replies = self.send_recv('invoke')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'invoke <object.method()> [full]\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'invoke comp.no_such_method()'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such method <no_such_method>.',)\r\n>")

    def test_list_array_values(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'listArrayValues comp'], count=3)
        self.assertEqual(replies[-1],
                         "ERROR: Exception: NotImplementedError('listArrayValues',)\r\n>")
        replies = self.send_recv(['start ASTestComp comp',
                                  'lav comp'], count=3)
        self.assertEqual(replies[-1],
                         "ERROR: Exception: NotImplementedError('listArrayValues',)\r\n>")

        replies = self.send_recv('listArrayValues')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listArrayValues,lav <object>\r\n>')

    def test_list_categories(self):
        replies = self.send_recv('listCategories /')
        self.assertEqual(replies[-1], '0 categories found:\r\n>')
        replies = self.send_recv('la')
        self.assertEqual(replies[-1], '0 categories found:\r\n>')

        replies = self.send_recv('listCategories category oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listCategories,la [category]\r\n>')

    def test_list_components(self):
        expected = """\
2 components found:
ASTestComp
ASTestComp2"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv('listComponents /')
        self.compare(replies[-1], expected)
        replies = self.send_recv('lc')
        self.compare(replies[-1], expected)
        replies = self.send_recv('lc ""')
        self.compare(replies[-1], expected)

        replies = self.send_recv('listComponents category oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listComponents,lc [category]\r\n>')

    def test_list_globals(self):
        replies = self.send_recv('listGlobals')
        self.assertEqual(replies[-1], '0 global objects started:\r\n>')
        replies = self.send_recv('lg')
        self.assertEqual(replies[-1], '0 global objects started:\r\n>')

        replies = self.send_recv('listGlobals oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listGlobals,lg\r\n>')

    def test_list_methods(self):
        expected = """\
4 methods found:
cause_exception()
float_method()
null_method()
str_method()"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listMethods comp'], count=3)
        self.compare(replies[-1], expected)
        replies = self.send_recv(['start ASTestComp comp',
                                  'lm comp'], count=3)
        self.compare(replies[-1], expected)

        expected = """\
4 methods found:
cause_exception() fullName="cause_exception"
float_method() fullName="float_method"
null_method() fullName="null_method"
str_method() fullName="str_method"\
"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listMethods comp full'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('listMethods')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listMethods,lm <object> [full]\r\n>')

    def test_list_monitors(self):
        expected = """\
3 monitors:
ASTestComp.py
ASTestComp_loader.py
__init__.py"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'

        replies = self.send_recv(['start ASTestComp comp',
                                  'listMonitors comp'], count=3)
        self.compare(replies[-1], expected)
        replies = self.send_recv(['start ASTestComp comp',
                                  'lo comp'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('listMonitors')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listMonitors,lo <objectName>\r\n>')

    def test_list_properties(self):
        expected = """\
7 properties found:
exe_count (type=com.phoenix_int.aserver.types.PHXLong) (access=g)
in_file (type=com.phoenix_int.aserver.types.PHXRawFile) (access=sg)
out_file (type=com.phoenix_int.aserver.types.PHXRawFile) (access=g)
sub_group (type=com.phoenix_int.aserver.PHXGroup) (access=sg)
x (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)
y (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)
z (type=com.phoenix_int.aserver.types.PHXDouble) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp'], count=3)
        self.compare(replies[-1], expected)
        replies = self.send_recv(['start ASTestComp comp',
                                  'l comp'], count=3)
        self.compare(replies[-1], expected)

        expected = """\
12 properties found:
b (type=com.phoenix_int.aserver.types.PHXBoolean) (access=sg)
f (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)
f1d (type=double[9]) (access=sg)
f2d (type=double[2][4]) (access=sg)
f3d (type=double[2][3][3]) (access=sg)
fe (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)
i (type=com.phoenix_int.aserver.types.PHXLong) (access=sg)
i1d (type=long[9]) (access=sg)
ie (type=com.phoenix_int.aserver.types.PHXLong) (access=sg)
s (type=com.phoenix_int.aserver.types.PHXString) (access=sg)
s1d (type=java.lang.String[3]) (access=sg)
se (type=com.phoenix_int.aserver.types.PHXString) (access=sg)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('listProperties')
        self.assertEqual(replies[-1], '0 objects started:\r\n>')

        replies = self.send_recv('listProperties comp oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listProperties,list,ls,l [object]\r\n>')

    def test_list_values(self):
        expected = """\
7 properties found:
exe_count (type=com.phoenix_int.aserver.types.PHXLong) (access=g)  vLen=1  val=0
   10 SubProps found:
description (type=java.lang.String) (access=g)  vLen=15  val=Execution count
enumAliases (type=java.lang.String[0]) (access=g)  vLen=0  val=
enumValues (type=long[0]) (access=g)  vLen=0  val=
hasLowerBound (type=boolean) (access=g)  vLen=4  val=true
hasUpperBound (type=boolean) (access=g)  vLen=4  val=true
lowerBound (type=long) (access=g)  vLen=%d  val=%d
units (type=java.lang.String) (access=g)  vLen=0  val=
upperBound (type=long) (access=g)  vLen=%d  val=%d
value (type=long) (access=g)  vLen=1  val=0
valueStr (type=java.lang.String) (access=g)  vLen=1  val=0
in_file (type=com.phoenix_int.aserver.types.PHXRawFile) (access=sg)  vLen=0  val=
   6 SubProps found:
description (type=java.lang.String) (access=g)  vLen=10  val=Input file
isBinary (type=boolean) (access=g)  vLen=5  val=false
mimeType (type=java.lang.String) (access=g)  vLen=0  val=
name (type=java.lang.String) (access=g)  vLen=0  val=
nameCoded (type=java.lang.String) (access=g)  vLen=0  val=
url (type=java.lang.String) (access=g)  vLen=0  val=
out_file (type=com.phoenix_int.aserver.types.PHXRawFile) (access=g)  vLen=0  val=
   6 SubProps found:
description (type=java.lang.String) (access=g)  vLen=11  val=Output file
isBinary (type=boolean) (access=g)  vLen=5  val=false
mimeType (type=java.lang.String) (access=g)  vLen=10  val=text/plain
name (type=java.lang.String) (access=g)  vLen=0  val=
nameCoded (type=java.lang.String) (access=g)  vLen=0  val=
url (type=java.lang.String) (access=g)  vLen=0  val=
sub_group (type=com.phoenix_int.aserver.PHXGroup) (access=sg)  vLen=16  val=Group: sub_group
x (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)  vLen=1  val=2
   11 SubProps found:
description (type=java.lang.String) (access=g)  vLen=7  val=X input
enumAliases (type=java.lang.String[0]) (access=g)  vLen=0  val=
enumValues (type=double[0]) (access=g)  vLen=0  val=
format (type=java.lang.String) (access=g)  vLen=4  val=null
hasLowerBound (type=boolean) (access=g)  vLen=5  val=false
hasUpperBound (type=boolean) (access=g)  vLen=5  val=false
lowerBound (type=double) (access=g)  vLen=3  val=0.0
units (type=java.lang.String) (access=g)  vLen=0  val=
upperBound (type=double) (access=g)  vLen=3  val=0.0
value (type=double) (access=sg)  vLen=1  val=2
valueStr (type=java.lang.String) (access=g)  vLen=1  val=2
y (type=com.phoenix_int.aserver.types.PHXDouble) (access=sg)  vLen=1  val=3
   11 SubProps found:
description (type=java.lang.String) (access=g)  vLen=7  val=Y input
enumAliases (type=java.lang.String[0]) (access=g)  vLen=0  val=
enumValues (type=double[0]) (access=g)  vLen=0  val=
format (type=java.lang.String) (access=g)  vLen=4  val=null
hasLowerBound (type=boolean) (access=g)  vLen=4  val=true
hasUpperBound (type=boolean) (access=g)  vLen=4  val=true
lowerBound (type=double) (access=g)  vLen=5  val=-10.0
units (type=java.lang.String) (access=g)  vLen=2  val=ft
upperBound (type=double) (access=g)  vLen=4  val=10.0
value (type=double) (access=sg)  vLen=1  val=3
valueStr (type=java.lang.String) (access=g)  vLen=1  val=3
z (type=com.phoenix_int.aserver.types.PHXDouble) (access=g)  vLen=1  val=0
   11 SubProps found:
description (type=java.lang.String) (access=g)  vLen=8  val=Z output
enumAliases (type=java.lang.String[0]) (access=g)  vLen=0  val=
enumValues (type=double[0]) (access=g)  vLen=0  val=
format (type=java.lang.String) (access=g)  vLen=4  val=null
hasLowerBound (type=boolean) (access=g)  vLen=5  val=false
hasUpperBound (type=boolean) (access=g)  vLen=5  val=false
lowerBound (type=double) (access=g)  vLen=3  val=0.0
units (type=java.lang.String) (access=g)  vLen=2  val=ft
upperBound (type=double) (access=g)  vLen=3  val=0.0
value (type=double) (access=g)  vLen=1  val=0
valueStr (type=java.lang.String) (access=g)  vLen=1  val=0""" \
            % (len(str(-sys.maxint)), -sys.maxint,
               len(str( sys.maxint)),  sys.maxint)
        expected = expected.replace('\n', '\r\n') + '\r\n>'

        replies = self.send_recv(['start ASTestComp comp',
                                  'listValues comp'], count=3)
        self.compare(replies[-1], expected)
        replies = self.send_recv(['start ASTestComp comp',
                                  'lv comp'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('listValues comp oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listValues,lv [object]\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'listValuesURL comp'], count=3)
        self.compare(replies[-1], expected)
        replies = self.send_recv(['start ASTestComp comp',
                                  'lvu comp'], count=3)
        self.compare(replies[-1], expected)

        replies = self.send_recv('listValuesURL comp oops')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'listValuesURL,lvu [object]\r\n>')

    def test_monitor(self):
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
        expected = expected.replace('\n', '\r\n')

        replies = self.send_recv(['start ASTestComp comp',
                                  'monitor start comp.ASTestComp_loader.py',
                                  'monitor stop None'], count=4)
        if replies[-2] == '>':  # Threading can alter order.
            self.assertEqual(replies[-1][:len(expected)], expected)
        else:
            self.assertEqual(replies[-2][:len(expected)], expected)

        replies = self.send_recv(['start ASTestComp comp',
                                  'monitor start comp.no-such-file'], count=3)
        msg = "Can't open '"
        self.assertEqual(replies[-1][:len(msg)], msg)

        replies = self.send_recv(['start ASTestComp comp',
                                  'monitor stop no-such-id'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: No monitor registered for 'no-such-id'\r\n>")

        replies = self.send_recv('monitor')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'monitor start <object.property>, '
                         'monitor stop <id>\r\n>')

        replies = self.send_recv('monitor start oops')
        self.assertEqual(replies[-1], 'ERROR: no such object: <oops>\r\n>')

        monitor = BaseMonitor('test', 10, '42', sys.stdout.write, False)
        monitor.start()
        assert_raises(self, 'monitor.start()', globals(), locals(), 
                      RuntimeError, 'start() may only be called once.')
        monitor.stop()

        assert_raises(self, 'monitor.poll()', globals(), locals(),
                      NotImplementedError, 'BaseMonitor.poll()')

    def test_move(self):
        expected = "ERROR: Exception: NotImplementedError('move',)\r\n>"

        replies = self.send_recv('move from to')
        self.assertEqual(replies[-1], expected)
        replies = self.send_recv('mv from to')
        self.assertEqual(replies[-1], expected)
        replies = self.send_recv('rename from to')
        self.assertEqual(replies[-1], expected)
        replies = self.send_recv('rn from to')
        self.assertEqual(replies[-1], expected)

        replies = self.send_recv('move')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'move,rename,mv,rn <from> <to>\r\n>')

    def test_ps(self):
        command = os.path.basename(sys.executable)
        expected = """\
<Processes length='1'>
 <Process pid='0'>
  <ParentPID>0</ParentPID>
  <PercentCPU>0.0</PercentCPU>
  <Memory>0</Memory>
  <Time>0</Time>
  <WallTime>0</WallTime>
  <Command>%s</Command>
 </Process>
</Processes>""" % command
        expected = expected.replace('\n', '\r\n') + '\r\n>'

        replies = self.send_recv(['start ASTestComp comp',
                                  'ps comp'], count=3)
        self.assertEqual(replies[-1], expected)

        replies = self.send_recv('ps')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'ps <object>\r\n>')

    def test_publish_egg(self):
        replies = self.send_recv('publishEgg')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'publishEgg <path> <version> <comment> <author> ZEROBYTE <eggdata>\r\n>')

    def test_quit(self):
        replies = self.send_recv('quit')
        self.assertEqual(len(replies), 1)  # Just the 'welcome' message.

    def test_set(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.x = 42'], count=3)
        self.assertEqual(replies[-1], 'value set for <x>\r\n>')

    def test_set_hierarchy(self):
        xml = """\
<?xml version='1.0' encoding='utf-8'?>
<Group>
<Variable name="in_file">test setHierarchy</Variable>
<Variable name="sub_group.b">false</Variable>
<Variable name="sub_group.f">-0.5</Variable>
<Variable name="sub_group.f1d">5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9</Variable>
<Variable name="sub_group.f2d">bounds[2, 4] {.1, .2, .3, .4, .5, .6, .7, .8}</Variable>
<Variable name="sub_group.f3d">bounds[2, 3, 3] {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9</Variable>
<Variable name="sub_group.fe">3.14159</Variable>
<Variable name="sub_group.i">-7</Variable>
<Variable name="sub_group.i1d">-1, -2, -3, -4, -5, -6, -7, -8, -9</Variable>
<Variable name="sub_group.ie">9</Variable>
<Variable name="sub_group.s">Cruel world :-(</Variable>
<Variable name="sub_group.se">hot</Variable>
<Variable name="x">6</Variable>
<Variable name="y">7</Variable>
</Group>"""

        cmd_1 = 'start ASTestComp comp'
        cmd_2 = 'setHierarchy comp %s' % xml
        expected = 'values set'
        replies = self.send_recv(['setMode raw\n',
                                  'setID 1\ncmdLen=%d\n%s' % (len(cmd_1), cmd_1),
                                  'setID 2\ncmdLen=%d\n%s' % (len(cmd_2), cmd_2)],
                                 raw=True, count=3)
        self.assertEqual(replies[-1], '2\r\nformat: string\r\n%d\r\n%s'
                                      % (len(expected), expected))

    def test_set_mode(self):
        replies = self.send_recv('setMode')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'setMode raw\r\n>')

        cmd_1 = 'listComponents'
        expected_1 = """\
2 components found:
ASTestComp
ASTestComp2"""

        cmd_2 = 'no-such-command'
        expected_2 = 'ERROR: command <no-such-command> not recognized'

        replies = \
            self.send_recv(['setMode raw\n',
                            'setID 1\ncmdLen=%d\n%s' % (len(cmd_1), cmd_1),
                            'setID 1\ncmdLen=%d\n%s' % (len(cmd_2), cmd_2)],
                           raw=True)

        welcome = replies.pop(0)
        for format, expected in (('string', expected_1),
                                 ('error',  expected_2)):
            if len(expected) <= 32:
                self.assertEqual(replies.pop(0),
                                 '1\r\nformat: %s\r\n%d\r\n%s'
                                 % (format, len(expected), expected))
            else:
                self.assertEqual([replies.pop(0), replies.pop(0)],
                                 ['1\r\nformat: %s\r\n%d\r\n'
                                  % (format, len(expected)), expected])

    def test_start(self):
        replies = self.send_recv('start ASTestComp comp')
        self.assertEqual(replies[-1], 'Object comp started.\r\n>')

        replies = self.send_recv('start')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'start <category/component> <instanceName>\r\n>')

        replies = self.send_recv('start NoSuchComp xyzzy')
        self.assertEqual(replies[-1],
                         'ERROR: component </NoSuchComp>'
                         ' does not match a known component\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'start ASTestComp comp'])
        self.assertEqual(replies[-1], 'ERROR: Name already in use: "comp"\r\n>')

    def test_versions(self):
        tstamp1 = time.ctime(os.path.getmtime('ASTestComp-0.1.cfg'))
        tstamp2 = time.ctime(os.path.getmtime('ASTestComp-0.2.cfg'))
        expected = """\
<Branch name='HEAD'>
 <Version name='0.1'>
  <author>anonymous</author>
  <date>%s</date>
  <description>Initial version.</description>
 </Version>
 <Version name='0.2'>
  <author>anonymous  ( &amp; &lt; &gt; )</author>
  <date>%s</date>
  <description>Added in_file explicitly.  ( &amp; &lt; &gt; )</description>
 </Version>
</Branch>""" % (tstamp1, tstamp2)
        expected = expected.replace('\n', '\r\n') + '\r\n>'

        replies = self.send_recv('versions ASTestComp')
        self.assertEqual(replies[-1], expected)
        replies = self.send_recv('v ASTestComp')
        self.assertEqual(replies[-1], expected)

        replies = self.send_recv('versions')
        self.assertEqual(replies[-1],
                         'ERROR: invalid syntax. Proper syntax:\r\n'
                         'versions,v category/component\r\n>')

        replies = self.send_recv('versions NoSuchComp')
        self.assertEqual(replies[-1],
                         'ERROR: component </NoSuchComp>'
                         ' does not match a known component\r\n>')

    def test_array(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.componentType'], count=3)
        self.assertEqual(replies[-1], 'double\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.dimensions'], count=3)
        self.assertEqual(replies[-1], '"9"\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.enumValues'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f2d.first'], count=3)
        self.assertEqual(replies[-1], '1.5\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.format'], count=3)
        self.assertEqual(replies[-1], 'null\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.hasUpperBound'], count=3)
        self.assertEqual(replies[-1], 'true\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.upperBound'], count=3)
        self.assertEqual(replies[-1], '10\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.hasLowerBound'], count=3)
        self.assertEqual(replies[-1], 'true\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.lowerBound'], count=3)
        self.assertEqual(replies[-1], '0\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.length'], count=3)
        self.assertEqual(replies[-1], '9\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.lockResize'], count=3)
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.numDimensions'], count=3)
        self.assertEqual(replies[-1], '1\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.f1d.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.f1d.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f1d.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.f1d.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.f1d.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.f1d.xyzzy>.',)\r\n>")

        expected = """\
15 properties found:
componentType (type=java.lang.Class) (access=g)
description (type=java.lang.String) (access=sg)
dimensions (type=int[1]) (access=sg)
enumAliases (type=java.lang.String[0]) (access=sg)
enumValues (type=double[0]) (access=sg)
first (type=java.lang.Object) (access=sg)
format (type=java.lang.String) (access=g)
hasLowerBound (type=boolean) (access=sg)
hasUpperBound (type=boolean) (access=sg)
length (type=int) (access=sg)
lockResize (type=boolean) (access=sg)
lowerBound (type=double) (access=sg)
numDimensions (type=int) (access=g)
units (type=java.lang.String) (access=sg)
upperBound (type=double) (access=sg)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.f1d'], count=3)
        self.compare(replies[-1], expected)

        expected = """\
15 properties found:
componentType (type=java.lang.Class) (access=g)
description (type=java.lang.String) (access=sg)
dimensions (type=int[1]) (access=sg)
enumAliases (type=java.lang.String[0]) (access=sg)
enumValues (type=long[0]) (access=sg)
first (type=java.lang.Object) (access=sg)
format (type=java.lang.String) (access=g)
hasLowerBound (type=boolean) (access=sg)
hasUpperBound (type=boolean) (access=sg)
length (type=int) (access=sg)
lockResize (type=boolean) (access=sg)
lowerBound (type=long) (access=sg)
numDimensions (type=int) (access=g)
units (type=java.lang.String) (access=sg)
upperBound (type=long) (access=sg)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.i1d'], count=3)
        self.compare(replies[-1], expected)

    def test_list(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s1d.dimensions'], count=3)
        self.assertEqual(replies[-1], '"3"\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s1d.first'], count=3)
        self.assertEqual(replies[-1], 'Hello\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s1d.length'], count=3)
        self.assertEqual(replies[-1], '3\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s1d.numDimensions'], count=3)
        self.assertEqual(replies[-1], '1\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.s1d = "some", "new", "values"'], count=3)
        self.assertEqual(replies[-1], 'value set for <sub_group.s1d>\r\n>')

        expected = """\
10 properties found:
componentType (type=java.lang.Class) (access=g)
description (type=java.lang.String) (access=sg)
dimensions (type=int[1]) (access=sg)
enumAliases (type=java.lang.String[0]) (access=sg)
enumValues (type=java.lang.String[0]) (access=sg)
first (type=java.lang.Object) (access=sg)
length (type=int) (access=sg)
lockResize (type=boolean) (access=sg)
numDimensions (type=int) (access=g)
units (type=java.lang.String) (access=sg)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.s1d'], count=3)
        self.compare(replies[-1], expected)

    def test_bool(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.b'], count=3)
        self.assertEqual(replies[-1], 'true\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.b = false',
                                  'get comp.sub_group.b'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.b>\r\n>',
                                        'false\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.b = true',
                                  'get comp.sub_group.b'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.b>\r\n>',
                                        'true\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.b = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            'ERROR: Exception: WrapperError("invalid boolean value \'xyzzy\' for <sub_group.b>",)\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.b.description'], count=3)
        self.assertEqual(replies[-1], 'A boolean\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.b.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.b.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.b.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.b.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.b.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.b.xyzzy>.',)\r\n>")

        expected = """\
3 properties found:
description (type=java.lang.String) (access=g)
value (type=boolean) (access=sg)
valueStr (type=boolean) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.b'], count=3)
        self.compare(replies[-1], expected)

    def test_enum(self):
        # Float enum.
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe'], count=3)
        self.assertEqual(replies[-1], '2.781828\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.fe = 3.14159',
                                  'get comp.sub_group.fe'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.fe>\r\n>',
                                        '3.14159\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.fe = pi',
                                  'get comp.sub_group.fe'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.fe>\r\n>',
                                        '3.14159\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.description'], count=3)
        self.assertEqual(replies[-1], 'Float enum\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.enumAliases'], count=3)
        self.assertEqual(replies[-1], '"e", "pi"\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.enumAliases[1]'], count=3)
        self.assertEqual(replies[-1], 'pi\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.enumValues'], count=3)
        self.assertEqual(replies[-1], '2.781828, 3.14159\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.enumValues[1]'], count=3)
        self.assertEqual(replies[-1], '3.14159\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.format'], count=3)
        self.assertEqual(replies[-1], 'null\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.hasLowerBound'], count=3)
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.lowerBound'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.hasUpperBound'], count=3)
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.upperBound'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.units'], count=3)
        self.assertEqual(replies[-1], 'm\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.fe.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.fe.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.fe.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.fe.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.fe.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.fe.xyzzy>.',)\r\n>")

        expected = """\
11 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[2]) (access=g)
enumValues (type=double[2]) (access=g)
format (type=java.lang.String) (access=g)
hasLowerBound (type=boolean) (access=g)
hasUpperBound (type=boolean) (access=g)
lowerBound (type=double) (access=g)
units (type=java.lang.String) (access=g)
upperBound (type=double) (access=g)
value (type=double) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.fe'], count=3)
        self.compare(replies[-1], expected)

        # Int enum.
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie'], count=3)
        self.assertEqual(replies[-1], '9\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.description'], count=3)
        self.assertEqual(replies[-1], 'Int enum\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.enumValues'], count=3)
        self.assertEqual(replies[-1], '9, 8, 7, 1\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.enumValues[1]'], count=3)
        self.assertEqual(replies[-1], '8\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.units'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.ie.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.ie.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.ie.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.ie.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.ie.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.ie.xyzzy>.',)\r\n>")

        expected = """\
10 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[0]) (access=g)
enumValues (type=long[4]) (access=g)
hasLowerBound (type=boolean) (access=g)
hasUpperBound (type=boolean) (access=g)
lowerBound (type=long) (access=g)
units (type=java.lang.String) (access=g)
upperBound (type=long) (access=g)
value (type=long) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.ie'], count=3)
        self.compare(replies[-1], expected)

        # String enum.
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se'], count=3)
        self.assertEqual(replies[-1], 'cold\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se.description'], count=3)
        self.assertEqual(replies[-1], 'Str enum\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se.enumValues'], count=3)
        self.assertEqual(replies[-1], '"cold", "hot", "nice"\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se.enumValues[1]'], count=3)
        self.assertEqual(replies[-1], 'hot\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.se.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.se.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.se.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.se.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.se.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.se.xyzzy>.',)\r\n>")

        expected = """\
5 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[0]) (access=g)
enumValues (type=java.lang.String[3]) (access=g)
value (type=java.lang.String) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.se'], count=3)
        self.compare(replies[-1], expected)

    def test_file(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file = "Hello world!"',
                                  'get comp.in_file'], count=4)
        self.assertEqual(replies[-2:], ['value set for <in_file>\r\n>',
                                        'Hello world!\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <in_file.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.in_file.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <in_file.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.in_file.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <in_file.xyzzy>.',)\r\n>")

    def test_float(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f'], count=3)
        self.assertEqual(replies[-1], '0.5\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.f = 3.14159',
                                  'get comp.sub_group.f'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.f>\r\n>',
                                        '3.14159\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.description'], count=3)
        self.assertEqual(replies[-1], 'A float\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.enumValues'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.format'], count=3)
        self.assertEqual(replies[-1], 'null\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.hasLowerBound'], count=3)
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.hasUpperBound'], count=3)
        self.assertEqual(replies[-1], 'false\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.lowerBound'], count=3)
        self.assertEqual(replies[-1], '0.0\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.upperBound'], count=3)
        self.assertEqual(replies[-1], '0.0\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.units'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.f.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.f.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.f.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.f.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.f.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.f.xyzzy>.',)\r\n>")

        expected = """\
11 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[0]) (access=g)
enumValues (type=double[0]) (access=g)
format (type=java.lang.String) (access=g)
hasLowerBound (type=boolean) (access=g)
hasUpperBound (type=boolean) (access=g)
lowerBound (type=double) (access=g)
units (type=java.lang.String) (access=g)
upperBound (type=double) (access=g)
value (type=double) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.f'], count=3)
        self.compare(replies[-1], expected)

    def test_int(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i'], count=3)
        self.assertEqual(replies[-1], '7\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.i = 42',
                                  'get comp.sub_group.i'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.i>\r\n>',
                                        '42\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.description'], count=3)
        self.assertEqual(replies[-1], 'An int\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.enumValues'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.hasUpperBound'], count=3)
        self.assertEqual(replies[-1], 'true\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.hasLowerBound'], count=3)
        self.assertEqual(replies[-1], 'true\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.units'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.i.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.i.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.i.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.i.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.i.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.i.xyzzy>.',)\r\n>")

        expected = """\
10 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[0]) (access=g)
enumValues (type=long[0]) (access=g)
hasLowerBound (type=boolean) (access=g)
hasUpperBound (type=boolean) (access=g)
lowerBound (type=long) (access=g)
units (type=java.lang.String) (access=g)
upperBound (type=long) (access=g)
value (type=long) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.i'], count=3)
        self.compare(replies[-1], expected)

    def test_str(self):
        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s'], count=3)
        self.assertEqual(replies[-1], 'Hello World!  ( & < > )\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.s = xyzzy',
                                  'get comp.sub_group.s'], count=4)
        self.assertEqual(replies[-2:], ['value set for <sub_group.s>\r\n>',
                                        'xyzzy\r\n>'])

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s.description'], count=3)
        self.assertEqual(replies[-1], 'A string\r\n>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s.enumAliases'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s.enumValues'], count=3)
        self.assertEqual(replies[-1], '>')

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.s.description = xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('cannot set <sub_group.s.description>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'get comp.sub_group.s.xyzzy'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.s.xyzzy>.',)\r\n>")

        replies = self.send_recv(['start ASTestComp comp',
                                  'set comp.sub_group.s.xyzzy = froboz'], count=3)
        self.assertEqual(replies[-1],
            "ERROR: Exception: WrapperError('no such property <sub_group.s.xyzzy>.',)\r\n>")

        expected = """\
5 properties found:
description (type=java.lang.String) (access=g)
enumAliases (type=java.lang.String[0]) (access=g)
enumValues (type=java.lang.String[0]) (access=g)
value (type=java.lang.String) (access=sg)
valueStr (type=java.lang.String) (access=g)"""
        expected = expected.replace('\n', '\r\n') + '\r\n>'
        replies = self.send_recv(['start ASTestComp comp',
                                  'listProperties comp.sub_group.s'], count=3)
        self.compare(replies[-1], expected)


if __name__ == '__main__':
    sys.argv.append('--cover-package=analysis_server')
    sys.argv.append('--cover-erase')
    nose.runmodule()

