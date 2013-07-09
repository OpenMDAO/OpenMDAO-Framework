import sys

# to run interactively, use the following command in directory with jsTestDriver.conf:
#   java -jar JsTestDriver-1.3.3c.jar --port 9876 --browser google-chrome --tests all
#
# or to run a specific test:
#   java -jar JsTestDriver-1.3.3c.jar --port 9876 --browser google-chrome --tests "test addListener"

# Because Xvfb does not exist on Windows, it is difficult
#   to do headless testing on Windows. So for now
#   we just test on Linux
if sys.platform.startswith("linux"):

    import os
    import tempfile
    from distutils.spawn import find_executable

    # For running tests using the JsTestDriver test runner
    from lazr.testing.jstestdriver import JsTestDriverTestCase

    from nose.plugins.skip import SkipTest

    from openmdao.util.network import get_unused_ip_port
    from openmdao.gui.util import get_executable_path

    # monkey patch away the ResultProxy.assertMyTest method
    #      which fails because the lazr.testing code is
    #      not completely compatible with nose
    from nose.proxy import ResultProxy
    ResultProxy.assertMyTest = lambda s, test: None

    import openmdao.gui
    gui_directory = os.path.abspath(openmdao.gui.__path__[0])

    def create_jstestdriver_config_file(port_num):
        '''Create the configuration file needed by jsTestDriver'''

        config_file_temp = tempfile.NamedTemporaryFile(delete=False)
        print >> config_file_temp, '''
    server: http://localhost:%(port_num)s

    load:
      - %(gd)s/static/js/jquery*.js
      - %(gd)s/static/js/slickgrid/*.js
      - %(gd)s/static/js/ba-debug.min.js
      - %(gd)s/static/js/openmdao/Util.js
      - %(gd)s/static/js/openmdao/Project.js
      - %(gd)s/static/js/openmdao/CellEditor.js
      - %(gd)s/static/js/openmdao/ValueEditor.js

      - %(gd)s/static/js/sinon-1.2.0.js
      - %(gd)s/test/js_unit_tests/src/*.js

    test:
      - %(gd)s/test/js_unit_tests/test/*.js

    ''' % {'port_num': str(port_num), 'gd': gui_directory}
        config_file_temp.close()
        return config_file_temp

    port_number = get_unused_ip_port()
    config_file = create_jstestdriver_config_file(port_number)
    jstestdriver_path = os.path.join(gui_directory,
                                     "test/js_unit_tests",
                                     "JsTestDriver-1.3.3c.jar")

    class BrowserJsUnitTestCase(JsTestDriverTestCase):
        """base class for testing GUI JavaScript"""

        config_filename = config_file.name

        def get_browser_info(self):
            '''returns browser name and exes'''
            pass

        def setUp(self):
            super(BrowserJsUnitTestCase, self).setUp()

            # If java not available, skip the test since it is needed
            #    by jsTestDriver
            if not find_executable("java"):
                raise SkipTest("java is needed to do the "
                               "GUI JavaScript unit testing")

            # Can't seem to pass a path with spaces in it correctly,
            # quoting isn't honored at receiver.
            if ' ' in jstestdriver_path:
                raise SkipTest('Install directory has spaces in path')

            # run headless if xvfb is available
            if find_executable("xvfb-run"):
                java_cmd = "xvfb-run java"
            else:
                java_cmd = "java"

            # What is the path to the exe of this browser, if any
            browser_name, browser_exe_filepath = self.get_browser_info()
            if not browser_exe_filepath:
                raise SkipTest("The browser '%s' is not available for use in "
                               "GUI JavaScript unit testing" % browser_name)

            # Set some env vars used by jsTestDriver
            jstd_opts = {
                'java_cmd':  java_cmd,
                'jstd_path': jstestdriver_path,
                'port_num':  port_number,
                'browser':   browser_exe_filepath,
            }
            os.environ['JSTESTDRIVER'] = "%(java_cmd)s -jar %(jstd_path)s " \
                "--port %(port_num)d --captureConsole --browser %(browser)s" \
                % jstd_opts
            os.environ['JSTESTDRIVER_PORT'] = str(port_number)
            os.environ['JSTESTDRIVER_SERVER'] = \
                       'http://localhost:%d' % (port_number)

        def tearDown(self):
            #os.unlink(self.config_filename)
            super(BrowserJsUnitTestCase, self).tearDown()

    class ChromeJsUnitTestCase(BrowserJsUnitTestCase):
        """test GUI JavaScript using Chrome"""

        config_filename = config_file.name

        def get_browser_info(self):
            '''Return the name and possible exes for chrome'''

            browser_name = "chrome"
            browser_exe_filepath = get_executable_path(
                ["chromium-browser", "google-chrome", "chrome"])
            return browser_name, browser_exe_filepath

    class FirefoxJsUnitTestCase(BrowserJsUnitTestCase):
        """test GUI JavaScript using Firefox"""

        config_filename = config_file.name

        def get_browser_info(self):
            '''Return the name and possible exes for firefox'''

            browser_name = "firefox"
            browser_exe_filepath = get_executable_path(["firefox"])
            return browser_name, browser_exe_filepath

    # If the following is not done, then nose will try to run
    # JsTestDriverTestCase as a real test case but it is really
    # more of an abstract class that isn't supposed to be used directly.
    # This fixes that. Same thing for BrowserJsUnitTestCase
    JsTestDriverTestCase.__test__ = False
    ChromeJsUnitTestCase.__test__ = True
    FirefoxJsUnitTestCase.__test__ = False
    BrowserJsUnitTestCase.__test__ = False
