import sys

# Because Xvfb does not exist on Windows, it is difficult
#   to do headless testing on Windows. So for now
#   we just test on Linux
if sys.platform.startswith( "linux" ):

    import os
    import tempfile
    from distutils.spawn import find_executable

    # for running Xvfb, so we can run our tests headlessly. Xvfb does not
    #    exist on Windows
    from pyvirtualdisplay import Display
    # For running tests using the JsTestDriver test runner
    from lazr.testing.jstestdriver import JsTestDriverTestCase

    from nose.plugins.skip import SkipTest

    from openmdao.util.network import get_unused_ip_port
    
    # monkey patch away the ResultProxy.assertMyTest method
    #      which fails because the lazr.testing code is
    #      not completely compatible with nose
    from nose.proxy import ResultProxy
    ResultProxy.assertMyTest = lambda s, test: None
    
    import openmdao.gui
    gui_directory = os.path.abspath( openmdao.gui.__path__[0] )

    def create_jstestdriver_config_file( port_num ):
        '''Create the configuration file needed by jsTestDriver'''
    
        config_file_temp = tempfile.NamedTemporaryFile(delete=False)
        print >> config_file_temp, '''
    server: http://localhost:%(port_num)s
        
    load:
      - %(gd)s/static/js/require-jquery*
    
      - %(gd)s/static/js/openmdao/Util.js
      - %(gd)s/static/js/openmdao/Model.js
    
      - %(gd)s/static/js/sinon-1.2.0.js
    
      - %(gd)s/test/js_unit_tests/test_model.js
    
    ''' % { 'port_num': str( port_num ), 'gd': gui_directory }
        config_file_temp.close()
        return config_file_temp
    
    port_number = get_unused_ip_port()
    config_file = create_jstestdriver_config_file( port_number )
    jstestdriver_path = os.path.join( gui_directory,
                                      "test/js_unit_tests",
                                      "JsTestDriver-1.3.3c.jar" )
    
    class BrowserJsUnitTestCase(JsTestDriverTestCase):
        """base class for testing GUI JavaScript"""
    
        config_filename = config_file.name
    
        def get_browser_info( self ):
            '''returns browser name and exes'''
            pass
    
        def setUp(self):
    
            if find_executable( "Xvfb" ):
                self.display = Display()
                self.display.start()
    
            super(BrowserJsUnitTestCase, self).setUp()
            
            # If java not available, skip the test since it is needed
            #    by jsTestDriver
            if not find_executable( "java" ):
                raise SkipTest( "java is needed to do the "
                                "GUI JavaScript unit testing" )
    
            # What is the path to the exe of this browser, if any
            browser_name, browser_exe_filepath = self.get_browser_info()
            if not browser_exe_filepath:
                raise SkipTest("The browser '%s' is not available for use in "
                               "GUI JavaScript unit testing"  % browser_name)
            
            # Set some env vars used by jsTestDriver
            os.environ[ 'JSTESTDRIVER' ] = """java -jar %(jstd_path)s
                                               --port %(port_num)d
                                               --captureConsole
                                               --browser %(browser)s""" % \
            { 'jstd_path': jstestdriver_path,
              'port_num' : port_number,
              'browser': browser_exe_filepath,
              }
            os.environ[ 'JSTESTDRIVER_PORT' ] = str( port_number )
            os.environ[ 'JSTESTDRIVER_SERVER' ] = \
                        'http://localhost:%d' % ( port_number )
            
        def tearDown(self):
            #os.unlink(self.config_filename)
            super(BrowserJsUnitTestCase, self).tearDown()
            if find_executable( "Xvfb" ):
                self.display.stop()
    
        def get_browser_exe_filepath( self, browser_exes ):
            '''Look for an executable for the browser,
            given an list of the possible names for the exes'''
            browser_exe_filepath = None
            for browser_exe in browser_exes :
                browser_exe_filepath = find_executable( browser_exe )
                if browser_exe_filepath:
                    break
            return browser_exe_filepath
            
    class ChromeJsUnitTestCase(BrowserJsUnitTestCase):
        """test GUI JavaScript using Chrome"""
    
        config_filename = config_file.name
    
        def get_browser_info( self ):
            '''Return the name and possible exes for chrome'''
    
            browser_name = "chrome"
            browser_exe_filepath = self.get_browser_exe_filepath(
                [ "chromium-browser", "google-chrome", "chrome" ] )
            return browser_name, browser_exe_filepath
            
    # class FirefoxJsUnitTestCase(BrowserJsUnitTestCase):
    #     """test GUI JavaScript using Firefox,
    #        when OpenMDAO GUI supports it again"""
    
    #     config_filename = config_file.name
    
    #     def get_browser_info( self ):
    #         '''Return the name and possible exes for chrome'''
    
    #         browser_name = "firefox"
    #         browser_exe_filepath = self.get_browser_exe_filepath( [ "firefox" ] )
    #         return browser_name, browser_exe_filepath
            
    
    # If the following is not done, then nose will try to run
    # JsTestDriverTestCase as a real test case but it is really
    # more of an abstract class that isn't supposed to be used directly.
    # This fixes that. Same thing for BrowserJsUnitTestCase
    JsTestDriverTestCase.__test__ = False
    ChromeJsUnitTestCase.__test__ = True
    #FirefoxJsUnitTestCase.__test__ = True
    BrowserJsUnitTestCase.__test__ = False
    
