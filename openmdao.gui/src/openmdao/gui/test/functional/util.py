"""
Utilities for GUI functional testing.
"""

import inspect
import logging
import os.path
import platform
import shutil
import socket
import stat
import subprocess
import sys
import time
import urllib2
import zipfile

from distutils.spawn import find_executable
from nose import SkipTest
from nose.tools import eq_ as eq
from pyvirtualdisplay import Display
from selenium import webdriver

from optparse import OptionParser

from openmdao.util.network import get_unused_ip_port

from pageobjects.project import ProjectsListPage
from pageobjects.util import abort

if '.' not in sys.path:  # Look like an interactive session.
    sys.path.append('.')

TEST_CONFIG = dict(browsers=[], server=None, port=None)
_display_set = False
_display = None


def check_for_chrome():
    """ Determine if Chrome is available. """
    if os.path.exists('/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'):
        return True
    for exe in ('google-chrome', 'chrome', 'chromium-browser'):
        if find_executable(exe):
            return True
    return False


def setup_chrome():
    """ Initialize the Chrome browser. """
    exe = 'chromedriver'
    path = find_executable(exe)
    if not path:
        # Download, unpack, and install in OpenMDAO 'bin'.
        prefix = 'http://chromedriver.googlecode.com/files/'
#        version = '19.0.1068.0'
        version = '21.0.1180.4'
        if sys.platform == 'darwin':
            flavor = 'mac'
        elif sys.platform == 'win32':
            flavor = 'win'
            version = '20.0.1133.0'
        elif '64bit' in platform.architecture():
            flavor = 'linux64'
        else:
            flavor = 'linux32'
        filename = '%s_%s_%s.zip' % (exe, flavor, version)
        orig_dir = os.getcwd()
        os.chdir(os.path.dirname(sys.executable))
        try:
            logging.critical('Downloading %s to %s', filename, os.getcwd())
            src = urllib2.urlopen(prefix + filename)
            with open(filename, 'wb') as dst:
                dst.write(src.read())
            src.close()
            zip = zipfile.ZipFile(filename)
            zip.extract(exe)
            zip.close()
            if sys.platform != 'win32':
                os.chmod(exe, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)
            path = os.path.join(os.getcwd(), exe)
            os.remove(filename)
        finally:
            os.chdir(orig_dir)
    driver = webdriver.Chrome(executable_path=path)
    driver.implicitly_wait(15)
    TEST_CONFIG['browsers'].append(driver)
    return driver


def check_for_firefox():
    """ Determine if Firefox is available. """
    if os.path.exists('/Applications/Firefox.app/Contents/MacOS/firefox'):
        return True
    for exe in ('firefox',):
        if find_executable(exe):
            return True
    return False


def setup_firefox():
    """ Initialize the Firefox browser. """
    profile = webdriver.FirefoxProfile()
    profile.native_events_enabled = True
    driver = webdriver.Firefox(profile)
    driver.implicitly_wait(15)
    TEST_CONFIG['browsers'].append(driver)
    return driver


_browsers_to_test = dict(
    Chrome=(check_for_chrome, setup_chrome),
    #Firefox=(check_for_firefox, setup_firefox),
)


def setup_server(virtual_display=True):
    """ Start server on ``localhost`` using an unused port. """
    global _display, _display_set

    # Check if already setup (or skipping setup).
    if _display_set:
        return

    # Start server.
    port = get_unused_ip_port()
    TEST_CONFIG['port'] = port
    server_dir = 'gui-server'
    if os.path.exists(server_dir):
        shutil.rmtree(server_dir)
    os.mkdir(server_dir)
    TEST_CONFIG['server_dir'] = server_dir
    orig = os.getcwd()
    os.chdir(server_dir)
    stdout = open('stdout', 'w')
    TEST_CONFIG['stdout'] = stdout
    try:
        server = subprocess.Popen(('python', '-m', 'openmdao.gui.omg',
                                   '--server', '--port', str(port)),
                                   stdout=stdout, stderr=subprocess.STDOUT)
    finally:
        os.chdir(orig)
    TEST_CONFIG['server'] = server

    # Wait for server port to open.
    for i in range(200):  # ~20 sec.
        time.sleep(.1)
        try:
            sock = socket.create_connection(('localhost', port))
        except socket.error as exc:
            if 'Connection refused' not in str(exc):
                raise RuntimeError('connect failed: %r' % exc)
        else:
            sock.close()
            break
    else:
        raise RuntimeError('Timeout trying to connect to localhost:%d' % port)

    # If running headless, setup the virtual display.
    if virtual_display:
        _display = Display(size=(1280, 1024))
        _display.start()
    _display_set = True


def teardown_server():
    """ This function gets called once after all of the tests are run. """
    global _display, _display_set

    # Do nothing if the server isn't started.
    if TEST_CONFIG['server'] is None:
        return

    # Shut down virtual framebuffer.
    if _display is not None:
        _display.stop()
        _display = None
    _display_set = False

    # Shut down server.
    TEST_CONFIG['server'].terminate()
    TEST_CONFIG['server'].wait()
    TEST_CONFIG['stdout'].close()

    # Clean up.
    server_dir = TEST_CONFIG['server_dir']
    if os.path.exists(server_dir):
        shutil.rmtree(server_dir)


def generate(modname):
    """ Generates tests for all configured browsers for `modname`. """
    global _display_set

    # Because Xvfb does not exist on Windows, it's difficult to do
    # headless (EC2) testing on Windows. So for now we don't test there.
    if sys.platform == 'win32':
        return

    # Check if functional tests are to be skipped.
    skip = int(os.environ.get('OPENMDAO_SKIP_GUI', '0'))
    if skip:
        return

    # Search for tests to run.
    module = sys.modules[modname]
    functions = inspect.getmembers(module, inspect.isfunction)
    tests = [func for name, func in functions if name.startswith('_test_')]
    if not tests:
        return

    # Check if any configured browsers are available.
    available_browsers = []
    for name in sorted(_browsers_to_test.keys()):
        if _browsers_to_test[name][0]():
            available_browsers.append(name)
    if not available_browsers:
        msg = 'No browsers available for GUI functional testing'
        _display_set = True  # Avoids starting the server.
        yield _Runner(tests[0]), SkipTest(msg)
        return

    # Due to the way nose handles generator functions, setup_server()
    # won't be called until *after* we yield a test, which is too late.
    if not _display_set:
        setup_server()

    for name in available_browsers:
        try:
            # Open browser and verify we can get page title.
            browser = _browsers_to_test[name][1]()
            browser.title
        except Exception as exc:
            msg = '%s setup failed: %s' % (name, exc)
            logging.critical(msg)
            yield _Runner(tests[0]), SkipTest(msg)
            continue

        abort(False)
        for test in tests:
            logging.critical('')
            if abort():
                msg = '%s tests aborting' % name
                logging.critical(msg)
                yield _Runner(test), SkipTest(msg)
            else:
                logging.critical('Run %s using %s', test.__name__, name)
                yield _Runner(test), browser

        if abort():
            logging.critical('Aborting tests, skipping browser close')
        else:
            browser.quit()
            if name == 'Chrome' and os.path.exists('chromedriver.log'):
                os.remove('chromedriver.log')


class _Runner(object):
    """
    Used to get better descriptions on tests.
    If `browser` is an exception, raise it rather than running the test.
    """

    def __init__(self, test):
        self.test = test
        if test.__doc__:
            self.description = test.__doc__.strip()
        else:
            self.description = '%s (%s)' % (test.__name__, test.__module__)

    def __call__(self, browser):
        if isinstance(browser, Exception):
            raise browser
        try:
            self.test(browser)
        except Exception:
            package, dot, module = self.test.__module__.rpartition('.')
            testname = '%s.%s' % (module, self.test.__name__)
            logging.exception(testname)
            filename = os.path.join(os.getcwd(), '%s.png' % testname)
            msg = 'Screenshot in %s' % filename
            print msg
            browser.save_screenshot(filename)
            logging.info(msg)
            raise


def startup(browser):
    """ Create a project and enter workspace."""
    print 'running %s...' % inspect.stack()[1][3]
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open library.
    workspace_page.show_library()

    return projects_page, project_info_page, project_dict, workspace_page


def closeout(projects_page, project_info_page, project_dict, workspace_page):
    """ Clean up after a test. """
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print '%s complete.' % inspect.stack()[1][3]


def begin(browser):
    """
    Load the projects page and return its page object.
    """
    projects_page = ProjectsListPage(browser, TEST_CONFIG['port'])
    projects_page.go_to()
    eq('Projects', projects_page.page_title)
    return projects_page


def new_project(new_project_page, verify=False):
    """
    Creates a randomly-named new project.
    Returns ``(info_page, info_dict)``
    """
    assert new_project_page.page_title.startswith('Project: New Project')

    if verify:
        data = dict(name=new_project_page.get_random_project_name(),
                    description='Just a project generated by a test script',
                    version='12345678')

        project_info_page = \
            new_project_page.create_project(data['name'], data['description'],
                                            data['version'])

        eq('Project: ' + data['name'], project_info_page.page_title)
    else:
        # Slightly quicker since we typically don't care about the
        # description and version fields.
        data = dict(name=new_project_page.get_random_project_name())
        project_info_page = new_project_page.create_project(data['name'])

    return (project_info_page, data)


def parse_test_args(args=None):
    """ parse test options from command line args
    """
    if args is None:
        args = sys.argv[1:]

    parser = OptionParser()
    parser.add_option("--nonose", action="store_true", dest='nonose',
                      help="if present, run outside of nose")
    parser.add_option("--test", action="store", type="string", dest='test',
                      help="specify a specific test to run", default=None)
    parser.add_option("--noclose", action="store_true", dest='noclose',
                      help="if present, don't close run outside of nose")
    parser.add_option("-v", action="store_true", dest='verbose',
                      help="show progress while running under nose")

    (options, args) = parser.parse_args(args)

    if len(args) > 0:
        print 'unrecognized args: %s' % args
        parser.print_help()
        sys.exit(-1)

    return options


def main(args=None):
    """ run tests for module
    """
    options = parse_test_args(args)

    if options.nonose or options.test:
        # Run tests outside of nose.
        module = sys.modules['__main__']
        functions = inspect.getmembers(module, inspect.isfunction)
        if options.test:
            func = module.__dict__.get('_test_' + options.test)
            if func is None:
                print 'No test named _test_%s' % options.test
                print 'Known tests are:', [name for name, func in functions
                                                if name.startswith('_test_')]
                sys.exit(1)
            tests = [func]
        else:
            # Run all tests.
            tests = [func for name, func in functions
                        if name.startswith('_test_')]

        setup_server(virtual_display=False)
        browser = setup_chrome()
        try:
            for test in tests:
                test(browser)
        finally:
            if not options.noclose:
                browser.quit()
                teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())

