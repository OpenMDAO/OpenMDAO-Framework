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
    for exe in ('chromium-browser', 'google-chrome', 'chrome'):
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
        if sys.platform == 'darwin':
            flavor = 'mac'
        elif sys.platform == 'win32':
            flavor = 'win'
        elif '64bit' in platform.architecture():
            flavor = 'linux64'
        else:
            flavor = 'linux32'
        filename = '%s_%s_19.0.1068.0.zip' % (exe, flavor)
        orig_dir = os.getcwd()
        os.chdir(os.path.dirname(sys.executable))
        try:
            logging.critical('Downloading %s to %s', filename, os.getcwd())
            src = urllib2.urlopen(prefix+filename)
            with open(filename, 'wb') as dst:
                dst.write(src.read())
            src.close()
            zip = zipfile.ZipFile(filename)
            zip.extract(exe)
            zip.close()
            if sys.platform != 'win32':
                os.chmod(exe, stat.S_IRUSR|stat.S_IWUSR|stat.S_IXUSR)
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
        _display = Display()
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
        self.test(browser)


def begin(browser):
    """
    Otherwise, load the projects page and return its page object.
    """
    projects_page = ProjectsListPage(browser, TEST_CONFIG['port'])
    projects_page.go_to()
    eq( 'Projects', projects_page.page_title )
    return projects_page


def new_project(new_project_page):
    """
    Creates a randomly-named new project.
    Returns ``(info_page, info_dict)``
    """
    assert new_project_page.page_title.startswith('Project: New Project')

    data = dict(name=new_project_page.get_random_project_name(),
                description='Just a project generated by a test script',
                version='12345678')

    project_info_page = \
        new_project_page.create_project(data['name'], data['description'],
                                        data['version'])
    eq( 'Project: '+data['name'], project_info_page.page_title )

    return (project_info_page, data)

