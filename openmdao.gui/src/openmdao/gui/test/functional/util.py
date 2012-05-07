"""
Utilities for GUI functional testing.
"""

import getpass
import inspect
import logging
import os.path
import platform
import shutil
import signal
import socket
import stat
import subprocess
import sys
import time
import urllib2
import zipfile

from distutils.spawn import find_executable
from nose.tools import eq_ as eq
from pyvirtualdisplay import Display
from selenium import webdriver

from openmdao.util.network import get_unused_ip_port

from pageobjects.project import ProjectsListPage

if '.' not in sys.path:  # Look like an interactive session.
    sys.path.append('.')

TEST_CONFIG = dict(browsers=[], server=None, port=None)
_display = None


def setup_chrome():
    """ Initialize the 'chrome' browser. """
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

def setup_firefox():
    """ Initialize the 'firefox' browser. """
    profile = webdriver.FirefoxProfile()
    profile.native_events_enabled = True
    driver = webdriver.Firefox(profile)
    driver.implicitly_wait(15)
    TEST_CONFIG['browsers'].append(driver)
    return driver

_browsers_to_test = dict(
    Chrome=setup_chrome,
    #Firefox=setup_firefox,
)


def setup_server(virtual_display=True):
    """ Start server on ``localhost`` using an unused port. """
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

# FIXME: this has no effect when running under nose (firefox on hx).
    if virtual_display:
        global _display
        _display = Display()
#        _display = Display(visible=0, size=(800, 600))
#        _display = Display(backend='xvfb')
        _display.start()


def teardown_server():
    """ The function gets called once after all of the tests are called. """
    # Kill chromedriver.
    for browser in TEST_CONFIG['browsers']:
        if isinstance(browser, webdriver.Chrome):
            psfu = subprocess.check_output(('ps', '-fu', getpass.getuser()))
            pid = os.getpid()
            for line in psfu.splitlines(True):
                if 'chromedriver' in line:
                    fields = line.split()
                    # UID PID PPID C STIME TTY TIME CMD
                    if int(fields[2]) == pid:
                        os.kill(int(fields[1]), signal.SIGTERM)

    # Shut down virtual framebuffer.
    if _display is not None:
        _display.stop()

    # Shut down server.
    TEST_CONFIG['server'].terminate()
    TEST_CONFIG['stdout'].close()

    # Clean up.
    server_dir = TEST_CONFIG['server_dir']
    if os.path.exists(server_dir):
        shutil.rmtree(server_dir)
    if os.path.exists('chromedriver.log'):
        os.remove('chromedriver.log')


def generate(modname):
    """ Generates tests for all configured browsers for `modname`. """
    # Because Xvfb does not exist on Windows, it's difficult to do
    # headless (EC2) testing on Windows. So for now we just test on Linux.
    if sys.platform == 'win32':
        return

    module = sys.modules[modname]
    functions = inspect.getmembers(module, inspect.isfunction)
    tests = [func for name, func in functions if name.startswith('_test_')]
    for name in sorted(_browsers_to_test.keys()):
        try:
            browser = _browsers_to_test[name]()  # Open browser.
            browser.title
        except Exception as exc:
            logging.critical('Skipping %s, caught: %s', name, exc)
            continue
        try:
            for _test in tests:
                logging.critical('')
                logging.critical('Running %s using %s', _test.__name__, name)
                yield _test, browser
        finally:
            browser.close()


def begin(browser):
    """ Start in projects page. Returns that page. """
    projects_page = ProjectsListPage(browser, TEST_CONFIG['port'])
    projects_page.go_to()
    eq( "Projects", projects_page.page_title )
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

