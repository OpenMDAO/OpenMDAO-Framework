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

try:
    WindowsError
except NameError:
    WindowsError = None

from distutils.spawn import find_executable
from nose import SkipTest
from nose.tools import eq_ as eq
from selenium import webdriver
if sys.platform != 'win32':
    from pyvirtualdisplay import Display

from optparse import OptionParser

from openmdao.util.network import get_unused_ip_port
from openmdao.util.fileutil import onerror
from openmdao.gui.util import find_chrome

from pageobjects.project import ProjectsPage
from pageobjects.util import SafeDriver, abort

from pageobjects.workspace import WorkspacePage

from selenium.webdriver.support.ui import WebDriverWait
from pageobjects.basepageobject import TMO
from selenium.webdriver.common.by import By

if '.' not in sys.path:  # Look like an interactive session.
    sys.path.append('.')

TEST_CONFIG = dict(browsers=[], server=None, port=None)
_display_set = False
_display = None


def check_for_chrome():
    if find_chrome() is not None:
        return True
    else:
        return False


def setup_chrome():
    """ Initialize the Chrome browser. """
    exe = 'chromedriver'
    path = find_executable(exe)
    if not path:
        # Download, unpack, and install in OpenMDAO 'bin'.
        prefix = 'http://chromedriver.googlecode.com/files/'
        version = '23.0.1240.0'
        if sys.platform == 'darwin':
            flavor = 'mac'
        elif sys.platform == 'win32':
            flavor = 'win'
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
            if sys.platform == 'win32':
                exe += '.exe'
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
    server_dir = 'gui-server'

    # Try to clean up old server dir. If this fails (looking at you Windows),
    # then use another directory.
    if os.path.exists(server_dir):
        try:
            shutil.rmtree(server_dir, onerror=onerror)
        except WindowsError as exc:
            print >> sys.stderr, 'Could not delete %s: %s' % (server_dir, exc)
            nxt = 1
            while os.path.exists(server_dir):
                nxt += 1
                server_dir = 'gui-server-%s' % nxt
    os.mkdir(server_dir)

    TEST_CONFIG['server_dir'] = server_dir
    orig = os.getcwd()
    os.chdir(server_dir)
    try:
        stdout = open('stdout', 'w')
        TEST_CONFIG['stdout'] = stdout
        sigfile = os.path.join(os.getcwd(), 'SIGTERM.txt')
        if os.path.exists(sigfile):
            os.remove(sigfile)
        TEST_CONFIG['sigfile'] = sigfile
        port = get_unused_ip_port()
        TEST_CONFIG['port'] = port
        server = subprocess.Popen(('python', '-m', 'openmdao.gui.omg',
                                   '--server', '--port', str(port)),
                                   stdout=stdout, stderr=subprocess.STDOUT)
    finally:
        os.chdir(orig)
    TEST_CONFIG['server'] = server
    TEST_CONFIG['failed'] = []

    # Wait for server port to open.
    for i in range(200):  # ~20 sec.
        time.sleep(.1)
        try:
            sock = socket.create_connection(('localhost', port))
        except socket.error as exc:
            if 'refused' not in str(exc):
                raise RuntimeError('connect failed: %r' % exc)
        else:
            sock.close()
            break
    else:
        raise RuntimeError('Timeout trying to connect to localhost:%d' % port)

    # If running headless, setup the virtual display.
    if sys.platform != 'win32' and virtual_display:
        _display = Display(size=(1280, 1024))
        _display.start()
    _display_set = True


def teardown_server():
    """ This function gets called once after all of the tests are run. """
    global _display, _display_set
    server = TEST_CONFIG['server']

    # Do nothing if the server isn't started.
    if server is None:
        return

    # Shut down virtual framebuffer.
    if _display is not None:
        _display.stop()
        _display = None
    _display_set = False

    # First try to have all subprocesses go down cleanly.
    if sys.platform == 'win32':
        with open(TEST_CONFIG['sigfile'], 'w') as sigfile:
            sigfile.write('Shutdown now\n')
    else:
        server.terminate()
    for i in range(10):
        time.sleep(1)
        if server.poll() is not None:
            break
    else:
        # No luck, at least terminate the server.
        print >> sys.stderr, 'teardown_server: Killing server'
        server.kill()
    TEST_CONFIG['stdout'].close()

    server_dir = TEST_CONFIG['server_dir']

    if TEST_CONFIG['failed']:
        # Save server log & stdout for post-mortem.
        modname = TEST_CONFIG['modname']
        if '.' in modname:
            prefix, dot, modname = modname.rpartition('.')
        time.sleep(5)  # Wait for Windows...
        logfile = os.path.join(server_dir, 'openmdao_log.txt')
        if os.path.exists(logfile):
            try:
                os.rename(logfile, '%s-log.txt' % modname)
            except Exception as exc:
                print >> sys.stderr, 'teardown_server: %s rename failed: %s' \
                                     % (logfile, exc)
        stdout = os.path.join(server_dir, 'stdout')
        if os.path.exists(stdout):
            try:
                os.rename(stdout, '%s-stdout.txt' % modname)
            except Exception as exc:
                print >> sys.stderr, 'teardown_server: %s rename failed: %s' \
                                     % (stdout, exc)
    # Clean up.
    if os.path.exists(server_dir):
        try:
            shutil.rmtree(server_dir, onerror=onerror)
        except Exception as exc:
            print >> sys.stderr, 'teardown_server: %s cleanup failed: %s' \
                                 % (server_dir, exc)


def generate(modname):
    """ Generates tests for all configured browsers for `modname`. """
    global _display_set

    # Check if functional tests are to be skipped.
    if int(os.environ.get('OPENMDAO_SKIP_GUI', '0')):
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
        TEST_CONFIG['modname'] = modname
        setup_server()

    for name in available_browsers:
        try:
            # Open browser and verify we can get page title.
            browser = SafeDriver(_browsers_to_test[name][1]())
            browser.title
        except Exception as exc:
            msg = '%s setup failed: %s' % (name, exc)
            logging.critical(msg)
            yield _Runner(tests[0]), SkipTest(msg)
            continue

        abort(False)
        cleanup = True
        runner = None
        for test in tests:
            if runner is not None and runner.failed:
                cleanup = False
            runner = _Runner(test)
            logging.critical('')
            if abort():
                msg = '%s tests aborting' % name
                logging.critical(msg)
                yield runner, SkipTest(msg)
            else:
                logging.critical('Run %s using %s', test.__name__, name)
                yield runner, browser
        if runner is not None and runner.failed:
            cleanup = False

        if abort():
            logging.critical('Aborting tests, skipping browser close')
        else:
            try:
                browser.close()
            except Exception as exc:
                print 'browser.close failed:', exc
        try:
            browser.quit()
        except Exception as exc:
            # if it already died, calling kill on a defunct process
            # raises a WindowsError: Access Denied
            print 'browser.quit failed:', exc
        if name == 'Chrome':
            if sys.platform == 'win32':
                time.sleep(2)
                # Kill any stubborn chromedriver processes.
                proc = subprocess.Popen(['taskkill', '/f', '/t', '/im', 'chromedriver.exe'],
                                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                stdout, stderr = proc.communicate()
                stdout = stdout.strip()
                if stdout != 'ERROR: The process "chromedriver.exe" not found.':
                    print 'taskkill output: %r' % stdout
            if cleanup and os.path.exists('chromedriver.log'):
                try:
                    os.remove('chromedriver.log')
                except Exception as exc:
                    print 'Could not delete chromedriver.log: %s' % exc


class _Runner(object):
    """
    Used to get better descriptions on tests and post-mortem screenshots.
    If `browser` is an exception, raise it rather than running the test.
    """

    def __init__(self, test):
        self.test = test
        if test.__doc__:
            self.description = test.__doc__.strip()
        else:
            self.description = '%s (%s)' % (test.__name__, test.__module__)
        self.failed = False

    def __call__(self, browser):
        if isinstance(browser, Exception):
            raise browser  # Likely a hung webdriver.
        base_window = browser.current_window_handle
        try:
            self.test(browser)
        except SkipTest:
            raise
        except Exception as exc:
            saved_exc = sys.exc_info()
            self.failed = True
            package, dot, module = self.test.__module__.rpartition('.')
            testname = '%s.%s' % (module, self.test.__name__)
            logging.exception(testname)
            TEST_CONFIG['failed'].append(testname)

            # Try a screenshot.
            filename = os.path.join(os.getcwd(), '%s.png' % testname)
            print 'Attempting to take screenshot...'
            if _save_screenshot(browser, filename):
                msg = 'Screenshot in %s' % filename
                print msg
                logging.critical(msg)

            # Close all extra windows.
            try:
                for window in browser.window_handles:
                    if window == base_window:
                        continue
                    elif window != browser.current_window_handle:
                        browser.switch_to_window(window)
                    browser.close()
                if browser.current_window_handle != base_window:
                    browser.switch_to_window(base_window)
            except Exception as exc:
                logging.exception('window closing failed')

            sys.stdout.flush()
            sys.stderr.flush()

            # Try to copy the log (it may be rewritten in subsequent tests).
            server_dir = TEST_CONFIG['server_dir']
            logfile = os.path.join(server_dir, 'openmdao_log.txt')
            if os.path.exists(logfile):
                try:
                    shutil.copy(logfile, '%s-log.txt' % testname)
                except Exception as exc:
                    print >> sys.stderr, '_Runner: %s copy failed: %s' \
                                         % (logfile, exc)

            # Finally, raise the original exception.
            raise saved_exc[0], saved_exc[1], saved_exc[2]


def _save_screenshot(browser, filename, retry=True):
    """ Attempt to take a screenshot. """
    try:
        browser.save_screenshot(filename)
    except Exception as exc:
        msg = 'Screenshot failed: %s' % exc
        print msg
        logging.critical(msg)
        if 'An open modal dialog blocked the operation' in msg and retry:
            alert = browser.switch_to_alert()
            msg = 'alert text: %s' % alert.text
            print msg
            logging.critical(msg)
            alert.dismiss()
            print 'Retrying...'
            return _save_screenshot(browser, filename, False)
        else:
            return False
    return True


def startup(browser):
    """ Create a project and enter workspace. """
    print 'running %s...' % inspect.stack()[1][3]
    browser.set_window_position(0, 0)
    browser.set_window_size(1280, 1024)
    projects_page = begin(browser)
    workspace_page, project_dict = new_project(projects_page.new_project(),
                                               load_workspace=True)
    return project_dict, workspace_page


def closeout(project_dict, workspace_page):
    """ Clean up after a test. """
    projects_page = workspace_page.close_workspace()
    projects_page.delete_project(project_dict['name'])
    print '%s complete.' % inspect.stack()[1][3]


def begin(browser):
    """
    Load the projects page and return its page object.
    """
    projects_page = ProjectsPage(browser, TEST_CONFIG['port'])
    projects_page.go_to()
    eq('Projects', projects_page.page_title)
    return projects_page


def get_browser_download_location_path(browser):
    """
    Get the location of the browser download directory.
    This leaves the browser window open
    """
    try:
        browser.get("chrome://settings-frame/settings")
    except Exception as err:
        print 'Error getting chrome settings page:', err
        raise SkipTest('Could not access Chrome settings to get download path')

    element = WebDriverWait(browser, TMO).until(
        lambda browser: browser.find_element(By.ID, 'downloadLocationPath'))
    download_location_path = element.get_attribute('value')
    return download_location_path


def submit_metadata(metadata_modal, name, description=None, version=None,
                    load_workspace=False):
    """
    Submits metadata for a project
    Returns ``(projects_page, data)``
    """

    metadata_modal.submit_metadata(name, description, version)
    workspace_page = WorkspacePage.verify(metadata_modal.browser,
                                          TEST_CONFIG['port'])
    if not load_workspace:
        return workspace_page.close_workspace()

    return workspace_page


def new_project(new_project_modal, verify=False, load_workspace=False):
    """
    Creates a randomly-named new project.
    Returns ``(projects_page, info_dict)``
    """
    if verify:
        data = dict(name=new_project_modal.get_random_project_name(),
                    description='Just a project generated by a test script',
                    version='12345678')
        page = submit_metadata(new_project_modal, data['name'],
                               data['description'], data['version'],
                               load_workspace=load_workspace)
    else:
        data = dict(name=new_project_modal.get_random_project_name())
        page = submit_metadata(new_project_modal, data['name'],
                               load_workspace=load_workspace)
    return (page, data)


def edit_project(edit_project_modal, name, description=None, version=None):
    """
    Creates a randomly-named new project.
    Returns ``(projects_page, info_dict)``
    """
    assert edit_project_modal.modal_title.startswith('Edit Project')
    return submit_metadata(edit_project_modal, name, description, version)


def import_project(import_project_modal, projectfile_path, verify=False,
                   load_workspace=False):
    """
    Creates a randomly-named imported new project.
    Returns ``(projects_page, info_dict)``
    """
    assert import_project_modal.modal_title.startswith('Import Project')

    # load project
    import_project_modal.load_project(projectfile_path)

    # submit the rest of the metadata
    if verify:
        data = dict(name=import_project_modal.get_random_project_name(),
                    description='An imported project from a test script',
                    version='87654321')
        page = submit_metadata(import_project_modal, data['name'],
                               data['description'], data['version'],
                               load_workspace)
    else:
        data = dict(name=import_project_modal.get_random_project_name())
        page = submit_metadata(import_project_modal, data['name'],
                               load_workspace=load_workspace)

    return (page, data)


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
                print 'Known tests are:', [name[6:] for name, func in functions
                                                if name.startswith('_test_')]
                sys.exit(1)
            tests = [func]
        else:
            # Run all tests.
            tests = [func for name, func in functions
                        if name.startswith('_test_')]

        TEST_CONFIG['modname'] = '__main__'
        setup_server(virtual_display=False)
        browser = SafeDriver(setup_chrome())
        try:
            for test in tests:
                try:
                    test(browser)
                except SkipTest:
                    pass
        finally:
            if not options.noclose:
                try:
                    browser.quit()
                except WindowsError:
                    # if it already died, calling kill on a defunct process
                    # raises a WindowsError: Access Denied
                    pass
                teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
