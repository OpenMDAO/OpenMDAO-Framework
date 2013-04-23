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
from pageobjects.component import NameInstanceDialog
from pageobjects.dataflow import DataflowFigure

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait
from pageobjects.basepageobject import TMO
from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

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
    port = get_unused_ip_port()
    TEST_CONFIG['port'] = port
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

            # Don't try screenshot if webdriver is hung.
            if not isinstance(exc, SkipTest):
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
    '''Get the location of the browser download directory.
        This leaves the browser window open
    '''

    browser.get("chrome://settings-frame/settings")

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

    time.sleep(4)  # Otherwise, intermittent failure of next assert
    eq(new_project_modal.modal_title[:11], 'New Project')

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


def slot_drop(browser, element, slot, should_drop, message='Slot'):
    '''Drop an element on a slot'''
    chain = drag_element_to(browser, element, slot, True)
    chain.move_by_offset(25, 0).perform()
    time.sleep(1.0)  # give it a second to update the figure
    check_highlighting(slot, should_highlight=should_drop, message=message)
    release(chain)


def slot_reset(workspace_page, editor=None, metamodel=None, remove_old=False):
    '''every successfull drop permanently fills the slot. because of this,
    we need to make a new metamodel (with empty slots) every successfull drop'''

    if remove_old:
        # first, close out the dialog box we have open
        editor.close()
        # remove the current metamodel
        metamodel.remove()

    #drop 'metamodel' onto the grid
    meta_name = put_element_on_grid(workspace_page, "MetaModel")
    #find it on the page
    metamodel = workspace_page.get_dataflow_figure(meta_name)

    #open the 'edit' dialog on metamodel
    editor = metamodel.editor_page(False)
    editor.move(-250, 0)
    editor.show_slots()

    #resize_editor(workspace_page, editor)

    #find the slots (this is both the drop target and highlight area)
    browser = workspace_page.browser
    slot_id = 'SlotFigure-' + meta_name + '-%s'
    caseiter = browser.find_element(By.ID, slot_id % 'warm_start_data')
    caserec  = browser.find_element(By.ID, slot_id % 'recorder')
    model    = browser.find_element(By.ID, slot_id % 'model')

    return editor, metamodel, caseiter, caserec, model, meta_name


def resize_editor(workspace_page, editor):
    '''ensure that the editor is not covering the library
    (or else we cannot drag things from it!)'''
    browser = workspace_page.browser

    page_width = browser.get_window_size()['width']

    lib_tab      = workspace_page('library_tab').find_element_by_xpath('..')
    lib_width    = lib_tab.size['width']
    lib_position = lib_tab.location['x']

    dialog_title    = editor('dialog_title').find_element_by_xpath('../..')
    dialog_width    = dialog_title.size['width']
    dialog_position = dialog_title.location['x']

    # how much overlap do we have?
    overlap = lib_position - (dialog_position + dialog_width)

    if overlap < 0:  # we are overlapping
        # check to see if we have enough room to move out of the way
        if page_width < dialog_width + lib_width:
            # not enough, need to rezize the editor

            # look for the resize handle
            sibblings = dialog_title.find_elements_by_xpath('../../div')
            handle = None
            for sib in sibblings:
                if "ui-resizable-se" in sib.get_attribute('class'):
                    handle = sib

            # do the resizing
            chain = ActionChains(browser)
            chain.click_and_hold(handle)
            # we can resize editor down to 425px, any less and we cover drop targets
            chain.move_by_offset(450 - dialog_width, 0).perform()
            # must click because release is not working. why? I do not know.
            chain.click().perform()
            chain.release(None).perform()

            # recalculate the overlap
            dialog_title = editor('dialog_title').find_element_by_xpath('../..')
            dialog_width = dialog_title.size['width']
            dialog_position = dialog_title.location['x']
            overlap = lib_position - (dialog_position + dialog_width)

        # We are good, move out!
        chain = ActionChains(browser)
        chain.click_and_hold(editor('dialog_title').element)
        chain.move_by_offset(overlap, 0).perform()
        # must click because release is not working. why? I do not know.
        chain.click().perform()
        chain.release(None).perform()

        # recalculate the overlap
        dialog_title = editor('dialog_title').find_element_by_xpath('../..')
        dialog_width = dialog_title.size['width']
        dialog_position = dialog_title.location['x']
        overlap = lib_position - (dialog_position + dialog_width)

        if overlap < 0:
            # we still have a problem.
            eq(True, False,
                "Could not move or rezise the editor dialog so it is not "
                "overlapping the library. The browser window is too small")


def get_dataflow_fig_in_assembly_editor(workspace_page, name):
    '''Find the named dataflow fig in the assembly editor'''
    all_figs = workspace_page.get_dataflow_figures()
    for fig in all_figs:
        location = fig.find_element_by_xpath("..").get_attribute('id')
        if location == "top-dataflow":
            return DataflowFigure(workspace_page.browser, workspace_page.port, fig)

    return None


def put_assembly_on_grid(workspace_page):
    '''Drop an Assembly on a grid'''
    return put_element_on_grid(workspace_page, 'Assembly')


def put_element_on_grid(workspace_page, element_str):
    '''find and get the 'assembly', and the div for the grid object'''
    browser = workspace_page.browser

    for retry in range(3):
        try:
            assembly = workspace_page.find_library_button(element_str)
            chain = ActionChains(browser)
            chain.click_and_hold(assembly)
            chain.move_by_offset(-100, 0).perform()
        except StaleElementReferenceException:
            if retry < 2:
                logging.warning('put_element_on_grid %s:'
                                ' StaleElementReferenceException', element_str)
            else:
                raise
        else:
            break

    grid = browser.find_element_by_xpath('//div[@id="-dataflow"]')
    check_highlighting(grid, True, "Grid")
    release(chain)

    # deal with the modal dialog
    name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # make sure it is on the grid
    ensure_names_in_workspace(workspace_page, [name],
        "Dragging '" + element_str + "' to grid did not produce a new element on page")

    return name


def ensure_names_in_workspace(workspace_page, names, message=None):
    """ensures the list of element names in included in the workspace"""

    allnames = workspace_page.get_dataflow_component_names()

    # sometimes does not load all of the names for some reason.
    # Reloading seems to fix the problem
    try_reload = False
    for name in names:
        if not name in allnames:
            try_reload = True
    if try_reload:
        time.sleep(.1)
        allnames = workspace_page.get_dataflow_component_names()

    # now we will assert that the elements that we added appear on the page
    for name in names:
        eq(name in allnames, True, '%s: %s' % (message, name))


def drag_element_to(browser, element, drag_to, centerx):
    '''Drag one element over to another element'''
    chain = ActionChains(browser)
    chain.move_to_element(element)
    chain.click_and_hold(element)
    chain.move_to_element(drag_to)
    if centerx:
        offset = int(drag_to.value_of_css_property('width')[:-2]) / 2
        chain.move_by_offset(offset, 1)
    else:
        chain.move_by_offset(5, 1)
    chain.perform()
    return chain


def release(chain):
    '''The drop part of the ActionChain when doing drag and drop'''
    chain.release(on_element=None).perform()
    time.sleep(0.5)  # Pacing for diagram update.


def check_highlighting(element, should_highlight=True, message='Element'):
    '''check to see that the background-color of the element is highlighted'''
    if 'SlotFigure' in element.get_attribute('class'):
        # a slot figure is a div containing a ul element (the context menu) and
        # one or more svg elements, each of which contains a rect and two texts
        # the last rect fill style is what we need to check for highlighting
        rect = element.find_elements_by_css_selector('svg rect')[-1]
        style = rect.get_attribute('style')
    else:
        style = element.get_attribute('style')
    highlighted = ('background-color: rgb(207, 214, 254)' in style) \
                or ('highlighted.png' in style) \
                or ('fill: #cfd6fe' in style)
    eq(highlighted, should_highlight, message +
        (' did not highlight (and should have) ' if should_highlight else
         ' highlighed (and should not have) ')
         + 'when dragging a dropable element to it')


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
                test(browser)
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
