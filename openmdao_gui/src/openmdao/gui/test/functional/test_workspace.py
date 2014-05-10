"""
Tests of overall workspace functions.
"""

import time

import pkg_resources

from nose import SkipTest
from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import StaleElementReferenceException, \
                                       WebDriverException
from util import main, setup_server, teardown_server, generate, \
                 startup, closeout, broken_chrome

from pageobjects.basepageobject import TMO
from pageobjects.util import NotifierPage
from pageobjects.workspace import WorkspacePage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_slots_sorted_by_name(browser):
    project_dict, workspace_page = startup(browser)

    #drop 'metamodel' onto the grid
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    args = ["('ratio1', 'ratio2')", "('torque_ratio', 'RPM')"]
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.metamodel.MetaModel', 'mm', args=args)
    #open the 'edit' dialog on metamodel
    metamodel = workspace_page.get_dataflow_figure('mm', 'top')
    mm_editor = metamodel.editor_page()

    # see if the slot names are sorted
    slot_name_elements = mm_editor.root.find_elements_by_css_selector('text#name')
    slot_names = [s.text for s in slot_name_elements]
    eq(slot_names, sorted(slot_names))

    closeout(project_dict, workspace_page)


def _test_console(browser):
    # Check basic console functionality.
    project_dict, workspace_page = startup(browser)

    workspace_page.do_command("print 'blah'")
    expected = ">>> print 'blah'\nblah"
    eq(workspace_page.history, expected)

    # Check that browser title contains project name.
    title = browser.title
    expected = 'OpenMDAO: ' + project_dict['name'] + ' - '
    eq(title[:len(expected)], expected)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_console_history(browser):
    # Check up and down arrow navigation through the command history
    project_dict, workspace_page = startup(browser)

    command_elem = browser.find_element(By.ID, "cmdline")

    # Fill up the command history
    workspace_page.do_command("import sys")
    workspace_page.do_command("import os")
    workspace_page.do_command("import time")

    # Try out the up and down arrows
    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import time")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import os")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import sys")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import sys")

    command_elem.send_keys(Keys.ARROW_DOWN)
    eq(workspace_page.command, "import os")

    command_elem.send_keys(Keys.ARROW_DOWN)
    eq(workspace_page.command, "import time")

    command_elem.send_keys(Keys.ARROW_DOWN)
    eq(workspace_page.command, "import time")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import os")

    workspace_page.do_command("import traceback")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import traceback")

    command_elem.send_keys(Keys.ARROW_UP)
    eq(workspace_page.command, "import time")

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_palette_update(browser):
    # Import some files and add components from them.
    project_dict, workspace_page = startup(browser)

    # View dataflow.
    workspace_page('dataflow_tab').click()

    # Get file paths
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')
    file2_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'optimization_unconstrained.py')

    # add first file from workspace
    workspace_page.add_file(file1_path)

    # Open code editor.and add second file from there
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    time.sleep(0.5)
    editor_page.add_file(file2_path)

    # Check code editor to make sure the files were added.
    time.sleep(0.5)
    file_names = editor_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Check workspace to make sure the files also show up there.
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Make sure there are only two dataflow figures (top & driver)
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    eq(len(workspace_page.get_dataflow_figures()), 2)

    # Drag element into workspace.
    paraboloid_name = 'parab'
    workspace_page.add_library_item_to_dataflow('paraboloid.Paraboloid',
                                                paraboloid_name)

    # Now there should be three.
    eq(len(workspace_page.get_dataflow_figures()), 3)

    # Make sure the item added is there with the name we gave it.
    component_names = workspace_page.get_dataflow_component_names()
    if paraboloid_name not in component_names:
        raise TestCase.failureException(
            "Expected component name, '%s', to be in list of existing"
            " component names, '%s'" % (paraboloid_name, component_names))

    workspace_page.commit_project('added paraboloid')
    projects_page = workspace_page.close_workspace()

    # Now try to re-open that project to see if items are still there.
    #project_info_page = projects_page.edit_project(project_dict['name'])
    workspace_page = projects_page.open_project(project_dict['name'])

    # Check to see that the added files are still there.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_names = editor_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_loading_docs(browser):
    project_dict, workspace_page = startup(browser)

    # Check that the docs are viewable
    workspace_page('help_menu').click()
    time.sleep(0.5)
    eq(workspace_page('doc_button').get_attribute('id'), 'help-doc')

    workspace_window = browser.current_window_handle
    current_windows = set(browser.window_handles)
    workspace_page('doc_button').click()
    new_windows = set(browser.window_handles) - current_windows
    docs_window = list(new_windows)[0]
    browser.switch_to_window(docs_window)
    time.sleep(0.5)
    eq("OpenMDAO User Guide" in browser.title, True)
    eq("OpenMDAO Documentation" in browser.title, True)

    browser.close()
    browser.switch_to_window(workspace_window)
    workspace_page.show_library()
    browser.switch_to_window(workspace_page.view_library_item_docs("openmdao.main.assembly.Assembly"))

    # Just check to see if a Traceback 404 message was sent.
    try:
        browser.find_element((By.XPATH, "/html/head/body/pre[1]"))
        assert False
    except:
        pass
    browser.close()
    browser.switch_to_window(workspace_window)
    closeout(project_dict, workspace_page)


def _test_menu(browser):
    project_dict, workspace_page = startup(browser)

    # Check enable/disable of commit/revert.
    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), 'omg-disabled')
    eq(workspace_page('revert_button').get_attribute('class'), 'omg-disabled')
    workspace_page('project_menu').click()

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.replace_driver('top', 'SLSQPdriver')

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), '')
    eq(workspace_page('revert_button').get_attribute('class'), '')
    workspace_page('project_menu').click()

    workspace_page.commit_project()

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), 'omg-disabled')
    eq(workspace_page('revert_button').get_attribute('class'), 'omg-disabled')
    workspace_page('project_menu').click()

    #FIXME: These need to verify that the request has been performed.
    # View menu.
    for item in ('console', 'library', 'objects', 'files',
                 'properties', 'workflow', 'dataflow', 'refresh'):
        workspace_page('view_menu').click()
        workspace_page('%s_button' % item).click()
        time.sleep(0.5)  # Just so we can see it.

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_file_commit(browser):
    project_dict, workspace_page = startup(browser)

    # Check that adding a file enables commit.
    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), 'omg-disabled')
    eq(workspace_page('revert_button').get_attribute('class'), 'omg-disabled')
    workspace_page('project_menu').click()

    stl_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/box.stl')
    workspace_page.add_file(stl_path)
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if file_names != ['box.stl']:
        raise TestCase.failureException('Expected box.stl, got %s' % file_names)

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), '')
    eq(workspace_page('revert_button').get_attribute('class'), '')  # Enabled?
    workspace_page('project_menu').click()

    # Commit and check that commit is disabled but revert is enabled.
    workspace_page.commit_project()

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), 'omg-disabled')
    eq(workspace_page('revert_button').get_attribute('class'), 'omg-disabled')
    workspace_page('project_menu').click()                     # Disabled?

    # Remove file and check commit & revert enabled.
    workspace_page.delete_file('box.stl')
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if file_names:
        raise TestCase.failureException('Unexpected files %s' % file_names)

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), '')
    eq(workspace_page('revert_button').get_attribute('class'), '')
    workspace_page('project_menu').click()

    # revert back to version with file.
    workspace_page = workspace_page.revert_project()
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    if file_names != ['box.stl']:
        raise TestCase.failureException('Expected box.stl, got %s' % file_names)

    workspace_page('project_menu').click()
    time.sleep(0.5)
    eq(workspace_page('commit_button').get_attribute('class'), 'omg-disabled')
    eq(workspace_page('revert_button').get_attribute('class'), 'omg-disabled')
    workspace_page('project_menu').click()

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_macro(browser):
    project_dict, workspace_page = startup(browser)

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Create a file (code editor automatically indents).
    editor_page.new_file('foo.py', """
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float

class Foo(Component):

a = Float(0.0, iotype='in')
b = Float(0.0, iotype='out')
""")
    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Add some Foo instances.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    time.sleep(2)  # Wait for it to get registered.
    workspace_page.set_library_filter('In Project')
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp1')
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp2')

    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    conn_page = workspace_page.connect(comp1, comp2)
    conn_page.connect_vars('comp1.b', 'comp2.a')
    conn_page.close()

    workspace_page.commit_project('added some Foos')

    if broken_chrome():
        raise SkipTest('Test broken for chrome/selenium combination')
    editor_page = workspace_page.open_editor()
    editor_page.edit_file('foo.py', dclick=False)
    editor_page.add_text_to_file('#just a comment\n')

    # forces a save and reload of project
    editor_page.save_document(overwrite=True, check=False)
    browser.switch_to_window(workspace_window)
    port = workspace_page.port
    workspace_page = WorkspacePage.verify(browser, port)

    workspace_page.show_dataflow('top')
    time.sleep(0.5)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['comp1', 'comp2', 'driver', 'top'])

    # Check if running a component is recorded (it shouldn't be).
    top = workspace_page.get_dataflow_figure('top')
    top.run()
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')
    history = workspace_page.history.split('\n')
    eq(history[-2], 'Executing...')
    eq(history[-1], 'Execution complete.')

    workspace_page.toggle_files('foo.py')
    workspace_page.expand_folder('_macros')
    editor = workspace_page.edit_file('_macros/default')
    contents = editor.get_code()
    browser.close()
    browser.switch_to_window(workspace_window)
    for line in contents.split('\n'):
        if 'run' in line:
            raise AssertionError(line)

    # Check if command errors are recorded (they shouldn't be).
    workspace_page.do_command('print xyzzy', ack=False)
    NotifierPage.wait(workspace_page, base_id='command')
    expected = "NameError: name 'xyzzy' is not defined"
    assert workspace_page.history.endswith(expected)

    editor = workspace_page.edit_file('_macros/default')
    contents = editor.get_code()
    browser.close()
    browser.switch_to_window(workspace_window)
    for line in contents.split('\n'):
        if 'xyzzy' in line:
            raise AssertionError(line)

    # Clean up.
    closeout(project_dict, workspace_page)

def _test_properties(browser):
    # Checks right-hand side properties display.
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')

    (header, inputs, outputs) = workspace_page.get_properties('top')
    eq(header, 'Driver: top.driver')
    eq(inputs.value, [
        ['directory',         ''],
        ['force_execute',     'True'],
        ['force_fd',          'False'],
        [' gradient_options', ''],  # vartree, has leading space after the [+]
    ])
    eq(outputs.value, [
        ['derivative_exec_count', '0'],
        ['exec_count',            '0'],
        ['itername',              '']
    ])

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_implicit_component(browser):
    project_dict, workspace_page = startup(browser)

    # create an assembly with an implicit component in it's workflow
    filename = pkg_resources.resource_filename('openmdao.main.test',
                                               'test_implicit_component.py')
    workspace_page.add_file(filename)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')

    workspace_page.add_library_item_to_dataflow('test_implicit_component.MyComp_Deriv',
                                                'comp', prefix='top')

    workspace_page.add_object_to_workflow('top.comp', 'top')

    # Verify that the evaluate menu option has the expected effect
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    comp_editor = comp.editor_page(base_type='ImplicitComponent')

    states = comp_editor.get_states()
    eq(states.value, [
        ['', 'x', '0', '', ''],
        ['', 'y', '0', '', ''],
        ['', 'z', '0', '', ''],
    ])

    residuals = comp_editor.get_residuals()
    eq(residuals.value, [
        ['', 'res', '[0.0, 0.0, 0.0]', '', '']
    ])

    comp_editor.set_state('x', '1')
    comp_editor.set_state('y', '2')
    comp_editor.set_state('z', '3')

    comp.evaluate()

    states = comp_editor.get_states()
    eq(states.value, [
        ['', 'x', '1', '', ''],
        ['', 'y', '2', '', ''],
        ['', 'z', '3', '', ''],
    ])

    residuals = comp_editor.get_residuals()
    eq(residuals.value, [
        ['', 'res', '[7.0, 12.0, -3.0]', '', '']
    ])

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_component_tree(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.select_objects_view('Components')

    # Add maxmin.py to project
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/maxmin.py')
    workspace_page.add_file(file_path)

    # Add MaxMin to 'top'.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin')

    # Maximize 'top' and 'top.maxmin'
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top'])
    workspace_page.expand_object('top')
    time.sleep(0.5)
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin'])
    workspace_page.expand_object('top.maxmin')
    time.sleep(0.5)
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub'])

    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin2')
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub', 'top.maxmin2'])

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_editable_inputs(browser):
    raise SkipTest

    def test_color(actual, expected, alpha=False):
        if alpha:
            eq(actual, expected)
        else:
            eq(actual[0:3], expected[0:3])

    def test_inputs(inputs):
        for i, row in enumerate(inputs):
            connected_to_cell = row.cells[len(row.cells) - 2]
            implicit_cell = row.cells[len(row.cells) - 1]
            name_cell = row.cells[0]
            value_cell = row.cells[2]

            if connected_to_cell.value:
                test_color(name_cell.color, [255, 255, 255, 1])
                test_color(value_cell.color, [255, 255, 255, 1])
                test_color(value_cell.background_color, [0, 0, 0, 1])
            elif implicit_cell.value:
                test_color(name_cell.color, [100, 180, 255, 1])
                test_color(value_cell.color, [100, 180, 255, 1])
                test_color(value_cell.background_color, [255, 255, 255, 1])
            else:
                test_color(name_cell.color, [255, 255, 255, 1])
                test_color(value_cell.color, [0, 0, 0, 1])
                test_color(value_cell.background_color, [255, 255, 255, 1])

    def test_outputs(outputs):
        for i, row in enumerate(outputs):
            implicit_cell = row.cells[len(row.cells) - 1]
            name_cell = row.cells[0]
            value_cell = row.cells[2]

            if implicit_cell.value:
                test_color(name_cell.color, [100, 180, 255, 1])
                test_color(value_cell.color, [100, 180, 255, 1])
            else:
                test_color(name_cell.color, [255, 255, 255, 1])
                test_color(value_cell.color, [255, 255, 255, 1])

            test_color(value_cell.background_color, [0, 0, 0, 1])

    project_dict, workspace_page = startup(browser)

    # Import vehicle_singlesim
    file_path_one = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                    'files/basic_model.py')
    file_path_two = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                    'vehicle_singlesim.py')
    workspace_page.add_file(file_path_one)
    workspace_page.add_file(file_path_two)

    assembly_name = "sim"
    workspace_page.add_library_item_to_dataflow('basic_model.Basic_Model',
                                                assembly_name)
    paraboloid = workspace_page.get_dataflow_figure('paraboloid', assembly_name)

    #Test highlighting for implicit connections
    component_editor = paraboloid.editor_page()
    test_inputs(component_editor.get_inputs())
    test_outputs(component_editor.get_outputs())

    component_editor.close()

    #Remove sim from the dataflow
    assembly = workspace_page.get_dataflow_figure(assembly_name)
    assembly.remove()

    #Add VehicleSim to the dataflow
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
                                                assembly_name)

    # Get component editor for transmission.
    workspace_page.expand_object(assembly_name)
    workspace_page.show_dataflow(assembly_name + ".vehicle")
    transmission = workspace_page.get_dataflow_figure('transmission',
                                                      assembly_name + '.vehicle')

    #Test highlighting for explicit connections
    component_editor = transmission.editor_page()
    test_inputs(component_editor.get_inputs())
    test_outputs(component_editor.get_outputs())

    component_editor.close()

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_console_errors(browser):
    project_dict, workspace_page = startup(browser)

    # Set input to illegal value.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False, base_type='Assembly')
    editor.move(-100, -40)  # Make viewable on small screen.
    inputs = editor.get_inputs()
    inputs.rows[4].cells[2].click()
    inputs[4][2] = '42'  # printvars
    expected = "TraitError: The 'printvars' trait of an "     \
               "Assembly instance must be a list of items "  \
               "which are a legal value, but a value of 42 " \
               "<type 'int'> was specified."
    time.sleep(0.5)
    assert workspace_page.history.endswith(expected)
    editor.close()

    # Attempt to save file with syntax error.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    editor_page.new_file('bug.py', """
from openmdao.main.api import Component
class Bug(Component):
def execute(self)
    pass
""", check=False)

    # We expect 2 notifiers: save successful and file error.
    # These will likely overlap in a manner that 'Ok' is found but
    # later is hidden by the second notifier.
    try:
        message = NotifierPage.wait(editor_page, base_id='file-error')
    except WebDriverException as exc:
        err = str(exc)
        if 'Element is not clickable' in err:
            NotifierPage.wait(editor_page)
            message = NotifierPage.wait(editor_page)
    else:
        NotifierPage.wait(editor_page)
    eq(message, 'Error in file bug.py: invalid syntax (bug.py, line 6)')

    browser.close()
    browser.switch_to_window(workspace_window)

    # Load file with instantiation error.
    workspace_window = browser.current_window_handle
    if broken_chrome():
        raise SkipTest('Test broken for chrome/selenium combination')
    editor_page = workspace_page.open_editor()
    editor_page.new_file('bug2.py', """
from openmdao.main.api import Component
class Bug2(Component):
def __init__(self):
raise RuntimeError("__init__ failed")
""")
    browser.close()
    browser.switch_to_window(workspace_window)

    workspace_page.add_library_item_to_dataflow('bug2.Bug2', 'bug', check=False)
    expected = "NameError: unable to create object of type 'bug2.Bug2': __init__ failed"
    assert workspace_page.history.endswith(expected)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_driver_config(browser):
    project_dict, workspace_page = startup(browser)

    # Add MetaModel so we can test events.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    args = ["('x', )", "('y', )"]
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.metamodel.MetaModel', 'mm', args=args)

    # Replace default driver with CONMIN and edit.
    workspace_page.replace_driver('top', 'CONMINdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-100, -40)  # Make viewable on small screen.

    # Add a (nonsense) named parameter.
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'mm.force_execute'
    dialog.low = '0'
    dialog.high = '1'
    dialog.name = 'nonsense'
    dialog('ok').click()
    parameters = editor.get_parameters()
    expected = [['', 'mm.force_execute', '0', '1', '1', '0', '', 'nonsense']]
    eq(len(parameters.value), len(expected))
    for i, row in enumerate(parameters.value):
        eq(row, expected[i])

    # Delete the parameter
    delbutton = editor('parameters').find_elements_by_css_selector('.ui-icon-trash')
    delbutton[0].click()
    parameters = editor.get_parameters()
    expected = []
    browser.implicitly_wait(1)  # Not expecting to find anything.
    try:
        for i, row in enumerate(parameters.value):
            eq(row, expected[i])
    finally:
        browser.implicitly_wait(TMO)

    # Add a (nonsense) named objective.
    editor('objectives_tab').click()
    dialog = editor.new_objective()
    dialog.expr = 'mm.force_execute'
    dialog.name = 'nonsense'
    dialog('ok').click()
    objectives = editor.get_objectives()
    expected = [['', 'mm.force_execute', 'nonsense']]
    eq(len(objectives.value), len(expected))
    for i, row in enumerate(objectives.value):
        eq(row, expected[i])

    # Delete the objective
    delbutton = editor('objectives').find_elements_by_css_selector('.ui-icon-trash')
    delbutton[0].click()
    objectives = editor.get_objectives()
    expected = []
    browser.implicitly_wait(1)  # Not expecting to find anything.
    try:
        for i, row in enumerate(objectives.value):
            eq(row, expected[i])
    finally:
        browser.implicitly_wait(TMO)

    # Add a (nonsense) named constraint.
    editor('constraints_tab').click()
    dialog = editor.new_constraint()
    dialog.expr = 'mm.force_execute > 0'
    dialog.name = 'nonsense'
    dialog('ok').click()
    constraints = editor.get_constraints()
    expected = [['', 'mm.force_execute > 0', 'nonsense']]
    eq(len(constraints.value), len(expected))
    for i, row in enumerate(constraints.value):
        eq(row, expected[i])

    # Delete the constraint
    delbutton = editor('constraints').find_elements_by_css_selector('.ui-icon-trash')
    delbutton[0].click()
    constraints = editor.get_constraints()
    expected = []
    browser.implicitly_wait(1)  # Not expecting to find anything.
    try:
        for i, row in enumerate(constraints.value):
            eq(row, expected[i])
    finally:
        browser.implicitly_wait(TMO)

    # Clean up.
    editor.close()
    closeout(project_dict, workspace_page)


def _test_remove(browser):
    project_dict, workspace_page = startup(browser)

    # Show assembly information.
    top = workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.select_object('top')
    workspace_page.show_dataflow('top')
    workspace_page.hide_left()

    # open various views on the top assembly
    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False)
    editor.move(100, 200)
    connections = top.connections_page()
    properties = top.properties_page()

    eq(editor.is_visible, True)
    eq(connections.is_visible, True)
    eq(properties.is_visible, True)

    # Remove component.
    top.remove()

    # make sure all the views on the top assembly go away
    time.sleep(1)
    eq(editor.is_visible, False)
    eq(connections.is_visible, False)
    eq(properties.is_visible, False)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_noslots(browser):
    project_dict, workspace_page = startup(browser)

    # Add ExternalCode to assembly.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    ext = workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.external_code.ExternalCode', 'ext',
        prefix='top')

    # Display editor and check that no 'Slots' tab exists.
    editor = ext.editor_page(double_click=False)
    eq(editor('inputs_tab').is_visible, True)  # This waits.
    eq(editor('inputs_tab').is_present, True)  # These are quick tests.
    eq(editor('slots_tab').is_present, False)
    eq(editor('outputs_tab').is_present, True)

    # Clean up.
    editor.close()
    closeout(project_dict, workspace_page)


def _test_logviewer(browser):
    # Verify log viewer functionality.
    # Note that by default the logging level is set to WARNING.
    project_dict, workspace_page = startup(browser)
    viewer = workspace_page.show_log()
    viewer.move(0, -200)  # Sometimes get a lot of 'send event' messages...

    # Incremental display.
    workspace_page.do_command("import logging")
    workspace_page.do_command("logging.error('1 Hello World')")
    msgs = viewer.get_messages()
    while "Shouldn't have handled a send event" in msgs[-1]:
        msgs = msgs[:-1]
    eq(msgs[-1][-13:], '1 Hello World')

    # Exercise pausing the display. Since there's room on-screen,
    # the lack of scrollbar update isn't noticable.
    text = viewer.pause()
    eq(text, 'Pause')
    for i in range(2, 4):
        workspace_page.do_command("logging.error('%d Hello World')" % i)
    text = viewer.pause()  # Toggle-back.
    eq(text, 'Resume')

    # Clear display.
    viewer.clear()
    msgs = viewer.get_messages()
    eq(msgs, [''])

    # move log viewer away from file tree pane
    viewer.move(300, 0)

    # Exercise filtering.
    logger = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                             'files/logger.py')
    workspace_page.add_file(logger)
    msgs = viewer.get_messages()
    # Remove any spurious messages and drop timestamp.
    initial = [msg[16:] for msg in msgs
                        if "Shouldn't have handled a send event" not in msg and
                           'Connection reset by peer' not in msg]
    eq(initial,
       [u'W root: warning 1',
        u'E root: error 1',
        u'C root: critical 1',
        u'W root: warning 2',
        u'E root: error 2',
        u'C root: critical 2',
        u'W root: warning 3',
        u'E root: error 3',
        u'C root: critical 3'])

    # Turn off errors.
    dialog = viewer.filter()
    dialog('error_button').click()
    dialog('ok_button').click()

    msgs = viewer.get_messages()
    # Remove any spurious messages and drop timestamp.
    filtered = [msg[16:] for msg in msgs
                         if 'Connection reset by peer' not in msg]
    eq(filtered,
       [u'W root: warning 1',
        u'C root: critical 1',
        u'W root: warning 2',
        u'C root: critical 2',
        u'W root: warning 3',
        u'C root: critical 3'])

    # Pop-out to separate window.
    workspace_window = browser.current_window_handle
    viewer.popout()
    time.sleep(1)
    for handle in browser.window_handles:
        if handle != workspace_window:
            browser.switch_to_window(handle)
            browser.close()
            break
    browser.switch_to_window(workspace_window)

    # Verify that viewer was closed.
    try:
        viewer.get_messages()
    except StaleElementReferenceException:
        pass
    else:
        raise RuntimeError('Expected StaleElementReferenceException')

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_libsearch(browser):
    # Verify library search functionality.
    project_dict, workspace_page = startup(browser)

    # Get default objects.
    def_objects = workspace_page.get_object_types()
    def_searches = workspace_page.get_library_searches()

    # Get 'doe' search results.
    workspace_page.set_library_filter('doe')
    objects = workspace_page.get_object_types()
    eq(objects, [
        'CentralComposite',
        'CSVFile',
        'DOEdriver',
        'FullFactorial',
        'NeighborhoodDOEdriver',
        'OptLatinHypercube',
        'PlugNozzleGeometry',
        'Uniform'])
    doe_searches = workspace_page.get_library_searches()
    eq(doe_searches, def_searches + ['doe'])

    # Clear search, now back to default objects.
    workspace_page.clear_library_filter()
    objects = workspace_page.get_object_types()
    eq(objects, def_objects)

    # Get 'xyzzy' search results.
    workspace_page.set_library_filter('xyzzy')
    objects = workspace_page.get_object_types()
    eq(objects, ['No matching records found'])
    searches = workspace_page.get_library_searches()
    eq(searches, doe_searches)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_casefilters(browser):
    # Verify that CaseFilter objects are listed in the library.
    project_dict, workspace_page = startup(browser)

    for classname in ('ExprCaseFilter', 'IteratorCaseFilter',
                      'SequenceCaseFilter', 'SliceCaseFilter'):
        workspace_page.find_library_button(classname)

    closeout(project_dict, workspace_page)


def _test_sorting(browser):
    # Check that inputs and outputs are sorted alphanumerically.
    project_dict, workspace_page = startup(browser)

    path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                           'files/sorting_test.py')
    workspace_page.add_file(path)

    workspace_page.add_library_item_to_dataflow(
        'openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'sorting_test.SortingComp', 'comp')
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    editor = comp.editor_page()

    # Check order of inputs.
    inputs = editor.get_inputs()
    expected = [
        ['', 'stress_i1', '0', '', ''],
        ['', 'stress_i2', '0', '', ''],
        ['', 'stress_i10', '0', '', ''],
        ['', 'directory',  '',  '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
        ['', 'force_fd', 'False', '',
         'If True, always finite difference this component.'],
        ['', 'missing_deriv_policy', 'error', '',
         'Determines behavior when some analytical derivatives are provided'
         ' but some are missing']
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # Check order of outputs.
    outputs = editor.get_outputs()
    expected = [
        ['', 'stress_o1', '0', '', ''],
        ['', 'stress_o2', '0', '', ''],
        ['', 'stress_o10', '0', '', ''],
        ['', 'derivative_exec_count', '0', '',
         "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count', '0', '',
         'Number of times this Component has been executed.'],
        ['', 'itername', '', '', 'Iteration coordinates.'],
    ]

    for i, row in enumerate(outputs.value):
        eq(row, expected[i])

    editor.close()
    closeout(project_dict, workspace_page)


def _test_standard_library(browser):
    project_dict, workspace_page = startup(browser)
    workspace_page.set_library_filter('optproblems')
    objects = workspace_page.get_object_types()

    eq(objects, [
        'BraninProblem',
        'PolyScalableProblem',
        'SellarProblem',])

    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
