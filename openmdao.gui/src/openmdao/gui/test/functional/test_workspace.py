"""
Tests of overall workspace functions.
"""

import sys
import time
import logging

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     startup, closeout
    from pageobjects.util import NotifierPage
    from pageobjects.workspace import WorkspacePage

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_console(browser):
    # Check basic console functionality.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    workspace_page.do_command("print 'blah'")
    expected = ">>> print 'blah'\nblah"
    eq(workspace_page.history, expected)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_palette_update(browser):
    # Import some files and add components from them.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()
    time.sleep(0.5)  # Just so we can see it.

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

    workspace_page.save_project()
    projects_page = workspace_page.close_workspace()

    # Now try to re-open that project to see if items are still there.
    project_info_page = projects_page.edit_project(project_dict['name'])
    workspace_page = project_info_page.load_project()

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

    # Now modify the parabola.py file and save the project again.  Pickling will fail
    # and we'll fall back to using the saved macro

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_menu(browser):
    # Just click on various main menu buttons.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Project-Run.
    workspace_page.run()
    expected = 'Executing...\nExecution complete.'
    eq(workspace_page.history, expected)
    top_figure = workspace_page.get_dataflow_figure('top')
    eq(top_figure.border, '1px solid rgb(0, 255, 0)')

    #FIXME: These need to verify that the request has been performed.
    # View menu.
    for item in ('console', 'library', 'objects', 'files',
                 'properties', 'workflow', 'dataflow', 'refresh'):
        workspace_page('view_menu').click()
        workspace_page('%s_button' % item).click()
        time.sleep(0.5)  # Just so we can see it.

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_macro(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Create a file (code editor automatically indents).
    editor_page.new_file('foo.py', """
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class Foo(Component):

a = Float(0.0, iotype='in')
b = Float(0.0, iotype='out')
""")
    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)
    
    # Add some Foo instances.
    workspace_page.show_dataflow('top')
    workspace_page.set_library_filter('In Project')
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp1')
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp2')

    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    conn_page = workspace_page.connect(comp1, comp2)
    conn_page.connect_vars('comp1.b', 'comp2.a')
    conn_page.close()

    workspace_page.save_project()

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

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_addfiles(browser):
    # Adds multiple files to the project.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Get path to  paraboloid file.
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')

    # Get path to optimization_unconstrained file.
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')

    # Add the files
    # would like to test adding multiple files but Selenium doesn't support it
    #workspace_page.add_files(paraboloidPath, optPath)
    workspace_page.add_file(paraboloidPath)
    workspace_page.add_file(optPath)

    # Check to make sure the files were added.
    time.sleep(0.5)
    file_names = workspace_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_properties(browser):
    # Checks right-hand side properties display.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Check default 'top'.
    workspace_page.show_properties()
    workspace_page.select_object('top')
    workspace_page.show_properties()
    time.sleep(0.5)
    eq(workspace_page.props_header, 'Assembly: top')
    inputs = workspace_page.props_inputs
    eq(inputs.value, [['directory',     ''],
                      ['force_execute', 'False']])

    # Check default 'top.driver'.
    workspace_page.expand_object('top')
    workspace_page.select_object('top.driver')
    time.sleep(0.5)
    eq(workspace_page.props_header, 'Run_Once: top.driver')
    inputs = workspace_page.props_inputs
    eq(inputs.value, [['directory',     ''],
                      ['force_execute', 'True'],
                      ['printvars',     '']]) # FIXME: printvars is really an empty list...
    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_objtree(browser):
    # Toggles maxmimize/minimize button on assemblies.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add maxmin.py to project
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'maxmin.py')
    workspace_page.add_file(file_path)

    # Add MaxMin to 'top'.
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
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_editable_inputs(browser):
    def test_color(actual, expected, alpha=False):
        if(alpha):
            eq(actual, expected)
        else:
            eq(actual[0:3], expected[0:3])

    def test_inputs(inputs):
        for i, row in enumerate(inputs):
            connected_to_cell = row.cells[len(row.cells)-2]
            implicit_cell = row.cells[len(row.cells)-1]
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
            connected_to_cell = row.cells[len(row.cells)-2]
            implicit_cell = row.cells[len(row.cells)-1]
            name_cell = row.cells[0]
            value_cell = row.cells[2]

            if implicit_cell.value:
                test_color(name_cell.color, [100, 180, 255, 1])
                test_color(value_cell.color, [100, 180, 255, 1])
            else:
                test_color(name_cell.color, [255, 255, 255, 1])
                test_color(value_cell.color, [255, 255, 255, 1])
            
            test_color(value_cell.background_color, [0, 0, 0, 1])

    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Import vehicle_singlesim
    file_path_one = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                    'basic_model.py')
    file_path_two = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                    'vehicle_singlesim.py')
    workspace_page.add_file(file_path_one)
    workspace_page.add_file(file_path_two)

    # Replace 'top' with Vehicle ThreeSim  top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
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
    workspace_page.show_dataflow(assembly_name+ ".vehicle")
    transmission = workspace_page.get_dataflow_figure('transmission',
                                                      assembly_name + '.vehicle')
   
    #Test highlighting for explicit connections
    component_editor = transmission.editor_page()
    test_inputs(component_editor.get_inputs()) 
    test_outputs(component_editor.get_outputs())

    component_editor.close()

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_console_errors(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Set input to illegal value.
    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False, base_type='Assembly')
    inputs = editor.get_inputs()
    inputs[0][2] = '42'  # directory
    message = NotifierPage.wait(editor)
    eq(message, "TraitError: The 'directory' trait of an Assembly instance"
                " must be a string, but a value of 42 <type 'int'> was"
                " specified.")
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

    # The error notifier can potentially arrive *before* the save notifier,
    # resulting in the error notifier being underneath and causing a
    # WebDriverException.  If that happens, try to handle the save and
    # then retry the error notifier.
    message = None
    try:
        message = NotifierPage.wait(editor_page, base_id='file-error')
    except Exception as exc:
        print 'Exception waiting for file-error:', str(exc) or repr(exc)
        logging.exception('Waiting for file-error')
    if message is None:
        message = NotifierPage.wait(editor_page, base_id='file-error')
    eq(message, 'invalid syntax (bug.py, line 6)')

    browser.close()
    browser.switch_to_window(workspace_window)

    # Load file with instantiation error.
    workspace_window = browser.current_window_handle
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
    message = NotifierPage.wait(workspace_page)
    eq(message, "NameError: unable to create object of type 'bug2.Bug2': __init__ failed")

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_driver_config(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Replace default driver with CONMIN and edit.
    workspace_page.replace('driver',
                           'openmdao.lib.drivers.conmindriver.CONMINdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')

    # Add a (nonsense) named parameter.
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'driver.force_execute'
    dialog.low = '0'
    dialog.high = '1'
    dialog.name = 'nonsense'
    dialog('ok').click()
    parameters = editor.get_parameters()
    expected = [['driver.force_execute', '0', '1', '', '', '', 'nonsense']]
    for i, row in enumerate(parameters.value):
        eq(row, expected[i])

    # Add a (nonsense) named objective.
    editor('objectives_tab').click()
    dialog = editor.new_objective()
    dialog.expr = 'driver.force_execute'
    dialog.name = 'nonsense'
    dialog('ok').click()
    objectives = editor.get_objectives()
    expected = [['driver.force_execute', 'nonsense']]
    for i, row in enumerate(objectives.value):
        eq(row, expected[i])

    # Add a (nonsense) named constraint.
    editor('constraints_tab').click()
    dialog = editor.new_constraint()
    dialog.expr = 'driver.force_execute > 0'
    dialog.name = 'nonsense'
    dialog('ok').click()
    constraints = editor.get_constraints()
    expected = [['driver.force_execute > 0', '1', '0', 'nonsense']]
    for i, row in enumerate(constraints.value):
        eq(row, expected[i])

    # Clean up.
    editor.close()
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_remove(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Show assembly information.
    workspace_page.select_object('top')
    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False)
    editor.move(-100, 100)  # Move it away from context menu.
    connections = top.connections_page()
    properties = top.properties_page()

    eq(editor.is_visible, True)
    eq(connections.is_visible, True)
    eq(properties.is_visible, True)

    # Remove component.
    top.remove()

    time.sleep(0.5)
    eq(editor.is_visible, False)
    eq(connections.is_visible, False)
    eq(properties.is_visible, False)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_noslots(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add ExternalCode to assembly.
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
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_savechanges(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add ExternalCode to assembly.
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.external_code.ExternalCode', 'ext')
    
    #first try to close without saving changes, but click CANCEL and stay 
    workspace_page.attempt_to_close_workspace(True, False)

    # add another object to the model to be sure it didn't close
    eq(len(workspace_page.get_dataflow_figures()), 3)
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.external_code.ExternalCode', 'ext2')
    eq(len(workspace_page.get_dataflow_figures()), 4)
    
    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_dontsavechanges(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add ExternalCode to assembly.
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.external_code.ExternalCode', 'ext')
    
    #Try to close without saving changes, but click OK and leave. 
    workspace_page.attempt_to_close_workspace(True, True)

    # Clean up.
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_dontsavechanges complete."


if __name__ == '__main__':
    main()

