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
    from selenium.common.exceptions import WebDriverException
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project
    from pageobjects.util import NotifierPage
    from pageobjects.workspace import WorkspacePage

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_console(browser):
    print "running _test_console..."
    # Check basic console functionality.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    workspace_page.do_command("print 'blah'")
    expected = ">>> print 'blah'\nblah"
    eq(workspace_page.history, expected)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_console complete."


def _test_editfile(browser):
    print "running _test_editfile..."
    # Check ability to open code editor by double clicking on file in workspace.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # create a couple of files
    file1 = 'test1.py'
    workspace_page.new_file(file1)
    file2 = 'test2.py'
    workspace_page.new_file(file2)

    # verify file is opened in code editor by double clicking
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), '/'+file1)

    # verify different file is opened in code editor by double clicking
    browser.switch_to_window(workspace_window)
    editor_page = workspace_page.edit_file(file2)
    eq(str(editor_page.get_tab_label()), '/'+file2)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # verify code editor can be re-opened by double clicking on file
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), '/'+file1)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_editfile complete."


def _test_palette_update(browser):
    print "running _test_palette_update..."
    # Import some files and add components from them.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

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

    # view library
    workspace_page.show_library()

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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_palette_update complete."


def _test_menu(browser):
    print "running _test_menu..."
    # Just click on various main menu buttons.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_menu complete."


def _test_newfile(browser):
    print "running _test_newfile..."
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # test the 'ok' and 'cancel' buttons on the new file dialog
    dlg = editor_page.new_file_dialog()
    dlg.set_text('ok_file1')
    dlg.click_ok()
    NotifierPage.wait(editor_page)

    dlg = editor_page.new_file_dialog()
    dlg.set_text('cancel_file')
    dlg.click_cancel()

    dlg = editor_page.new_file_dialog()
    dlg.set_text('ok_file2')
    dlg.click_ok()
    NotifierPage.wait(editor_page)

    file_names = editor_page.get_files()
    expected_file_names = ['ok_file1', 'ok_file2']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Create the file (code editor automatically indents).
    editor_page.new_file('plane.py', """
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

# lines will be auto-indented by ace editor
class Plane(Component):

x1 = Float(0.0, iotype='in')
x2 = Float(0.0, iotype='in')
x3 = Float(0.0, iotype='in')

f_x = Float(0.0, iotype='out')
""")

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Drag over Plane.
    time.sleep(0.5)
    workspace_page.show_dataflow('top')
    workspace_page.show_library()
    workspace_page.set_library_filter('In Project')
    time.sleep(2)
    workspace_page.find_library_button('Plane', 0.5).click()
    workspace_page.add_library_item_to_dataflow('plane.Plane', 'plane')

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_newfile complete."


## for now I'm giving up on this test.  If the save & reload option is chosen,
## the browser attribute becomes stale when the project gets reloaded and I'm not 
## sure how to get back to a 'good' browser handle after that.  begin() fails when using the
## stale handle.  Also, cleaning up at the end of the test is a problem when you can't
## get access to the WebElements in the current page.
## -- this has been turned into a manual test (see gui/test/functional/manual/save_and_reload
#def _test_macro(browser):
    #print "running _test_macro..."
    ## Creates a file in the GUI.
    #projects_page = begin(browser)
    #project_info_page, project_dict = new_project(projects_page.new_project())
    #workspace_page = project_info_page.load_project()

    ## Open code editor.
    #workspace_window = browser.current_window_handle
    #editor_page = workspace_page.open_editor()

    ## Create a file (code editor automatically indents).
    #editor_page.new_file('foo.py', """
#from openmdao.main.api import Component
#from openmdao.lib.datatypes.api import Float

## lines will be auto-indented by ace editor
#class Foo(Component):

#a = Float(0.0, iotype='in')
#b = Float(0.0, iotype='in')
#c = Float(0.0, iotype='out')
#d = Float(0.0, iotype='out')

#""")
    #time.sleep(1)
    ## Back to workspace.
    #browser.close()
    #browser.switch_to_window(workspace_window)

    #port = workspace_page.port
    
    ## Drag over Plane.
    #workspace_page.show_dataflow('top')
    #workspace_page.show_library()
    #workspace_page.set_library_filter('In Project')

    #workspace_page.find_library_button('Foo', 0.5).click()
    #workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp1')
    #workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp2')

    #comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    #comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    #conn_page = workspace_page.connect(comp1, comp2)
    #conn_page.connect_vars('comp1.c', 'comp2.a')
    #time.sleep(1)  # Wait for display update.
    #conn_page.close()

    #workspace_page.save_project()

    #editor_page = workspace_page.open_editor()
    #editor_page.edit_file('foo.py', dclick=False)
    #editor_page.add_text_to_file('#just a comment\n')
    
    #editor_page.save_document(overwrite=True, check=False) # forces a save and reload of project
    #time.sleep(3)

    #workspace_page =  WorkspacePage.verify(browser, port)
    
    #workspace_page.show_dataflow('top')
    #time.sleep(0.5)
    #eq(sorted(workspace_page.get_dataflow_component_names()),
       #['comp1', 'comp2', 'driver', 'top'])

    ## Clean up.
    #projects_page = workspace_page.close_workspace()
    #project_info_page = projects_page.edit_project(project_dict['name'])
    #project_info_page.delete_project()
    #print "_test_macro complete."


def _test_addfiles(browser):
    print "running _test_addfiles..."
    # Adds multiple files to the project.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Opens code editor
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Get path to  paraboloid file.
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')

    # Get path to optimization_unconstrained file.
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')

    # Add the files
    # would like to test adding multiple files but Selenium doesn't support it
    #editor_page.add_files(paraboloidPath, optPath)
    editor_page.add_file(paraboloidPath)
    editor_page.add_file(optPath)

    # Check to make sure the files were added.
    time.sleep(1)
    file_names = editor_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    browser.switch_to_window(workspace_window)
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_addfiles complete."


def _test_properties(browser):
    print "running _test_properties..."
    # Checks right-hand side properties display.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Check default 'top'.
    workspace_page.show_properties()
    workspace_page.select_object('top')
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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_properties complete."


def _test_objtree(browser):
    print "running _test_objtree..."
    # Toggles maxmimize/minimize button on assemblies.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Add maxmin.py to project
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'maxmin.py')
    editor_page.add_file(file_path)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Add MaxMin to 'top'.
    workspace_page.show_dataflow('top')
    time.sleep(1)
    workspace_page.show_library()
    time.sleep(1)
    workspace_page.find_library_button('MaxMin', 0.5).click()
    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin')

    # Maximize 'top' and 'top.maxmin'
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top'])
    workspace_page.expand_object('top')
    time.sleep(1)
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin'])
    workspace_page.expand_object('top.maxmin')
    time.sleep(1)
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub'])

    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin2')
    time.sleep(1)
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub', 'top.maxmin2'])

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_objtree complete."


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

    print "running _test_editable_inputs..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Import vehicle_singlesim
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path_one = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                    'basic_model.py')
    file_path_two = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                    'vehicle_singlesim.py')
    editor_page.add_file(file_path_one)
    editor_page.add_file(file_path_two)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Replace 'top' with Vehicle ThreeSim  top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.show_library()
    workspace_page.find_library_button('Basic_Model', 0.5).click()
    assembly_name = "sim"
    workspace_page.add_library_item_to_dataflow('basic_model.Basic_Model',
            assembly_name)

    paraboloid = workspace_page.get_dataflow_figure('paraboloid',
            assembly_name)

    #Test highlighting for implicit connections
    component_editor = paraboloid.editor_page()
    test_inputs(component_editor.get_inputs()) 
    test_outputs(component_editor.get_outputs())
    
    component_editor.close()

    #Remove sim from the dataflow
    assembly = workspace_page.get_dataflow_figure(assembly_name)
    assembly.remove()

    #Add VehicleSim to the dataflow
    workspace_page.show_library()
    workspace_page.find_library_button('VehicleSim', 0.5).click()
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
                        assembly_name)
    
    # Get component editor for transmission.
    workspace_page.expand_object(assembly_name)
    workspace_page.show_dataflow(assembly_name+ ".vehicle")
    transmission = workspace_page.get_dataflow_figure('transmission',
                        assembly_name + '.vehicle')# Get component editor for transmission.
   
    #Test highlighting for explicit connections
    component_editor = transmission.editor_page()
    test_inputs(component_editor.get_inputs()) 
    test_outputs(component_editor.get_outputs())

    component_editor.close()

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_editable_inputs complete."


def _test_console_errors(browser):
    print "running _test_console_errors..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

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
    editor_window = browser.current_window_handle
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
    workspace_page.show_library()
    time.sleep(0.5)
    workspace_page.find_library_button('Bug2', 0.5).click()
    workspace_page.add_library_item_to_dataflow('bug2.Bug2', 'bug', check=False)
    message = NotifierPage.wait(workspace_page)
    eq(message, "NameError: unable to create object of type 'bug2.Bug2': __init__ failed")

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_console_errors complete."


def _test_driver_config(browser):
    print "running _test_driver_config..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Replace default driver with CONMIN and edit.
    workspace_page.show_library()
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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_driver_config complete."


def _test_remove(browser):
    print "running _test_driver_config..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_remove complete."


def _test_noslots(browser):
    print "running _test_noslots..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Add ExternalCode to assembly.
    workspace_page.show_dataflow('top')
    time.sleep(0.5)
    workspace_page.show_library()
    time.sleep(0.5)
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.external_code.ExternalCode', 'ext')

    # Display editor and check that no 'Slots' tab exists.
    ext = workspace_page.get_dataflow_figure('ext', 'top')
    editor = ext.editor_page(double_click=False)
    eq(editor('inputs_tab').is_visible, True)  # This waits.
    eq(editor('inputs_tab').is_present, True)  # These are quick tests.
    eq(editor('slots_tab').is_present, False)
    eq(editor('outputs_tab').is_present, True)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_noslots complete."

def _test_savechanges(browser):
    print "running _test_savechanges..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Add ExternalCode to assembly.
    workspace_page.show_dataflow('top')
    time.sleep(0.5)
    workspace_page.show_library()
    workspace_page.set_library_filter('ExternalCode')
    time.sleep(0.5)
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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_savechanges complete."

def _test_dontsavechanges(browser):
    print "running _test_savechanges..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Add ExternalCode to assembly.
    workspace_page.show_dataflow('top')
    time.sleep(0.5)
    workspace_page.show_library()
    time.sleep(0.5)
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

