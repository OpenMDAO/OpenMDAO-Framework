"""
Tests of overall workspace functions.
"""

import sys
import time

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

if sys.platform != 'win32':  # No testing on Windows yet.
    from selenium.common.exceptions import TimeoutException, WebDriverException
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project
    from pageobjects.util import NotifierPage

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def k(browser):
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
    dlg = workspace_page.new_file_dialog()
    dlg.set_text(file1)
    dlg.click_ok()
    time.sleep(0.5)
    file2 = 'test2.py'
    dlg = workspace_page.new_file_dialog()
    dlg.set_text(file2)
    dlg.click_ok()
    time.sleep(1.0)

    # verify file is opened in code editor by double clicking
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), file1)

    # verify different file is opened in code editor by double clicking
    browser.switch_to_window(workspace_window)
    editor_page = workspace_page.edit_file(file2)
    eq(str(editor_page.get_tab_label()), file2)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # verify code editor can be re-opened by double clicking on file
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), file1)

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
    workspace_page('files_tab').click()
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
    time.sleep(1)
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
    time.sleep(1.0)

    dlg = editor_page.new_file_dialog()
    dlg.set_text('cancel_file')
    dlg.click_cancel()
    time.sleep(1.0)

    dlg = editor_page.new_file_dialog()
    dlg.set_text('ok_file2')
    dlg.click_ok()
    time.sleep(1.0)

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
    workspace_page.library_search = 'In Project\n'
    time.sleep(2)
    workspace_page.find_library_button('Plane').click()
    workspace_page.add_library_item_to_dataflow('plane.Plane', 'plane')

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_newfile complete."


def _test_macro(browser):
    print "running _test_macro..."
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Create a file (code editor automatically indents).
    editor_page.new_file('foo.py', """
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

# lines will be auto-indented by ace editor
class Foo(Component):

a = Float(0.0, iotype='in')
b = Float(0.0, iotype='in')
c = Float(0.0, iotype='out')
d = Float(0.0, iotype='out')

""")
    time.sleep(1)
    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Drag over Plane.
    workspace_page.show_dataflow('top')
    workspace_page.show_library()
    workspace_page.library_search = 'In Project\n'

    workspace_page.find_library_button('Foo').click()
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp1')
    workspace_page.add_library_item_to_dataflow('foo.Foo', 'comp2')

    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    conn_page = workspace_page.connect(comp1, comp2)
    conn_page.connect_vars('comp1.c', 'comp2.a')
    time.sleep(1)  # Wait for display update.
    conn_page.close()

    workspace_page.save_project()

    editor_page = workspace_page.open_editor()
    editor_window = browser.current_window_handle
    editor_page.edit_file('foo.py', dclick=False)
    editor_page.add_text_to_file('#just a comment\n')
    editor_page.save_document(overwrite=True)

    browser.close()
    browser.switch_to_window(workspace_window)
    workspace_page.save_project()  # the pickle should fail here because an imported file has been modified

    time.sleep(3)
    projects_page = workspace_page.close_workspace()

    workspace_page = projects_page.open_project(project_dict['name'])
    workspace_page.show_dataflow('top')
    time.sleep(0.5)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['comp1', 'comp2', 'driver', 'top'])

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_macro complete."


def _test_addfiles(browser):
    print "running _test_addfiles..."
    # Adds multiple files to the project.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Opens code editor
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    editor_window = browser.current_window_handle

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
                      ['printvars',     '[]']])
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
    workspace_page.find_library_button('MaxMin').click()
    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin')

    # Maximize 'top' and 'top.maxmin'
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top'])
    workspace_page.expand_object('top')
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin'])
    workspace_page.expand_object('top.maxmin')
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub'])

    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin2')
    visible = workspace_page.get_objects_attribute('path', True)
    eq(visible, ['top', 'top.driver', 'top.maxmin',
                 'top.maxmin.driver', 'top.maxmin.sub', 'top.maxmin2'])

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_objtree complete."


def _test_editable_inputs(browser):
    print "running _test_editable_inputs..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Import vehicle_singlesim
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                'vehicle_singlesim.py')
    editor_page.add_file(file_path)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Replace 'top' with Vehicle ThreeSim  top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.show_library()
    workspace_page.find_library_button('VehicleSim').click()
    assembly_name = "sim"
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
            assembly_name)

    # Get component editor for transmission.
    workspace_page.expand_object(assembly_name)
    workspace_page.show_dataflow(assembly_name + ".vehicle")
    transmission = workspace_page.get_dataflow_figure('transmission',
            assembly_name + '.vehicle')

    component_editor = transmission.editor_page()

    # Find rows in inputs table
    # for transmission for single sim vehicle
    # that are editable.
    elements = component_editor.browser.find_elements_by_xpath(\
            "//div[@id='Inputs_props']")[1]
            #/div[@class='slick-viewport']")
            #/div[@id='grid-canvas']\
            #/div[@row='1'] | div[@row='3']")

    elements = elements.find_elements_by_xpath(\
            "div[@class='slick-viewport']\
            /div[@class='grid-canvas']\
            /div[@row='1' or @row='3']\
            /div[contains(@class, 'ui-state-editable')]")

    # Verify that the rows are highlighted
    
    for element in elements:
        assert("rgb(255, 255, 255)" == element.value_of_css_property("background-color"))
        assert("rgb(0, 0, 0)" == element.value_of_css_property("color"))
    
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
    editor = top.editor_page(double_click=False)
    inputs = editor.get_inputs()
    inputs[1][2] = '42'  # force_execute
    message = NotifierPage.wait(editor)
    eq(message, "TraitError: The 'force_execute' trait of an Assembly instance"
                " must be a boolean, but a value of 42 <type 'int'> was"
                " specified.")
    editor.close()

    # Save file with syntax error.
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
    except WebDriverException:
        pass
    NotifierPage.wait(editor_page)  # Save complete.
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
from nowhere import nothing
class Bug2(Component):
pass
""")
    browser.close()
    browser.switch_to_window(workspace_window)
    workspace_page.show_library()
    workspace_page.library_search = 'In Project\n'
    time.sleep(0.5)
    workspace_page.find_library_button('Bug2').click()
    workspace_page.add_library_item_to_dataflow('bug2.Bug2', 'bug', check=False)
    message = NotifierPage.wait(workspace_page)
    eq(message, "NameError: unable to create object of type 'bug2.Bug2'")

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_console_errors complete."


if __name__ == '__main__':
    main()

