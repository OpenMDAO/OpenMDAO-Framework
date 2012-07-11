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
    from util import setup_server, teardown_server, generate, begin, new_project

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

    workspace_page.do_command('dir()')
    expected = ">>> dir()\n['__builtins__', 'path', 'top']"
    eq(workspace_page.history, expected)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_console complete."


def _test_import(browser):
    print "running _test_import..."
    # Import some files and add components from them.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()
    time.sleep(0.5)  # Just so we can see it.

    # View dataflow.
    workspace_page('dataflow_tab').click()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Add paraboloid file.
    file_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                'paraboloid.py')
    editor_page.add_file(file_path)

    # Add optimization_unconstrained file.
    file_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                'optimization_unconstrained.py')
    editor_page.add_file(file_path)

    time.sleep(1.0)

    # Check to make sure the files were added.
    file_names = editor_page.get_files()
    expected_file_names = ['optimization_unconstrained.py', 'paraboloid.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Go into Libraries/working section.
    workspace_page('libraries_tab').click()
    time.sleep(1)
    workspace_page.find_palette_button('Paraboloid').click()

    # Make sure there are only two dataflow figures (top & driver)
    workspace_page.show_dataflow('top')
    time.sleep(1)
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
    editor_page('files_tab').click()
    file_names = editor_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_import complete."


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
    for item in ('console', 'libraries', 'objects',
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

class Plane(Component):

    x1 = Float(0.0, iotype='in')
# subsequent lines will be auto-indented by ace editor
x2 = Float(0.0, iotype='in')
x3 = Float(0.0, iotype='in')

f_x = Float(0.0, iotype='out')
""")

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Drag over Plane.
    workspace_page.show_dataflow('top')
    workspace_page('libraries_tab').click()
    workspace_page.libraries_search = 'In Project\n'
    time.sleep(2)
    workspace_page.find_palette_button('Plane').click()
    workspace_page.add_library_item_to_dataflow('plane.Plane', 'plane')

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_newfile complete."


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

    upload_page = editor_page.add_files()

    # Get path to  paraboloid file.
    paraboloidPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                                     'paraboloid.py')

    # Get path to optimization_unconstrained file.
    optPath = pkg_resources.resource_filename('openmdao.examples.simple',
                                              'optimization_unconstrained.py')

    # Add the files
    upload_page.select_files((paraboloidPath, optPath))
    upload_page.upload_files()

    time.sleep(1.0)

    # Check to make sure the files were added.
    browser.switch_to_window(editor_window)
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

if __name__ == '__main__':
    if '--nonose' in sys.argv:
        # Run outside of nose.
        # tests should be in alpha order as that's how they will run under nose
        from util import setup_chrome  # , setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_addfiles(browser)
        _test_console(browser)
        _test_import(browser)
        _test_menu(browser)
        _test_newfile(browser)
        _test_properties(browser)
        browser.quit()
        teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
