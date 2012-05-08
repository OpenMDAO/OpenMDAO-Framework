import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase

from pageobjects.workspace import NotifierPage

from util import setup_server, teardown_server, generate, begin, new_project


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_console(browser):
    # Check basic console functionality.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    workspace_page.do_command('dir()')
    expected = ">>> dir()\n['__builtins__', 'path', 'top']"
    eq( workspace_page.history, expected )

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()


def _test_import(browser):
    # Import some files and add components from them.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # View the Code Editor.
    workspace_page('code_tab').click()
    time.sleep(0.5)  # Just so we can see it.

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()
    time.sleep(0.5)  # Just so we can see it.

    # View structure.
    workspace_page('structure_tab').click()

    # Add paraboloid file.
    import openmdao.examples.simple.paraboloid
    file_path = openmdao.examples.simple.paraboloid.__file__
    workspace_page.add_file(file_path)

    # Add optimization_unconstrained file.
    import openmdao.examples.simple.optimization_unconstrained
    file_path = openmdao.examples.simple.optimization_unconstrained.__file__
    workspace_page.add_file(file_path)

    # Refresh to update file tree.
    workspace_page.view_refresh()

    # Check to make sure the files were added.
    file_names = workspace_page.get_files()
    expected_file_names = ['paraboloid.py', 'optimization_unconstrained.py']
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Import * from paraboloid.
    workspace_page.import_file('paraboloid.py')

    # Import * from optimization_unconstrained.
    workspace_page.import_file('optimization_unconstrained.py')

    # Go into Libraries/working section.
    workspace_page('libraries_tab').click()
    workspace_page('working_section').click()

    # Make sure there is only one dataflow component.
    eq( len(workspace_page.get_dataflow_figures()), 1 )

    # Drag element into workspace.
    paraboloid_name = 'parab'
    workspace_page.add_library_item_to_structure('Paraboloid', paraboloid_name)

    # Now there should be two.
    eq( len(workspace_page.get_dataflow_figures()), 2 )

    # Make sure the item added is there with the name we gave it.
    component_names = workspace_page.get_dataflow_component_names()
    if paraboloid_name not in component_names :
        raise TestCase.failureException(
            "Expected component name, '%s', to be in list of existing"
            " component names, '%s'" % (paraboloid_name, component_names))

    workspace_page.save_project()
    projects_page = workspace_page.close_workspace()

    # Now try to re-open that project to see if items are still there.
    project_info_page = projects_page.edit_project(project_dict['name'])
    workspace_page = project_info_page.load_project()

    # Check to see that the added files are still there.
    workspace_page('files_tab').click()
    file_names = workspace_page.get_files()
    if sorted(file_names) != sorted(expected_file_names):
        raise TestCase.failureException(
            "Expected file names, '%s', should match existing file names, '%s'"
            % (expected_file_names, file_names))

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()


def _test_menu(browser):
    # Just click on various main menu buttons.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Project-Run.
    workspace_page.run()
    expected = 'Executing...\nExecution complete.'
    eq( workspace_page.history, expected )
    top_figure = workspace_page.get_dataflow_figures()[0]
#FIXME: halo seems to go away now...
    eq( top_figure.value_of_css_property('border'), '3px solid rgb(0, 255, 0)' )

#FIXME: These need to verify that the request has been performed.
    # View menu.
    for item in ('code', 'cmdline', 'console', 'files', 'libraries', 'objects',
                 'properties', 'workflow', 'structure'):
        workspace_page('view_menu').click()
        workspace_page('%s_button' % item).click()
        time.sleep(0.5)  # Just so we can see it.

    workspace_page('view_menu').click()
    workspace_page('refresh_button').click()
    msg = NotifierPage.wait(workspace_page.browser, workspace_page.port)
    eq( msg, 'Refresh complete' )

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()


def _test_newfile(browser):
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Create the file (code editor automatically indents).
    workspace_page.new_file('plane.py', """
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class Plane(Component):

x1 = Float(0.0, iotype='in')
x2 = Float(0.0, iotype='in')
x3 = Float(0.0, iotype='in')

f_x = Float(0.0, iotype='out')
""")

    # Import it.
    workspace_page.import_file('plane.py')

    # Drag over Plane.
    workspace_page.show_structure('top')
    workspace_page('libraries_tab').click()
    workspace_page('working_section').click()
    workspace_page.add_library_item_to_structure('Plane', 'plane')

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()


if __name__ == '__main__':
    if True:
        # Run under nose.
        import sys
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
    else:
        # Run outside of nose.
        from util import setup_chrome, setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_console(browser)
        _test_import(browser)
        _test_menu(browser)
        _test_newfile(browser)
        teardown_server()

