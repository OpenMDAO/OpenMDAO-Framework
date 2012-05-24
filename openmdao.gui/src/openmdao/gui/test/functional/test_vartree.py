import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from unittest import TestCase


if sys.platform != 'win32':  # No testing on Windows yet.
    from pageobjects.workspace import NotifierPage
    from util import setup_server, teardown_server, generate, begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_display(browser):
    # Show that displaying a VarTree doesn't generate errors regarding __name__.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Import vtree.py.
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'vtree.py')
    editor_page.add_file(file_path)
    editor_page.import_file('vtree.py')

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)
# FIXME: refresh shouldn't be required.
    workspace_page('view_menu').click()
    workspace_page('refresh_button').click()

    # View 'top' assembly dataflow.
    workspace_page.show_dataflow('top')

    # Drag components into workspace.
    workspace_page('libraries_tab').click()
    workspace_page('working_section').click()
    workspace_page.add_library_item_to_dataflow('VTComp', 'vt1')

    workspace_page('libraries_tab').click()
    workspace_page('working_section').click()
    workspace_page.add_library_item_to_dataflow('VTComp', 'vt2')

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()


if __name__ == '__main__':
    if True:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
    else:
        # Run outside of nose.
        from util import setup_chrome, setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_display(browser)
        teardown_server()

