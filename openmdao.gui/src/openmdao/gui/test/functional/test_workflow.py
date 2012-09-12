"""
Tests of workflow functions.
"""

import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup


if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_global(browser):
    print "running _test_global"
    # 'global' view should be populated (originally was blank).
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    workspace_window = browser.current_window_handle
    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'rosen_suzuki.py')
    workspace_page.add_file(filename)

    # Add a NestedSimulation.
    workspace_page.show_library()
    workspace_page.set_library_filter('In Project')
    time.sleep(0.5)
    workspace_page.find_library_button('NestedSimulation', 0.5).click()
    workspace_page.add_library_item_to_dataflow('rosen_suzuki.NestedSimulation',
                                                'nested', offset=(300, 300))
    # Verify full workflow shown.
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_figures()), 3)
    eq(len(workspace_page.get_workflow_component_figures()), 6)

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_global complete."


if __name__ == '__main__':
    main()

