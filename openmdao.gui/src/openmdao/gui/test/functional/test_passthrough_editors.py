"""
Tests of code editor functions.
"""

import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     startup, closeout

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_value_editors(browser):
    # Creates a file in the GUI.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Import variable_editor.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional','variable_editors.py')
    workspace_page.add_file(file_path)
    
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.add_library_item_to_dataflow('variable_editors.Topp',"top")
    
    time.sleep(10)
    top = workspace_page.get_dataflow_figure("top")
    top.context_click()
    time.sleep(1)
    top.find_by_xpath('//*[@id="-dataflow"]/div[11]/a[10]').click()
    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)
    

if __name__ == '__main__':
    main()

