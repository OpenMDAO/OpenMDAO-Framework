"""
Tests of code editor functions.
"""

import sys
import time

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup
from nose.tools import assert_not_equal as neq
from unittest import TestCase

if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_multitab(browser):
    print "running _test_multitab..."
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

    # Create the file (code editor automatically indents).
    test_code1 = """
def f(x):
    return math.sqrt(x)"""
    
    test_code2 = """
def g(x):
    return x**2"""
    
    editor_page.new_file('test1.py', test_code1)
    editor_page.new_file('test2.py', test_code2)
    
    editor_page.edit_file('test1.py')
    editor_page.add_text_to_file('\n #an extra comment line')
    input_code1 = editor_page.get_code()
    editor_page.save_document()

    editor_page.edit_file('test2.py')
    editor_page.add_text_to_file('\n #an extra comment line')
    input_code2 = editor_page.get_code()
    
    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Go back to code editor, open file, verify source code
    
    editor_page = workspace_page.edit_file('test1.py') #this file was saved
    time.sleep(1)
    loaded_code = editor_page.get_code()
    eq(input_code1, loaded_code)
    
    
    editor_page.edit_file('test2.py') #this file was not saved
    time.sleep(1)
    loaded_code = editor_page.get_code()
    neq(input_code2, loaded_code)
    
    # Clean up.
    browser.close()
    browser.switch_to_window(workspace_window)
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_multitab complete."


if __name__ == '__main__':
    main()
