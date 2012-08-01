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
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_newfile(browser):
    print "running _test_newfile..."
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Open code editor.
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()

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

    # Go back to code editor, open file, verify source code
    
    editor_page = workspace_page.edit_file('plane.py')
    eq(str(editor_page.editor_label), 'plane.py')
    
    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_newfile complete."


if __name__ == '__main__':
    main()
