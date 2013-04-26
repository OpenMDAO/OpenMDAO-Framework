"""
Tests of code editor functions.
"""

import time
import pkg_resources

from unittest import TestCase

from nose.tools import eq_ as eq
from nose.tools import with_setup
from nose.tools import assert_not_equal as neq

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout
from pageobjects.util import NotifierPage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_crlf(browser):
    # Test ability to handle a file with Windows-style CR/LF line terminations
    project_dict, workspace_page = startup(browser)

    # add a Windows notepad generated python file
    filename = 'notepad.py'
    filepath = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/notepad.py')
    workspace_page.add_file(filepath)

    # open file in code editor
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(filename)
    eq(str(editor_page.get_tab_label()), '/' + filename)

    # add a comment and save
    comment = '# a comment'
    editor_page.append_text_to_file(comment)
    editor_page.save_document()

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # re-open file and verify comment was successfully added
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(filename)
    assert editor_page.get_code().endswith(comment)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_editfile(browser):
    # Check ability to open code editor by double clicking on file in workspace.
    project_dict, workspace_page = startup(browser)

    # create a couple of files
    file1 = 'test1.py'
    workspace_page.new_file(file1)
    file2 = 'test2.py'
    workspace_page.new_file(file2)

    # verify file is opened in code editor by double clicking
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), '/' + file1)

    # verify different file is opened in code editor by double clicking
    browser.switch_to_window(workspace_window)
    editor_page = workspace_page.edit_file(file2)
    eq(str(editor_page.get_tab_label()), '/' + file2)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # verify code editor can be re-opened by double clicking on file
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.edit_file(file1)
    eq(str(editor_page.get_tab_label()), '/' + file1)

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_multitab(browser):
    project_dict, workspace_page = startup(browser)

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

    editor_page = workspace_page.edit_file('test1.py')  # this file was saved
    time.sleep(1)
    loaded_code = editor_page.get_code()
    eq(input_code1, loaded_code)

    editor_page.edit_file('test2.py')  # this file was not saved
    time.sleep(1)
    loaded_code = editor_page.get_code()
    neq(input_code2, loaded_code)

    # Clean up.
    browser.close()
    browser.switch_to_window(workspace_window)
    closeout(project_dict, workspace_page)


def _test_newfile(browser):
    # Creates a file in the GUI.
    project_dict, workspace_page = startup(browser)

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
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow('plane.Plane', 'plane')

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
