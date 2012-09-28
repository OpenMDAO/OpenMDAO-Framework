"""
Tests of code editor functions.
"""

import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup
from nose.tools import assert_not_equal as neq

if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     begin, new_project

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_value_editors(browser):
    print "running _test_value_editors..."
    # Creates a file in the GUI.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Import variable_editor.py
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'variable_editors.py')
    editor_page.add_file(file_path)
    
    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    top = workspace_page.get_dataflow_figure('top')
    top.remove()

    workspace_page.show_library()
    time.sleep(1)
    workspace_page.find_library_button('Topp', 0.5).click()
    workspace_page.add_library_item_to_dataflow('variable_editors.Topp',"top")
    
    #workspace_page.select_object('top.p1')

    paraboloid = workspace_page.get_dataflow_figure('p1',"top")

    #Test highlighting for implicit connections
    component_editor = paraboloid.editor_page()
    
    dict_path = '//*[@id="Inputs_props"]/div[4]/div/div[1]/div[3]'
    str_path = '//*[@id="Inputs_props"]/div[4]/div/div[2]/div[3]'
    enum_path = '//*[@id="Inputs_props"]/div[4]/div/div[3]/div[3]'
    bool_path = '//*[@id="Inputs_props"]/div[4]/div/div[4]/div[3]'
    array1d_path = '//*[@id="Inputs_props"]/div[4]/div/div[5]/div[3]'
    float_path = '//*[@id="Inputs_props"]/div[4]/div/div[6]/div[3]'
    array2d_path = '//*[@id="Inputs_props"]/div[4]/div/div[7]/div[3]'
    
    #edit dictionary - remove 'e', add 'phi', round down 'pi'
    browser.find_element_by_xpath(dict_path).click()
    
    pi_value_path = '//*[@id="d-editor"]/input[2]'
    pi_value = browser.find_element_by_xpath(pi_value_path)
    pi_value.clear()
    pi_value.send_keys("3.0")
    
    e_remove_btn = '//*[@id="e"]'
    browser.find_element_by_xpath(e_remove_btn).click()
    
    key_path = '//*[@id="d-dialog"]/input[1]'
    value_path = '//*[@id="d-dialog"]/input[2]'
    add_new_path = '//*[@id="d-dialog"]/button'
    submit_path = '//*[@id="dict-edit-d-submit"]'
    
    browser.find_element_by_xpath(key_path).send_keys("phi")
    browser.find_element_by_xpath(value_path).send_keys("1.61")
    browser.find_element_by_xpath(add_new_path).click()
    browser.find_element_by_xpath(submit_path).click()
    time.sleep(1)     
    
    # string editor - set to "abcd"
    browser.find_element_by_xpath(str_path).click()
    cell_path = '//*[@id="Inputs_props"]/div[4]/div/div[2]/div[3]'
    browser.find_element_by_xpath(cell_path).click()
    cell_input_path = '//*[@id="Inputs_props"]/div[4]/div/div[2]/div[3]/input'
    cell_input = browser.find_element_by_xpath(cell_input_path)
    cell_input.clear()
    cell_input.send_keys("abcd")
    browser.find_element_by_xpath('//*[@id="Inputs_props"]/div[4]/div/div[3]/div[4]').click()
    time.sleep(1)
        
    #enum editor - set to 3
    browser.find_element_by_xpath(enum_path).click()
    selector_path = '//*[@id="Inputs_props"]/div[4]/div/div[3]/div[3]/select/option[4]'
    browser.find_element_by_xpath(selector_path).click()
    browser.find_element_by_xpath('//*[@id="Inputs_props"]/div[4]/div/div[3]/div[4]').click()
    time.sleep(1)
    
    #bool editor - set to true
    
    browser.find_element_by_xpath(bool_path).click()
    selection_path = '//*[@id="bool-editor-force_execute"]/option[1]'
    browser.find_element_by_xpath(selection_path).click()
    browser.find_element_by_xpath('//*[@id="Inputs_props"]/div[4]/div/div[3]/div[4]').click()
    time.sleep(1)
    
    
    #array 1d editor - add element, set to 4
    browser.find_element_by_xpath(array1d_path).click()
    add_path = '//*[@id="array-edit-add-X"]'
    browser.find_element_by_xpath(add_path).click()
    new_cell_path = '//*[@id="array-editor-dialog-X"]/div/input[5]'
    new_cell = browser.find_element_by_xpath(new_cell_path)
    new_cell.clear()
    new_cell.send_keys("4.")
    
    submit_path = '//*[@id="array-edit-X-submit"]'
    browser.find_element_by_xpath(submit_path).click()
    time.sleep(1)
    
    # float editor - set to 2.71
    browser.find_element_by_xpath(float_path).click()
    cell_path = '//*[@id="Inputs_props"]/div[4]/div/div[6]/div[3]'
    browser.find_element_by_xpath(cell_path).click()
    cell_input_path = '//*[@id="Inputs_props"]/div[4]/div/div[6]/div[3]/input'
    cell_input = browser.find_element_by_xpath(cell_input_path)
    cell_input.clear()
    cell_input.send_keys("2.71")
    browser.find_element_by_xpath('//*[@id="Inputs_props"]/div[4]/div/div[3]/div[4]').click()
    time.sleep(1)
        
    
    # array 2d editor - set to [[1, 4],[9, 16]]
    browser.find_element_by_xpath(array2d_path).click()
    for i in xrange(1,5):
        cell_path = '//*[@id="array-editor-dialog-Y"]/div/input['+str(i)+']'
        cell_input = browser.find_element_by_xpath(cell_path)
        cell_input.clear()
        cell_input.send_keys(str(i**2))
    submit_path = '//*[@id="array-edit-Y-submit"]'
    browser.find_element_by_xpath(submit_path).click()
    time.sleep(1)
    
    
    component_editor.close()    
    time.sleep(2)
    
    #check that all values were set correctly by the editors
    commands = ["top.p1.d['pi']", "top.p1.d['phi']", "top.p1.force_execute", 
                "top.p1.e", "top.p1.x", "top.p1.X", "top.p1.directory"]
    values = ["3.0", "1.61", "True", "3", "2.71", "[ 0.  1.  2.  3.  4.]", "abcd"]
    
    for cmd_str, check_val in zip(commands, values):
        workspace_page.do_command(cmd_str)
        output = workspace_page.history.split("\n")[-1]
        eq(output, check_val)    
    
    #separate check for 2d arrays
    workspace_page.do_command("top.p1.Y")
    output = workspace_page.history.split("\n")
    eq(output[-2], "[[ 1  4]")   
    eq(output[-1], " [ 9 16]]")   
    
    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_value_editors complete."
    

if __name__ == '__main__':
    main()
