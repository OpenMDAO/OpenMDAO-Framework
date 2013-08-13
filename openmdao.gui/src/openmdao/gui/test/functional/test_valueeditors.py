"""
Tests of grid value editor functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup
from unittest import TestCase
from selenium.webdriver import ActionChains

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_partial_array_connections(browser):
    # Creates a file in the GUI.
    project_dict, workspace_page = startup(browser)

    # Import partial_connections.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/partial_connections.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('partial_connections.PartialConnectionAssembly', 'top')
   
    workspace_page.add_library_item_to_dataflow('partial_connections.PartialConnectionAssembly2', 'top_2')

    paraboloid = workspace_page.get_dataflow_figure("paraboloid_1", "top")
    var_fields_path = '//*[@id="inArray-editor"]/input'

    props = paraboloid.properties_page()

    #array 1d editor - check that implicitly connected elements are disabled
    inputs = props.inputs
    inputs.rows[0].cells[1].click()
    array_inputs_path = '//*[@id="inArray-editor"]/input'
    cancel_path = '//*[@id="array-edit-inArray-cancel"]'

    array_inputs = browser.find_elements_by_xpath(array_inputs_path)

    for array_input in array_inputs:
        eq(array_input.is_enabled(), False)
    
    browser.find_element_by_xpath(cancel_path).click()

    props.close()

    paraboloid = workspace_page.get_dataflow_figure("paraboloid_2", "top")
    props = paraboloid.properties_page()

    #array 1d editor - check that explicitly connected elements are disabled
    inputs = props.inputs
    inputs.rows[0].cells[1].click()

    array_inputs = browser.find_elements_by_xpath(array_inputs_path)

    for array_input in array_inputs:
        eq(array_input.is_enabled(), False)

    browser.find_element_by_xpath(cancel_path).click()
    props.close()

    #array 2d editor - check that implicitly connected elements are disabled
    paraboloid = workspace_page.get_dataflow_figure("array_comp_1", "top_2")
    props = paraboloid.properties_page()

    inputs = props.inputs
    inputs.rows[0].cells[1].click()
    array_inputs_path = '//*[@id="inArray-editor"]/input'
    cancel_path = '//*[@id="array-edit-inArray-cancel"]'

    array_inputs = browser.find_elements_by_xpath(array_inputs_path)

    for index, array_input in enumerate(array_inputs):
        if( index % 3 == 2):
            eq(array_input.is_enabled(), True)
        else:
            eq(array_input.is_enabled(), False)
    
    browser.find_element_by_xpath(cancel_path).click()

    props.close()

    #array 2d editor - check that explicitly connected elements are disabled
    paraboloid = workspace_page.get_dataflow_figure("array_comp_2", "top_2")
    props = paraboloid.properties_page()

    inputs = props.inputs
    inputs.rows[0].cells[1].click()

    array_inputs = browser.find_elements_by_xpath(array_inputs_path)

    for index, array_input in enumerate(array_inputs):
        if( index % 3 == 1 ):
            eq(array_input.is_enabled(), True)
        else:
            eq(array_input.is_enabled(), False)

    browser.find_element_by_xpath(cancel_path).click()
    props.close()

    # Clean up.
    closeout(project_dict, workspace_page)
    
def _test_value_editors(browser):
    # Creates a file in the GUI.
    project_dict, workspace_page = startup(browser)

    # Import variable_editor.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/variable_editors.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('variable_editors.Topp', 'top')

    paraboloid = workspace_page.get_dataflow_figure('p1', 'top')
    props = paraboloid.properties_page()
    props.move(-100, -100)  # Ensure Project menu fully visible.
    inputs = props.inputs

    #edit dictionary - remove 'e', add 'phi', round down 'pi'
    #action_chain = ActionChains(browser)
    #action_chain.double_click(inputs.rows[0].cells[1]).perform()
    inputs.rows[0].cells[1].click()

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
    time.sleep(0.5)
    inputs = props.inputs

    # string editor - set to "abcd"
    inputs.rows[8].cells[1].click()
    inputs[8][1] = "abcd"
    time.sleep(1)

    #enum editor - set to 3
    inputs = props.inputs
    inputs.rows[1].cells[1].click()
    selection_path = '//*[@id="editor-enum-e"]/option[4]'
    browser.find_element_by_xpath(selection_path).click()
    time.sleep(0.5)

    # float editor - set to 2.71
    inputs = props.inputs
    inputs.rows[2].cells[1].click()
    inputs[2][1] = '2.71'
    time.sleep(0.5)

    #bool editor - set to true
    inputs = props.inputs
    inputs.rows[9].cells[1].click()
    selection_path = '//*[@id="bool-editor-force_execute"]/option[1]'
    browser.find_element_by_xpath(selection_path).click()
    time.sleep(0.5)

    #array 1d editor - add element, set to 4
    inputs = props.inputs
    inputs.rows[3].cells[1].click()
    add_path = '//*[@id="array-edit-add-X"]'
    browser.find_element_by_xpath(add_path).click()
    new_cell_path = '//*[@id="array-editor-dialog-X"]/div/input[5]'
    new_cell = browser.find_element_by_xpath(new_cell_path)
    new_cell.clear()
    new_cell.send_keys("4.")
    submit_path = '//*[@id="array-edit-X-submit"]'
    browser.find_element_by_xpath(submit_path).click()
    time.sleep(0.5)

    # array 2d editor - set to [[1, 4],[9, 16]]
    inputs = props.inputs
    inputs.rows[4].cells[1].click()
    for i in range(1, 5):
        cell_path = '//*[@id="array-editor-dialog-Y"]/div/input[' + str(i) + ']'
        cell_input = browser.find_element_by_xpath(cell_path)
        cell_input.clear()
        cell_input.send_keys(str(i**2))
    submit_path = '//*[@id="array-edit-Y-submit"]'
    browser.find_element_by_xpath(submit_path).click()

    # array 2d editor - special case for a bug - set to [[5],[7]]
    inputs = props.inputs
    inputs.rows[5].cells[1].click()
    cell_path = '//*[@id="array-editor-dialog-Y2"]/div/input[2]'
    cell_input = browser.find_element_by_xpath(cell_path)
    cell_input.clear()
    cell_input.send_keys(str(7))
    submit_path = '//*[@id="array-edit-Y2-submit"]'
    browser.find_element_by_xpath(submit_path).click()

    # array 2d editor - special case for a bug - set to [[99]]
    inputs = props.inputs
    inputs.rows[6].cells[1].click()
    cell_path = '//*[@id="array-editor-dialog-Y3"]/div/input[1]'
    cell_input = browser.find_element_by_xpath(cell_path)
    cell_input.clear()
    cell_input.send_keys(str(99))
    submit_path = '//*[@id="array-edit-Y3-submit"]'
    browser.find_element_by_xpath(submit_path).click()

    #list editor - set to [1, 2, 3, 4, 5]
    inputs = props.inputs
    eq(inputs[7][1].startswith("["), True)
    eq(inputs[7][1].endswith("]"), True)
    values = [int(value.strip()) for value in inputs[7][1].strip("[]").split(",")]
    eq(len(values), 4)
    eq(values, [1, 2, 3, 4])

    values.append(5)
    values = str([value for value in values])
    inputs[7][1] = values

    props.close()

    #check that all values were set correctly by the editors
    commands = ["top.p1.d['pi']", 
                "top.p1.d['phi']", 
                "top.p1.force_execute",
                "top.p1.e", 
                "top.p1.x", 
                "top.p1.X", 
                "top.p1.directory", 
                "top.p1.Z"]
    values = ["3.0", 
              "1.61", 
              "True", 
              "3", 
              "2.71", 
              "[ 0.  1.  2.  3.  4.]", 
              "abcd", 
              "[1, 2, 3, 4, 5]"]

    for cmd_str, check_val in zip(commands, values):
        workspace_page.do_command(cmd_str)
        output = workspace_page.history.split("\n")[-1]
        eq(output, check_val)

    #separate check for 2d arrays
    workspace_page.do_command("top.p1.Y")
    output = workspace_page.history.split("\n")
    eq(output[-2], "[[ 1  4]")
    eq(output[-1], " [ 9 16]]")

    #separate check for 2d arrays
    workspace_page.do_command("top.p1.Y2")
    output = workspace_page.history.split("\n")
    eq(output[-2], "[[5]")
    eq(output[-1], " [7]]")

    #separate check for 2d arrays
    workspace_page.do_command("top.p1.Y3")
    output = workspace_page.history.split("\n")
    eq(output[-1], "[[99]]")

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_Avartrees(browser):
    project_dict, workspace_page = startup(browser)

    # Import variable_editor.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/model_vartree.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('model_vartree.Topp', "top")

    comp = workspace_page.get_dataflow_figure('p1', "top")
    editor = comp.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', ' cont_in', '',  '', ''],
        ['', 'directory', '', '',
            'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
            'If True, always execute even if all IO traits are valid.'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # Expand first vartree
    inputs.rows[0].cells[1].click()
    inputs = editor.get_inputs()
    expected = [
        ['', ' cont_in', '',  '', ''],
        ['', 'v1', '1',  '', 'vv1'],
        ['', 'v2', '2',  '', 'vv2'],
        ['', ' vt2', '',  '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # While expanded, verify that 'v1' is editable.
    inputs.rows[1].cells[2].click()
    inputs = editor.get_inputs()
    inputs[1][2] = "42"
    expected[1][2] = "42"

    time.sleep(0.5)
    inputs = editor.get_inputs()
#FIXME sometimes row 2 gets a value of '' because slickgrid is editing it.
#    for i, row in enumerate(inputs.value):
#        eq(row, expected[i])
    eq(inputs.value[1], expected[1])

    # While expanded, verify that cell that became the 2nd vartree is now
    # uneditable
    inputs.rows[3].cells[1].click()
    inputs = editor.get_inputs()
    try:
        inputs[3][2] = "abcd"
    except IndexError:
        pass
    else:
        raise TestCase.failureException(
            'Exception expected: VarTree value should not be settable on inputs.')

    # Contract first vartree
    inputs.rows[0].cells[1].click()
    inputs = editor.get_inputs()
    expected = [
        ['', ' cont_in', '',  '', ''],
        ['', 'directory', '', '',
            'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
            'If True, always execute even if all IO traits are valid.'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    editor.close()

    # Now, do it all again on the Properties Pane
    workspace_page('properties_tab').click()
    obj = workspace_page.get_dataflow_figure('p1', 'top')
    chain = ActionChains(browser)
    chain.click(obj.root)
    chain.perform()
    inputs = workspace_page.props_inputs
    expected = [
        [' cont_in',      ''],
        ['directory',     ''],
        ['force_execute', 'False'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # Expand first vartree
    inputs.rows[0].cells[0].click()
    inputs = workspace_page.props_inputs
    expected = [
        [' cont_in',      ''],
        ['v1', '42'],
        ['v2', '2'],
        [' vt2', ''],
        ['directory',     ''],
        ['force_execute', 'False'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # While expanded, verify that 'v1' is editable.
    inputs.rows[1].cells[1].click()
    inputs = workspace_page.props_inputs
    inputs[1][1] = "43"
    expected[1][1] = "43"

    time.sleep(1)
    inputs = workspace_page.props_inputs
#FIXME sometimes row 2 gets a value of '' because slickgrid is editing it.
#    for i, row in enumerate(inputs.value):
#        eq(row, expected[i])
    eq(inputs.value[1], expected[1])

    # Contract first vartree
    inputs.rows[0].cells[0].click()
    inputs = workspace_page.props_inputs
    expected = [
        [' cont_in',      ''],
        ['directory',     ''],
        ['force_execute', 'False'],
    ]

    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # Clean up.
    closeout(project_dict, workspace_page)

if __name__ == '__main__':
    main()
