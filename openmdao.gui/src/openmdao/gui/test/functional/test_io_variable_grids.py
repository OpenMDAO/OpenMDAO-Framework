import pkg_resources
import sys
import time
from nose.tools import eq_ as eq
from nose.tools import with_setup

from pageobjects.component import ComponentPage
from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser

def _test_grid(browser):
    #This is for testing the new GridRow functionality.
    #Columns that have non-empty names are added as properties
    #to a row. So you can used grid.rows[row_number].name to the
    #cell corresponding to name, With the cell, you can use cell.value
    #to get the text within the cell.

    #This functionality was added for two reasons
    # - Being able to intuitively refer to cells of a row
    # - Reduce the need to compare arrays of arrays in testing
    #   by calling grid.value

    # New methods were also added to ComponentPage for accessing a 
    # single input or output variable by its name. The function will
    # return the corresponding row object if it exists. Currently,
    # if there are name conflicts, only the first match is returned.

    project_dict, workspace_page = startup(browser)
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')
    workspace_page.add_file(file1_path)
    workspace_page.add_library_item_to_dataflow('paraboloid.Paraboloid', 'paraboloid')
    paraboloid = workspace_page.get_dataflow_figure("paraboloid")
    editor = paraboloid.editor_page(version=ComponentPage.Version.NEW)


    #Checks all the inputs
    inputs = editor.get_inputs()
    eq(inputs[2].name.value, "directory")
    eq(inputs[2].value.value, "")
    eq(inputs[2].units.value, "")
    eq(inputs[2].description.value, "If non-blank, the directory to execute in.")

    eq(inputs[3].name.value, "force_execute")
    eq(inputs[3].value.value, "False")
    eq(inputs[3].units.value, "")
    eq(inputs[3].description.value, "If True, always execute even if all IO traits are valid.")

    eq(inputs[0].name.value, "x")
    eq(inputs[0].value.value, "0")
    eq(inputs[0].units.value, "")
    eq(inputs[0].description.value, "The variable x")

    eq(inputs[1].name.value, "y")
    eq(inputs[1].value.value, "0")
    eq(inputs[1].units.value, "")
    eq(inputs[1].description.value, "The variable y")

    #Checks all the outputs
    outputs = editor.get_outputs()
    eq(outputs[1].name.value, "derivative_exec_count")
    eq(outputs[1].value.value, "0")
    eq(outputs[1].units.value, "")
    eq(outputs[1].description.value, "Number of times this Component's derivative function has been executed.")

    eq(outputs[2].name.value, "exec_count")
    eq(outputs[2].value.value, "0")
    eq(outputs[2].units.value, "")
    eq(outputs[2].description.value, "Number of times this Component has been executed.")

    eq(outputs[0].name.value, "f_xy")
    eq(outputs[0].value.value, "0")
    eq(outputs[0].units.value, "")
    eq(outputs[0].description.value, "F(x,y)")

    eq(outputs[3].name.value, "itername")
    eq(outputs[3].value.value, "")
    eq(outputs[3].units.value, "")
    eq(outputs[3].description.value, "Iteration coordinates.")

    #Access and test a single varible by name
    x = editor.get_input("x")
    eq(x.name.value, inputs[0].name.value)
    eq(x.value.value, inputs[0].value.value)
    eq(x.units.value, inputs[0].units.value)
    eq(x.description.value, inputs[0].description.value)

    y = editor.get_input("y")
    eq(y.name.value, inputs[1].name.value)
    eq(y.value.value, inputs[1].value.value)
    eq(y.units.value, inputs[1].units.value)
    eq(y.description.value, inputs[1].description.value)

    f_xy = editor.get_output("f_xy")
    eq(f_xy.name.value, outputs[0].name.value)
    eq(f_xy.value.value, outputs[0].value.value)
    eq(f_xy.units.value, outputs[0].units.value)
    eq(f_xy.description.value, outputs[0].description.value)

    #Set a value
    x = editor.get_input("x")
    x.value.value = "1"

    #Fetching a single variable refetches the grid
    x = editor.get_input("x")
    eq(x.value.value, "1")

    #Try setting a non-editable cell. 
    f_xy = editor.get_output("f_xy")

    try:
        f_xy.value.value = "1"
    except IndexError:
        #Attribute error should be raised because the cell is not editable, and thus, the property should not have a setter.
        pass
    else:
        #Otherwise, something went wrong
        self.fail("Exception: An AttributeError was not raised and f_xy.value.value should have no setter.")

    #Cleanup and closeout
    editor.close()
    closeout(project_dict, workspace_page)

if __name__ == '__main__':
    main()
