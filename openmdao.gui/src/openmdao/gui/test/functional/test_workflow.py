"""
Tests of workflow functions.
"""

import pkg_resources
import time
from unittest import TestCase
from nose.tools import eq_ as eq
from nose.tools import with_setup

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.util import NotifierPage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_basic(browser):
    project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/rosen_suzuki.py')
    workspace_page.add_file(filename)

    # Add a NestedSimulation.
    workspace_page.add_library_item_to_dataflow('rosen_suzuki.NestedSimulation',
                                                'nested', offset=(300, 300))
    # Verify full workflow shown.
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_figures()), 2)
    eq(len(workspace_page.get_workflow_component_figures()), 5)

    # Verify flow layout is horizontal and can be switched to vertical
    sim = workspace_page.get_workflow_figure('sim.driver')
    assert sim.horizontal
    sim.flip()
    assert not sim.horizontal

    # Verify workflow can be collapsed and expanded
    sim.collapse()
    assert sim.collapsed
    sim.expand()
    assert sim.expanded

    # Verify that component state is represented properly
    driver = workspace_page.get_workflow_component_figure('sim.driver')
    driver.run()
    time.sleep(2.0)
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')

    # Verify workflow can be cleared
    nested = workspace_page.get_workflow_figure('nested.driver')
    nested.clear()
    eq(len(workspace_page.get_workflow_component_figures()), 1)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_evaluate(browser):
    project_dict, workspace_page = startup(browser)

    # create an assembly with an implicit component in it's workflow
    filename = pkg_resources.resource_filename('openmdao.main.test',
                                               'test_implicit_component.py')
    workspace_page.add_file(filename)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')

    workspace_page.add_library_item_to_dataflow('test_implicit_component.MyComp_Deriv',
                                                'comp', prefix='top')

    workspace_page.add_object_to_workflow('top.comp', 'top')

    # Verify that the evaluate menu option has the expected effect
    (header, inputs, outputs) = workspace_page.get_properties('comp')
    eq(outputs.value, [
        ['y_out', '0'],
        ['derivative_exec_count', '0'],
        ['exec_count', '0'],
        ['itername', '']
    ])

    workspace_page('workflow_tab').click()

    comp = workspace_page.get_workflow_component_figure('comp')
    comp.evaluate()

    (header, inputs, outputs) = workspace_page.get_properties('comp')
    eq(outputs.value, [
        ['y_out', '2'],
        ['derivative_exec_count', '0'],
        ['exec_count', '0'],
        ['itername', '']
    ])

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_update(browser):
    # Adding a parameter to a driver should update the driver's workflow.
    project_dict, workspace_page = startup(browser)

    # Create model with CONMIN and ExecComp.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.replace_driver('top', 'CONMINdriver')
    workspace_page.add_library_item_to_dataflow(
        'openmdao.test.execcomp.ExecComp', 'exe', args=["('z = x * y',)"])

    # Add parameter to CONMIN.
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'exe.x'
    dialog.low = '-1'
    dialog.high = '1'
    dialog('ok').click()
    editor.close()

    # Verify workflow contains ExecComp.
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_figures()), 1)
    eq(len(workspace_page.get_workflow_component_figures()), 2)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_duplicates(browser):
    # Duplicate unconnected components are legal in a workflow.
    project_dict, workspace_page = startup(browser)

    # Create model with multiple ExecComps.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'openmdao.test.execcomp.ExecComp', 'exe', args=["('z = x * y',)"])
    workspace_page.expand_object('top')
    workspace_page.add_object_to_workflow('top.exe', 'top')
    workspace_page.add_object_to_workflow('top.exe', 'top')
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_figures()), 1)
    eq(len(workspace_page.get_workflow_component_figures()), 3)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_parameter_auto(browser):
    # Test auto-filling the min and max for a parameter.
    project_dict, workspace_page = startup(browser)

    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/connect.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('connect.Conn_Assy',
                                                'top')
    # Add parameter to driver.
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'comp.x'
    dialog('ok').click()

    parameters = editor.get_parameters()
    expected = [['', 'comp.x', '0', '299', '1', '0', '', 'comp.x']]
    eq(len(parameters.value), len(expected))
    for i, row in enumerate(parameters.value):
        eq(row, expected[i])

    editor.close()

    closeout(project_dict, workspace_page)


def _test_array_parameter(browser):
    # Test adding an array parameter.
    project_dict, workspace_page = startup(browser)

    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/array_parameters.py')
    workspace_page.add_file(file_path)
    workspace_page.add_library_item_to_dataflow('array_parameters.ArrayParameters',
                                                'top')
    # Add parameter to driver.
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'paraboloid.x'
    dialog.low = '-50'
    dialog.high = '[40, 50]'
    dialog.scaler = '[[1., 1]]'
    dialog('ok').click()

    parameters = editor.get_parameters()
    expected = [['', 'paraboloid.x', '-50', '40,50', '1,1', '0', '', 'paraboloid.x']]
    eq(len(parameters.value), len(expected))
    for i, row in enumerate(parameters.value):
        eq(row, expected[i])

    editor.close()
    time.sleep(1)

    # Run optimization.
    top = workspace_page.get_dataflow_figure('top')
    top.run()
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')

    # Check results.
    workspace_page.do_command("top.paraboloid.x[0][0]")
    x00 = workspace_page.history.split("\n")[-1]
    workspace_page.do_command("top.paraboloid.x[0][1]")
    x01 = workspace_page.history.split("\n")[-1]

    if abs(float(x00) - 6.6667) > 0.01:
        raise TestCase.failureException(
            "Parameter x[0][0] did not reach correct value, but instead is %s"
            % x00)

    if abs(float(x01) - -7.3333) > 0.01:
        raise TestCase.failureException(
            "Parameter x[0][1] did not reach correct value, but instead is %s"
            % x01)

    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
