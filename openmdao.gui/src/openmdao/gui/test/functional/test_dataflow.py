"""
Tests of dataflow functions.
"""

import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup


if sys.platform != 'win32':  # No testing on Windows yet.
    from util import main, setup_server, teardown_server, generate, \
                     startup, closeout
    from pageobjects.util import NotifierPage

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_maxmin(browser):
    # Toggles maxmimize/minimize button on assemblies.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # verify that the globals figure is invisible
    globals_figure = workspace_page.get_dataflow_figure('')
    assert globals_figure.border.find('none') >= 0
    eq(globals_figure.background_color, 'rgba(0, 0, 0, 0)')

    # Add maxmin.py to project
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'maxmin.py')
    workspace_page.add_file(file_path)

    # Add MaxMin to 'top'.
    workspace_page.show_dataflow('top')
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'top'])
    maxmin = workspace_page.add_library_item_to_dataflow('maxmin.MaxMin',
                                                         'maxmin', prefix='top')
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'maxmin', 'top'])

    workspace_page.hide_left()

    # Maximize maxmin.
    background = maxmin('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0

    maxmin('top_right').click()
    time.sleep(0.5)
    background = maxmin('top_right').value_of_css_property('background')
    assert background.find('circle-minus.png') >= 0
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'maxmin', 'sub', 'top'])

    sub = workspace_page.get_dataflow_figure('sub')
    sub('top_right').click()
    time.sleep(0.5)
    background = sub('top_right').value_of_css_property('background')
    assert background.find('circle-minus.png') >= 0
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'driver', 'extcode', 'maxmin', 'sub', 'top'])

    # issue a command and make sure maxmin is still maximized
    workspace_page.do_command('dir()')
    background = maxmin('top_right').value_of_css_property('background')
    assert background.find('circle-minus.png') >= 0
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'driver', 'extcode', 'maxmin', 'sub', 'top'])

    # Minimize sub
    sub('top_right').click()
    background = sub('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'maxmin', 'sub', 'top'])

    # remove maxmin and make sure its children are removed as well
    maxmin.remove()
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'top'])

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_connect(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Import connect.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'connect.py')
    workspace_page.add_file(file_path)

    # Replace 'top' with connect.py's top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.add_library_item_to_dataflow('connect.Topp', 'top')

    # Connect components.
    workspace_page.show_dataflow('top')
    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    conn_page = workspace_page.connect(comp1, comp2)
    eq(conn_page.dialog_title, 'Connections: top')
    eq(conn_page.source_component, 'comp1')
    eq(conn_page.target_component, 'comp2')
    for prefix in ('b', 'e', 'f', 'i', 's'):
        conn_page.connect_vars('comp1.' + prefix + '_out',
                               'comp2.' + prefix + '_in')
        time.sleep(0.5)  # Wait for display update.
    conn_page.close()

    # Set inputs (re-fetch required after updating).
    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    props = comp1.properties_page()
    eq(props.header, 'Connectable: top.comp1')
    inputs = props.inputs
    eq(inputs[3].value, ['f_in', '0'])
    inputs[3][1] = '2.781828'
    inputs = props.inputs
    eq(inputs[5].value, ['i_in', '0'])
    inputs[5][1] = '42'
    inputs = props.inputs
    eq(inputs[6].value, ['s_in', ''])
    inputs[6][1] = "'xyzzy'"
    
    inputs = props.inputs
    eq(inputs[0].value, ['b_in', 'False'])
    inputs.rows[0].cells[1].click()
    browser.find_element_by_xpath('//*[@id="bool-editor-b_in"]/option[1]').click()
    inputs.rows[0].cells[0].click()
    #inputs[0][1] = 'True'
    
    inputs = props.inputs
    eq(inputs[2].value, ['e_in', '1'])
    inputs.rows[2].cells[1].click()
    browser.find_element_by_xpath('//*[@id="editor-enum-e_in"]/option[3]').click()
    inputs.rows[2].cells[0].click()
    #inputs[2][1] = '3'    
    
    props.close()

    # Run the simulation.
    workspace_page.run()

    # Verify outputs.
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    editor = comp2.editor_page()
    editor.move(-100, 0)
    eq(editor.dialog_title, 'Connectable: top.comp2')
    outputs = editor.get_outputs()
    expected = [
        ['b_out', 'bool',  'True',     '', 'true', '', '', ''],
        ['e_out', 'enum',  '3',        '', 'true', '', '', ''],
        ['f_out', 'float', '2.781828', '', 'true', '', '', ''],
        ['i_out', 'int',   '42',       '', 'true', '', '', ''],
        ['s_out', 'str',   'xyzzy',    '', 'true', '', '', '']
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])
    editor.close()

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_connections(browser):
    # Check connection frame functionality.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                               'vehicle_singlesim.py')
    workspace_page.add_file(filename)

    # Replace 'top' with VehicleSim.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    asm_name = 'sim'
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
                                                asm_name)
    # show dataflow for vehicle
    workspace_page.expand_object('sim')
    workspace_page.show_dataflow('sim.vehicle')
    vehicle = workspace_page.get_dataflow_figure('vehicle', 'sim')

    # no connections between assembly vars
    conn_page = vehicle.connections_page()
    eq(conn_page.dialog_title, 'Connections: vehicle')
    eq(conn_page.source_component, '<Assembly>')
    eq(conn_page.target_component, '<Assembly>')
    eq(conn_page.check_variable_figures(), 0)

    # two connections between engine and chassis
    conn_page.set_source_component('engine')
    conn_page.set_target_component('chassis')
    eq(conn_page.source_variable, '')
    eq(conn_page.target_variable, '')
    eq(len(conn_page.get_variable_figures()), 4)
    eq(sorted(conn_page.get_variable_names()),
       ['engine_torque', 'engine_weight', 'mass_engine', 'torque'])

    # one connection between transmission and engine (RPM)
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    eq(conn_page.source_variable, '')
    eq(conn_page.target_variable, '')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # disconnect transmission
    tranny = workspace_page.get_dataflow_figure('transmission', 'sim.vehicle')
    tranny.disconnect()
    time.sleep(0.5)

    # now there are no connections between transmission and engine
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    time.sleep(0.5)
    eq(conn_page.check_variable_figures(), 0)

    # reconnect transmission RPM to engine RPM
    conn_page.connect_vars('transmission.RPM', 'engine.RPM')
    time.sleep(0.5)
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # no connections between transmission and chassis
    conn_page.set_target_component('chassis')
    time.sleep(0.5)
    eq(conn_page.check_variable_figures(), 0)

    # reconnect transmission torque to chassis torque
    conn_page.connect_vars('transmission.torque_ratio', 'chassis.torque_ratio')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['torque_ratio', 'torque_ratio'])

    # no connections between vehicle assembly and transmission
    conn_page.set_source_component('')
    conn_page.set_target_component('transmission')
    time.sleep(0.5)
    eq(conn_page.check_variable_figures(), 0)

    # connect assembly variable to component variable
    conn_page.connect_vars('current_gear', 'transmission.current_gear')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['current_gear', 'current_gear'])

    # one connection from chassis component to vehicle assembly
    conn_page.set_source_component('chassis')
    conn_page.set_target_component('')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['acceleration', 'acceleration'])

    # disconnect chassis
    conn_page.close()
    chassis = workspace_page.get_dataflow_figure('chassis', 'sim.vehicle')
    chassis.disconnect()
    vehicle = workspace_page.get_dataflow_figure('vehicle', 'sim')
    conn_page = vehicle.connections_page()
    eq(conn_page.check_variable_figures(), 0)

    # connect component variable to assembly variable
    conn_page.connect_vars('chassis.acceleration', 'acceleration')
    conn_page.set_source_component('chassis')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['acceleration', 'acceleration'])

    conn_page.close()

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_driverflows(browser):
    # Excercises display of driver flows (parameters, constraints, objectives).
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'rosen_suzuki.py')
    workspace_page.add_file(filename)

    # Replace 'top' with Simulation.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.add_library_item_to_dataflow('rosen_suzuki.Simulation', 'top')

    # Show dataflow for Simulation.
    workspace_page.show_dataflow('top')
    workspace_page.hide_left()

    # Select different displays.
    top = workspace_page.get_dataflow_figure('top')
    top.display_dataflows(False)
    time.sleep(0.5)

    # While only driver flows are displayed, check on context menu.
    preproc = workspace_page.get_dataflow_figure('preproc', 'top')
    editor = preproc.input_edit_driver('top.driver')
    editor.move(-100, 0)
    eq(editor.dialog_title, 'CONMINdriver: top.driver')
    outputs = editor.get_parameters()
    expected = [
        ["('preproc.x_in[0]', 'preproc.x_in[1]', 'preproc.x_in[2]', 'preproc.x_in[3]')",
         '-10', '99', '', '', '', 
         "('preproc.x_in[0]', 'preproc.x_in[1]', 'preproc.x_in[2]', 'preproc.x_in[3]')"],
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])
    editor.close()

#FIXME: can't seem to do context-click on output port.

    top.display_driverflows(False)
    time.sleep(0.5)
    top.display_dataflows(True)
    time.sleep(0.5)
    top.display_driverflows(True)
    time.sleep(0.5)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_replace(browser):
    # Replaces various connected components.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'rosen_suzuki.py')
    workspace_page.add_file(filename)

    # Replace 'top' with Simulation.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page.add_library_item_to_dataflow('rosen_suzuki.Simulation', 'top')

    # Show dataflow for Simulation.
    workspace_page.show_dataflow('top')
    workspace_page.hide_left()

    # Verify preproc is a PreProc.
    preproc = workspace_page.get_dataflow_figure('preproc', 'top')
    editor = preproc.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
        ['x_in', 'ndarray', '[ 1. 1. 1. 1.]', '',  'true', '', '', ''],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace preproc with a ScalingPreProc.
    workspace_page.replace('preproc', 'rosen_suzuki.ScalingPreProc')
    preproc = workspace_page.get_dataflow_figure('preproc', 'top')
    editor = preproc.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
        ['scaler', 'float', '1', '', 'true', '', '', ''],
        ['x_in', 'ndarray', '[ 1. 1. 1. 1.]', '', 'true', '', '', ''],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify postproc is a PostProc.
    postproc = workspace_page.get_dataflow_figure('postproc', 'top')
    editor = postproc.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
        ['result_in', 'float', '0', '', 'false', '', "['parent.comp.result']", ''],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace postproc with a ScalingPostProc.
    workspace_page.replace('postproc', 'rosen_suzuki.ScalingPostProc')
    postproc = workspace_page.get_dataflow_figure('postproc', 'top')
    editor = postproc.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
        ['result_in', 'float', '0', '', 'false', '', "['parent.comp.result']", ''],
        ['scaler', 'float', '1', '', 'true', '', '', ''],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify driver is a CONMINdriver.
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    eq(inputs.value[0],
       ['cons_is_linear', 'ndarray', '[]', '', 'true',
        'Array designating whether each constraint is linear.', '', ''])
    editor.close()

    # Replace driver with an SLSQPdriver.
    workspace_page.replace('driver',
                           'openmdao.lib.drivers.slsqpdriver.SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    eq(inputs.value[0],
       ['accuracy', 'float', '0.000001', '', 'true', 'Convergence accuracy', '', ''])
    editor.close()

    # Verify comp is a OptRosenSuzukiComponent.
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    editor = comp.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
        ['x', 'ndarray', '[]', '', 'false', '', "['parent.preproc.x_out']", "['driver']"],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace comp with an Assembly.
    workspace_page.replace('comp', 'openmdao.main.assembly.Assembly')
    message = NotifierPage.wait(workspace_page)
    eq(message, "RuntimeError: top: Can't connect 'comp.result' to"
                " 'postproc.result_in': top: Can't find 'comp.result'")
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    editor = comp.editor_page()
    editor.move(-100, 0)
    inputs = editor.get_inputs()
    expected = [
        ['directory',     'str',  '',      '',  'true',
         'If non-blank, the directory to execute in.', '', ''],
        ['force_execute', 'bool', 'False', '',  'true',
         'If True, always execute even if all IO traits are valid.', '', ''],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify new figure.
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    background = comp('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_ordering(browser):
    # Verify that adding parameter to driver moves it ahead of target.
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add ExternalCode and SLSQP.
    workspace_page.show_dataflow('top')
    ext = workspace_page.add_library_item_to_dataflow(
              'openmdao.lib.components.external_code.ExternalCode', 'ext',
              prefix='top')
    opt = workspace_page.add_library_item_to_dataflow(
              'openmdao.lib.drivers.slsqpdriver.SLSQPdriver', 'opt',
              prefix='top')

    # Check that ExternalCode is before SLSQP.
    assert ext.coords[0] < opt.coords[0]

    # Add parameter to SLSQP.
    editor = opt.editor_page(base_type='Driver')
    editor('parameters_tab').click()
    dialog = editor.new_parameter()
    dialog.target = 'ext.timeout'
    dialog.low = '0'
    dialog.high = '1'
    dialog.name = 'tmo'
    dialog('ok').click()

    # Check that SLSQP is now ahead of ExternalCode.
    ext = workspace_page.get_dataflow_figure('ext', 'top')
    opt = workspace_page.get_dataflow_figure('opt', 'top')
    assert ext.coords[0] > opt.coords[0]

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


if __name__ == '__main__':
    main()
