"""
Tests of dataflow functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout
from pageobjects.util import NotifierPage
from pageobjects.component import ComponentPage
from pageobjects.slot import find_slot_figure


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_maxmin(browser):
    # Toggles maxmimize/minimize button on assemblies.
    project_dict, workspace_page = startup(browser)

    # verify that the globals figure is invisible
    globals_figure = workspace_page.get_dataflow_figure('')
    assert globals_figure.border.find('none') >= 0
    eq(globals_figure.background_color, 'rgba(0, 0, 0, 0)')

    # Add maxmin.py to project
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/maxmin.py')
    workspace_page.add_file(file_path)

    # Add MaxMin to 'top'.
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
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
    closeout(project_dict, workspace_page)


def _test_connect(browser):
    project_dict, workspace_page = startup(browser)

    # Import connect.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/connect.py')
    workspace_page.add_file(file_path)
    workspace_page.add_library_item_to_dataflow('connect.Topp', 'top')

    # Connect components.
    workspace_page.show_dataflow('top')
    comp1 = workspace_page.get_dataflow_figure('comp1', 'top')
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    conn_page = workspace_page.connect(comp1, comp2)
    conn_page.move(-100, -100)
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
    props.move(0, -120)  # Move up for short displays.
    time.sleep(0.5)      # Wait for header update.
    eq(props.header, 'Connectable: top.comp1')
    props.move(-100, -100)
    inputs = props.inputs
    eq(inputs[4].value, ['s_in', ''])
    inputs[4][1] = 'xyzzy'
    inputs = props.inputs
    eq(inputs[2].value, ['f_in', '0'])
    inputs[2][1] = '2.781828'
    inputs = props.inputs
    eq(inputs[3].value, ['i_in', '0'])
    inputs[3][1] = '42'

    inputs = props.inputs
    eq(inputs[0].value, ['b_in', 'False'])
    inputs.rows[0].cells[1].click()
    browser.find_element_by_xpath('//*[@id="bool-editor-b_in"]/option[1]').click()
    #inputs.rows[0].cells[0].click()
    #inputs[0][1] = 'True'

    inputs = props.inputs
    eq(inputs[1].value, ['e_in', '1'])
    inputs.rows[1].cells[1].click()
    browser.find_element_by_xpath('//*[@id="editor-enum-e_in"]/option[3]').click()
    #inputs.rows[2].cells[0].click()
    #inputs[2][1] = '3'

    props.close()

    # Run the simulation.
    top = workspace_page.get_dataflow_figure('top')
    top.run()
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')

    # Verify outputs.
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    editor = comp2.editor_page()
    editor.move(-100, 0)
    eq(editor.dialog_title, 'Connectable: top.comp2')
    outputs = editor.get_outputs()
    expected = [
        ['', 'b_out', 'True', '', ''],
        ['', 'e_out', '3', '', ''],
        ['', 'f_out', '2.781828', '', ''],
        ['', 'i_out', '42', '', ''],
        ['', 's_out', 'xyzzy', '', ''],
        ['', 'derivative_exec_count', '0', '',
         "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count', '1', '',
         'Number of times this Component has been executed.'],
        ['', 'itername', '1-2', '', 'Iteration coordinates.'],
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])
    editor.close()

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_connections(browser):
    # Check connection frame functionality.
    project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                               'vehicle_singlesim.py')
    workspace_page.add_file(filename)

    asm_name = 'sim'
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
                                                asm_name)
    # show dataflow for vehicle
    workspace_page.expand_object('sim')
    workspace_page.show_dataflow('sim.vehicle')
    workspace_page.hide_left()
    vehicle = workspace_page.get_dataflow_figure('vehicle', 'sim')

    # no connections between assembly vars
    conn_page = vehicle.connections_page()
    conn_page.move(-50, -100)
    eq(conn_page.dialog_title, 'Connections: vehicle')
    eq(conn_page.source_component, '-- Assembly --')
    eq(conn_page.target_component, '-- Assembly --')
    eq(conn_page.count_variable_connections(), 0)

    # two connections between engine and chassis
    conn_page.set_source_component('engine')
    conn_page.set_target_component('chassis')
    eq(conn_page.count_variable_figures(), 20)
    eq(conn_page.count_variable_connections(), 2)
    conn_page.show_connected_variables()
    time.sleep(0.5)
    eq(conn_page.count_variable_figures(), 4)
    eq(conn_page.count_variable_connections(), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['engine_torque', 'engine_weight', 'mass_engine', 'torque'])

    # one connection between transmission and engine (RPM)
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    eq(conn_page.count_variable_figures(), 2)
    eq(conn_page.count_variable_connections(), 1)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # disconnect transmission
    conn_page.close()  # Sometimes obscures dataflow.
    tranny = workspace_page.get_dataflow_figure('transmission', 'sim.vehicle')
    tranny.disconnect()
    vehicle = workspace_page.get_dataflow_figure('vehicle', 'sim')
    conn_page = vehicle.connections_page()
    conn_page.move(-50, -100)
    conn_page.show_connected_variables()

    # now there are no connections between transmission and engine
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    time.sleep(0.5)
    eq(conn_page.count_variable_figures(), 0)
    eq(conn_page.count_variable_connections(), 0)

    # reconnect transmission RPM to engine RPM
    conn_page.connect_vars('transmission.RPM', 'engine.RPM')
    time.sleep(1)
    eq(conn_page.count_variable_figures(), 2)
    eq(conn_page.count_variable_connections(), 1)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # no connections between transmission and chassis
    conn_page.set_target_component('chassis')
    time.sleep(0.5)
    eq(conn_page.count_variable_figures(), 0)
    eq(conn_page.count_variable_connections(), 0)

    # reconnect transmission torque to chassis torque by dragging
    # conn_page.connect_vars('transmission.torque_ratio', 'chassis.torque_ratio')
    conn_page.show_all_variables()
    time.sleep(0.5)
    torque_vars = conn_page.find_variable_name('torque_ratio')
    eq(len(torque_vars), 2)
    chain = ActionChains(browser)
    chain.click_and_hold(torque_vars[0])
    chain.move_to_element(torque_vars[1])
    chain.release(on_element=None).perform()
    time.sleep(1.0)
    eq(conn_page.count_variable_connections(), 1)
    conn_page.show_connected_variables()
    time.sleep(0.5)
    eq(conn_page.count_variable_figures(), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['torque_ratio', 'torque_ratio'])

    # no connections between vehicle assembly and transmission
    conn_page.set_source_component('')
    conn_page.set_target_component('transmission')
    time.sleep(0.5)
    eq(conn_page.count_variable_figures(), 0)
    eq(conn_page.count_variable_connections(), 0)

    # connect assembly variable to component variable
    conn_page.connect_vars('current_gear', 'transmission.current_gear')
    eq(conn_page.count_variable_figures(), 2)
    eq(conn_page.count_variable_connections(), 1)
    eq(sorted(conn_page.get_variable_names()),
       ['current_gear', 'current_gear'])

    # one connection from chassis component to vehicle assembly
    conn_page.set_source_component('chassis')
    conn_page.set_target_component('')
    eq(conn_page.count_variable_figures(), 2)
    eq(conn_page.count_variable_connections(), 1)
    eq(sorted(conn_page.get_variable_names()),
       ['acceleration', 'acceleration'])

    conn_page.close()

    # disconnect chassis
    chassis = workspace_page.get_dataflow_figure('chassis', 'sim.vehicle')
    chassis.disconnect()
    vehicle = workspace_page.get_dataflow_figure('vehicle', 'sim')

    conn_page = vehicle.connections_page()
    conn_page.move(-50, -100)

    eq(conn_page.count_variable_connections(), 0)

    # test invalid variable
    conn_page.connect_vars('chassis.acceleration', 'acceleration')
    message = NotifierPage.wait(workspace_page)
    eq(message, "Invalid source variable")

    # connect component variable to assembly variable
    conn_page.set_source_component('chassis')
    conn_page.connect_vars('chassis.acceleration', 'acceleration')
    eq(conn_page.count_variable_connections(), 1)
    conn_page.show_connected_variables()
    eq(sorted(conn_page.get_variable_names()),
       ['acceleration', 'acceleration'])

    conn_page.close()

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_connect_nested(browser):
    project_dict, workspace_page = startup(browser)

    # Import bem.py
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/bem.py')
    workspace_page.add_file(file_path)

    workspace_page.add_library_item_to_dataflow('bem.BEM', 'top')

    # get connection frame
    workspace_page.show_dataflow('top')
    top = workspace_page.get_dataflow_figure('top')
    conn_page = top.connections_page()

    # select BE0 and perf components
    conn_page.move(-100, -100)
    eq(conn_page.dialog_title, 'Connections: top')
    conn_page.set_source_component('BE0')
    conn_page.set_target_component('perf')
    eq(conn_page.source_component, 'BE0')
    eq(conn_page.target_component, 'perf')
    time.sleep(0.5)
    connection_count = conn_page.count_variable_connections()

    # check that array is not expanded
    delta_Cts = conn_page.find_variable_name('delta_Ct[0]')
    eq(len(delta_Cts), 0)

    # expand the destination array and connect the source to array variable
    delta_Cts = conn_page.find_variable_name('delta_Ct')
    eq(len(delta_Cts), 2)
    x0 = delta_Cts[0].location['x']
    x1 = delta_Cts[1].location['x']
    if x0 > x1:
        perf_delta_Ct = delta_Cts[0]
    else:
        perf_delta_Ct = delta_Cts[1]
    chain = ActionChains(browser)
    chain.double_click(perf_delta_Ct).perform()
    delta_Cts = conn_page.find_variable_name('delta_Ct[0]')
    eq(len(delta_Cts), 1)
    conn_page.connect_vars('BE0.delta_Ct', 'perf.delta_Ct[0]')
    time.sleep(0.5)
    eq(conn_page.count_variable_connections(), connection_count + 1)

    # switch source component, destination array should still be expanded
    conn_page.set_source_component('BE1')
    eq(conn_page.source_component, 'BE1')
    time.sleep(0.5)
    connection_count = conn_page.count_variable_connections()
    delta_Cts = conn_page.find_variable_name('delta_Ct[1]')
    eq(len(delta_Cts), 1)
    conn_page.connect_vars('BE1.delta_Ct', 'perf.delta_Ct[1]')
    time.sleep(0.5)
    eq(conn_page.count_variable_connections(), connection_count + 1)

    # check connecting var tree to var tree
    conn_page.set_source_component('-- Assembly --')
    eq(conn_page.source_component, '-- Assembly --')
    time.sleep(0.5)
    connection_count = conn_page.count_variable_connections()
    conn_page.connect_vars('free_stream', 'perf.free_stream')
    time.sleep(0.5)
    eq(conn_page.count_variable_connections(), connection_count + 1)

    # collapse delta_Ct array and confirm that it worked
    chain = ActionChains(browser)
    delta_Cts = conn_page.find_variable_name('delta_Ct')
    eq(len(delta_Cts), 1)
    chain.double_click(delta_Cts[0]).perform()
    delta_Cts = conn_page.find_variable_name('delta_Ct[0]')
    eq(len(delta_Cts), 0)

    # check connecting var tree variable to variable
    conn_page.set_target_component('BE0')
    eq(conn_page.target_component, 'BE0')
    time.sleep(0.5)
    connection_count = conn_page.count_variable_connections()
    free_streams = conn_page.find_variable_name('free_stream')
    eq(len(free_streams), 1)
    chain = ActionChains(browser)
    chain.double_click(free_streams[0]).perform()
    free_stream_V = conn_page.find_variable_name('free_stream.V')
    eq(len(free_stream_V), 1)
    free_stream_rho = conn_page.find_variable_name('free_stream.rho')
    eq(len(free_stream_rho), 1)
    conn_page.connect_vars('free_stream.rho', 'BE0.rho')
    time.sleep(0.5)
    eq(conn_page.count_variable_connections(), connection_count + 1)

    # Clean up.
    conn_page.close()
    closeout(project_dict, workspace_page)


def _test_driverflows(browser):
    # Excercises display of driver flows (parameters, constraints, objectives).
    project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/rosen_suzuki.py')
    workspace_page.add_file(filename)

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
        ['',
         "('preproc.x_in[0]', 'preproc.x_in[1]', 'preproc.x_in[2]', 'preproc.x_in[3]')",
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
    closeout(project_dict, workspace_page)


def _test_replace(browser):
    # Replaces various connected components.
    project_dict, workspace_page = startup(browser)

    filename = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/rosen_suzuki.py')
    workspace_page.add_file(filename)

    workspace_page.add_library_item_to_dataflow('rosen_suzuki.Simulation', 'top')

    # Show dataflow for Simulation.
    workspace_page.show_dataflow('top')
    workspace_page.hide_left()

    # Verify preproc is a PreProc.
    preproc = workspace_page.get_dataflow_figure('preproc', 'top')
    editor = preproc.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'x_in', '[1.0, 1.0, 1.0, 1.0]', '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace preproc with a ScalingPreProc.
    workspace_page.replace('preproc', 'rosen_suzuki.ScalingPreProc')
    preproc = workspace_page.get_dataflow_figure('preproc', 'top')
    editor = preproc.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'scaler', '1', '', ''],
        ['', 'x_in', '[1.0, 1.0, 1.0, 1.0]', '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify postproc is a PostProc.
    postproc = workspace_page.get_dataflow_figure('postproc', 'top')
    editor = postproc.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'result_in', '0', '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace postproc with a ScalingPostProc.
    workspace_page.replace('postproc', 'rosen_suzuki.ScalingPostProc')
    postproc = workspace_page.get_dataflow_figure('postproc', 'top')
    editor = postproc.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'result_in', '0', '', ''],
        ['', 'scaler', '1', '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify driver is a CONMINdriver.
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    eq(inputs.value[0],
       ['', 'cons_is_linear', '[]', '',
        'Array designating whether each constraint is linear.'])
    editor.close()

    # Replace driver with an SLSQPdriver.
    workspace_page.replace_driver('top', 'SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    eq(inputs.value[0],
       ['', 'accuracy', '0.000001', '', 'Convergence accuracy'])
    editor.close()

    # Verify comp is a OptRosenSuzukiComponent.
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    editor = comp.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'x', '[]', '', ''],
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Replace comp with an Assembly.
    workspace_page.replace('comp', 'openmdao.main.assembly.Assembly')
    expected = "RuntimeError: top: Can't connect 'comp.result' to" \
               " 'postproc.result_in': top: Can't find 'comp.result'"
    time.sleep(0.5)
    assert workspace_page.history.endswith(expected)

    comp = workspace_page.get_dataflow_figure('comp', 'top')
    editor = comp.editor_page()
    editor.move(-400, 0)
    inputs = editor.get_inputs()
    expected = [
        ['', 'directory', '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute', 'False', '',
         'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])
    editor.close()

    # Verify new figure.
    comp = workspace_page.get_dataflow_figure('comp', 'top')
    background = comp('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_ordering(browser):
    # Verify that adding parameter to driver moves it ahead of target.
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
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
    editor.move(-100, -100)
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
    editor.close()
    closeout(project_dict, workspace_page)


def _test_io_filter_without_vartree(browser):

    project_dict, workspace_page = startup(browser)
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.replace_driver('top', 'CONMINdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page()

    #Test filtering inputs

    #filter on name='ctlmin'
    editor.filter_inputs("ctlmin")
    eq([u'', u'ctlmin', u'0.001', u'', u'Minimum absolute value of ctl used in optimization.'], editor.get_inputs().value[0])
    editor.clear_inputs_filter()

    #filter on description='conjugate'
    editor.filter_inputs("conjugate")
    eq([u'', u'icndir', u'0', u'', u'Conjugate gradient restart. parameter.'], editor.get_inputs().value[0])
    editor.clear_inputs_filter()

    #filter on description='Conjugate'
    editor.filter_inputs("Conjugate")
    eq([u'', u'icndir', u'0', u'', u'Conjugate gradient restart. parameter.'], editor.get_inputs().value[0])
    editor.clear_inputs_filter()

    #filter on term='print'
    #filter should match items in name and description column
    expected = [
        [u'', u'iprint', u'0', u'', u'Print information during CONMIN solution. Higher values are more verbose. 0 suppresses all output.'],
        [u'', u'printvars', u'[]', u'', u'List of extra variables to output in the recorders.']
    ]

    editor.filter_inputs("print")
    inputs = editor.get_inputs()
    eq(expected, inputs.value)

    # Verify that editing a value doesn't clear the filter.
    inputs[0].cells[2].select(1)
    expected[0][2] = u'1'
    inputs = editor.get_inputs()
    eq(expected, inputs.value)

    editor.clear_inputs_filter()

    editor.show_outputs()

    #Test filtering outputs

    #filter on name='derivative_exec_count'
    editor.filter_outputs("derivative_exec_count")
    eq([u'', u'derivative_exec_count', u'0', u'', u"Number of times this Component's derivative function has been executed."], editor.get_outputs().value[0])
    editor.clear_outputs_filter()

    #filter on description='coordinates'
    editor.filter_outputs("coordinates")
    eq([u'', u'itername', u'', u'', u"Iteration coordinates."], editor.get_outputs().value[0])
    editor.clear_outputs_filter()

    #filter on term='time'.
    editor.filter_outputs("time")
    expected = [
        [u'', u'derivative_exec_count', u'0', u'', u"Number of times this Component's derivative function has been executed."],
        [u'', u'exec_count', u'0', u'',  u"Number of times this Component has been executed."]
    ]

    eq(expected, editor.get_outputs().value)

    #filter on term='Time'.
    editor.filter_outputs("Time")
    expected = [
        [u'', u'derivative_exec_count', u'0', u'', u"Number of times this Component's derivative function has been executed."],
        [u'', u'exec_count', u'0', u'', u"Number of times this Component has been executed."]
    ]

    eq(expected, editor.get_outputs().value)
    editor.close()

    closeout(project_dict, workspace_page)


def _test_io_filter_with_vartree(browser):
    project_dict, workspace_page = startup(browser)

    #Test filtering variable trees
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/model_vartree.py')
    workspace_page.add_file(file_path)
    workspace_page.add_library_item_to_dataflow('model_vartree.Topp', "vartree", prefix=None)
    workspace_page.show_dataflow("vartree")

    comp = workspace_page.get_dataflow_figure('p1', "vartree")
    editor = comp.editor_page()
    inputs = editor.get_inputs()
    #editor.move(-100, 0)

    #filter when tree is expanded, filter on name="b"
    editor.filter_inputs("b")
    expected = [
        [u'', u' cont_in', u'', u'', u''],
        [u'', u' vt2', u'', u'', u''],
        [u'', u' vt3', u'', u'', u''],
        [u'', u'b', u'12', u'inch', u''],
        [u'', u'directory', u'', u'', u'If non-blank, the directory to execute in.']
    ]

    eq(expected, editor.get_inputs().value)
    time.sleep(3)

    #filter when tree is collapsed, filter on units="ft"
    editor.filter_inputs("ft")
    expected = [
        [u'', u' cont_in', u'', u'', u''],
        [u'', u' vt2', u'', u'', u''],
        [u'', u' vt3', u'', u'', u''],
        [u'', u'a', u'1', u'ft', u''],
    ]
    eq(expected, editor.get_inputs().value)

    editor.show_outputs()

    #filter when tree is expanded, filter on name="b"
    editor.filter_outputs("b")
    expected = [
        [u'', u' cont_out', u'', u'', u''],
        [u'', u' vt2', u'', u'', u''],
        [u'', u' vt3', u'', u'', u''],
        [u'', u'b', u'12', u'inch', u''],
        [u'', u'derivative_exec_count', u'0', u'', u"Number of times this Component's derivative function has been executed."],
        [u'', u'exec_count', u'0', u'', u"Number of times this Component has been executed."]
    ]

    eq(expected, editor.get_outputs().value)
    time.sleep(3)

    #filter when tree is collapsed, filter on units="ft"
    editor.filter_outputs("ft")
    expected = [
        [u'', u' cont_out', u'', u'', u''],
        [u'', u' vt2', u'', u'', u''],
        [u'', u' vt3', u'', u'', u''],
        [u'', u'a', u'1', u'ft', u''],
    ]
    eq(expected, editor.get_outputs().value)

    editor.close()
    closeout(project_dict, workspace_page)


def _test_column_sorting(browser):
    Version = ComponentPage.Version
    SortOrder = ComponentPage.SortOrder

    def test_sorting(expected, grid, sort_order):
        names = None
        variables = None

        if (grid == "inputs"):
            editor.show_inputs()
            editor.sort_inputs_column("Name", sort_order)
            variables = editor.get_inputs()

        else:
            editor.show_outputs()
            editor.sort_outputs_column("Name", sort_order)
            variables = editor.get_outputs()

        names = [variable.name.value for variable in variables]

        for index, name in enumerate(names):
            eq(name, expected[index])

    project_dict, workspace_page = startup(browser)
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.replace_driver('top', 'SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(version=Version.NEW)

    test_sorting(
        ["accuracy", "iout", "iprint", "maxiter", "output_filename", "printvars", "directory", "force_execute"],
        "inputs",
        SortOrder.ASCENDING
    )

    test_sorting(
        ["force_execute", "directory", "printvars", "output_filename", "maxiter", "iprint", "iout", "accuracy"],
        "inputs",
        SortOrder.DESCENDING
    )

    test_sorting(
        ["error_code", "derivative_exec_count", "exec_count", "itername"],
        "outputs",
        SortOrder.ASCENDING
    )

    test_sorting(
        ["itername", "exec_count", "derivative_exec_count", "error_code"],
        "outputs",
        SortOrder.DESCENDING
    )

    editor.close()

    top = workspace_page.get_dataflow_figure('top')
    top.remove()

    workspace_page.reload_project()
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/model_vartree.py')
    workspace_page.add_file(file_path)
    workspace_page.add_library_item_to_dataflow('model_vartree.Topp', "apples", offset=(120, 90))
    #workspace_page.show_dataflow("vartree")

    comp = workspace_page.get_dataflow_figure('p1', "apples")
    editor = comp.editor_page(version=Version.NEW)

    editor.get_input(" cont_in").name.click()
    editor.get_input(" vt2").name.click()
    editor.get_input(" vt3").name.click()

    editor.get_output(" cont_out").name.click()
    editor.get_output(" vt2").name.click()
    editor.get_output(" vt3").name.click()

    #Testing sort for inputs

    test_sorting(
        [" cont_in", "v1", "v2", " vt2", " vt3", "a", "b", "x", "y", "directory", "force_execute"],
        "inputs",
        SortOrder.ASCENDING
    )

    test_sorting(
        ["force_execute", "directory", " cont_in", " vt2", "y", "x", " vt3", "b", "a", "v2", "v1"],
        "inputs",
        SortOrder.DESCENDING
    )

    #Testing sort for outputs

    test_sorting(
        [" cont_out", "v1", "v2", " vt2", " vt3", "a", "b", "x", "y", "derivative_exec_count", "exec_count", "itername"],
        "outputs",
        SortOrder.ASCENDING
    )

    test_sorting(
        ["itername", "exec_count", "derivative_exec_count", " cont_out", " vt2", "y", "x", " vt3", "b", "a", "v2", "v1"],
        "outputs",
        SortOrder.DESCENDING
    )

    editor.close()
    closeout(project_dict, workspace_page)


def _test_taborder(browser):
    # Replaces various connected components.
    project_dict, workspace_page = startup(browser)
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')

    # Replace driver with an SLSQPdriver.
    workspace_page.replace_driver('top', 'SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')

    eq(editor.get_tab_labels(),
       ['Inputs', 'Outputs', 'Parameters', 'Objectives', 'Constraints',
        'Triggers', 'Workflow', 'Slots'])

    editor.close()

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_column_picking(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.replace_driver('top', 'SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page()

    expected_column_names = ["", "Name", "Value", "Units", "Description"]
    editor.show_inputs()

    input_column_names = [header.value for header in editor.inputs.headers]
    eq(input_column_names, expected_column_names)

    editor.show_outputs()

    output_column_names = [header.value for header in editor.outputs.headers]
    eq(output_column_names, expected_column_names)

    editor.close()
    top = workspace_page.get_dataflow_figure('driver', 'top')
    editor = top.editor_page()

    #Testing for Inputs tab

    #Test that the default columns are loaded first
    expected_column_names = ["", "Name", "Value", "Units", "Description"]

    editor.show_inputs()
    input_column_names = [header.value for header in editor.inputs.headers]

    eq(input_column_names, expected_column_names)

    #Test that low, high and type are added
    editor.toggle_column_visibility("Low")
    editor.toggle_column_visibility("High")
    editor.toggle_column_visibility("Type")

    expected_column_names[2:2] = ["Type"]
    expected_column_names[4:4] = ["High"]
    expected_column_names[5:5] = ["Low"]

    input_column_names = [header.value for header in editor.inputs.headers]

    eq(input_column_names, expected_column_names)

    #Test that the name and description columns are removed
    editor.toggle_column_visibility("Name")
    editor.toggle_column_visibility("Description")

    del expected_column_names[1]
    del expected_column_names[-1]

    input_column_names = [header.value for header in editor.inputs.headers]

    eq(input_column_names, expected_column_names)

    #Testing for Outputs tab

    #Test that the default columns are loaded first.
    editor.show_outputs()
    expected_column_names = ["", "Name", "Value", "Units", "Description"]

    output_column_names = [header.value for header in editor.outputs.headers]
    eq(output_column_names, expected_column_names)

    #Test that the units and name columns are removed
    #column_picker = editor.outputs.headers[0].get_column_picker()

    editor.toggle_column_visibility("Units")
    editor.toggle_column_visibility("Name")

    output_column_names = [header.value for header in editor.outputs.headers]

    del expected_column_names[1]
    del expected_column_names[2]

    eq(output_column_names, expected_column_names)

    #Test that the low column is shown
    editor.toggle_column_visibility("Low")

    expected_column_names[2:2] = ["Low"]
    output_column_names = [header.value for header in editor.outputs.headers]
    eq(output_column_names, expected_column_names)

    editor.close()

    editor = top.editor_page()

    #Reload the editor and check that the column settings
    #for the Inputs and Outputs tabs were recalled
    editor.show_inputs()
    expected_column_names = ["", "Type", "Value", "High", "Low", "Units"]
    input_column_names = [header.value for header in editor.inputs.headers]
    eq(input_column_names, expected_column_names)

    editor.show_outputs()
    expected_column_names = ["", "Value", "Low", "Description"]
    output_column_names = [header.value for header in editor.outputs.headers]
    eq(output_column_names, expected_column_names)

    editor.close()

    closeout(project_dict, workspace_page)


def _test_remove_tla(browser):
    # verify that adding, removing, and adding a top level assembly works.
    project_dict, workspace_page = startup(browser)
    eq(len(workspace_page.get_dataflow_figures()), 1)

    # create a top assembly and check number of figures
    workspace_page.add_library_item_to_dataflow(
        'openmdao.main.assembly.Assembly', 'top1')
    eq(len(workspace_page.get_dataflow_figures()), 3)

    # add component to top assembly and check for additional figure
    workspace_page.add_library_item_to_dataflow(
                    'openmdao.lib.components.external_code.ExternalCode', 'ext',
                    target_name='top1')
    eq(len(workspace_page.get_dataflow_figures()), 4)

    # remove top and check that it and it's child figures are gone
    top = workspace_page.get_dataflow_figure('top1')
    top.remove()
    eq(len(workspace_page.get_dataflow_figures()), 1)

    # add a new top, verify on screen.
    workspace_page.add_library_item_to_dataflow(
        'openmdao.main.assembly.Assembly', 'top2')
    eq(len(workspace_page.get_dataflow_figures()), 3)

    # clean up
    closeout(project_dict, workspace_page)


def _test_display_differentiator(browser):
    # Verify that we can display a differentiator (based on Container).
    project_dict, workspace_page = startup(browser)
    eq(len(workspace_page.get_dataflow_figures()), 1)

    # Create assembly with an SLSQPdriver.
    workspace_page.add_library_item_to_dataflow(
        'openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')
    workspace_page.replace_driver('top', 'SLSQPdriver')
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(base_type='Driver')
    editor.move(-400, 0)

    # Display & verify differentiator.
    editor.show_slots()
    diff = find_slot_figure(workspace_page, 'differentiator',
                            prefix='top.driver')
    diff_editor = diff.edit()
    inputs = diff_editor.get_inputs()
    expected = [
        ['', 'default_stepsize', '0.000001', '',
         'Default finite difference step size.'],
        ['', 'form', 'central', '',
         'Finite difference form (central, forward, backward).'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    # Clean up.
    diff_editor.close()
    editor.close()
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
