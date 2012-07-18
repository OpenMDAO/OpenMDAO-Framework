"""
Tests of dataflow functions.
"""

import logging
import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup


if sys.platform != 'win32':  # No testing on Windows yet.
    from util import setup_server, teardown_server, generate, begin, new_project
    from selenium.common.exceptions import StaleElementReferenceException

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_maxmin(browser):
    print "running _test_maxmin..."
    # Toggles maxmimize/minimize button on assemblies.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # verify that the globals figure is invisible
    globals_figure = workspace_page.get_dataflow_figure('')
    assert globals_figure.border.find('none') >= 0
    eq(globals_figure.background_color, 'rgba(0, 0, 0, 0)')

    # Add maxmin.py to project
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'maxmin.py')
    editor_page.add_file(file_path)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Add MaxMin to 'top'.
    workspace_page.show_dataflow('top')
    time.sleep(1)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'top'])
    workspace_page('library_tab').click()
    time.sleep(1)
    workspace_page.find_library_button('MaxMin').click()
    workspace_page.add_library_item_to_dataflow('maxmin.MaxMin', 'maxmin')
    time.sleep(1)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'maxmin', 'top'])

    workspace_page.hide_left()
    workspace_page.hide_right()
    workspace_page.hide_console()

    # Maximize maxmin.
    maxmin = workspace_page.get_dataflow_figure('maxmin')
    background = maxmin('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0

    maxmin('top_right').click()
    background = maxmin('top_right').value_of_css_property('background')
    time.sleep(1)
    assert background.find('circle-minus.png') >= 0
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'maxmin', 'sub', 'top'])

    sub = workspace_page.get_dataflow_figure('sub')
    sub('top_right').click()
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'driver', 'extcode', 'maxmin', 'sub', 'top'])

    # issue a command and make sure maxmin is still maximized
    workspace_page.show_console()
    time.sleep(0.5)
    workspace_page.do_command('dir()')
    background = maxmin('top_right').value_of_css_property('background')
    assert background.find('circle-minus.png') >= 0
    time.sleep(1)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'driver', 'extcode', 'maxmin', 'sub', 'top'])

    # Minimize sub
    sub('top_right').click()
    background = sub('top_right').value_of_css_property('background')
    assert background.find('circle-plus.png') >= 0
    time.sleep(1)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'driver', 'maxmin', 'sub', 'top'])

    # remove maxmin and make sure it'c children are removed as well
    maxmin.remove()
    time.sleep(1)
    eq(sorted(workspace_page.get_dataflow_component_names()),
       ['driver', 'top'])

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_maxmin complete."


def _test_connect(browser):
    print "running _test_connect..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Import connect.py
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'connect.py')
    editor_page.add_file(file_path)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Replace 'top' with connect.py's top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page('library_tab').click()
    for retry in range(5):
        try:
            workspace_page.find_library_button('Top').click()
        except StaleElementReferenceException:
            logging.warning('StaleElementReferenceException in _test_connect')
        else:
            break
    else:
        raise RuntimeError('Too many StaleElementReferenceExceptions')
    workspace_page.add_library_item_to_dataflow('connect.Top', 'top')

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
    eq(inputs[0].value, ['b_in', 'False'])
    inputs[0][1] = 'True'
    inputs = props.inputs
    eq(inputs[2].value, ['e_in', '1'])
    inputs[2][1] = '3'
    inputs = props.inputs
    eq(inputs[3].value, ['f_in', '0.0'])
    inputs[3][1] = '2.781828'
    inputs = props.inputs
    eq(inputs[5].value, ['i_in', '0'])
    inputs[5][1] = '42'
    inputs = props.inputs
    eq(inputs[6].value, ['s_in', ''])
    inputs[6][1] = "'xyzzy'"
    props.close()

    # Run the simulation.
    workspace_page.run()

    # Verify outputs.
    comp2 = workspace_page.get_dataflow_figure('comp2', 'top')
    editor = comp2.editor_page()
    eq(editor.dialog_title, 'Connectable: top.comp2')
    outputs = editor.get_outputs()
    expected = [
        ['b_out', 'bool',  'True',     '', 'true', '', ''],
        ['e_out', 'int',   '3',        '', 'true', '', ''],
        ['f_out', 'float', '2.781828', '', 'true', '', ''],
        ['i_out', 'int',   '42',       '', 'true', '', ''],
        ['s_out', 'str',   'xyzzy',    '', 'true', '', '']
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])
    editor.close()

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_connect complete."


def _test_connections(browser):
    print "running _test_connections..."
    # Check connection frame functionality.
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    filename = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                               'vehicle_singlesim.py')
    editor_page.add_file(filename)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Replace 'top' with VehicleSim.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page('library_tab').click()
    for retry in range(2):
        try:
            workspace_page.find_library_button('VehicleSim').click()
        except StaleElementReferenceException:
            logging.warning('StaleElementReferenceException in _test_connect')
        else:
            break
    else:
        raise RuntimeError('Too many StaleElementReferenceExceptions')
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

    # one connection between transmission and engine (RPM)
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    eq(conn_page.source_variable, '')
    eq(conn_page.target_variable, '')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # two connections between engine and chassis
    conn_page.set_source_component('engine')
    conn_page.set_target_component('chassis')
    eq(conn_page.source_variable, '')
    eq(conn_page.target_variable, '')
    eq(len(conn_page.get_variable_figures()), 4)
    eq(sorted(conn_page.get_variable_names()),
       ['engine_torque', 'engine_weight', 'mass_engine', 'torque'])

    # disconnect transmission
    tranny = workspace_page.get_dataflow_figure('transmission', 'sim.vehicle')
    tranny.disconnect()

    # now there are no connections between transmission and engine
    conn_page.set_source_component('transmission')
    conn_page.set_target_component('engine')
    eq(conn_page.check_variable_figures(), 0)

    # reconnect transmission RPM to engine RPM
    conn_page.connect_vars('transmission.RPM', 'engine.RPM')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['RPM', 'RPM'])

    # no connections between transmission and chassis
    conn_page.set_target_component('chassis')
    eq(conn_page.check_variable_figures(), 0)

    # reconnect transmission torque to chassis torque
    conn_page.connect_vars('transmission.torque_ratio', 'chassis.torque_ratio')
    eq(len(conn_page.get_variable_figures()), 2)
    eq(sorted(conn_page.get_variable_names()),
       ['torque_ratio', 'torque_ratio'])

    # no connections between vehicle assembly and transmission
    conn_page.set_source_component('')
    conn_page.set_target_component('transmission')
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
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_connections complete."


if __name__ == '__main__':
    if '--nonose' in sys.argv:
        # Run outside of nose.
        from util import setup_chrome  # , setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_connect(browser)
        _test_connections(browser)
        _test_maxmin(browser)
        browser.quit()
        teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
