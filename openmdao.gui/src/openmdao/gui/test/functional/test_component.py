import logging
import pkg_resources
import re
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

def _test_editable_inputs(browser):
    print "running _test_editable_inputs..."
    projects_page = begin(browser)
    project_info_page, project_dict = new_project(projects_page.new_project())
    workspace_page = project_info_page.load_project()

    # Import vehicle_singlesim
    workspace_window = browser.current_window_handle
    editor_page = workspace_page.open_editor()
    file_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                'vehicle_singlesim.py')
    editor_page.add_file(file_path)
    browser.close()
    browser.switch_to_window(workspace_window)

    # Replace 'top' with Vehicle ThreeSim  top.
    top = workspace_page.get_dataflow_figure('top')
    top.remove()
    workspace_page('libraries_tab').click()
    for retry in range(2):
        try:
            workspace_page.find_palette_button('VehicleSim').click()
        except StaleElementReferenceException:
            logging.warning('StaleElementReferenceException in _test_connect')
        else:
            break
    else:
        raise RuntimeError('Too many StaleElementReferenceExceptions')
    assembly_name = "sim"
    workspace_page.add_library_item_to_dataflow('vehicle_singlesim.VehicleSim',
            assembly_name)

    # Connect components.
    workspace_page.expand_object(assembly_name)
    time.sleep(1)
    workspace_page.show_dataflow(assembly_name + ".vehicle")
    transmission = workspace_page.get_dataflow_figure('transmission',
            assembly_name + '.vehicle')
    component_editor = transmission.editor_page()

    elements = component_editor.browser.find_elements_by_xpath(\
            "//div[@id='Inputs_props']")[1]
            #/div[@class='slick-viewport']")
            #/div[@id='grid-canvas']\
            #/div[@row='1'] | div[@row='3']")
    
    elements = elements.find_elements_by_xpath(\
            "div[@class='slick-viewport']\
            /div[@class='grid-canvas']\
            /div[@row='1' or @row='3']\
            /div[contains(@class, 'ui-state-highlight')]")
   
    for element in elements:
        assert("rgb(204, 204, 204)" == element.value_of_css_property("background-color"))
        assert("rgb(46, 125, 178)" == element.value_of_css_property("color"))

    component_editor.close()

    # Clean up.
    projects_page = workspace_page.close_workspace()
    project_info_page = projects_page.edit_project(project_dict['name'])
    project_info_page.delete_project()
    print "_test_editable_inputs complete."

if __name__ == '__main__':
    if '--nonose' in sys.argv:
        # Run outside of nose.
        from util import setup_chrome  # , setup_firefox
        setup_server(virtual_display=False)
        browser = setup_chrome()
        _test_editable_inputs(browser)
        browser.quit()
        teardown_server()
    else:
        # Run under nose.
        import nose
        sys.argv.append('--cover-package=openmdao.')
        sys.argv.append('--cover-erase')
        sys.exit(nose.runmodule())
