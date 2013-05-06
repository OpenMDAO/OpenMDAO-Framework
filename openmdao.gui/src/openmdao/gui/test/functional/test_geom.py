"""
Tests of overall workspace functions.
"""

import time
import filecmp

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.gui.test.functional.util import main, \
				 setup_server, teardown_server, generate, \
                 startup, closeout, put_element_on_grid

from openmdao.gui.test.functional.pageobjects.slot import SlotFigure

@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_view_geom(browser):
    project_dict, workspace_page = startup(browser)

    #drop 'GeomComponent' onto the grid
    geom_comp_name = put_element_on_grid(workspace_page, "GeomComponent")

    #find it on the page
    geom_comp = workspace_page.get_dataflow_figure(geom_comp_name)

    #open the 'edit' dialog on GeomComponent
    geom_comp_editor = geom_comp.editor_page(False)
    geom_comp_editor.show_slots()

    # Plug BoxParametricGeometry into parametric_geometry
    slot = SlotFigure(workspace_page, geom_comp_name + '.parametric_geometry')
    slot.fill_from_library('BoxParametricGeometry')

    # Should be one window before we open the geom window
    eq(len(browser.window_handles), 1)
    
    # Open the geom window
    geom_comp_editor('outputs_tab').click()
    outputs = geom_comp_editor.get_outputs()
    outputs.rows[0].cells[2].click()

    time.sleep(2) # wait to make sure it is displayed

    # Should be two windows now
    eq(len(browser.window_handles), 2)

    # switch to the geom window so we can take a screenshot of it
    geom_window = browser.window_handles[-1]
    browser.switch_to_window(geom_window)

    browser.save_screenshot( "geom.png")

    # Compare it to what we expect to get
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/box-geom-screenshot.png')
    assert filecmp.cmp( "geom.png", file_path)

    # Need to do this otherwise the close out fails
    workspace_window = browser.window_handles[0]
    browser.switch_to_window(workspace_window)

    closeout(project_dict, workspace_page)

if __name__ == '__main__':
    main()
