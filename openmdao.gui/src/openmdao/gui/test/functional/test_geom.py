"""
Tests of overall workspace functions.
"""

import os
import time
import filecmp

import pkg_resources

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.gui.test.functional.util import main, \
                setup_server, teardown_server, generate, \
                startup, closeout, put_element_on_grid

from openmdao.gui.test.functional.pageobjects.slot import find_slot_figure


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
    slot = find_slot_figure(workspace_page, 'parametric_geometry', prefix=geom_comp_name)
    workspace_page.fill_slot_from_library(slot, 'BoxParametricGeometry')

    # Should be one window before we open the geom window
    eq(len(browser.window_handles), 1)

    # Open the geom window
    geom_comp_editor('outputs_tab').click()
    outputs = geom_comp_editor.get_outputs()
    outputs.rows[0].cells[2].click()

    time.sleep(2)  # wait to make sure it is displayed

    # Should be two windows now
    eq(len(browser.window_handles), 2)

    # switch to the geom window so we can take a screenshot of it
    geom_window = browser.window_handles[-1]
    browser.switch_to_window(geom_window)

    # Compare it to what we expect to get
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/box-geom-screenshot.png')

    # FIXME: there are still problems with diffing the PNG files.  Not sure
    # if there are differences due to platform or what.  Also on windows
    # document.getElementById("statusline") returns null (could be just a timing thing)
    #  For now, just commenting all of this out until someone has time to
    #  fix it and verify it works on all 3 platforms
    # hide the framerate status line
    #browser.execute_script( 'document.getElementById("statusline").style.display = "none"')
    #browser.save_screenshot( "geom.png")
    #assert filecmp.cmp( "geom.png", file_path)

    #try:
    #    os.remove("geom.png")
    #except IOError:
    #    pass

    # Need to do this otherwise the close out fails
    workspace_window = browser.window_handles[0]
    browser.switch_to_window(workspace_window)

    closeout(project_dict, workspace_page)

if __name__ == '__main__':
    main()
