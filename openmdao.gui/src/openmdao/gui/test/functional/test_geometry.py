"""
Tests of overall workspace functions.
"""

import time

import pkg_resources

# these imports are for comparing screen capture to existing file
# import os
# import filecmp

from nose.tools import eq_ as eq
from nose.tools import with_setup

from openmdao.gui.test.functional.util import main, \
                setup_server, teardown_server, generate, \
                startup, closeout

from openmdao.gui.test.functional.pageobjects.slot import find_slot_figure
from openmdao.gui.test.functional.pageobjects.geometry import GeometryPage


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_view_geometry(browser):
    project_dict, workspace_page = startup(browser)
    workspace_window = browser.current_window_handle

    #drop 'GeomComponent' onto the grid
    geom_comp_name = workspace_page.put_element_on_grid('GeomComponent')

    #find it on the page
    geom_comp = workspace_page.get_dataflow_figure(geom_comp_name)

    #open the 'edit' dialog on GeomComponent
    geom_comp_editor = geom_comp.editor_page(False)
    geom_comp_editor.show_slots()

    # Plug BoxParametricGeometry into parametric_geometry
    slot = find_slot_figure(workspace_page, 'parametric_geometry', prefix=geom_comp_name)
    workspace_page.fill_slot_from_library(slot, 'BoxParametricGeometry')

    # Open the geom window
    geom_comp_editor('outputs_tab').click()
    outputs = geom_comp_editor.get_outputs()
    outputs.rows[0].cells[2].click()

    time.sleep(2)  # wait to make sure it is displayed

    # Should be two windows now
    eq(len(browser.window_handles), 2)

    # switch to the geom window
    geom_window = browser.window_handles[-1]
    browser.switch_to_window(geom_window)

    # FIXME: there are still problems with diffing the PNG files.  Not sure
    # if there are differences due to platform or what.  Also on windows
    # document.getElementById("statusline") returns null (could be just a timing thing)
    #  For now, just commenting all of this out until someone has time to
    #  fix it and verify it works on all 3 platforms

    # Compare it to what we expect to get
    # file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
    #                                             'files/box-geom-screenshot.png')

    # hide the framerate status line
    #browser.execute_script( 'document.getElementById("statusline").style.display = "none"')
    #browser.save_screenshot( "geom.png")
    #assert filecmp.cmp( "geom.png", file_path)

    #try:
    #    os.remove("geom.png")
    #except IOError:
    #    pass

    # give it a bit
    time.sleep(3)

    geom_page = GeometryPage.verify(browser, workspace_page.port)

    # if we have a canvas... (some test platforms don't support canvas)
    if geom_page.has_canvas():
        # give it a bit more
        time.sleep(5)

        geom_page.expand_edges()
        edges = geom_page.get_edge_names()
        eq(edges, ['Edge 1', 'Edge 2', 'Edge 3', 'Edge 4', 'Edge 5', 'Edge 6'])

        edges = geom_page.get_edge('Edges')
        edge1 = geom_page.get_edge('Edge 1')
        edge2 = geom_page.get_edge('Edge 2')

        eq([edges.viz, edges.grd, edges.ori], [True, False, False])
        eq([edge1.viz, edge1.grd, edge1.ori], [True, False, False])
        eq([edge2.viz, edge2.grd, edge2.ori], [True, False, False])

        # toggle visibility for an edge
        edge1.viz = False

        # check for expected new edges state
        eq([edges.viz, edges.grd, edges.ori], [False, False, False])
        eq([edge1.viz, edge1.grd, edge1.ori], [False, False, False])
        eq([edge2.viz, edge2.grd, edge2.ori], [True, False, False])

        # toggle visibility for all edges
        edges.viz = True

        # check for expected new edges state
        eq([edges.viz, edges.grd, edges.ori], [True, False, False])
        eq([edge1.viz, edge1.grd, edge1.ori], [True, False, False])
        eq([edge2.viz, edge2.grd, edge2.ori], [True, False, False])

        # check initial state of faces tree
        geom_page.expand_faces()
        faces = geom_page.get_face_names()
        eq(faces, ['Face 1', 'Face 2', 'Face 3', 'Face 4', 'Face 5', 'Face 6'])

        faces = geom_page.get_face('Faces')
        face3 = geom_page.get_face('Face 3')
        face5 = geom_page.get_face('Face 5')

        eq([faces.viz, faces.grd, faces.trn], [True, False, False])
        eq([face3.viz, face3.grd, face3.trn], [True, False, False])
        eq([face5.viz, face5.grd, face5.trn], [True, False, False])

        # toggle transparency for a face
        face3.trn = True

        # check for expected new faces state
        eq([faces.viz, faces.grd, faces.trn], [True, False, False])
        eq([face3.viz, face3.grd, face3.trn], [True, False, True])
        eq([face5.viz, face5.grd, face5.trn], [True, False, False])

        # toggle grid for all faces
        faces.grd = True

        # check for expected new faces state
        eq([faces.viz, faces.grd, faces.trn], [True, True, False])
        eq([face3.viz, face3.grd, face3.trn], [True, True, True])
        eq([face5.viz, face5.grd, face5.trn], [True, True, False])

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_view_csm(browser):
    project_dict, workspace_page = startup(browser)
    workspace_window = browser.current_window_handle

    # add a CSM geometry file
    file_name = 'box.csm'
    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                               'files/box.csm')
    workspace_page.add_file(file_path)

    time.sleep(2)

    # view the CSM file
    geom_page = workspace_page.view_geometry(file_name)

    # if we have a canvas... (some test platforms don't support canvas)
    if geom_page.has_canvas():
        time.sleep(5)

        geom_page.expand_edges()
        edges = geom_page.get_edge_names()
        eq(edges, ['Body 1 Edge 1',  'Body 1 Edge 2',  'Body 1 Edge 3',
                   'Body 1 Edge 4',  'Body 1 Edge 5',  'Body 1 Edge 6',
                   'Body 1 Edge 7',  'Body 1 Edge 8',  'Body 1 Edge 9',
                   'Body 1 Edge 10', 'Body 1 Edge 11', 'Body 1 Edge 12'])

        geom_page.expand_faces()
        faces = geom_page.get_face_names()
        eq(faces, ['Body 1 Face 1',  'Body 1 Face 2',  'Body 1 Face 3',
                   'Body 1 Face 4',  'Body 1 Face 5',  'Body 1 Face 6'])

    # Back to workspace.
    browser.close()
    browser.switch_to_window(workspace_window)

    # Clean up.
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
