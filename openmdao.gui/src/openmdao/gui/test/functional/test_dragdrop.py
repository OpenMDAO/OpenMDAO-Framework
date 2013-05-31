"""
Tests of drag and drop functionality.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.component import NameInstanceDialog


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_drop_on_driver(browser):
    project_dict, workspace_page = startup(browser)

    # replace the 'top' assembly driver with a CONMINdriver
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.replace_driver('top', 'CONMINdriver')

    # Check to see that the content area for the driver is now CONMINdriver
    driver_element = workspace_page.get_dataflow_figure('driver')
    eq(driver_element('content_area').find_element_by_xpath('center/i').text,
        'CONMINdriver', "Dropping CONMINdriver onto existing driver did not replace it")

    closeout(project_dict, workspace_page)


def _test_workspace_dragdrop(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    top = workspace_page.get_dataflow_figure('top')

    assembly = workspace_page.find_library_button('Assembly')

    names = []
    for div in top.get_drop_targets():
        chain = workspace_page.drag_element_to(assembly, div, False)
        workspace_page.check_highlighting(top('content_area').element, True,
                           "Top's content_area")
        workspace_page.release(chain)

        #deal with the modal dialog
        name = NameInstanceDialog(workspace_page).create_and_dismiss()
        names.append(name)

    workspace_page.ensure_names_in_workspace(names,
        "Dragging 'assembly' to 'top' in one of the drop areas did not "
        "produce a new element on page")

    # now test to see if all the new elements are children of 'top'

    # generate what the pathnames SHOULD be
    guess_pathnames = ["top." + name for name in names]

    # get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [fig.get_pathname() for fig in figs]

    # see if they match up! (keeping in mind that there are more elements
    # we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True, "An element did not drop into 'top' when "
           "dragged onto one of its drop areas.\nIt was created somewhere else")

    closeout(project_dict, workspace_page)


def _test_drop_on_existing_assembly(browser):
    project_dict, workspace_page = startup(browser)

    assembly = workspace_page.find_library_button('Assembly')

    outer_name = workspace_page.put_element_on_grid('Assembly')
    outer_figure = workspace_page.get_dataflow_figure(outer_name)
    outer_path = outer_figure.pathname

    eq(outer_path, outer_name, "Assembly did not produce an instance on the grid")

    div = outer_figure.get_drop_targets()[0]
    chain = workspace_page.drag_element_to(assembly, div, False)
    workspace_page.check_highlighting(outer_figure('content_area').element, True,
                                      "Assembly's content_area")
    workspace_page.release(chain)

    middle_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    middle_figure = workspace_page.get_dataflow_figure(middle_name)
    middle_path = middle_figure.pathname

    eq(middle_path, outer_path + '.' + middle_name,
        "Assembly did not produce an instance inside outer Assembly")

    div = middle_figure.get_drop_targets()[0]
    chain = workspace_page.drag_element_to(assembly, div, True)
    workspace_page.check_highlighting(middle_figure('content_area').element, True,
                       "Assembly's content_area")
    workspace_page.release(chain)

    inner_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    #expand the middle div so that the inner one shows up in the workspace.
    middle_figure('top_right').element.click()
    inner_figure = workspace_page.get_dataflow_figure(inner_name)
    inner_path = inner_figure.pathname

    eq(inner_path, middle_path + '.' + inner_name,
        "Assembly did not produce an instance inside of the middle Assembly")

    workspace_page.ensure_names_in_workspace([outer_name, middle_name, inner_name],
        "Dragging Assembly onto Assembly did not create a new instance on page")

    closeout(project_dict, workspace_page)


def _test_drop_on_component_editor(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    top = workspace_page.get_dataflow_figure('top', '')

    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')

    editor = top.editor_page(double_click=False, base_type='Assembly')
    editor.show_dataflow()

    # move the editor window down and to the left, away from the library
    editor.move(-200, 200)

    # in order to get the elements in the editor dataflow, we must
    # distinguish them from the elements in the main dataflow
    editor_top = workspace_page.get_dataflow_fig_in_globals('top')

    # sort through these to find the correct 'top'
    names = []
    for div in editor_top.get_drop_targets()[:-1]:
        chain = workspace_page.drag_element_to(assembly, div, False)
        time.sleep(1)
        workspace_page.check_highlighting(editor_top('content_area').element,
            True, "Top in component editor's content_area")
        workspace_page.release(chain)

        #deal with the modal dialog
        name = NameInstanceDialog(workspace_page).create_and_dismiss()
        names.append(name)

    workspace_page.ensure_names_in_workspace(names,
        "Dragging 'assembly' to 'top' (in component editor) in one of the "
        "drop areas did not produce a new element on page")

    #now test to see if all the new elements are children of 'top'

    #generate what the pathnames SHOULD be
    guess_pathnames = ["top." + name for name in names]

    #get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [fig.get_pathname() for fig in figs]

    # see if they match up! (keeping in mind that there are more elements
    # we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True,
           "An element did not drop into 'top' (in component editor) when "
           "dragged onto one of its drop areas.\nIt was created somewhere else")

    closeout(project_dict, workspace_page)


def _test_drop_on_component_editor_grid(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    top = workspace_page.get_dataflow_figure('top', '')

    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')

    editor = top.editor_page(double_click=False, base_type='Assembly')
    editor.show_dataflow()

    editor_top = workspace_page.get_dataflow_fig_in_globals('top')

    # sort through these to find the correct 'top'

    chain = ActionChains(browser)
    chain.click_and_hold(assembly)
    chain.move_to_element(editor_top('header').find_element_by_xpath("..")).perform()
    chain.move_by_offset(200, 1).perform()
    chain.release(None).perform()

    # don't bother checking to see if it appeared,
    # the UI box will appear and screw the test if it did

    closeout(project_dict, workspace_page)


def _test_component_to_complex_workflow(browser):
    project_dict, workspace_page = startup(browser)

    # Add paraboloid and vehicle_threesim files
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')
    file2_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                 'vehicle_threesim.py')
    workspace_page.add_file(file1_path)
    workspace_page.add_file(file2_path)

    # create an instance of VehicleSim2
    sim_name = workspace_page.put_element_on_grid("VehicleSim2")

    # Drag paraboloid element into sim dataflow figure
    sim = workspace_page.get_dataflow_figure(sim_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = workspace_page.drag_element_to(paraboloid, sim('content_area').element, False)
    workspace_page.release(chain)
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    paraboloid_pathname = sim_name + "." + paraboloid_name

    # Switch to Workflow pane and show the sim workflow
    workspace_page('workflow_tab').click()
    workspace_page.show_workflow(sim_name)

    # See how many workflow component figures there are before we add to it
    eq(len(workspace_page.get_workflow_component_figures()), 16)

    ############################################################################
    # Drop paraboloid component onto the top level workflow for sim
    ############################################################################
    workspace_page('dataflow_tab').click()
    workspace_page.expand_object(sim_name)
    workspace_page.add_object_to_workflow(paraboloid_pathname, sim_name)

    # Confirm that there is one more workflow component figure
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_component_figures()), 17)

    # Confirm that the paraboloid has been added to the sim workflow by trying
    # to access it.
    workspace_page.find_object_button(sim_name + "." + paraboloid_name)

    ############################################################################
    # Drop paraboloid component onto the sim_acc workflow under sim
    ############################################################################
    workspace_page('dataflow_tab').click()
    simsim_name = sim_name + '.sim_acc'
    workspace_page.add_object_to_workflow(paraboloid_pathname, simsim_name)

    # Confirm that there is one more workflow component figure
    workspace_page('workflow_tab').click()
    eq(len(workspace_page.get_workflow_component_figures()), 18)

    # Confirm that the paraboloid has been added to the sim workflow by trying
    # to access it.
    workspace_page.find_object_button(sim_name + "." + paraboloid_name)

    ############################################################################
    # Drop paraboloid component onto the vehicle workflow under sim_acc
    # This should NOT work since the paraboloid is not in the vehicle assembly
    ############################################################################

    # These error messages are tested in SequentialFlow, though we may want
    # to have one test that makes sure that the error dialog makes it through.

    #workspace_page('dataflow_tab').click()
    #workspace_page.expand_object(simsim_name)
    #simsimsim_name = simsim_name + '.vehicle'
    #workspace_page.add_object_to_workflow(paraboloid_pathname, simsimsim_name)
    #message = NotifierPage.wait(workspace_page)
    #eq(message, "x")

    # Confirm that there is NOT a new workflow component figure
    #workspace_page('workflow_tab').click()
    #eq(len(workspace_page.get_workflow_component_figures()), 18)

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_drop_onto_layered_div(browser):
    # FIXME: problem with test successfully DnDing from dataflow to workflow figure
    return

    project_dict, workspace_page = startup(browser)

    # Add paraboloid and vehicle_threesim files
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                'paraboloid.py')
    file2_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                            'vehicle_threesim.py')
    workspace_page.add_file(file1_path)
    workspace_page.add_file(file2_path)

    # add VehicleSim2 to the globals
    sim_name = workspace_page.put_element_on_grid('VehicleSim2')

    # add Paraboloid to VehicleSim dataflow assembly
    sim = workspace_page.get_dataflow_figure(sim_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = workspace_page.drag_element_to(paraboloid,
                            sim('content_area').element, False)
    workspace_page.release(chain)
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    paraboloid_pathname = sim_name + "." + paraboloid_name

    # Open up the component editor for the sim_EPA_city inside the vehicle sim
    sim_EPA_city_driver = workspace_page.get_dataflow_figure('sim_EPA_city',
                                                             sim_name)
    driver_editor = sim_EPA_city_driver.editor_page(base_type='Driver')
    driver_editor.move(800, 800)
    driver_editor.show_workflow()

    # Confirm expected number of workflow component figures before adding one
    eq(len(driver_editor.get_workflow_component_figures()), 5)
    eq(len(workspace_page.get_workflow_component_figures()), 22)

    # Drag paraboloid component into sim_EPA_city workflow
    workspace_page('dataflow_tab').click()
    workspace_page.add_object_to_workflow_figure(
        paraboloid_pathname, 'sim_EPA_city', target_page=driver_editor)

    # Confirm there is one more workflow component figure in the editor
    eq(len(driver_editor.get_workflow_component_figures()), 6)

    # Clean up.
    driver_editor.close()
    closeout(project_dict, workspace_page)


if __name__ == '__main__':
    main()
