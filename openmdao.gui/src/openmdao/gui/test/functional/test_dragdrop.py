"""
Tests of overall workspace functions.
"""

import logging
import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

if sys.platform != 'win32':  # No testing on Windows yet.
    from selenium.webdriver.common.action_chains import ActionChains
    from selenium.webdriver.common.by import By

    from selenium.common.exceptions import StaleElementReferenceException

    from util import main, setup_server, teardown_server, generate, \
                     startup, closeout

    from pageobjects.component import NameInstanceDialog
    from pageobjects.dataflow import DataflowFigure
    from pageobjects.dialog import NotifyDialog
    from pageobjects.util import ConfirmationPage

    @with_setup(setup_server, teardown_server)
    def test_generator():
        for _test, browser in generate(__name__):
            yield _test, browser


def _test_drop_on_driver(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # replace the 'top' assembly driver with a CONMINdriver
    replace_driver(workspace_page, 'top', 'CONMINdriver')

    # Check to see that the content area for the driver is now CONMINdriver
    driver_element = workspace_page.get_dataflow_figure('driver')
    eq(driver_element('content_area').find_element_by_xpath('center/i').text,
        'CONMINdriver', "Dropping CONMINdriver onto existing driver did not replace it")

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_workspace_dragdrop(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    #find and get the 'assembly', and 'top' objects
    assembly = workspace_page.find_library_button('Assembly')
    top = workspace_page.get_dataflow_figure('top')

    names = []
    for div in getDropableElements(top):
        chain = drag_element_to(browser, assembly, div, False)
        check_highlighting(top('content_area').element, True,
                           "Top's content_area")
        release(chain)

        #deal with the modal dialog
        name = NameInstanceDialog(workspace_page).create_and_dismiss()
        names.append(name)

    ensure_names_in_workspace(workspace_page, names,
        "Dragging 'assembly' to 'top' in one of the drop areas did not "
        "produce a new element on page\n")

    # now test to see if all the new elements are children of 'top'

    # generate what the pathnames SHOULD be
    guess_pathnames = ["top." + name for name in names]

    # get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    # see if they match up! (keeping in mind that there are more elements
    # we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True, "An element did not drop into 'top' when "
           "dragged onto one of its drop areas.\nIt was created somewhere else")

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_drop_on_grid(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    #other tests also need to put an assembly on the grid, so put in seperate method
    put_assembly_on_grid(workspace_page)

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_drop_on_existing_assembly(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    assembly = workspace_page.find_library_button('Assembly')

    outer_name = put_assembly_on_grid(workspace_page)
    outer_figure = workspace_page.get_dataflow_figure(outer_name)
    outer_path = outer_figure.pathname

    eq(outer_path, outer_name, "Assembly did not produce an instance on the grid")

    div = getDropableElements(outer_figure)[0]
    chain = drag_element_to(browser, assembly, div, False)
    check_highlighting(outer_figure('content_area').element, True,
                       "Assembly's content_area")
    release(chain)

    middle_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    middle_figure = workspace_page.get_dataflow_figure(middle_name)
    middle_path = middle_figure.pathname

    eq(middle_path, outer_path + '.' + middle_name,
        "Assembly did not produce an instance inside outer Assembly")

    div = getDropableElements(middle_figure)[0]
    chain = drag_element_to(browser, assembly, div, True)
    check_highlighting(middle_figure('content_area').element, True,
                       "Assembly's content_area")
    release(chain)

    inner_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    #expand the middle div so that the inner one shows up in the workspace.
    middle_figure('top_right').element.click()
    inner_figure = workspace_page.get_dataflow_figure(inner_name)
    inner_path = inner_figure.pathname

    eq(inner_path, middle_path + '.' + inner_name,
        "Assembly did not produce an instance inside of the middle Assembly")

    ensure_names_in_workspace(workspace_page, [outer_name, middle_name, inner_name],
        "Dragging Assembly onto Assembly did not create a new instance on page")

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_drop_on_component_editor(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    #find and get the 'assembly', and 'top' objects
    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')
    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False, base_type='Assembly')
    editor.show_dataflow()

    #in order to get the elements in the editor workflow, we must
    #distinguish them from the elements in the main workflow
    editor_top = get_dataflow_fig_in_assembly_editor(workspace_page, 'top')
    # sort through these to find the correct 'top'
    names = []
    for div in getDropableElements(editor_top)[:-1]:
        chain = drag_element_to(browser, assembly, div, False)
        check_highlighting(editor_top('content_area').element, True,
                           "Top in component editor's content_area")
        release(chain)

        #deal with the modal dialog
        name = NameInstanceDialog(workspace_page).create_and_dismiss()
        names.append(name)

    ensure_names_in_workspace(workspace_page, names,
        "Dragging 'assembly' to 'top' (in component editor) in one of the "
        "drop areas did not produce a new element on page\n")

    #now test to see if all the new elements are children of 'top'

    #generate what the pathnames SHOULD be
    guess_pathnames = ["top." + name for name in names]

    #get the actual pathnames
    figs = workspace_page.get_dataflow_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    # see if they match up! (keeping in mind that there are more elements
    # we have pathnames for than we put there)
    for path in guess_pathnames:
        eq(path in pathnames, True,
           "An element did not drop into 'top' (in component editor) when "
           "dragged onto one of its drop areas.\nIt was created somewhere else")

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_drop_on_component_editor_grid(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)
    #find and get the 'assembly', and 'top' objects
    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')

    top = workspace_page.get_dataflow_figure('top', '')
    editor = top.editor_page(double_click=False, base_type='Assembly')
    editor.show_dataflow()

    editor_top = get_dataflow_fig_in_assembly_editor(workspace_page, 'top')

    # sort through these to find the correct 'top'

    chain = ActionChains(browser)
    chain.click_and_hold(assembly)
    chain.move_to_element(editor_top('header').find_element_by_xpath("..")).perform()
    chain.move_by_offset(200, 1).perform()
    release(chain)

    # don't bother checking to see if it appeared,
    # the UI box will appear and screw the test if it did

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_slots(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    top = workspace_page.get_dataflow_figure('top')

    editor, metamodel, caseiter, caserec, comp, meta_name = slot_reset(workspace_page)

    workspace_page.set_library_filter('ExecComp')
    execcomp = workspace_page.find_library_button('ExecComp')

    ##################################################
    # First part of test: Drag and drop ExecComp from the Library
    # onto the recorder slot of a MetaModel. This should fail.
    ##################################################
    #drag one success and one failure onto slots
    #failure:
    slot_drop(browser, execcomp, caserec, False, 'Component')

    slot_id = 'SlotFigure-%s-%s'

    #refresh
    time.sleep(1.0)  # give it a second to update the figure
    caserec = browser.find_element(By.ID, slot_id % (meta_name, 'recorder'))

    #check for class change
    eq(False, ("filled" in caserec.get_attribute('class')),
        "Component dropped into CaseRecorder (should not have)")

    ##################################################
    # Second part of test: Drag and drop ExecComp from the Library onto the
    # model (IComponent) slot of a MetaModel. This should be successful even
    # though a dialog will popup with this notification message:
    #   RuntimeError: m: surrogate must be set before the model or any includes/excludes of variables
    ##################################################
    slot_drop(browser, execcomp, comp, True, 'Component')

    NotifyDialog(browser, top.port).close()

    #refresh
    time.sleep(1.0)  # give it a second to update the figure
    comp = browser.find_element(By.ID, slot_id % (meta_name, 'model'))

    #check for class change
    eq(True, ("filled" in comp.get_attribute('class')),
        "Component did not drop into Component slot")

    #for the future:
    """
    # get the objects we need for the test
    # setup a data structure explaining which things can be dropped where
    # element, dropOnCaseIter, dropOnCaseRec, dropOnComp
    dropdata = [('CSVCaseIterator', True, False, False),\
                 ('CSVCaseRecorder', False, True, False),\
                 ('ExecComp', False, False, True),\
                 ('Assembly', False, False, True)]

    drop_elements = [(workspace_page.find_library_button(ele[0]), ele[1], ele[2], ele[3]) for ele in dropdata]

    #now loop through each dropable item, and see what happens when it lands on the target
    for ele in drop_elements:
        #drop on caseiter
        slot_drop(browser, ele[0].element, caseiter, ele[1], 'CaseIterator')
    #TODO: REFRESH THE SLOTS, CHECK THEIR FONT COLOR
        #drop on caserec
        slot_drop(browser, ele[0].element, caserec, ele[2], 'CaseRecorder')
    #TODO: REFRESH THE SLOTS, CHECK THEIR FONT COLOR
        #drop on comp
        slot_drop(browser, ele[0].element, comp, ele[3], 'Component')
    #TODO: REFRESH THE SLOTS, CHECK THEIR FONT COLOR

        editor, metamodel, caseiter, caserec, comp = slot_reset(workspace_page, editor, metamodel, True)
    """

    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_list_slot(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # replace the 'top' assembly driver with a DOEdriver
    # (this additionally verifies that an issue with DOEdriver slots is fixed)
    replace_driver(workspace_page, 'top', 'DOEdriver')

    # open the object editor dialog for the driver
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(False)
    editor.move(-100, 0)
    editor.show_slots()

    # get the generator slot figure
    slot_id = 'SlotFigure-%s-%s' % ('top-driver', 'DOEgenerator')
    generator_slot = browser.find_element(By.ID, slot_id)

    # check that slot is not filled
    eq(False, ("filled" in generator_slot.get_attribute('class')),
        "generator slot is showing as filled when it should not be")

    # drop a FullFactorial onto the generator slot
    workspace_page.set_library_filter('DOEgenerator')
    generator = workspace_page.find_library_button('FullFactorial')
    slot_drop(browser, generator, generator_slot, True, 'generator')

    # refresh
    time.sleep(1.0)  # give it a second to update the figure
    generator_slot = browser.find_element(By.ID, slot_id)

    # check for class change (should now be filled)
    eq(True, ("filled" in generator_slot.get_attribute('class')),
        "FullFactorial did not drop into generator slot")

    # get the recorders slot figure
    slot_id = 'SlotFigure-%s-%s' % ('top-driver', 'recorders')
    recorders_slot = browser.find_element(By.ID, slot_id)

    # check that slot is not filled
    eq(False, ("filled" in recorders_slot.get_attribute('class')),
        "recorders slot is showing as filled when it should not be")

    # set center pane to workflow to make sure workflow doesn't steal drops
    workspace_page('workflow_tab').click()

    # drop a DumpCaseRecorder onto the recorders slot
    workspace_page.set_library_filter('ICaseRecorder')
    case_recorder = workspace_page.find_library_button('DumpCaseRecorder')
    slot_drop(browser, case_recorder, recorders_slot, True, 'recorders')

    # refresh
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = browser.find_element(By.ID, slot_id)

    # check for class change (should now be filled)
    eq(True, ("filled" in recorders_slot.get_attribute('class')),
        "DumpCaseRecorder did not drop into recorders slot")

    # check that recorders fig now has one filled and one empty rect
    rects = recorders_slot.find_elements_by_css_selector('rect')
    eq(len(rects), 2)
    eq(True, ('stroke: #0b93d5' in rects[0].get_attribute('style')),
        "Filled slot element should be outlined in blue")
    eq(True, ('stroke: #808080' in rects[1].get_attribute('style')),
        "Unfilled slot element should be outlined in gray")

    klass = recorders_slot.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'DumpCaseRecorder',
        "Filled slot element should show the correct type (DumpCaseRecorder)")
    eq(klass[1].text, 'ICaseRecorder',
        "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # drop another CaseRecorder onto the recorders slot
    case_recorder = workspace_page.find_library_button('CSVCaseRecorder')
    slot_drop(browser, case_recorder, recorders_slot, True, 'recorders')

    # refresh
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = browser.find_element(By.ID, slot_id)

    # check for class change (it should not change... still filled)
    eq(True, ("filled" in recorders_slot.get_attribute('class')),
        "CSVCaseRecorder did not drop into recorders slot")

    # check that recorders fig now has two filled and one empty rect
    rects = recorders_slot.find_elements_by_css_selector('rect')
    eq(len(rects), 3)
    eq(True, ('stroke: #0b93d5' in rects[0].get_attribute('style')),
        "Filled slot element should be outlined in blue")
    eq(True, ('stroke: #0b93d5' in rects[1].get_attribute('style')),
        "Filled slot element should be outlined in blue")
    eq(True, ('stroke: #808080' in rects[2].get_attribute('style')),
        "Unfilled slot element should be outlined in gray")

    klass = recorders_slot.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'DumpCaseRecorder',
        "Filled slot element should show the correct type (DumpCaseRecorder)")
    eq(klass[1].text, 'CSVCaseRecorder',
        "Filled slot element should show the correct type (CSVCaseRecorder)")
    eq(klass[2].text, 'ICaseRecorder',
        "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # drop another CaseRecorder onto the recorders slot
    case_recorder = workspace_page.find_library_button('DBCaseRecorder')
    slot_drop(browser, case_recorder, recorders_slot, True, 'recorders')

    # refresh
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = browser.find_element(By.ID, slot_id)

    # check that recorders fig now has four total rects
    rects = recorders_slot.find_elements_by_css_selector('rect')
    eq(len(rects), 4)

    # remove an item from the list (the only context menu option)
    menu_item_remove = recorders_slot.find_element_by_css_selector('ul li')
    chain = ActionChains(browser)
    chain.move_to_element_with_offset(recorders_slot, 25, 25)
    chain.context_click(recorders_slot).perform()
    menu_item_remove.click()

    # refresh
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = browser.find_element(By.ID, slot_id)

    # check that recorders fig now has only three rect
    # TODO: check that the correct one was removed
    rects = recorders_slot.find_elements_by_css_selector('rect')
    eq(len(rects), 3)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_simple_component_to_workflow(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Get file paths
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')

    # add first file from workspace
    workspace_page.add_file(file1_path)

    # Drag element into top dataflow figure
    top = workspace_page.get_dataflow_figure('top')
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid, top('content_area').element, False)
    release(chain)
    #deal with the modal dialog
    name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()

    # Show the top level workflow
    workspace_page.show_workflow('top')
    time.sleep(0.5)  # Just so we can see it.

    eq(len(workspace_page.get_workflow_component_figures()), 1)

    # Drop the paraboloid component from the component tree onto the workflow for top
    workspace_page.expand_object('top')
    paraboloid_component = workspace_page.find_object_button('top.' + name)
    top = workspace_page.get_workflow_figure('top')
    chain = drag_element_to(browser, paraboloid_component, top.root, False)
    check_highlighting(top.root, True, "Top's workflow")
    release(chain)

    eq(len(workspace_page.get_workflow_component_figures()), 2)

    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    assert  ("top." + name) in pathnames

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_library_to_workflow(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Get file paths
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')

    # add first file from workspace
    workspace_page.add_file(file1_path)

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()

    # Show the top level workflow
    workspace_page.show_workflow('top')
    time.sleep(0.5)  # Just so we can see it.

    eq(len(workspace_page.get_workflow_component_figures()), 1)

    # Drop the paraboloid component from the library onto the workflow for top
    top = workspace_page.get_workflow_figure('top')
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid, top.root, True)
    chain.move_by_offset(int(paraboloid.value_of_css_property('width')[:-2])/3, 1).perform()
    check_highlighting(top.root, True, "Top's workflow")
    release(chain)
    #deal with the modal dialog
    name = NameInstanceDialog(workspace_page).create_and_dismiss()

    time.sleep(0.5)  # Just so we can see it.

    eq(len(workspace_page.get_workflow_component_figures()), 2)

    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]

    assert  ("top." + name) in pathnames

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_component_to_complex_workflow(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add paraboloid and vehicle_threesim files
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')
    file2_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                 'vehicle_threesim.py')
    workspace_page.add_file(file1_path)
    workspace_page.add_file(file2_path)

    # add VehicleSim2 to the globals
    workspace_page.set_library_filter('In Project')
    vehicle_name = put_element_on_grid(workspace_page, "VehicleSim2")

    # Drag paraboloid element into vehicle dataflow figure
    vehicle = workspace_page.get_dataflow_figure(vehicle_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid, vehicle('content_area').element, False)
    release(chain)
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()

    # Show the vehicle workflow
    workspace_page.show_workflow(vehicle_name)

    # See how many workflow component figures there are before we add to it
    eq(len(workspace_page.get_workflow_component_figures()), 16)

    ########################################
    # Drop the paraboloid component from the component tree onto the top level
    #     workflow for VehicleSim2
    ########################################
    workspace_page.expand_object(vehicle_name)
    paraboloid_component = workspace_page.find_object_button(vehicle_name + "." + paraboloid_name)
    vehicle_workflow_figure = workspace_page.get_workflow_figure(vehicle_name)
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.root, False)
    check_highlighting(vehicle_workflow_figure.root, True,
                       "Three Sim Vehicle workflow")
    release(chain)
    # Check to make sure there is one more workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 17)
    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]
    assert  (vehicle_name + "." + paraboloid_name) in pathnames

    ########################################
    # Drop the paraboloid component from the component tree onto the
    #     workflow for sim_acc under VehicleSim2
    ########################################
    # Need to do this again, for some reason. Cannot re-use the one from the previous section
    paraboloid_component = workspace_page.find_object_button(vehicle_name + "." + paraboloid_name)
    sim_acc_workflow_figure = workspace_page.get_workflow_figure("sim_acc")  # returns a pageobject

    chain = drag_element_to(browser, paraboloid_component,
                            sim_acc_workflow_figure.root, False)
    check_highlighting(sim_acc_workflow_figure.root, True, "sim_acc workflow")
    release(chain)
    # Check to make sure there is one more workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 18)
    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()  # returns Selenium WebElements
    pathnames = [get_pathname(browser, fig) for fig in figs]

    # should have two instances of the vehicle_name.paraboloid_name
    pathnames_matching_vehicle_name_paraboloid_name = \
           [p for p in pathnames if p == (vehicle_name + "." + paraboloid_name)]
    #assert  ("sim_acc" + "." + paraboloid_name) in pathnames
    eq(len(pathnames_matching_vehicle_name_paraboloid_name), 2)

    ########################################
    # Drop the paraboloid component from the component tree onto the
    #     workflow for vehicle under sim_acc under VehicleSim2.
    # This should NOT work since the paraboloid is not at the right level
    ########################################
    # Need to do this again, for some reason. Cannot re-use the one from the previous section
    paraboloid_component = workspace_page.find_object_button(vehicle_name + "." + paraboloid_name)
    vehicle_workflow_figure = workspace_page.get_workflow_figure("vehicle")
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.root, False)
    check_highlighting(vehicle_workflow_figure.root, False, "vehicle workflow")
    release(chain)
    # Check to make sure there is no new workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 18)

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def _test_drop_onto_layered_div(browser):
    projects_page, project_info_page, project_dict, workspace_page = startup(browser)

    # Add paraboloid and vehicle_threesim files
    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                'paraboloid.py')
    file2_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                            'vehicle_threesim.py')
    workspace_page.add_file(file1_path)
    workspace_page.add_file(file2_path)

    # add VehicleSim2 to the globals
    workspace_page.set_library_filter('In Project')
    vehicle_name = put_element_on_grid(workspace_page, "VehicleSim2")

    # add Paraboloid to VehicleSim dataflow assembly
    vehicle = workspace_page.get_dataflow_figure(vehicle_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid,
                            vehicle('content_area').element, False)
    release(chain)
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # Open up the component editor for the sim_EPA_city inside the vehicle sim
    sim_EPA_city_driver = workspace_page.get_dataflow_figure('sim_EPA_city',
                                                             vehicle_name)
    driver_editor = sim_EPA_city_driver.editor_page(base_type='Driver')
    driver_editor.move(-100, 0)
    driver_editor.show_workflow()

    # Check to make sure we have the expected number of
    #   workflow component figures before we add to it
    eq(len(driver_editor.get_workflow_component_figures()), 5)
    eq(len(workspace_page.get_workflow_component_figures()), 22)

    # Drag paraboloid component into sim_EPA_city workflow figure
    # should add to the list of workflow component figures
    workspace_page.expand_object(vehicle_name)
    paraboloid_component = workspace_page.find_object_button(vehicle_name + "." + paraboloid_name)
    vehicle_workflow_figure = workspace_page.get_workflow_figure("sim_EPA_city")
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.root, False)
    check_highlighting(vehicle_workflow_figure.root, True,
                       "Three Sim Vehicle workflow")
    release(chain)
    # Check to make sure there is one more workflow component figure
    eq(len(driver_editor.get_workflow_component_figures()), 6)
    eq(len(workspace_page.get_workflow_component_figures()), 24)
    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]
    assert  (vehicle_name + "." + paraboloid_name) in pathnames

    # Try dragging paraboloid component into vehicle workflow figure under
    #     sim_EPA_city workflow figure
    # should NOT add to the list of workflow component figures
    workspace_page.expand_object(vehicle_name)
    paraboloid_component = workspace_page.find_object_button(vehicle_name + "." + paraboloid_name)
    vehicle_workflow_figure = workspace_page.get_workflow_figure("vehicle")
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.root, False)
    check_highlighting(vehicle_workflow_figure.root, False,
                       "Three Sim Vehicle workflow")
    release(chain)
    # Check to make sure there is one more workflow component figure
    eq(len(driver_editor.get_workflow_component_figures()), 6)
    eq(len(workspace_page.get_workflow_component_figures()), 24)
    # Check to see that the new div inside the workflow is there
    figs = workspace_page.get_workflow_component_figures()
    pathnames = [get_pathname(browser, fig) for fig in figs]
    assert  (vehicle_name + "." + paraboloid_name) in pathnames

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


def slot_drop(browser, element, slot, should_drop, message='Slot'):
    '''Drop an element on a slot'''
    chain = drag_element_to(browser, element, slot, True)
    chain.move_by_offset(25, 0).perform()
    time.sleep(1.0)  # give it a second to update the figure
    check_highlighting(slot, should_highlight=should_drop, message=message)
    release(chain)


def slot_reset(workspace_page, editor=None, metamodel=None, remove_old=False):
    '''every successfull drop permanently fills the slot. because of this,
    we need to make a new metamodel (with empty slots) every successfull drop'''

    if remove_old:
        # first, close out the dialog box we have open
        editor.close()
        # remove the current metamodel
        metamodel.remove()

    #drop 'metamodel' onto the grid
    meta_name = put_element_on_grid(workspace_page, "MetaModel")
    #find it on the page
    metamodel = workspace_page.get_dataflow_figure(meta_name)

    #open the 'edit' dialog on metamodel
    editor = metamodel.editor_page(False)
    editor.move(-100, 0)
    editor.show_slots()

    #resize_editor(workspace_page, editor)

    #find the slots (this is both the drop target and highlight area)
    browser = workspace_page.browser
    slot_id = 'SlotFigure-'+meta_name+'-%s'
    caseiter = browser.find_element(By.ID, slot_id % 'warm_start_data')
    caserec  = browser.find_element(By.ID, slot_id % 'recorder')
    model    = browser.find_element(By.ID, slot_id % 'model')

    return editor, metamodel, caseiter, caserec, model, meta_name


def resize_editor(workspace_page, editor):
    '''ensure that the editor is not covering the library (or else we cannot drag things from it!)'''
    browser = workspace_page.browser

    page_width = browser.get_window_size()['width']
    lib_width = workspace_page('library_tab').find_element_by_xpath('..').size['width']
    lib_position = workspace_page('library_tab').find_element_by_xpath('..').location['x']
    dialog_width = editor('dialog_title').find_element_by_xpath('../..').size['width']
    dialog_position = editor('dialog_title').find_element_by_xpath('../..').location['x']

    # how much overlap do we have?
    overlap = lib_position - (dialog_position + dialog_width)

    if overlap < 0:  # we are overlapping
        # check to see if we have enough room to move out of the way
        if page_width < dialog_width + lib_width:
            # not enough, need to rezize the editor

            # look for the resize handle
            sibblings = editor('dialog_title').find_elements_by_xpath('../../div')
            handle = None
            for sib in sibblings:
                if "ui-resizable-se" in sib.get_attribute('class'):
                    handle = sib

            # do the resizing
            chain = ActionChains(browser)
            chain.click_and_hold(handle)
            chain.move_by_offset(450 - dialog_width, 0).perform()  # we can resize editor down to 425px, any less and we cover drop targets
            chain.click().perform()  # must click because release is not working. why? I do not know.
            chain.release(None).perform()

            # recalculate the overlap
            dialog_width = editor('dialog_title').find_element_by_xpath('../..').size['width']
            dialog_position = editor('dialog_title').find_element_by_xpath('../..').location['x']
            overlap = lib_position - (dialog_position + dialog_width)

        # We are good, move out!
        chain = ActionChains(browser)
        chain.click_and_hold(editor('dialog_title').element)
        chain.move_by_offset(overlap, 0).perform()
        chain.click().perform()  # must click because release is not working. why? I do not know.
        chain.release(None).perform()

        # recalculate the overlap
        dialog_width = editor('dialog_title').find_element_by_xpath('../..').size['width']
        dialog_position = editor('dialog_title').find_element_by_xpath('../..').location['x']
        overlap = lib_position - (dialog_position + dialog_width)

        if overlap < 0:
            # we still have a problem.
            eq(True, False,
                "Could not move or rezise the editor dialog so it is not " \
                "overlapping the library. The browser window is too small")


def get_slot_target(labels, element_str):
    '''Return the element with the given label string'''
    for label in labels:
        if element_str in label.text:
            return label.find_element_by_xpath("..")

    return None


def get_dataflow_fig_in_assembly_editor(workspace_page, name):
    '''Find the named dataflow fig in the assembly editor'''
    allFigs = workspace_page.get_dataflow_figures()
    for fig in allFigs:
        location = fig.find_element_by_xpath("..").get_attribute('id')
        if location == "top-dataflow":
            return DataflowFigure(workspace_page.browser, workspace_page.port, fig)

    return None


def put_assembly_on_grid(workspace_page):
    '''Drop an Assembly on a grid'''
    return put_element_on_grid(workspace_page, 'Assembly')


def put_element_on_grid(workspace_page, element_str):
    '''find and get the 'assembly', and the div for the grid object'''
    browser = workspace_page.browser

    for retry in range(3):
        try:
            assembly = workspace_page.find_library_button(element_str)
            chain = ActionChains(browser)
            chain.click_and_hold(assembly)
            chain.move_by_offset(-100, 0).perform()
        except StaleElementReferenceException:
            if retry < 2:
                logging.warning('put_element_on_grid %s:'
                                ' StaleElementReferenceException', element_str)
            else:
                raise
        else:
            break

    grid = browser.find_element_by_xpath('//div[@id="-dataflow"]')
    check_highlighting(grid, True, "Grid")
    release(chain)

    # deal with the modal dialog
    name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # make sure it is on the grid
    ensure_names_in_workspace(workspace_page, [name],
        "Dragging '" + element_str + "' to grid did not produce a new element on page")

    return name


def get_pathname(browser, fig):
    '''Get the OpenMDAO pathname for a figure'''
    figid = fig.get_attribute('id')  # get the ID of the element here
    script = "return jQuery('#" + figid + "').data('pathname')"
    return browser.execute_script(script)


def ensure_names_in_workspace(workspace_page, names, message=None):
    """ensures the list of element names in included in the workspace"""

    allnames = workspace_page.get_dataflow_component_names()

    # sometimes does not load all of the names for some reason.
    # Reloading seems to fix the problem
    try_reload = False
    for name in names:
        if not name in allnames:
            try_reload = True
    if try_reload:
        time.sleep(.1)
        allnames = workspace_page.get_dataflow_component_names()

    # now we will assert that the elements that we added appear on the page
    for name in names:
        eq(name in allnames, True, message)


def drag_element_to(browser, element, drag_to, centerx):
    '''Drag one element over to another element'''
    chain = ActionChains(browser)
    chain.move_to_element(element).perform()
    chain.click_and_hold(element)
    chain.move_to_element(drag_to).perform()
    if centerx:
        chain.move_by_offset(int(drag_to.value_of_css_property('width')[:-2])/2, 1).perform()
    else:
        chain.move_by_offset(2, 1).perform()
    return chain


def release(chain):
    '''The drop part of the ActionChain when doing drag and drop'''
    chain.release(on_element=None).perform()


def check_highlighting(element, should_highlight=True, message='Element'):
    '''check to see that the background-color of the element is rgb(207, 214, 254)'''
    if 'SlotFigure' in element.get_attribute('class'):
        # a slot figure is a div containing a ul element (the context menu) and
        # one or more svg elements, each of which contains a rect and two texts
        # the last rect fill style is what we need to check for highlighting
        rect = element.find_elements_by_css_selector('svg rect')[-1]
        style = rect.get_attribute('style')
    else:
        style = element.get_attribute('style')
    highlighted = ('background-color: rgb(207, 214, 254)' in style) \
                or ('highlighted.png' in style) \
                or ('fill: #cfd6fe' in style)
    eq(highlighted, should_highlight, message +
        (' did not highlight (and should have) ' if should_highlight else
         ' highlighed (and should not have) ')
         + 'when dragging a dropable element to it')


def getDropableElements(dataflow_figure):
    '''Dataflow figures are made of many subelements. This function
    returns a list of them so that we can try dropping on any one
    of the elements
    '''
    # return [dataflow_figure(area).element for area in \
    #        ['top_left','header','top_right', 'content_area',
    #         'bottom_left', 'footer', 'bottom_right']]

    # add back 'top_left' 'bottom_left' at some point. right now that test fails
    arr = ['content_area', 'header', 'footer', 'bottom_right', 'top_right']
    return [dataflow_figure(area).element for area in arr]


def replace_driver(workspace_page, assembly_name, driver_type):
    #find and get the 'comnindriver', 'top', and 'driver' objects
    newdriver = workspace_page.find_library_button(driver_type)
    assembly = workspace_page.get_dataflow_figure(assembly_name)
    driver_element = workspace_page.get_dataflow_figure('driver')

    div = getDropableElements(driver_element)[0]
    chain = drag_element_to(workspace_page.browser, newdriver, div, True)
    check_highlighting(driver_element('content_area').element, True,
                       "Driver's content_area")
    release(chain)

    # brings up a confirm dialog for replacing the existing driver.
    dialog = ConfirmationPage(assembly)
    dialog.click_ok()


if __name__ == '__main__':
    main()
