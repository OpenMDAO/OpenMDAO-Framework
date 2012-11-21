"""
Tests of overall workspace functions.
"""

import pkg_resources
import sys
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout
from util import slot_drop, slot_reset, \
                 get_dataflow_fig_in_assembly_editor, put_assembly_on_grid, \
                 put_element_on_grid, get_pathname, ensure_names_in_workspace, \
                 drag_element_to, release, check_highlighting, getDropableElements, \
                 replace_driver

from pageobjects.component import NameInstanceDialog
from pageobjects.util import ArgsPrompt

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
        "produce a new element on page")

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
        "drop areas did not produce a new element on page")

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
    # model (IComponent) slot of a MetaModel.
    ##################################################
    slot_drop(browser, execcomp, comp, True, 'Component')
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

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
    editor.move(-200, 0)
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
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

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
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

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
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

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
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

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
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()

    # View the Workflow Pane.
    workspace_page('workflow_tab').click()

    # Show the top level workflow
    workspace_page.show_workflow('top')
    eq(len(workspace_page.get_workflow_component_figures()), 1)

    # Drop the paraboloid component from the component tree onto the workflow for top
    workspace_page.expand_object('top')
    paraboloid_component = workspace_page.find_object_button('top.' + paraboloid_name)
    top = workspace_page.get_workflow_figure('top.driver')
    chain = drag_element_to(browser, paraboloid_component, top.flow, False)
    assert top.highlighted
    release(chain)

    eq(len(workspace_page.get_workflow_component_figures()), 2)

    # Confirm that the paraboloid has been added to the top workflow
    assert paraboloid_name in top.component_names

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
    top = workspace_page.get_workflow_figure('top.driver')
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid, top.flow, True)
    chain.move_by_offset(int(paraboloid.value_of_css_property('width')[:-2])/3, 1).perform()
    assert top.highlighted
    release(chain)
    #deal with the modal dialog
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()

    time.sleep(0.5)  # Just so we can see it.

    eq(len(workspace_page.get_workflow_component_figures()), 2)

    # Confirm that the paraboloid has been added to the top workflow
    assert paraboloid_name in top.component_names

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

    # create an instance of VehicleSim2
    workspace_page.set_library_filter('In Project')
    sim_name = put_element_on_grid(workspace_page, "VehicleSim2")

    # Drag paraboloid element into sim dataflow figure
    sim = workspace_page.get_dataflow_figure(sim_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid, sim('content_area').element, False)
    release(chain)
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
    workspace_page.expand_object(sim_name)
    paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    sim_workflow_figure = workspace_page.get_workflow_figure(sim_name+'.driver')
    chain = drag_element_to(browser, paraboloid_component,
                            sim_workflow_figure.components[0], True)
    assert sim_workflow_figure.highlighted
    release(chain)

    # Confirm that there is one more workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 17)

    # Confirm that the paraboloid has been added to the sim workflow
    assert paraboloid_name in sim_workflow_figure.component_names

    ############################################################################
    # Drop paraboloid component onto the sim_acc workflow under sim
    ############################################################################
    # Need to do this again, for some reason. Cannot re-use the one from the previous section
    paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    sim_acc_workflow_figure = workspace_page.get_workflow_figure("sim_acc")
    chain = drag_element_to(browser, paraboloid_component,
                            sim_acc_workflow_figure.components[0], True)
    assert sim_acc_workflow_figure.highlighted
    release(chain)

    # Confirm that there is one more workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 18)

    # Confirm that the paraboloid has been added to the sim_acc workflow
    assert paraboloid_name in sim_acc_workflow_figure.component_names

    ############################################################################
    # Drop paraboloid component onto the vehicle workflow under sim_acc
    # This should NOT work since the paraboloid is not in the vehicle assembly
    ############################################################################
    # Need to do this again, for some reason. Cannot re-use the one from the previous section
    paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    vehicle_workflow_figure = workspace_page.get_workflow_figure("vehicle.driver")
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.components[0], True)
    assert not vehicle_workflow_figure.highlighted
    release(chain)

    # Confirm that there is NOT a new workflow component figure
    eq(len(workspace_page.get_workflow_component_figures()), 18)

    # Confirm that the paraboloid has NOT been added to the vehicle workflow
    assert paraboloid_name not in vehicle_workflow_figure.component_names

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
    sim_name = put_element_on_grid(workspace_page, 'VehicleSim2')

    # add Paraboloid to VehicleSim dataflow assembly
    sim = workspace_page.get_dataflow_figure(sim_name)
    paraboloid = workspace_page.find_library_button('Paraboloid')
    chain = drag_element_to(browser, paraboloid,
                            sim('content_area').element, False)
    release(chain)
    paraboloid_name = NameInstanceDialog(workspace_page).create_and_dismiss()
    paraboloid_pathname = sim_name + "." + paraboloid_name

    # Open up the component editor for the sim_EPA_city inside the vehicle sim
    sim_EPA_city_driver = workspace_page.get_dataflow_figure('sim_EPA_city',
                                                             sim_name)
    driver_editor = sim_EPA_city_driver.editor_page(base_type='Driver')
    driver_editor.move(-200, 0)
    driver_editor.show_workflow()

    # Confirm expected number of workflow component figures before adding one
    eq(len(driver_editor.get_workflow_component_figures()), 5)
    eq(len(workspace_page.get_workflow_component_figures()), 22)

    # Drag paraboloid component into sim_EPA_city workflow
    workspace_page.expand_object(sim_name)
    paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    city_workflow_figure = workspace_page.get_workflow_figure('sim_EPA_city')
    chain = drag_element_to(browser, paraboloid_component,
                            city_workflow_figure.components[0], True)
    assert city_workflow_figure.highlighted
    release(chain)

    # Confirm there is one more workflow component figure in the editor
    eq(len(driver_editor.get_workflow_component_figures()), 6)

    # Confirm two more workflow component figures in the workspace as a whole
    eq(len(workspace_page.get_workflow_component_figures()), 24)

    # Confirm that the paraboloid has been added to the sim_EPA_city workflow
    assert paraboloid_name in city_workflow_figure.component_names

    # Try dragging paraboloid component into vehicle workflow under sim_EPA_city
    # should NOT add to the list of workflow component figures
    workspace_page.expand_object(sim_name)
    paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    vehicle_workflow_figure = workspace_page.get_workflow_figure("vehicle.driver")
    chain = drag_element_to(browser, paraboloid_component,
                            vehicle_workflow_figure.components[0], True)
    assert not vehicle_workflow_figure.highlighted
    release(chain)

    # Confirm that there is NOT a new workflow component figure in either place
    eq(len(driver_editor.get_workflow_component_figures()), 6)
    eq(len(workspace_page.get_workflow_component_figures()), 24)

    # Confirm that the paraboloid has NOT been added to the vehicle workflow
    assert paraboloid_name not in vehicle_workflow_figure.component_names

    # Clean up.
    closeout(projects_page, project_info_page, project_dict, workspace_page)


if __name__ == '__main__':
    main()
