"""
Tests of overall workspace functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.component import NameInstanceDialog
from pageobjects.util import ArgsPrompt


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_drop_on_driver(browser):
    project_dict, workspace_page = startup(browser)

    # replace the 'top' assembly driver with a CONMINdriver
    workspace_page.replace_driver('top', 'CONMINdriver')

    # Check to see that the content area for the driver is now CONMINdriver
    driver_element = workspace_page.get_dataflow_figure('driver')
    eq(driver_element('content_area').find_element_by_xpath('center/i').text,
        'CONMINdriver', "Dropping CONMINdriver onto existing driver did not replace it")

    closeout(project_dict, workspace_page)


def _test_workspace_dragdrop(browser):
    project_dict, workspace_page = startup(browser)

    #find and get the 'assembly', and 'top' objects
    assembly = workspace_page.find_library_button('Assembly')
    top = workspace_page.get_dataflow_figure('top')

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

    #find and get the 'assembly', and 'top' objects
    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')
    top = workspace_page.get_dataflow_figure('top', '')
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
    #find and get the 'assembly', and 'top' objects
    workspace_page.set_library_filter('Assembly')   # put Assembly at top of lib
    assembly = workspace_page.find_library_button('Assembly')

    top = workspace_page.get_dataflow_figure('top', '')
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


def _test_slots(browser):
    project_dict, workspace_page = startup(browser)

    editor, metamodel, caseiter, caserec, comp, meta_name = slot_reset(workspace_page)

    execcomp = workspace_page.find_library_button('ExecComp')

    # drop ExecComp onto MetaModel 'recorder' slot. This should fail.
    workspace_page.slot_drop(execcomp, caserec, False, 'Component')

    slot_id = 'SlotFigure-%s-%s'

    time.sleep(1.0)  # give it a second to update the figure
    caserec = browser.find_element(By.ID, slot_id % (meta_name, 'recorder'))
    eq(False, ("filled" in caserec.get_attribute('class')),
        "Component dropped into CaseRecorder (should not have)")

    # drop ExecComp onto the MetaModel 'model' slot. This should succeed.
    workspace_page.slot_drop(execcomp, comp, True, 'Component')
    args_page = ArgsPrompt(workspace_page.browser, workspace_page.port)
    args_page.click_ok()

    time.sleep(1.0)  # give it a second to update the figure
    comp = browser.find_element(By.ID, slot_id % (meta_name, 'model'))

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

    editor.close()
    closeout(project_dict, workspace_page)


def _test_list_slot(browser):
    project_dict, workspace_page = startup(browser)

    # replace the 'top' assembly driver with a DOEdriver
    # (this additionally verifies that an issue with DOEdriver slots is fixed)
    workspace_page.replace_driver('top', 'DOEdriver')

    # open the object editor dialog for the driver
    driver = workspace_page.get_dataflow_figure('driver', 'top')
    editor = driver.editor_page(False)
    editor.move(-200, 200)
    editor.show_slots()

    # get the generator slot figure
    slot_id = 'SlotFigure-%s-%s' % ('top-driver', 'DOEgenerator')
    generator_slot = browser.find_element(By.ID, slot_id)

    # check that slot is not filled
    eq(False, ("filled" in generator_slot.get_attribute('class')),
        "generator slot is showing as filled when it should not be")

    # drop a FullFactorial onto the generator slot
    generator = workspace_page.find_library_button('FullFactorial')
    workspace_page.slot_drop(generator, generator_slot, True, 'generator')
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
    case_recorder = workspace_page.find_library_button('DumpCaseRecorder')
    workspace_page.slot_drop(case_recorder, recorders_slot, True, 'recorders')
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
    workspace_page.slot_drop(case_recorder, recorders_slot, True, 'recorders')
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
    workspace_page.slot_drop(case_recorder, recorders_slot, True, 'recorders')
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
    editor.close()
    closeout(project_dict, workspace_page)


# Note, I removed the component_to_simple_workflow because it provides nothing
# that this test does not. Also, I removed library_to_workflow because that
# operation is unsupported in the new workflow. -- KTM

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
    driver_editor.move(-200, 200)
    driver_editor.show_workflow()

    # Confirm expected number of workflow component figures before adding one
    eq(len(driver_editor.get_workflow_component_figures()), 5)
    eq(len(workspace_page.get_workflow_component_figures()), 22)

    # Drop onto the object editor's workflow figure is no longer supported.
    # -- KTM

    # Drag paraboloid component into sim_EPA_city workflow
    #workspace_page('dataflow_tab').click()
    #workspace_page.expand_object(sim_name)
    #simsim_name = sim_name + '.' + 'sim_EPA_city'
    #workspace_page.add_object_to_workflow(paraboloid_pathname, simsim_name)

    ## Confirm there is one more workflow component figure in the editor
    #workspace_page('workflow_tab').click()
    #eq(len(driver_editor.get_workflow_component_figures()), 6)

    ## Confirm two more workflow component figures in the workspace as a whole
    #eq(len(workspace_page.get_workflow_component_figures()), 24)

    ## Confirm that the paraboloid has been added to the sim_EPA_city workflow
    ## by trying to access it.
    #obj = workspace_page.find_object_button(simsim_name + "." + paraboloid_name)

    # Don't see the reason to verfiy again that you can't add something to an
    # out-of-scope workflow. -- KTM

    ## Try dragging paraboloid component into vehicle workflow under sim_EPA_city
    ## should NOT add to the list of workflow component figures
    #workspace_page.expand_object(sim_name)
    #paraboloid_component = workspace_page.find_object_button(paraboloid_pathname)
    #vehicle_workflow_figure = workspace_page.get_workflow_figure("vehicle.driver")
    #chain = drag_element_to(browser, paraboloid_component,
                            #vehicle_workflow_figure.components[0], True)
    #assert not vehicle_workflow_figure.highlighted
    #release(chain)

    ## Confirm that there is NOT a new workflow component figure in either place
    #eq(len(driver_editor.get_workflow_component_figures()), 6)
    #eq(len(workspace_page.get_workflow_component_figures()), 24)

    ## Confirm that the paraboloid has NOT been added to the vehicle workflow
    #assert paraboloid_name not in vehicle_workflow_figure.component_names

    # Clean up.
    driver_editor.close()
    closeout(project_dict, workspace_page)


def slot_reset(workspace_page, editor=None, metamodel=None, remove_old=False):
    '''every successfull drop permanently fills the slot. because of this,
    we need to make a new metamodel (with empty slots) every successfull drop'''

    if remove_old:
        # first, close out the dialog box we have open
        editor.close()
        # remove the current metamodel
        metamodel.remove()

    #drop 'metamodel' onto the grid
    meta_name = workspace_page.put_element_on_grid("MetaModel")
    #find it on the page
    metamodel = workspace_page.get_dataflow_figure(meta_name)

    #open the 'edit' dialog on metamodel
    editor = metamodel.editor_page(False)
    editor.move(-250, 0)
    editor.show_slots()

    #resize_editor(workspace_page, editor)

    #find the slots (this is both the drop target and highlight area)
    browser = workspace_page.browser
    slot_id = 'SlotFigure-' + meta_name + '-%s'
    caseiter = browser.find_element(By.ID, slot_id % 'warm_start_data')
    caserec  = browser.find_element(By.ID, slot_id % 'recorder')
    model    = browser.find_element(By.ID, slot_id % 'model')

    return editor, metamodel, caseiter, caserec, model, meta_name

if __name__ == '__main__':
    main()
