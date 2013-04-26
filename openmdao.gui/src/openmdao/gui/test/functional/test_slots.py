"""
Tests of slot related functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.util import ArgsPrompt, NotifierPage

from pageobjects.slot import SlotFigure


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


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
    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
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


def _test_slot_subclass(browser):
    # test that a slot will accept subclasses
    project_dict, workspace_page = startup(browser)

    file_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/slot_test.py')
    workspace_page.add_file(file_path)

    name = workspace_page.put_element_on_grid("AutoAssemb")
    aa = workspace_page.get_dataflow_figure(name)
    editor = aa.editor_page(double_click=False)
    editor.move(-200, 200)

    inputs = editor.get_inputs()
    expected = [
        ['', 'input',              '0', '', ''],
        ['', 'directory',           '', '', 'If non-blank, the directory to execute in.'],
        ['', 'force_execute',  'False', '', 'If True, always execute even if all IO traits are valid.'],
    ]
    for i, row in enumerate(inputs.value):
        eq(row, expected[i])

    inputs[0][2] = "10"
    aa.run()
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')

    outputs = editor.get_outputs()
    expected = [
        ['', 'output',                '80', '', ''],
        ['', 'derivative_exec_count',  '0', '', "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count',             '1', '', 'Number of times this Component has been executed.'],
        ['', 'itername',                '', '', 'Iteration coordinates.'],
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])

    editor.show_slots()
    dummy2 = workspace_page.find_library_button('Dummy2')
    d2_slot = browser.find_element(By.ID, 'SlotFigure-%s-d2' % name)
    workspace_page.slot_drop(dummy2, d2_slot, True, 'd2 (Dummy)')

    aa.run()
    message = NotifierPage.wait(workspace_page)
    eq(message, 'Run complete: success')

    outputs = editor.get_outputs()
    expected = [
        ['', 'output',                 '160', '', ''],
        ['', 'derivative_exec_count',    '0', '', "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count',               '2', '', 'Number of times this Component has been executed.'],
        ['', 'itername',                  '', '', 'Iteration coordinates.'],
    ]
    for i, row in enumerate(outputs.value):
        eq(row, expected[i])

    # Clean up.
    closeout(project_dict, workspace_page)


def _test_dict_slot(browser):
    project_dict, workspace_page = startup(browser)

    workspace_page.add_library_item_to_dataflow('openmdao.main.assembly.Assembly', 'top')
    workspace_page.show_dataflow('top')

    # load in some files needed for the tests

    file1_path = pkg_resources.resource_filename('openmdao.examples.simple',
                                                 'paraboloid.py')
    workspace_page.add_file(file1_path)

    file2_path = pkg_resources.resource_filename('openmdao.examples.enginedesign',
                                                 'transmission.py')
    workspace_page.add_file(file2_path)

    vt_comp_path = pkg_resources.resource_filename('openmdao.gui.test.functional',
                                                'files/simple_vartree_component.py')
    workspace_page.add_file(vt_comp_path)

    workspace_page.show_dataflow('top')
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.metamodel.MetaModel', 'mm')
    mm_figure = workspace_page.get_dataflow_figure('mm', 'top')
    mm_editor = mm_figure.editor_page()
    mm_editor.show_slots()
    mm_editor.move(-150, 0)

    model_slot = SlotFigure(workspace_page, 'top.mm.model')

    # Should not be any surrogates slots without a model in the slot
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(0, len(surrogates),
        "There should not be any surrogates in the surrogates dict but "
        "%d surrogate(s) are being displayed" % len(surrogates))

    # Fill the model slot
    workspace_page.do_command('from openmdao.examples.simple.paraboloid import Paraboloid')
    workspace_page.do_command('top.mm.model = Paraboloid()')
    #model_slot.fill_from_library('Paraboloid')

    # Should be one surrogates slot in the dict
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(1, len(surrogates),
        "There should be one surrogate in the surrogate slot but "
        "%d surrogate is being displayed" % len(surrogates))

    # remove the model
    model_elem = browser.find_element(By.ID, 'SlotFigure-top-mm-model')
    menu_item_remove = model_elem.find_element_by_css_selector('ul li')
    chain = ActionChains(browser)
    chain.move_to_element_with_offset(model_elem, 25, 25)
    chain.context_click(model_elem).perform()
    menu_item_remove.click()

    # There should not be any surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(0, len(surrogates),
        "There should not be any surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # see what happens when you change the model
    model_slot.fill_from_library('Transmission')

    # There should two surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(2, len(surrogates),
        "There should be two surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # They should all be empty: RPM and torque_ratio
    for surrogate in surrogates:
        eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    # Fill the torque_ratio surrogate slot with FloatKrigingSurrogate
    # The ID of that slot div is SlotFigure-top-mm-surrogates-torque_ratio
    surrogates_torque_ratio_slot = SlotFigure(workspace_page, 'top.mm.surrogates.torque_ratio')
    surrogates_torque_ratio_slot.fill_from_library('KrigingSurrogate')

    # One should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(1, num_surrogates_filled,
       "Exactly one surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the RPM surrogate slot with FloatKrigingSurrogate
    # The ID of that slot div is SlotFigure-top-mm-surrogates-RPM
    surrogates_torque_ratio_slot = SlotFigure(workspace_page, 'top.mm.surrogates.RPM')
    surrogates_torque_ratio_slot.fill_from_library('FloatKrigingSurrogate')

    # Two should be filled now
    time.sleep(1.0)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(2, num_surrogates_filled,
       "Exactly two surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Test with components that have variable trees

    # TODO: Change the model without removing it first ################

    # # remove the model
    # model_elem = browser.find_element(By.ID, 'SlotFigure-top-mm-model')
    # menu_item_remove = model_elem.find_element_by_css_selector('ul li')
    # chain = ActionChains(browser)
    # chain.move_to_element_with_offset(model_elem, 25, 25)
    # chain.context_click(model_elem).perform()
    # menu_item_remove.click()

    # test vartree with metamodel
    model_slot.fill_from_library('InandOutTree')

    # There should two surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(3, len(surrogates),
        "There should be three surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # They should all be empty
    for surrogate in surrogates:
        eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    # Fill the outs.x surrogate slot with FloatKrigingSurrogate
    surrogates_slot = SlotFigure(workspace_page, 'top.mm.surrogates.outs.x')
    surrogates_slot.fill_from_library('FloatKrigingSurrogate')

    # One should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(1, num_surrogates_filled,
       "Exactly one surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the outs.y surrogate slot with KrigingSurrogate
    surrogates_slot = SlotFigure(workspace_page, 'top.mm.surrogates.zzz')
    surrogates_slot.fill_from_library('KrigingSurrogate')

    # Two should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(2, num_surrogates_filled,
       "Exactly two surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the outs.y surrogate slot with KrigingSurrogate
    surrogates_slot = SlotFigure(workspace_page, 'top.mm.surrogates.outs.y')
    args = [1, 1]
    surrogates_slot.fill_from_library('ResponseSurface', args)

    # Three should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(3, num_surrogates_filled,
       "Exactly three surrogate slots should be filled but %d are filled" % num_surrogates_filled)

    # Check to see that excludes and includes work
    mm_editor.set_input('excludes', '[]')
    # There should two surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(3, len(surrogates),
        "There should be three surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # set an exclude
    mm_editor.set_input('excludes', '["outs"]')
    # There should not be any surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(1, len(surrogates),
        "There should be one surrogate in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # Clean up.
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
