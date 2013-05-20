"""
Tests of slot related functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.by import By

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.util import NotifierPage

from pageobjects.slot import find_slot_figure


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


def _test_slots(browser):
    project_dict, workspace_page = startup(browser)

    editor, metamodel, caseiter, caserec, comp, meta_name = slot_reset(workspace_page)

    execcomp = workspace_page.find_library_button('ExecComp')

    # drop ExecComp onto MetaModel 'recorder' slot. This should fail.
    workspace_page.drag_and_drop(execcomp, caserec, False, 'Component')
    time.sleep(1.0)  # give it a second to update the figure
    caserec = find_slot_figure(workspace_page, 'recorder', prefix=meta_name)
    eq(False, caserec.filled,
        "Component dropped into CaseRecorder (should not have)")

    # drop ExecComp onto the MetaModel 'model' slot. This should succeed.
    comp = find_slot_figure(workspace_page, 'model', prefix=meta_name)
    workspace_page.fill_slot_from_library(comp, 'ExecComp')

    time.sleep(1.0)  # give it a second to update the figure
    comp = find_slot_figure(workspace_page, 'model', prefix=meta_name)
    eq(True, comp.filled,
        "Component did not drop into Component slot")

    # remove ExecComp from the MetaModel 'model' slot. This should succeed.
    comp.remove()
    time.sleep(1.0)  # give it a second to update the figure
    comp = find_slot_figure(workspace_page, 'model', prefix=meta_name)
    eq(False, comp.filled,
        "Component slot was not emptied")

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
        drag_and_drop(browser, ele[0].element, caseiter, ele[1], 'CaseIterator')
    #TODO: REFRESH THE SLOTS, CHECK THEIR FONT COLOR
        #drop on caserec
        drag_and_drop(browser, ele[0].element, caserec, ele[2], 'CaseRecorder')
    #TODO: REFRESH THE SLOTS, CHECK THEIR FONT COLOR
        #drop on comp
        drag_and_drop(browser, ele[0].element, comp, ele[3], 'Component')
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
    generator_slot = find_slot_figure(workspace_page, 'DOEgenerator', prefix='top.driver')

    # check that slot is not filled
    eq(False, generator_slot.filled,
        "generator slot is showing as filled when it should not be")

    # drop a FullFactorial onto the generator slot
    workspace_page.fill_slot_from_library(generator_slot, 'FullFactorial')

    # refresh and check that slot is now filled
    time.sleep(1.0)
    generator_slot = find_slot_figure(workspace_page, 'DOEgenerator', prefix='top.driver')
    eq(True, generator_slot.filled,
        "FullFactorial did not drop into generator slot")

    # get the recorders slot figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top.driver')

    # check that slot is not filled
    eq(False, recorders_slot.filled,
        "recorders slot is showing as filled when it should not be")

    # set center pane to workflow to make sure workflow doesn't steal drops
    workspace_page('workflow_tab').click()

    # drop a DumpCaseRecorder onto the recorders slot
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top.driver')
    workspace_page.fill_slot_from_library(recorders_slot, 'DumpCaseRecorder')

    # refresh and check that there is now a DumpCaseRecorder in the first slot
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]', prefix='top.driver')
    eq(True, recorders_slot.filled,
        "DumpCaseRecorder did not drop into recorders slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'DumpCaseRecorder',
        "Filled slot element should show the correct type (DumpCaseRecorder)")

    # check that there is still an unfilled slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top.driver')
    eq(False, recorders_slot.filled,
        "recorders slot is not showing an unfilled slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'ICaseRecorder',
        "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # drop another CaseRecorder onto the recorders slot
    workspace_page.fill_slot_from_library(recorders_slot, 'CSVCaseRecorder')
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[1]', prefix='top.driver')
    eq(True, recorders_slot.filled,
        "CSVCaseRecorder did not drop into recorders slot")

    # check that there is still an unfilled slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top.driver')
    eq(False, recorders_slot.filled,
        "recorders slot is not showing an unfilled slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'ICaseRecorder',
        "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # remove the DumpCaseRecorder from the first slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]', prefix='top.driver')
    recorders_slot.remove()

    # check that the CSVCaseRecorder is now in the first filled slot
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]', prefix='top.driver')
    eq(True, recorders_slot.filled,
        "CSVCaseRecorder did not drop into recorders slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'CSVCaseRecorder',
        "Filled slot element should show the correct klass (CSVCaseRecorder)")

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
    recorders_slot = find_slot_figure(workspace_page, 'd2', prefix=name)
    workspace_page.fill_slot_from_library(recorders_slot, 'Dummy2')

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

    # Should not be any surrogates slots without a model in the slot
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(0, len(surrogates),
        "There should not be any surrogates in the surrogates dict but "
        "%d surrogate(s) are being displayed" % len(surrogates))

    # Fill the model slot
    model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    workspace_page.fill_slot_from_library(model_slot, 'Paraboloid')

    # Should be one surrogates slot in the dict
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(1, len(surrogates),
        "There should be one surrogate in the surrogate slot but "
        "%d surrogate is being displayed" % len(surrogates))

    # remove the model
    model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    model_slot.remove()

    # There should not be any surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(0, len(surrogates),
        "There should not be any surrogates in the surrogates dict but "
        "%d surrogate(s) are being displayed" % len(surrogates))

    # see what happens when you change the model
    model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    workspace_page.fill_slot_from_library(model_slot, 'Transmission')

    # There should two surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(2, len(surrogates),
        "There should be two surrogates in the surrogates dict but "
        "%d surrogate(s) are being displayed" % len(surrogates))

    # They should all be empty: RPM and torque_ratio
    for surrogate in surrogates:
        eq(False, ("filled" in surrogate.get_attribute('class')),
            "Surrogate should not be filled")

    # Fill the torque_ratio surrogate slot with FloatKrigingSurrogate
    surrogate_slot = find_slot_figure(workspace_page, 'torque_ratio', prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot, 'KrigingSurrogate')

    # One should be filled now
    time.sleep(2)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(1, num_surrogates_filled,
       "Exactly one surrogate slot should be filled but "
       "%d are filled" % num_surrogates_filled)

    # Fill the RPM surrogate slot with FloatKrigingSurrogate
    surrogate_slot = find_slot_figure(workspace_page, 'RPM', prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot, 'FloatKrigingSurrogate')

    # Two should be filled now
    time.sleep(2)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath(
        "//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(2, num_surrogates_filled,
       "Exactly two surrogate slot should be filled but "
       "%d are filled" % num_surrogates_filled)

    # Test with components that have variable trees

    # test vartree with metamodel
    model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    workspace_page.fill_slot_from_library(model_slot, 'InandOutTree')

    # There should 3 surrogates slots
    time.sleep(2)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(3, len(surrogates),
        "There should be three surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # They should all be empty
    for surrogate in surrogates:
        eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    # Fill the outs.x surrogate slot with FloatKrigingSurrogate
    surrogate_slot = find_slot_figure(workspace_page, 'outs.x', prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot, 'FloatKrigingSurrogate')

    # One should be filled now
    time.sleep(2)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(1, num_surrogates_filled,
       "Exactly one surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the zzz surrogate slot with KrigingSurrogate
    surrogate_slot = find_slot_figure(workspace_page, 'zzz', prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot, 'KrigingSurrogate')

    # Two should be filled now
    time.sleep(2)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates:
        if "filled" in surrogate.get_attribute('class'):
            num_surrogates_filled += 1
    eq(2, num_surrogates_filled,
       "Exactly two surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the outs.y surrogate slot with ResponseSurface
    surrogate_slot = find_slot_figure(workspace_page, 'outs.y', prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot, 'ResponseSurface', [1, 1])

    # Three should be filled now
    time.sleep(2)  # give it a bit to update the figure
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
    time.sleep(2)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq(3, len(surrogates),
        "There should be three surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    # set an exclude
    mm_editor.set_input('excludes', '["outs"]')
    # There should not be any surrogates slots
    time.sleep(2)  # give it a bit to update the figure
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

    #find the slots (this is both the drop target and highlight area)
    browser = workspace_page.browser
    slot_id = 'SlotFigure-' + meta_name + '-%s'
    caseiter = browser.find_element(By.ID, slot_id % 'warm_start_data')
    caserec  = browser.find_element(By.ID, slot_id % 'recorder')
    model    = browser.find_element(By.ID, slot_id % 'model')

    return editor, metamodel, caseiter, caserec, model, meta_name


if __name__ == '__main__':
    main()
