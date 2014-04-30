"""
Tests of slot related functions.
"""

import pkg_resources
import time

from nose.tools import eq_ as eq
from nose.tools import with_setup

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.util import NotifierPage

from pageobjects.slot import find_slot_figure


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser


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
    generator_slot = find_slot_figure(workspace_page, 'DOEgenerator',
                                      prefix='top.driver')

    # check that slot is not filled
    eq(False, generator_slot.filled,
        "generator slot is showing as filled when it should not be")

    # drop a FullFactorial onto the generator slot
    workspace_page.fill_slot_from_library(generator_slot, 'FullFactorial')

    # refresh and check that slot is now filled
    time.sleep(1.0)
    generator_slot = find_slot_figure(workspace_page, 'DOEgenerator',
                                      prefix='top.driver')
    eq(True, generator_slot.filled,
       "FullFactorial did not drop into generator slot")

    editor.close()

    # open the object editor dialog for the assembly
    assembly = workspace_page.get_dataflow_figure('top', '')
    editor = assembly.editor_page(False)
    editor.move(-200, 200)
    editor.show_slots()

    # get the recorders slot figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top')

    # check that slot is not filled
    eq(False, recorders_slot.filled,
       "recorders slot is showing as filled when it should not be")

    # set center pane to workflow to make sure workflow doesn't steal drops
    workspace_page('workflow_tab').click()

    # drop a DumpCaseRecorder onto the recorders slot
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top')
    workspace_page.fill_slot_from_library(recorders_slot, 'DumpCaseRecorder')

    # refresh and check that there is now a DumpCaseRecorder in the first slot
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]',
                                      prefix='top')
    eq(True, recorders_slot.filled,
       "DumpCaseRecorder did not drop into recorders slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'DumpCaseRecorder',
       "Filled slot element should show the correct type (DumpCaseRecorder)")

    # check that there is still an unfilled slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top')
    eq(False, recorders_slot.filled,
       "recorders slot is not showing an unfilled slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'ICaseRecorder',
       "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # drop another CaseRecorder onto the recorders slot
    workspace_page.fill_slot_from_library(recorders_slot, 'CSVCaseRecorder')
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[1]',
                                      prefix='top')
    eq(True, recorders_slot.filled,
       "CSVCaseRecorder did not drop into recorders slot")

    # check that there is still an unfilled slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders', prefix='top')
    eq(False, recorders_slot.filled,
       "recorders slot is not showing an unfilled slot")
    klass = recorders_slot.root.find_elements_by_css_selector('text#klass')
    eq(klass[0].text, 'ICaseRecorder',
       "Unfilled slot element should show the correct klass (ICaseRecorder)")

    # remove the DumpCaseRecorder from the first slot in the list
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]',
                                      prefix='top')
    recorders_slot.remove()

    # check that the CSVCaseRecorder is now in the first filled slot
    time.sleep(1.0)  # give it a second to update the figure
    recorders_slot = find_slot_figure(workspace_page, 'recorders[0]',
                                      prefix='top')
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
        ['', 'directory',           '', '',
         'If non-blank, the directory to execute in.'],
        ['', 'force_execute',  'False', '',
         'If True, always execute even if all IO traits are valid.'],
        ['', 'force_fd', 'False', '',
         'If True, always finite difference this component.'],
        ['', 'missing_deriv_policy', 'assume_zero', '',
         'Determines behavior when some analytical derivatives are provided but'
         ' some are missing'],
        ['', 'printvars', '[]', '',
         'List of extra variables to output in the recorders.']
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
        ['', 'derivative_exec_count',  '0', '',
         "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count',             '1', '',
         'Number of times this Component has been executed.'],
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
        ['', 'derivative_exec_count',    '0', '',
         "Number of times this Component's derivative function has been executed."],
        ['', 'exec_count',               '2', '',
         'Number of times this Component has been executed.'],
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
    args = ["('ratio1', 'ratio2')", "('torque_ratio', 'RPM')"]
    workspace_page.add_library_item_to_dataflow(
        'openmdao.lib.components.metamodel.MetaModel', 'mm', args=args)
    mm_figure = workspace_page.get_dataflow_figure('mm', 'top')
    mm_editor = mm_figure.editor_page()
    mm_editor.show_slots()
    mm_editor.move(-500, 0)  # need clear LOS to the library

    # see what happens when you change the model
    #model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    #workspace_page.fill_slot_from_library(model_slot, 'Transmission')

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
    surrogate_slot = find_slot_figure(workspace_page, 'torque_ratio',
                                      prefix='top.mm.surrogates')
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
    surrogate_slot = find_slot_figure(workspace_page, 'RPM',
                                      prefix='top.mm.surrogates')
    workspace_page.fill_slot_from_library(surrogate_slot,
                                          'FloatKrigingSurrogate')

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

    # Vartrees currently not supported in the new Metamodel -- KTM

    ## Test with components that have variable trees

    ## test vartree with metamodel
    #model_slot = find_slot_figure(workspace_page, 'model', prefix='top.mm')
    #workspace_page.fill_slot_from_library(model_slot, 'InandOutTree')

    ## There should 3 surrogates slots
    #time.sleep(2)  # give it a bit to update the figure
    #surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    #eq(3, len(surrogates),
        #"There should be three surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len(surrogates))

    ## They should all be empty
    #for surrogate in surrogates:
        #eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    ## Fill the outs.x surrogate slot with FloatKrigingSurrogate
    #surrogate_slot = find_slot_figure(workspace_page, 'outs.x', prefix='top.mm.surrogates')
    #workspace_page.fill_slot_from_library(surrogate_slot, 'FloatKrigingSurrogate')

    ## One should be filled now
    #time.sleep(2)  # give it a bit to update the figure
    #num_surrogates_filled = 0
    #surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    #for surrogate in surrogates:
        #if "filled" in surrogate.get_attribute('class'):
            #num_surrogates_filled += 1
    #eq(1, num_surrogates_filled,
       #"Exactly one surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    ## Fill the zzz surrogate slot with KrigingSurrogate
    #surrogate_slot = find_slot_figure(workspace_page, 'zzz', prefix='top.mm.surrogates')
    #workspace_page.fill_slot_from_library(surrogate_slot, 'KrigingSurrogate')

    ## Two should be filled now
    #time.sleep(2)  # give it a bit to update the figure
    #num_surrogates_filled = 0
    #surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    #for surrogate in surrogates:
        #if "filled" in surrogate.get_attribute('class'):
            #num_surrogates_filled += 1
    #eq(2, num_surrogates_filled,
       #"Exactly two surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    ## Fill the outs.y surrogate slot with ResponseSurface
    #surrogate_slot = find_slot_figure(workspace_page, 'outs.y', prefix='top.mm.surrogates')
    #workspace_page.fill_slot_from_library(surrogate_slot, 'ResponseSurface', [1, 1])

    ## Three should be filled now
    #time.sleep(2)  # give it a bit to update the figure
    #num_surrogates_filled = 0
    #surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    #for surrogate in surrogates:
        #if "filled" in surrogate.get_attribute('class'):
            #num_surrogates_filled += 1
    #eq(3, num_surrogates_filled,
       #"Exactly three surrogate slots should be filled but %d are filled" % num_surrogates_filled)

    # Clean up.
    closeout(project_dict, workspace_page)



if __name__ == '__main__':
    main()
