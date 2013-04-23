"""
Tests of overall workspace functions.
"""

import pkg_resources
import time
import tempfile

from nose.tools import eq_ as eq
from nose.tools import with_setup

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By

from util import main, setup_server, teardown_server, generate, \
                 startup, closeout

from pageobjects.component import NameInstanceDialog
from pageobjects.util import ArgsPrompt, NotifierPage

from pageobjects.slot import SlotFigure


@with_setup(setup_server, teardown_server)
def test_generator():
    for _test, browser in generate(__name__):
        yield _test, browser



def _test_dict_slot(browser):
    project_dict, workspace_page = startup(browser)

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
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq( 0, len( surrogates),
        "There should not be any surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len( surrogates ) )

    # Fill the model slot
    model_slot.fill_from_library('Paraboloid')

    # Should be one surrogates slot in the dict
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq( 1, len( surrogates),
        "There should be one surrogate in the surrogate slot but %d surrogate is being displayed" % len( surrogates ) )

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
    eq( 0, len( surrogates),
        "There should not be any surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len( surrogates ) )

    # see what happens when you change the model
    model_slot.fill_from_library('Transmission')

    # There should two surrogates slots
    time.sleep(1.0)  # give it a bit to update the figure
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    eq( 2, len( surrogates),
        "There should be two surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len( surrogates ) )

    # They should all be empty: RPM and torque_ratio
    for surrogate in surrogates :
        eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    # Fill the torque_ratio surrogate slot with FloatKrigingSurrogate
    # The ID of that slot div is SlotFigure-top-mm-surrogates-torque_ratio
    surrogates_torque_ratio_slot = SlotFigure(workspace_page, 'top.mm.surrogates.torque_ratio')
    surrogates_torque_ratio_slot.fill_from_library('KrigingSurrogate')

    # One should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates :
        if "filled" in surrogate.get_attribute('class') :
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
    for surrogate in surrogates :
        if "filled" in surrogate.get_attribute('class') :
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
    eq( 2, len( surrogates),
        "There should be two surrogates in the surrogates dict but %d surrogate(s) are being displayed" % len( surrogates ) )

    # They should all be empty
    for surrogate in surrogates :
        eq(False, ("filled" in surrogate.get_attribute('class')), "Surrogate should not be filled")

    # Fill the outs.x surrogate slot with FloatKrigingSurrogate
    surrogates_outs_x_slot = SlotFigure(workspace_page, 'top.mm.surrogates.outs.x')
    surrogates_outs_x_slot.fill_from_library('FloatKrigingSurrogate')

    # One should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates :
        if "filled" in surrogate.get_attribute('class') :
            num_surrogates_filled += 1
    eq(1, num_surrogates_filled,
       "Exactly one surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Fill the outs.y surrogate slot with KrigingSurrogate
    surrogates_outs_x_slot = SlotFigure(workspace_page, 'top.mm.surrogates.outs.y')
    surrogates_outs_x_slot.fill_from_library('KrigingSurrogate')

    # Two should be filled now
    time.sleep(1.5)  # give it a bit to update the figure
    num_surrogates_filled = 0
    surrogates = browser.find_elements_by_xpath("//div[starts-with( @id,'SlotFigure-top-mm-surrogates')]")
    for surrogate in surrogates :
        if "filled" in surrogate.get_attribute('class') :
            num_surrogates_filled += 1
    eq(2, num_surrogates_filled,
       "Exactly two surrogate slot should be filled but %d are filled" % num_surrogates_filled)

    # Clean up.
    closeout(project_dict, workspace_page)

if __name__ == '__main__':
    main()
