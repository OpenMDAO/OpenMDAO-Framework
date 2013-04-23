import logging
import time

from nose.tools import eq_ as eq

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from component import ComponentPage

from util import ArgsPrompt


class SlotFigure(BasePageObject):
    """ Represents a slot in an editor's Slots page. """

    def __init__(self, workspace, pathname):
        super(SlotFigure, self).__init__(workspace.browser, workspace.port)
        self.workspace = workspace
        self.pathname = pathname

    @property
    def slot(self):
        locator = (By.ID, 'SlotFigure-%s' % self.pathname.replace('.', '-'))
        return self.workspace.browser.find_element(*locator)

    def editor_page(self):
        """ Return :class:`ComponentPage` for this component. """
        for retry in range(3):
            try:
                chain = ActionChains(self.browser)
                chain.double_click(self.slot).perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('StaleElementReferenceException'
                                    ' in SlotFigure.editor_page()')
                else:
                    raise
            else:
                break
        editor_id = 'CE-%s' % self.pathname.replace('.', '-')
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

    def fill_from_library(self, classname, args=None):
        """ Fill slot with `classname` instance from library. """
        for retry in range(3):
            try:
                button = self.workspace.find_library_button(classname)
                chain = ActionChains(self.browser)
                chain.move_to_element(button)
                chain.click_and_hold(button)
                chain.move_to_element(self.slot)
                chain.move_by_offset(20, 20)
                chain.release(None)
                chain.perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('StaleElementReferenceException'
                                    ' in SlotFigure.fill_from_library()')
                else:
                    raise
            else:
                break
            
        page = ArgsPrompt(self.browser, self.port)
        argc = page.argument_count()
        if argc > 0:
            if args is not None:
                for i, arg in enumerate(args):
                    page.set_argument(i, arg)
            page.click_ok()

        # Check that the prompt is gone so we can distinguish a prompt problem
        # from a dataflow update problem.
        #time.sleep(0.25)
        #self.browser.implicitly_wait(1)  # We don't expect to find anything.
        #try:
        #    eq(len(self.browser.find_elements(*page('prompt')._locator)), 0)
        #finally:
        #    self.browser.implicitly_wait(TMO)            
            
            

