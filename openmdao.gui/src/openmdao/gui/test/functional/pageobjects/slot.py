import logging

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject
from component import ComponentPage


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

    def fill_from_library(self, classname):
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

