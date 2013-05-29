import logging
import time

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement
from component import ComponentPage


class SlotFigure(BasePageObject):
    """ Represents a slot in an editor's Slots page. """

    # Context menu.
    edit_button        = ButtonElement((By.XPATH, "./ul/li[text()='Edit']"))
    remove_button      = ButtonElement((By.XPATH, "./ul/li[text()='Remove']"))

    def __init__(self, browser, port, root):
        super(SlotFigure, self).__init__(browser, port, root)
        self._pathname = None

    @property
    def filled(self):
        """ property indicating if the slot is filled. """
        element = self.root.find_element_by_xpath('.')
        classes = element.get_attribute('class')
        return ('filled' in classes)

    def editor_page(self, double_click=True):
        """ Return :class:`ComponentPage` for this component. """
        for retry in range(3):
            try:
                chain = ActionChains(self.browser)
                if double_click:
                    chain.double_click(self.root).perform()
                else:
                    self._context_click('edit_button')
                chain.release(None).perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('StaleElementReferenceException'
                                    ' in SlotFigure.editor_page()')
                else:
                    raise
            else:
                break
        editor_id = 'ObjectFrame_%s' % self.pathname.replace('.', '-')
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

    def edit(self, offset=15):
        """ Edit this component. """
        self._context_click('edit_button', offset)
        editor_id = 'ObjectFrame_%s' % self.pathname.replace('.', '-')
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

    def remove(self, offset=15):
        """ Remove this component. """
        self._context_click('remove_button', offset)

    def _context_click(self, name, offset=15):
        """ Display context menu. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.root, offset, 15)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self(name).click()


def find_slot_figures(page):
    """ Returns workflow figure elements in `page`. """
    time.sleep(0.5)  # Pause for stable display.
    root = page.root or page.browser
    return root.find_elements_by_class_name('SlotFigure')


def find_slot_figure(page, name, prefix=None, retries=5):
    """ Return :class:`SlotFigure` for `name`. """
    root = page.root or page.browser
    for retry in range(retries):
        time.sleep(0.5)  # Pause for stable display.
        figures = root.find_elements_by_class_name('SlotFigure')
        if not figures:
            continue
        slot_name = None
        for figure in figures:
            page.browser.implicitly_wait(1)
            try:
                # a slot figure is a div containing a ul element (the context menu) and
                # one or more svg elements, each of which contains a rect and two texts
                # the first text contains the slot name
                texts = figure.find_elements_by_css_selector('svg text')
                if texts[0].text != '[':
                    slot_name = texts[0].text
                else:
                    slot_name = texts[-3].text  # last 3 are: name, type, rbracket
            except StaleElementReferenceException:
                logging.warning('get_workflow_figure:'
                                ' StaleElementReferenceException')
            else:
                if slot_name == name:
                    fig = SlotFigure(page.browser, page.port, figure)
                    if prefix is not None:
                        if prefix:
                            fig.pathname = '%s.%s' % (prefix, name)
                        else:
                            fig.pathname = name
                    return fig
            finally:
                page.browser.implicitly_wait(TMO)
    return None
