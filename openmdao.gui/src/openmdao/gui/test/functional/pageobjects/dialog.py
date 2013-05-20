import time

from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, TextElement


class BootstrapModal(BasePageObject):

    modal_title = TextElement((By.XPATH, "div[@class='modal-header']/h3"))
    close_button = TextElement((By.XPATH, "div[@class='modal-header']/button"))

    """ Base for Twitter Bootstrap modals """
    def __init__(self, browser, port, locator):
        root = WebDriverWait(browser, TMO).until(
                lambda browser: browser.find_element(*locator))
        super(BootstrapModal, self).__init__(browser, port, root)

    @property
    def is_visible(self):
        """ True if modal is visible. """
        return self('modal_title').is_visible

    def close(self):
        """ Close modal. """
        self('close_button').click()


class DialogPage(BasePageObject):
    """ Base for various dialog pages. """

    dialog_title = TextElement((By.XPATH, '../div/span'))
    close_button = ButtonElement((By.XPATH, '../div/a/span'))
    dialog_resize = ButtonElement((By.XPATH, '../div[contains(@class, "ui-resizable-se")]'))

    def __init__(self, browser, port, locator):
        root = WebDriverWait(browser, TMO).until(
                   lambda browser: browser.find_element(*locator))
        super(DialogPage, self).__init__(browser, port, root)

    @property
    def is_visible(self):
        """ True if dialog is visible. """
        return self('dialog_title').is_visible

    def close(self):
        """ Close dialog. """
        # Ensure close button is on screen.
        width = self.browser.get_window_size()['width']
        x = self('close_button').location['x']
        shift = width - x
        if shift < 0:
            self.move(shift, 0)
        self('close_button').click()

    def move(self, delta_x, delta_y):
        """ Move dialog. """
        chain = ActionChains(self.browser)
        chain.click_and_hold(self('dialog_title').element)
        chain.move_by_offset(delta_x, delta_y)
        chain.release(None)
        chain.perform()
        time.sleep(0.5)

# Does not work on Windows :-(
#    def resize(self, delta_x, delta_y):
#        """ Resize dialog. """
#        chain = ActionChains(self.browser)
#        chain.click_and_hold(self('dialog_resize').element)
#        chain.move_by_offset(delta_x, delta_y).perform()
#        chain.click()  # Not clear why click is needed here.
#        chain.release(None).perform()


class NotifyDialog(DialogPage):
    """The dialog that appears when there is an error"""

    okButton = ButtonElement((By.ID, 'notify-ok'))

    def __init__(self, browser, port):
        # The div that contains the actual message has a div of notify-msg.
        #   The div for the dialog is the parent of that div
        super(NotifyDialog, self).__init__(browser, port, (By.XPATH, '//div[@id="notify-msg"]/..'))

    def close(self):
        """ Close dialog. """
        self('okButton').click()
