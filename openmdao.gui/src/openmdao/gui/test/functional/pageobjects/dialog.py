from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, TextElement


class DialogPage(BasePageObject):
    """ Base for various dialog pages. """

    dialog_title = TextElement((By.XPATH, '../div/span'))
    close_button = ButtonElement((By.XPATH, '../div/a'))

    def __init__(self, browser, port, locator):
        root = WebDriverWait(browser, TMO).until(
                   lambda browser: browser.find_element(*locator))
        super(DialogPage, self).__init__(browser, port, root)

    def close(self):
        """ Close dialog. """
        self('close_button').click()

