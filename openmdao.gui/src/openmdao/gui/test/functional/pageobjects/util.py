import time

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement, TextElement


# Set this True on fatal driver errors.
_ABORT = False


def abort(value=None):
    """ Return current abort status and optionally update it. """
    global _ABORT
    current = _ABORT
    if value is not None:
        _ABORT = value
    return current


class ValuePrompt(BasePageObject):
    """ Overlay displayed by ``openmdao.Util.promptForValue()``. """

    prompt = TextElement((By.ID, 'get-value-prompt'))
    value = InputElement((By.ID, 'get-value-input'))
    ok_button = ButtonElement((By.ID, 'get-value-ok'))
    cancel_button = ButtonElement((By.ID, 'get-value-cancel'))

    def set_value(self, value):
        self.value = value + Keys.RETURN

    def set_text(self, text):
        self.value = text

    def click_ok(self):
        self('ok_button').click()

    def click_cancel(self):
        self('cancel_button').click()


class NotifierPage(BasePageObject):
    """ Overlay displayed by ``openmdao.Util.notify()``. """

    message = TextElement((By.ID, 'notify-msg'))
    ok_button = ButtonElement((By.ID, 'notify-ok'))

    @staticmethod
    def wait(browser, port, timeout=TMO):
        """ Wait for notification. Returns notification message. """
        time.sleep(0.1)  # Pacing.
        page = NotifierPage(browser, port)
        WebDriverWait(browser, timeout).until(
            lambda browser: browser.find_element(*page('message')._locator))
        message = page.message
        page('ok_button').click()
        return message

