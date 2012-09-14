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


class ConfirmationPage(BasePageObject):
    """ Overlay displayed by ``openmdao.Util.confirm()``. """

    prompt = TextElement((By.ID, 'confirm-prompt'))
    ok_button = ButtonElement((By.ID, 'confirm-ok'))
    cancel_button = ButtonElement((By.ID, 'confirm-cancel'))

    def __init__(self, parent):
        super(ConfirmationPage, self).__init__(parent.browser, parent.port)

    def click_ok(self):
        self('ok_button').click()

    def click_cancel(self):
        self('cancel_button').click()


class NotifierPage(object):
    """
    Overlay displayed by ``openmdao.Util.notify()``.
    There can potentially be more than one of these displayed at the same
    time in test mode (such as when saving a file with a syntax error).
    """

    @staticmethod
    def wait(parent, timeout=TMO, base_id=None):
        """ Wait for notification. Returns notification message. """
        time.sleep(0.5)  # Pacing.
        base_id = base_id or 'notify'
        msg_id = base_id+'-msg'
        ok_id  = base_id+'-ok'
        msg = WebDriverWait(parent.browser, timeout).until(
                  lambda browser: browser.find_element(By.ID, msg_id))
        ok = WebDriverWait(parent.browser, timeout).until(
                  lambda browser: browser.find_element(By.ID, ok_id))
        message = msg.text
        ok.click()
        return message

