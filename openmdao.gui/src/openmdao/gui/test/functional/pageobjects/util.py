import Queue
import threading
import time
import logging

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import WebDriverException, TimeoutException

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


class ArgsPrompt(BasePageObject):
    """ Dialog displayed by ``openmdao.Util.promptForArgs()``. """

    prompt = TextElement((By.ID, 'get-args-prompt'))
    name = InputElement((By.ID, 'get-args-name'))
    ok_button = ButtonElement((By.ID, 'get-args-ok'))
    cancel_button = ButtonElement((By.ID, 'get-args-cancel'))

    def set_name(self, value):
        self.name = value + Keys.RETURN

    def set_text(self, text):
        self.name = text

    def set_argument(self, index, text):
        table = self.browser.find_element(By.ID, 'get-args-tbl')
        arg_inputs = table.find_elements(By.XPATH, 'tbody/tr/td/input')
        arg_inputs[index].send_keys(text)

    def argument_count(self):
        self.browser.implicitly_wait(1)
        try:
            table = self.browser.find_elements_by_css_selector('#get-args-tbl')
        finally:
            self.browser.implicitly_wait(TMO)
        if table:
            arg_inputs = table[0].find_elements(By.XPATH, 'tbody/tr/td/input')
            return len(arg_inputs)
        else:
            return 0

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
    Overlay displayed by ``openmdao.Util.notify()``. There can potentially be
    more than one of these displayed at the same time.
    """

    @staticmethod
    def wait(parent, timeout=TMO, base_id=None, retries=5):
        """
        Wait for notification. Returns notification message.
        If `retries` <= 0 then we're checking for something we anticipate
        won't be there, so don't worry if it isn't.
        """
        for retry in range(max(retries, 1)):
            time.sleep(0.5)  # Pacing.
            base_id = base_id or 'notify'
            msg_id = base_id + '-msg'
            ok_id  = base_id + '-ok'
            try:
                msg = WebDriverWait(parent.browser, timeout).until(
                          lambda browser: browser.find_element(By.ID, msg_id))
                ok = WebDriverWait(parent.browser, timeout).until(
                          lambda browser: browser.find_element(By.ID, ok_id))
            except WebDriverException as err:
                if retries > 0 or not isinstance(err, TimeoutException):
                    logging.warning('NotifierPage.wait(%s): %r', base_id, err)
            else:
                # Sometimes the 'Ok' button is temporarily obscured.
                try:
                    message = msg.text
                    ok.click()
                    return message
                except WebDriverException as err:
                    logging.warning('NotifierPage.wait(%s): %r', base_id, err)

        if retries > 0 or not isinstance(err, TimeoutException):
            raise err


class SafeBase(object):
    """ Wrap a WebDriver delegate to handle driver hangs. """

    def __init__(self, delegate, invoker):
        self._delegate = delegate
        self._invoker = invoker

    def _invoke(self, what, args, kwargs):
        """ Send request to worker and wait (with timeout) for results. """
        if what == 'getattr':
            return self._invoker.invoke(getattr, args, kwargs)
        else:
            method = getattr(self._delegate, what)
            return self._invoker.invoke(method, args, kwargs)


class SafeElementBase(SafeBase):
    """ Wrap a WebDriver element or driver to handle driver hangs. """

    def find_element(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element', args, kwargs))

    def find_elements(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements', args, kwargs))

    def find_element_by_tag_name(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_tag_name', args, kwargs))

    def find_elements_by_tag_name(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements_by_tag_name', args, kwargs))

    def find_element_by_class_name(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_class_name', args, kwargs))

    def find_elements_by_class_name(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements_by_class_name', args, kwargs))

    def find_element_by_css_selector(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_css_selector', args, kwargs))

    def find_elements_by_css_selector(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements_by_css_selector', args, kwargs))

    def find_element_by_id(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_id', args, kwargs))

    def find_element_by_link_text(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_link_text', args, kwargs))

    def find_elements_by_link_text(self, *args, **kwargs):
        return self._wrap(self._invoke('find_elements_by_link_text', args, kwargs))

    def find_element_by_partial_link_text(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_partial_link_text', args, kwargs))

    def find_elements_by_partial_link_text(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements_by_partial_link_text', args, kwargs))

    def find_element_by_xpath(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_element_by_xpath', args, kwargs))

    def find_elements_by_xpath(self, *args, **kwargs):
        return self._wrap(
            self._invoke('find_elements_by_xpath', args, kwargs))

    def _wrap(self, element):
        """ Wrap `element` as :class:`SafeElement`. """
        if element is not None:
            if isinstance(element, list):
                element = [SafeElement(item, self._invoker) for item in element]
            else:
                element = SafeElement(element, self._invoker)
        return element


class SafeElement(SafeElementBase):
    """ Wrap a WebDriver element to handle driver hangs. """

    @property
    def id(self):
        return self._invoke('getattr', (self._delegate, 'id'), {})

    @property
    def location(self):
        return self._invoke('getattr', (self._delegate, 'location'), {})

    @property
    def text(self):
        return self._invoke('getattr', (self._delegate, 'text'), {})

    def clear(self, *args, **kwargs):
        return self._invoke('clear', args, kwargs)

    def click(self, *args, **kwargs):
        return self._invoke('click', args, kwargs)

    def get_attribute(self, *args, **kwargs):
        return self._invoke('get_attribute', args, kwargs)

    def is_displayed(self, *args, **kwargs):
        return self._invoke('is_displayed', args, kwargs)

    def is_enabled(self, *args, **kwargs):
        return self._invoke('is_enabled', args, kwargs)

    def is_selected(self, *args, **kwargs):
        return self._invoke('is_selected', args, kwargs)

    def send_keys(self, *args, **kwargs):
        return self._invoke('send_keys', args, kwargs)

    def value_of_css_property(self, *args, **kwargs):
        return self._invoke('value_of_css_property', args, kwargs)


class SafeAlert(SafeBase):
    """ Wrap a WebDriver :class:`Alert` to handle driver hangs. """

    @property
    def text(self):
        return self._invoke('getattr', (self._delegate, 'text'), {})

    def dismiss(self, *args, **kwargs):
        return self._invoke('dismiss', args, kwargs)

    def accept(self, *args, **kwargs):
        return self._invoke('accept', args, kwargs)

    def send_keys(self, *args, **kwargs):
        return self._invoke('send_keys', args, kwargs)


class SafeDriver(SafeElementBase):
    """ Wrap :class:`WebDriver` to handle driver hangs. """

    def __init__(self, driver):
        # All elements share same invoker.
        super(SafeDriver, self).__init__(driver, SafeInvoker())

    def __repr__(self):
        return 'SafeDriver(%s)' % (self._delegate.__class__.__name__)

    @property
    def current_window_handle(self):
        return self._invoke('getattr', (self._delegate, 'current_window_handle'), {})

    @property
    def window_handles(self):
        return self._invoke('getattr', (self._delegate, 'window_handles'), {})

    @property
    def title(self):
        return self._invoke('getattr', (self._delegate, 'title'), {})

    def close(self, *args, **kwargs):
        return self._invoke('close', args, kwargs)

    def execute(self, *args, **kwargs):
        return self._invoke('execute', args, kwargs)

    def execute_script(self, *args, **kwargs):
        return self._invoke('execute_script', args, kwargs)

    def get(self, *args, **kwargs):
        return self._invoke('get', args, kwargs)

    def get_window_size(self, *args, **kwargs):
        return self._invoke('get_window_size', args, kwargs)

    def set_window_size(self, *args, **kwargs):
        return self._invoke('set_window_size', args, kwargs)

    def set_window_position(self, *args, **kwargs):
        return self._invoke('set_window_position', args, kwargs)

    def implicitly_wait(self, *args, **kwargs):
        return self._invoke('implicitly_wait', args, kwargs)

    def quit(self, *args, **kwargs):
        self._invoke('quit', args, kwargs)
        self._invoker.shutdown()

    def save_screenshot(self, *args, **kwargs):
        return self._invoke('save_screenshot', args, kwargs)

    def switch_to_alert(self, *args, **kwargs):
        alert = self._invoke('switch_to_alert', args, kwargs)
        if alert is not None:
            alert = SafeAlert(alert, self._invoker)
        return alert

    def switch_to_window(self, *args, **kwargs):
        return self._invoke('switch_to_window', args, kwargs)


class SafeInvoker(object):
    """ Invokes methods on a delegate with a timeout. """

    def __init__(self):
        self._request_q = Queue.Queue()
        self._reply_q = Queue.Queue()
        self._worker = threading.Thread(target=self._service_loop)
        self._worker.daemon = True
        self._worker.start()

    def invoke(self, method, args, kwargs):
        """ Send request to worker and wait (with timeout) for results. """
        self._request_q.put((method, args, kwargs))
        try:
            retval, exc = self._reply_q.get(timeout=2*60)
        except Queue.Empty:
            self._request_q.put((None, None, None))  # In case it finishes.
            abort(True)
            raise RuntimeError('WebDriver hung :-(')
        if exc is not None:
            raise exc
        return retval

    def shutdown(self):
        """ Shutdown worker thread. """
        self._request_q.put((None, None, None))

    def _service_loop(self):
        while True:
            method, args, kwargs = self._request_q.get()
            if method is None:
                return  # Shutdown.

            exc = None
            retval = None
            try:
                retval = method(*args, **kwargs)
            except Exception as exc:
                pass  # We'll pass exc back for the caller to raise.

            self._request_q.task_done()
            self._reply_q.put((retval, exc))
