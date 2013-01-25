import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, TextElement


class LogViewer(DialogPage):
    """ Log message viewer. """

    data = TextElement((By.ID, 'logdata'))

    # Context menu.
    pause_button  = ButtonElement((By.XPATH, 'ul/li[1]'))
    popout_button = ButtonElement((By.XPATH, 'ul/li[2]'))
    filter_button = ButtonElement((By.XPATH, 'ul/li[3]'))
    copy_button   = ButtonElement((By.XPATH, 'ul/li[4]'))
    clear_button  = ButtonElement((By.XPATH, 'ul/li[6]'))

    def __init__(self, browser, port):
        super(LogViewer, self).__init__(browser, port, (By.ID, 'logframe'))
        time.sleep(1)  # Wait for display to load-up.

    def clear(self):
        """ Clear display. """
        self._context_click('clear_button')

    def filter(self):
        """ Return filtering dialog. """
        self._context_click('filter_button')
        return FilterDialog.verify(self.browser, self.port)

    def pause(self):
        """ Pause/resume display. """
        return self._context_click('pause_button')

    def popout(self):
        """ Pop-out display to separate window. """
        return self._context_click('popout_button')

    def _context_click(self, name):
        """ Display context menu. """
        chain = ActionChains(self.browser)
        # Just using center of self.root had an isolated 'overlap' failure.
        chain.move_to_element_with_offset(self('data').element, 2, 2)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)  # Wait for menu to display.
        text = self(name).text
        self(name).click()
        time.sleep(0.5)  # Wait to pop-down.
        return text

    def get_messages(self):
        """ Return messages as a list. """
        time.sleep(0.5)  # Wait for update.
        return self.data.split('\n')


class FilterDialog(DialogPage):
    """ Log filtering. """

    critical_button = ButtonElement((By.ID, 'logfilter-critical'))
    error_button    = ButtonElement((By.ID, 'logfilter-error'))
    warning_button  = ButtonElement((By.ID, 'logfilter-warning'))
    info_button     = ButtonElement((By.ID, 'logfilter-info'))
    debug_button    = ButtonElement((By.ID, 'logfilter-debug'))

    ok_button     = ButtonElement((By.XPATH, "..//button[@id='logfilter-ok']"))
    cancel_button = ButtonElement((By.XPATH, "..//button[@id='logfilter-cancel']"))

    def __init__(self, browser, port):
        super(FilterDialog, self).__init__(browser, port, (By.ID, 'logfilter'))

