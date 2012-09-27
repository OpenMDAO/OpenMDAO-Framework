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
    clear_button  = ButtonElement((By.XPATH, 'ul/li[5]'))

    def __init__(self, browser, port):
        super(LogViewer, self).__init__(browser, port, (By.ID, 'logframe'))
        time.sleep(1)  # Wait for display to load-up.

    def clear(self):
        """ Clear display. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('clear_button').click()

    def filter(self):
        """ Return filtering dialog. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('filter_button').click()
        return FilterDialog.verify(self.browser, self.port)

    def pause(self):
        """ Pause/resume display. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        text = self('pause_button').text
        self('pause_button').click()
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

