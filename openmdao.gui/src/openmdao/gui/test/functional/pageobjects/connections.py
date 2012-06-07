from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, InputElement


class ConnectionsPage(DialogPage):
    """ Connection editor page. """

    src_variable   = InputElement((By.ID, 'output_list'))
    dst_variable   = InputElement((By.ID, 'input_list'))
    connect_button = ButtonElement((By.ID, 'connect_button'))

    def connect(self, src, dst):
        """ Connect `src` to `dst`. """
        self.src_variable = src
        self.dst_variable = dst
        self('connect_button').click()

