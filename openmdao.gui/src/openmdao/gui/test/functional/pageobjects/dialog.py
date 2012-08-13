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


