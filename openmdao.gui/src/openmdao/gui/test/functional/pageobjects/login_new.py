from selenium.webdriver.common.by import By

from basepageobject import BasePageObject
from elements import ButtonElement, InputElement


class LoginPage(BasePageObject):
    """ There doesn't seem to be a 'login' page anymore... """

#    url = '/accounts/login/?next=/'
    url = '/'

    username = InputElement((By.ID, 'id_username'))
    password = InputElement((By.ID, 'id_password'))
    submit_button = ButtonElement((By.XPATH,
                                   '/html/body/div/div[2]/form/input'))

    def login_successfully(self, username, password):
        """ Login using valid parameters. """
        self.username = username
        self.password = password
        self.submit()
        from project import ProjectsPage
        return ProjectsPage(self.browser, self.port)

    def login_unsuccessfully(self, username, password):
        """ Login using invalid parameters. """
        self.username = username
        self.password = password
        self.submit()
        return LoginPage(self.browser, self.port)

    def magic_login(self, username, password):
        '''Need a way to login to the app directly,
        not manually via the GUI'''
        pass
    
    def submit(self):
        """ Clicks the login button. """
        self('submit_button').click()

