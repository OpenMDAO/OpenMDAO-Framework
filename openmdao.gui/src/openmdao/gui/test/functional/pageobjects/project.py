"""
Pages related to project management.
"""

import string
import random

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject
from elements import ButtonElement, CheckboxElement, InputElement, TextElement


class ProjectPage(BasePageObject):
    '''A super class for the new project page
    and the project info page where details of the
    project can be edited
    '''
    
    project_name = InputElement((By.ID, 'id_projectname'))
    description = InputElement((By.ID, 'id_description'))
    version = InputElement((By.ID, 'id_version'))
    shared = CheckboxElement((By.ID, 'id_shared'))


class NewProjectPage(ProjectPage):
    """ Page for creating a new project. """

    create_button = ButtonElement((By.XPATH,
                                   '/html/body/div/div[2]/form/input'))

    def __init__(self, browser, port):
        super(NewProjectPage, self).__init__(browser, port)

    def submit(self):
        """ Clicks the 'create' button. """
        self('create_button').click()

    @staticmethod
    def get_random_project_name(size=6, chars=None):
        """ Return a random project name. """
        chars = chars or string.ascii_uppercase + string.digits
        return "testing project " + \
               ''.join(random.choice(chars) for x in range(size))

    def create_project(self, project_name, description, version, shared):
        """ Create a project, returns :class:`ProjectInfoPage`. """
        self.project_name = project_name
        self.description = description
        self.version = version
        self.shared = shared
        self.submit()
        return ProjectInfoPage(self.browser, self.port)


class ProjectInfoPage(ProjectPage):
    """ Page displaying project information. """

    project_title = TextElement((By.XPATH, '/html/body/div/div[2]/h2'))
    update_button = ButtonElement((By.XPATH,
                                   '/html/body/div/div[2]/form/input'))
    delete_button = ButtonElement((By.XPATH,
                                   '/html/body/div/div[2]/form[2]/input[2]'))
    load_button = ButtonElement((By.XPATH,
                                 '/html/body/div/div[2]/form[3]/input[2]'))
    save_button = ButtonElement((By.XPATH,
                                 '/html/body/div/div[2]/form[4]/input[2]'))
    back_button = ButtonElement((By.LINK_TEXT, 'Back to Projects'))
    logout_button = ButtonElement((By.LINK_TEXT, 'Exit'))

    def __init__(self, browser, port):
        super(ProjectInfoPage, self).__init__(browser, port)

    def load_project(self):
        """ Clicks the 'load' button. Returns :class:`WorkspacePage`. """
        self('load_button').click()
        from workspace import WorkspacePage
        return WorkspacePage(self.browser, self.port)

    def delete_project(self):
        """ Clicks the 'delete' button. Returns :class:`ProjectsListPage`. """
        self('delete_button').click()
        return ProjectsListPage(self.browser, self.port)

    def go_to_projects_page(self):
        """ Clicks the 'back' button. Returns :class:`ProjectsListPage`. """
        self('back_button').click()
        return ProjectsListPage(self.browser, self.port)

    def logout(self):
        """
        Clicks the 'logout' button. Returns :class:`LoginPage`.

        ..warning::

            Since login isn't done anymore, this actually results in
            exiting the server.

        """
        self('logout_button').click()
        from login import LoginPage
        return LoginPage(self.browser, self.port)


class ProjectsListPage(BasePageObject):
    """ Displays list of projects. """

    new_button = ButtonElement((By.CSS_SELECTOR,
                                'html body div#body div.content p a'))
    logout_button = ButtonElement((By.LINK_TEXT, 'Exit'))

    def __init__(self, browser, port):
        super(ProjectsListPage, self).__init__(browser, port)
        self._page_url = "http://localhost:%d/projects" % self.port

    def new_project(self):
        """ Clicks the 'new' button. Returns :class:`NewProjectPage`. """
        self('new_button').click()
        return NewProjectPage(self.browser, self.port)

    def logout(self):
        """
        Clicks the 'logout' button. Returns :class:`LoginPage`.

        ..warning::

            Since login isn't done anymore, this actually results in
            exiting the server.

        """
        self('logout_button').click()
        WebDriverWait(self.browser, 10).until(
            lambda browser: browser.title.startswith('Login'))
        from login import LoginPage
        return LoginPage(self.browser, self.port)

    def contains(self, project_name):
        """ Returns True if `project_name` is in the list of projects. """
        return len(self.browser.find_elements_by_link_text(project_name)) > 0

    def open_project(self, project_name):
        """ Clicks the 'open' button. Returns :class:`ProjectInfoPage`. """
        self.browser.find_element_by_link_text(project_name).click()
        return ProjectInfoPage(self.browser, self.port)

