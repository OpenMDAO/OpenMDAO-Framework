"""
Pages related to project management.
"""

import random
import string

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement, TextElement


class ProjectPage(BasePageObject):
    """ Superclass of :class:`NewProjectPage` and :class:`ProjectInfoPage`. """

    title_prefix = 'Project:'

    project_name = InputElement((By.ID, 'id_projectname'))
    description = InputElement((By.ID, 'id_description'))
    version = InputElement((By.ID, 'id_version'))


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

    def create_project(self, project_name, description, version):
        """ Create a project, returns :class:`ProjectInfoPage`. """
        self.project_name = project_name
        self.description = description
        self.version = version
        self.submit()
        title = ProjectInfoPage.project_title(project_name)
        return ProjectInfoPage.verify(self.browser, self.port, title)


class ProjectInfoPage(ProjectPage):
    """ Page displaying project information. """

    project_header = TextElement((By.XPATH, '/html/body/div/div[2]/h2'))
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

    @staticmethod
    def project_title(name):
        """ Return page title for project `name`. """
        return '%s %s' % (ProjectInfoPage.title_prefix, name)

    def load_project(self):
        """ Clicks the 'load' button. Returns :class:`WorkspacePage`. """
        self('load_button').click()
        from workspace import WorkspacePage
        return WorkspacePage.verify(self.browser, self.port)

    def delete_project(self):
        """ Clicks the 'delete' button. Returns :class:`ProjectsListPage`. """
        self('delete_button').click()
        return ProjectsListPage.verify(self.browser, self.port)

    def go_to_projects_page(self):
        """ Clicks the 'back' button. Returns :class:`ProjectsListPage`. """
        self('back_button').click()
        return ProjectsListPage.verify(self.browser, self.port)

    def logout(self):
        """
        Clicks the 'logout' button. Returns :class:`LoginPage`.

        ..warning::

            Since login isn't done anymore, this actually results in
            exiting the server.

        """
        self('logout_button').click()
        from login import LoginPage
        return LoginPage.verify(self.browser, self.port)


class ProjectsListPage(BasePageObject):
    """ Displays list of projects. """

    url = '/projects'
    title_prefix = 'Projects'

    new_button = ButtonElement((By.LINK_TEXT, 'Start new project'))
    add_button = ButtonElement((By.LINK_TEXT, 'Add existing project'))
    logout_button = ButtonElement((By.LINK_TEXT, 'Exit'))

    def new_project(self):
        """ Clicks the 'new' button. Returns :class:`NewProjectPage`. """
        self('new_button').click()
        return NewProjectPage.verify(self.browser, self.port)

    def logout(self):
        """
        Clicks the 'logout' button. Returns :class:`LoginPage`.

        ..warning::

            Since login isn't done anymore, this actually results in
            exiting the server.

        """
        self('logout_button').click()
        from login import LoginPage
        return LoginPage.verify(self.browser, self.port)

    def contains(self, project_name):
        """ Returns True if `project_name` is in the list of projects. """
        return len(self.browser.find_elements_by_link_text(project_name)) > 0

    def open_project(self, project_name):
        """ Clicks the named link. Returns :class:`WorkspacePage`. """
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element.click()
        from workspace import WorkspacePage
        return WorkspacePage.verify(self.browser, self.port)

    def edit_project(self, project_name):
        """ Clicks the 'edit' button. Returns :class:`ProjectInfoPage`. """
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element = element.find_element_by_xpath('../../td[6]/form/input')
        element.click()
        title = ProjectInfoPage.project_title(project_name)
        return ProjectInfoPage.verify(self.browser, self.port, title)

