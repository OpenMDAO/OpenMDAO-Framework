"""
Pages related to project management.
"""

import random
import string
import sys
import time

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement, TextElement
from dialog import DialogPage, BootstrapModal


class ProjectsPage(BasePageObject):
    """ Displays list of projects. """

    url = '/projects'
    title_prefix = 'Projects'

    welcome_text = TextElement((By.XPATH, "//h3/strong"))
    search_input = InputElement((By.XPATH, "//div[@id='project_table_filter']/label/input"))
    import_button = ButtonElement((By.LINK_TEXT, 'Import Project'))
    new_button = ButtonElement((By.LINK_TEXT, 'New Project'))
    logout_button = ButtonElement((By.LINK_TEXT, 'Exit'))

    def new_project(self):
        """ Clicks the 'new' button. Returns :class:`NewDialog`. """
        self('new_button').click()
        page = NewDialog(self.browser, self.port, (By.ID, "newProjectModal"))
        WebDriverWait(self.browser, TMO).until(
                      lambda browser: page.modal_title[:11] == 'New Project')
        return page

    def import_project(self):
        """ Clicks the 'import' button. Returns :class:`ImportDialog`. """
        self('import_button').click()
        page = ImportDialog(self.browser, self.port, (By.ID, "importProjectModal"))
        time.sleep(1)  # Wait for silly fade-in.
        return page

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

    def contains(self, project_name, expected=True):
        """ Returns True if `project_name` is in the list of projects. """
        if expected:
            self.search_input = project_name
            elements = self.browser.find_elements_by_link_text(project_name)
        else:
            self.browser.implicitly_wait(1)  # Not expecting to find anything.
            try:
                self.search_input = project_name  # No search input if no projects.
                elements = self.browser.find_elements_by_link_text(project_name)
            except TimeoutException:
                elements = []
            finally:
                self.browser.implicitly_wait(TMO)
        return len(elements) > 0

    def open_project(self, project_name):
        """ Clicks the named link. Returns :class:`WorkspacePage`. """
        self.search_input = project_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element.click()
        from workspace import WorkspacePage
        return WorkspacePage.verify(self.browser, self.port)

    def edit_project(self, project_name):
        """ Clicks the 'edit' button. Returns :class:`EditDialog`. """
        self.search_input = project_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element = element.find_element_by_xpath('../../td[6]/a')
        element.click()

        page = EditDialog(self.browser, self.port, (By.ID, "editProjectModal"))
        time.sleep(1)  # Wait for silly fade-in.
        return page

    def export_project(self, project_name):
        """ Clicks the 'export' button. Returns :class:`ExportDialog`. """
        self.search_input = project_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element = element.find_element_by_xpath('../../td[6]/form[2]/button')
        element.click()

    def delete_project(self, project_name):
        self.search_input = project_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_link_text(project_name))
        element = element.find_element_by_xpath('../../td[6]/form[1]/button')
        element.click()

        delete_dialog = DeleteDialog(self.browser, self.port, (By.XPATH, "/html/body/div[2]"))
        time.sleep(1)  # Wait for silly fade-in.
        delete_dialog.submit()
        time.sleep(1)  # Wait for silly fade-out.

    def delete_projects(self, project_filter, verbose=False):
        """ Removes all projects with 'test project' in the name. """
        self.search_input = project_filter + '\n'
        elements = self.browser.find_elements_by_partial_link_text(project_filter)
        while len(elements) > 0:
            for i in range(len(elements)):
                element = WebDriverWait(self.browser, TMO).until(
                    lambda browser: browser.find_element_by_partial_link_text(project_filter))

                project_name = element.text
                self.delete_project(project_name)
                if verbose:
                    print >>sys.stderr, 'Deleted', project_name
                self.search_input = project_filter + '\n'
            # there may be more that were previously hidden due to the row limit
            elements = self.browser.find_elements_by_partial_link_text(project_filter)


class MetadataModal(BootstrapModal):

    submit_button = None
    cancel_button = None

    project_name = InputElement((By.ID, 'id_project_name'))
    description = InputElement((By.ID, 'id_description'))
    version = InputElement((By.ID, 'id_version'))

    def cancel(self):
        """ Clicks the 'cancel' button """
        self('cancel_button').click()

    def submit(self):
        """ Clicks the 'confirm' button """
        self('submit_button').click()

    def submit_metadata(self, project_name, description=None, version=None):
        """ Submits metadata for a project. """
        self.project_name = project_name
        if description is not None:
            self.description = description
        if version is not None:
            self.version = version
        self.submit()


class NewDialog(MetadataModal):
    """ Modal for creating a new project """

    submit_button = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='New Project']"))
    cancel_button = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='Cancel']"))

    create_button = submit_button

    @staticmethod
    def get_random_project_name(size=6, chars=None):
        """ Return a random project name. """
        chars = chars or string.ascii_uppercase + string.digits
        return "testing project " + \
               ''.join(random.choice(chars) for x in range(size))


class EditDialog(MetadataModal):
    """ Dialog for exporting a project """

    submit_button = ButtonElement((By.XPATH, "div[2]/form/div[@class='modal-footer']/div/input"))
    cancel_button = ButtonElement((By.XPATH, "div[2]/form/div[@class='modal-footer']/div/button"))


class DeleteDialog(DialogPage):
    """ Dialog for deleting a project """

    delete_button = ButtonElement((By.XPATH, "div[@class='modal-footer']/a[text()='OK']"))
    cancel_button = ButtonElement((By.XPATH, "div[@class='modal-footer']/a[text()='Cancel']"))

    def submit(self):
        """Clicks the 'delete' button"""
        self('delete_button').click()

    def cancel(self):
        """Clicks the 'cancel' button"""
        self('cancel_button').click()


class ImportDialog(MetadataModal):
    """ Dialog for importing a project """

    submit_button = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='Import Project']"))
    cancel_button = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='Cancel']"))
    load_button   = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='Load Project']"))

    ##### need to add input element for file input
    #####      self.input_element = "" # path to project file
    projectfile_input = InputElement((By.ID, 'id_projectfile'))

    def load_project(self, projectfile_path):
        '''Just load the project using the dialog. This
           does not complete the import. That is a separate step
        '''
        self.projectfile_input = projectfile_path
        self('load_button').click()

    @staticmethod
    def get_random_project_name(size=6, chars=None):
        """ Return a random project name. """
        chars = chars or string.ascii_uppercase + string.digits
        return "testing project " + \
               ''.join(random.choice(chars) for x in range(size))
