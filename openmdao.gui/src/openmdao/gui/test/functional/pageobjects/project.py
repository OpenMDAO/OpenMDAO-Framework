"""
Pages related to project management.
"""

import random
import string
import sys

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement, TextElement
from dialog import DialogPage, BootstrapModal

class ProjectsPage(BasePageObject):
    """ Displays list of projects. """

    url = '/projects'
    title_prefix = 'Projects'

    search_input = InputElement((By.XPATH, "//div[@id='project_table_filter']/label/input"))
    import_button = ButtonElement((By.LINK_TEXT, 'Import Project'))
    new_button = ButtonElement((By.LINK_TEXT, 'New Project'))
    logout_button = ButtonElement((By.LINK_TEXT, 'Exit'))

    def new_project(self):
        """ Clicks the 'new' button. Returns :class:`NewDialog`. """
        self('new_button').click()
        return NewDialog(self.browser, self.port, (By.ID, "newProjectModal"))

    def import_project(self):
        """ Clicks the 'import' button. Returns :class:`ImportDialog`. """
        self('import_button').click()
        return ImportDialog(self.browser, self.port, (By.ID, "importProjectModal"))

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
        self.search_input = project_name
        return len(self.browser.find_elements_by_link_text(project_name)) > 0

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

        edit_dialog = EditDialog(self.browser, self.port, (By.ID, "editProjectModal"))
        return edit_dialog 

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


        import time; time.sleep(5)
        
        delete_dialog.submit()

    def delete_all_test_projects(self, verbose=False):
        """ Removes all projects with 'test project' in the name. """
        elements = self.browser.find_elements_by_partial_link_text('testing project')
        while len(elements) > 0:
            for i in range(len(elements)):
                element = WebDriverWait(self.browser, TMO).until(
                    lambda browser: browser.find_element_by_partial_link_text('testing project'))

                project_name = element.text
                self.delete_project(project_name)
                if verbose:
                    print >>sys.stderr, 'Deleted', project_name
            # there may be more that were previously hidden due to the row limit
            elements = self.browser.find_elements_by_partial_link_text('testing project')

class MetadataModal(BootstrapModal):
    submit_button = None
    cancel_button = None

    project_name = InputElement((By.ID, 'id_project_name'))
    description = InputElement((By.ID, 'id_description'))
    version = InputElement((By.ID, 'id_version'))

    def __init__(self, browser, port, locator):
        super(MetadataModal, self).__init__(browser, port, locator)

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

    def __init__(self, browser, port, locator):
        super(NewDialog, self).__init__(browser, port, locator)
    
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

    def __init__(self, browser, port, locator):
        super(EditDialog, self).__init__(browser, port, locator)

class DeleteDialog(DialogPage):
    """ Dialog for deleting a project """
    delete_button = ButtonElement((By.XPATH, "div[@class='modal-footer']/a[text()='OK']"))
    cancel_button = ButtonElement((By.XPATH, "div[@class='modal-footer']/a[text()='Cancel']"))

    def __init__(self, browser, port, locator):
        super(DeleteDialog, self).__init__(browser, port, locator)

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
    load_button = ButtonElement((By.XPATH, "form/div[@class='modal-footer']/button[text()='Load Project']"))

    ##### need to add input element for file input
    #####      self.input_element = "" # path to project file
    projectfile_input = InputElement((By.ID, 'id_projectfile'))


    def __init__(self, browser, port, locator):
        super(ImportDialog, self).__init__(browser, port, locator)

    def load_project(self, projectfile_path ):
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

