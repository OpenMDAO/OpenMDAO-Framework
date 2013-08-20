import logging
import os.path
import time

from nose.tools import eq_ as eq

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException
from selenium.common.exceptions import TimeoutException

from basepageobject import BasePageObject, TMO
from component import NameInstanceDialog

from connections import ConnectionsPage
from dataflow import find_dataflow_figure, find_dataflow_figures, \
                     find_dataflow_component_names

from editor import EditorPage
from geometry import GeometryPage
from images import ImagesPage

from elements import ButtonElement, GridElement, InputElement, \
                     SelectElement, TextElement
from logviewer import LogViewer
from workflow import find_workflow_figure, find_workflow_figures, \
                     find_workflow_component_figures, \
                     find_workflow_component_figure

from util import ArgsPrompt, ValuePrompt, NotifierPage, ConfirmationPage


class WorkspacePage(BasePageObject):

    title_prefix = 'OpenMDAO:'

    # Top.
    project_menu      = ButtonElement((By.ID, 'project-menu'))
    commit_button     = ButtonElement((By.ID, 'project-commit'))
    revert_button     = ButtonElement((By.ID, 'project-revert'))
    reload_button     = ButtonElement((By.ID, 'project-reload'))
    close_button      = ButtonElement((By.ID, 'project-close'))
    exit_button       = ButtonElement((By.ID, 'project-exit'))

    view_menu         = ButtonElement((By.ID, 'view-menu'))
    objects_button    = ButtonElement((By.ID, 'view-components'))
    console_button    = ButtonElement((By.ID, 'view-console'))
    dataflow_button   = ButtonElement((By.ID, 'view-dataflow'))
    files_button      = ButtonElement((By.ID, 'view-files'))
    library_button    = ButtonElement((By.ID, 'view-library'))
    properties_button = ButtonElement((By.ID, 'view-properties'))
    workflow_button   = ButtonElement((By.ID, 'view-workflow'))
    refresh_button    = ButtonElement((By.ID, 'view-refresh'))

    tools_menu        = ButtonElement((By.ID, 'tools-menu'))
    editor_button     = ButtonElement((By.ID, 'tools-editor'))
    plotter_button    = ButtonElement((By.ID, 'tools-plotter'))
    drawing_button    = ButtonElement((By.ID, 'tools-drawing'))
    log_button        = ButtonElement((By.ID, 'tools-log'))

    help_menu         = ButtonElement((By.ID, 'help-menu'))
    doc_button        = ButtonElement((By.ID, 'help-doc'))

    about_button      = ButtonElement((By.ID, 'about-item'))

    # Left side.
    objects_tab       = ButtonElement((By.ID, 'otree_tab'))
    objects_selector  = SelectElement((By.ID, 'otree_pane_select'))
    files_tab         = ButtonElement((By.ID, 'ftree_tab'))

    # Object context menu.
    obj_properties = ButtonElement((By.XPATH, "//a[(@rel='properties')]"))
    obj_dataflow   = ButtonElement((By.XPATH, "//a[(@rel='show_dataflow')]"))
    obj_workflow   = ButtonElement((By.XPATH, "//a[(@rel='show_workflow')]"))
    obj_run        = ButtonElement((By.XPATH, "//a[(@rel='run')]"))
    #obj_toggle     = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))
    obj_remove     = ButtonElement((By.XPATH, "//a[(@rel='remove')]"))

    # File menu
    file_menu = ButtonElement((By.XPATH,
                           '/html/body/div/div/div/nav2/ul/li/a'))
    newfile_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/div/nav2/ul/li/ul/li[1]/a'))
    newfolder_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/div/nav2/ul/li/ul/li[2]/a'))
    add_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/div/nav2/ul/li/ul/li[3]/a'))
    delete_files_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/div/nav2/ul/li/ul/li[4]/a'))

    # File context menu.
    file_create   = ButtonElement((By.XPATH, "//a[(@rel='createFile')]"))
    file_add      = ButtonElement((By.XPATH, "//a[(@rel='addFile')]"))
    file_folder   = ButtonElement((By.XPATH, "//a[(@rel='createFolder')]"))
    file_view     = ButtonElement((By.XPATH, "//a[(@rel='viewFile')]"))
    file_edit     = ButtonElement((By.XPATH, "//a[(@rel='editFile')]"))
    file_exec     = ButtonElement((By.XPATH, "//a[(@rel='execFile')]"))
    file_image    = ButtonElement((By.XPATH, "//a[(@rel='viewImage')]"))
    file_geometry = ButtonElement((By.XPATH, "//a[(@rel='viewGeometry')]"))
    file_rename   = ButtonElement((By.XPATH, "//a[(@rel='renameFile')]"))
    file_delete   = ButtonElement((By.XPATH, "//a[(@rel='deleteFile')]"))
    file_toggle   = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))

    file_chooser = InputElement((By.ID, 'filechooser'))

    # Center.
    dataflow_tab = ButtonElement((By.ID, 'dataflow_tab'))
    workflow_tab = ButtonElement((By.ID, 'workflow_tab'))

    # Right side.
    properties_tab = ButtonElement((By.ID,  'properties_tab'))
    props_header   = TextElement((By.XPATH, "//div[@id='properties_pane']/h3"))
    props_inputs   = GridElement((By.XPATH, "//div[@id='properties_pane']/div[1]"))
    props_outputs  = GridElement((By.XPATH, "//div[@id='properties_pane']/div[2]"))

    library_tab    = ButtonElement((By.ID, 'library_tab'))
    library_search = InputElement((By.ID,  'objtt-filter'))
    library_clear  = ButtonElement((By.ID, 'objtt-clear'))

    library_item_docs     = ButtonElement((By.XPATH, "//ul[@id='lib-cmenu']/li[1]"))
    library_item_metadata = ButtonElement((By.XPATH, "//ul[@id='lib-cmenu']/li[2]"))

    # Bottom.
    history = TextElement((By.ID, 'history'))
    command = InputElement((By.ID, 'cmdline'))
    submit  = ButtonElement((By.ID, 'command-button'))

    def __init__(self, browser, port):
        super(WorkspacePage, self).__init__(browser, port)

        self.locators = {}
        self.locators["objects"] = \
            (By.XPATH, "//div[@id='otree_pane']//li[@path]")
        self.locators["files"] = \
            (By.XPATH, "//div[@id='ftree_pane']//a[@class='file ui-draggable']")

        # Wait for bulk of page to load.
        WebDriverWait(self.browser, TMO).until(
            lambda browser: len(self.get_dataflow_figures()) > 0)

        # Now wait for all WebSockets open.
        browser.execute_script('openmdao.project.webSocketsReady(2);')

        try:  # We may get 2 notifiers: sockets open and sockets closed.
            NotifierPage.wait(self, base_id='ws_open')
        except Exception as exc:
            if 'Element is not clickable' in str(exc):
                NotifierPage.wait(self, base_id='ws_closed')
                NotifierPage.wait(self, base_id='ws_open')
            else:
                raise
        else:
            self.browser.implicitly_wait(1)
            try:
                NotifierPage.wait(self, timeout=1, base_id='ws_closed',
                                  retries=0)
            except TimeoutException:
                pass  # ws closed dialog may not exist
            finally:
                self.browser.implicitly_wait(TMO)

    def find_library_button(self, name, delay=0):
        path = "//table[(@id='objtypetable')]//td[text()='%s']" % name
        for retry in range(5):
            try:
                element = WebDriverWait(self.browser, TMO).until(
                        lambda browser: browser.find_element(By.XPATH, path))
            except TimeoutException as err:
                logging.warning(str(err))
            else:
                break
        else:
            raise err
        if delay:
            time.sleep(delay)
        return element

    def find_object_button(self, name, delay=0):
        #path = "//div[@id='otree_pane']//li[(@path='%s')]//a" % name
        path = "//div[@id='otree_pane']//li[(@path='%s')]//a" % name
        for retry in range(5):
            try:
                element = WebDriverWait(self.browser, TMO).until(
                        lambda browser: browser.find_element(By.XPATH, path))
            except TimeoutException as err:
                logging.warning(str(err))
            else:
                break
        else:
            raise err
        if delay:
            time.sleep(delay)
        return element

    def do_command(self, cmd, timeout=TMO, ack=True):
        """ Execute a command. """
        self.command = cmd
        self('submit').click()
        if ack:
            NotifierPage.wait(self, timeout, base_id='command')

    def close_workspace(self, commit=False):
        """ Close the workspace page. Returns :class:`ProjectsPage`. """
        if commit:
            self.commit_project()
        self.browser.execute_script('openmdao.project.closeWebSockets();')
        NotifierPage.wait(self, base_id='ws_closed')
        self('project_menu').click()
        self('close_button').click()

        from project import ProjectsPage
        return ProjectsPage.verify(self.browser, self.port)

    def attempt_to_close_workspace(self, expectDialog, confirm):
        """ Close the workspace page. Returns :class:`ProjectsPage`. """
        self('project_menu').click()
        self('close_button').click()

        #if you expect the "close without saving?" dialog
        if expectDialog:
            dialog = ConfirmationPage(self)
            if confirm:  # close without saving
                self.browser.execute_script('openmdao.project.closeWebSockets();')
                NotifierPage.wait(self)
                dialog.click_ok()
                from project import ProjectsPage
                return ProjectsPage.verify(self.browser, self.port)
            else:  # return to the project, intact.
                dialog.click_cancel()
        else:      # no unsaved changes
            from project import ProjectsPage
            return ProjectsPage.verify(self.browser, self.port)

    def open_editor(self):
        """ Open code editor.  Returns :class:`EditorPage`. """
        self('tools_menu').click()
        self('editor_button').click()
        self.browser.switch_to_window('Code Editor')
        return EditorPage.verify(self.browser, self.port)

    def get_files(self):
        """ Return names in the file tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'ftree_pane'))
        # FIXME: absolute delay for tree population.
        time.sleep(1)
        file_items = self.browser.find_elements(*self.locators["files"])
        file_names = []
        for i in range(len(file_items)):
            for retry in range(10):  # This has had issues...
                try:
                    file_names.append(self.browser.find_elements(*self.locators["files"])[i].text.strip())
                except StaleElementReferenceException:
                    logging.warning('get_files: StaleElementReferenceException')
                else:
                    break
        return file_names

    def add_file(self, file_path):
        """ Read in `file_path` """
        if file_path.endswith('.pyc'):
            file_path = file_path[:-1]

        self('files_tab').click()
        self('file_menu').click()
        self('add_button').click()

        self.file_chooser = file_path
        time.sleep(1)  # Some extra time for the file tree update.
        self.find_file(os.path.basename(file_path))  # Verify added.
        time.sleep(1)  # Some extra time for the library update.

    def add_file_to_folder(self, folder_path, file_path):
        """ Read in `file_path` """
        if file_path.endswith('.pyc'):
            file_path = file_path[:-1]

        self('files_tab').click()
        element = self.find_file(folder_path)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        time.sleep(0.5)
        self('file_add').click()
        time.sleep(0.5)

        self.file_chooser = file_path
        time.sleep(1)  # Some extra time for the file tree update.

    def new_file_dialog(self):
        """ bring up the new file dialog """
        self('files_tab').click()
        self('file_menu').click()
        self('newfile_button').click()
        return ValuePrompt(self.browser, self.port)

    def new_folder_dialog(self):
        """ bring up the new folder dialog """
        self('files_tab').click()
        self('file_menu').click()
        self('newfolder_button').click()
        return ValuePrompt(self.browser, self.port)

    def new_file(self, filename):
        """ Make a new empty file `filename`. """
        page = self.new_file_dialog()
        page.set_value(filename)
        NotifierPage.wait(self)  # Wait for creation to complete.

    def new_folder(self, foldername):
        """ Make a new empty folder `foldername`. """
        page = self.new_folder_dialog()
        page.set_value(foldername)

    def find_file(self, filename, tmo=TMO):
        """ Return element corresponding to `filename`. """
        xpath = "//a[(@path='/%s')]" % filename
        return WebDriverWait(self.browser, tmo).until(
            lambda browser: browser.find_element_by_xpath(xpath))

    def edit_file(self, filename, dclick=True):
        """ Edit `filename` via double-click or context menu. """
        self('files_tab').click()
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        if dclick:  # This has had issues...
            for i in range(10):
                try:
                    chain.double_click(element).perform()
                except StaleElementReferenceException:
                    logging.warning('edit_file: StaleElementReferenceException')
                    element = self.find_file(filename, 1)
                    chain = ActionChains(self.browser)
                else:
                    break
        else:
            chain.context_click(element).perform()
            self('file_edit').click()
        self.browser.switch_to_window('Code Editor')
        return EditorPage.verify(self.browser, self.port)

    def view_file(self, filename):
        """ View image `filename` in another window via context menu. """
        self('files_tab').click()
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        self('file_view').click()
        self.browser.switch_to_window(self.browser.window_handles[-1])
        return BasePageObject.verify(self.browser, self.port)

    def view_image(self, filename, dclick=True):
        """ View image `filename` via double-click or context menu. """
        self('files_tab').click()
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        if dclick:  # This has had issues...
            for i in range(10):
                try:
                    chain.double_click(element).perform()
                except StaleElementReferenceException:
                    logging.warning('edit_file: StaleElementReferenceException')
                    element = self.find_file(filename, 1)
                    chain = ActionChains(self.browser)
                else:
                    break
        else:
            chain.context_click(element).perform()
            self('file_image').click()
        self.browser.switch_to_window(self.browser.window_handles[-1])
        return ImagesPage.verify(self.browser, self.port)

    def view_geometry(self, filename, dclick=True):
        """ View geometry `filename` via double-click or context menu. """
        self('files_tab').click()
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        if dclick:  # This has had issues...
            for i in range(10):
                try:
                    chain.double_click(element).perform()
                except StaleElementReferenceException:
                    logging.warning('edit_file: StaleElementReferenceException')
                    element = self.find_file(filename, 1)
                    chain = ActionChains(self.browser)
                else:
                    break
        else:
            chain.context_click(element).perform()
            self('file_geometry').click()
        self.browser.switch_to_window(self.browser.window_handles[-1])
        return GeometryPage.verify(self.browser, self.port)

    def delete_file(self, filename):
        """ Delete `filename`. """
        self('files_tab').click()
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        time.sleep(0.5)
        self('file_delete').click()
        time.sleep(0.5)

    def delete_files(self, file_paths):
        """ Delete all the files in the list `file_paths` """

        # need select all the files given in file_paths
        self('files_tab').click()
        for filename in file_paths:
            element = self.find_file(filename)
            chain = ActionChains(self.browser)
            #FIXME: Mac OSX does not use CONTROL key
            chain.key_down(Keys.CONTROL).click(element).key_up(Keys.CONTROL).perform()

        self('files_tab').click()
        self('file_menu').click()
        self('delete_files_button').click()

    def expand_folder(self, filename):
        """ Expands `filename`. """
        self('files_tab').click()
        xpath = "//div[@id='ftree_pane']//a[(@path='/%s')]/../ins" % filename
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        element.click()
        time.sleep(1)  # Wait for cute animation.

    def rename_file(self, old, new):
        """ Rename `old` to `new`. """
        self('files_tab').click()
        element = self.find_file(old)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        self('file_rename').click()
        prompt = ValuePrompt(self.browser, self.port)
        prompt.set_value(new)

    def toggle_files(self, filename):
        """ Toggle files display, using context menu of `filename`. """
        self('files_tab').click()
        time.sleep(0.5)
        element = self.find_file(filename)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        time.sleep(0.5)
        self('file_toggle').click()
        time.sleep(0.5)

    def commit_project(self, comment='no comment'):
        """ Commit current project. """
        self('project_menu').click()
        self('commit_button').click()
        page = ValuePrompt(self.browser, self.port)
        page.set_value(comment)
        NotifierPage.wait(self)

    def reload_project(self):
        """ Reload current project. """
        self('project_menu').click()
        self('reload_button').click()
        WorkspacePage.verify(self.browser, self.port)

    def get_objects_attribute(self, attribute, visible=False):
        """ Return list of `attribute` values for all objects. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'otree_pane'))
        object_elements = self.browser.find_elements(*self.locators["objects"])
        values = []
        for element in object_elements:
            if not visible or element.is_displayed():
                values.append(element.get_attribute(attribute))
        return values

    def select_objects_view(self, tree_name):
        """ Select the object tree view ('Components' or 'Workflow)'. """
        self('objects_selector').value = tree_name

    def select_object(self, component_name):
        """ Select `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree_pane']//li[(@path='%s')]//a" % component_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        element.click()

    def expand_object(self, component_name):
        """ Expands `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree_pane']//li[(@path='%s')]//ins" % component_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        element.click()
        time.sleep(1)  # Wait for cute animation.

    def show_dataflow(self, component_name):
        """ Show dataflow of `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree_pane']//li[(@path='%s')]//a" % component_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        element.click()
        time.sleep(0.5)
        # Try to recover from context menu not getting displayed.
        for retry in range(3):
            chain = ActionChains(self.browser)
            chain.context_click(element).perform()
            try:
                self('obj_dataflow').click()
                break
            except TimeoutException:
                if retry >= 2:
                    raise

    def show_workflow(self, component_name):
        """ Show workflow of `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree_pane']//li[(@path='%s')]//a" % component_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        element.click()
        time.sleep(0.5)
        # Try to recover from context menu not getting displayed.
        for retry in range(3):
            chain = ActionChains(self.browser)
            chain.context_click(element).perform()
            try:
                self('obj_workflow').click()
                break
            except TimeoutException:
                if retry >= 2:
                    raise

    def show_properties(self):
        """ Display properties. """
        # This has had some odd failures where the tab is highlighted as if
        # hovering over it, yet the Library tab is still the selected one.
        for retry in range(5):
            try:
                self('properties_tab').click()
                WebDriverWait(self.browser, 1).until(
                    lambda browser: self('props_header').is_visible)
            except TimeoutException:
                if retry:
                    logging.warning('TimeoutException in show_properties')
            else:
                break
        else:
            raise RuntimeError('Too many TimeoutExceptions')

    def show_library(self):
        """ Display library. """
        # For some reason the first try never works, so the wait is set
        # low and we expect to retry at least once.
        for retry in range(5):
            try:
                self('library_tab').click()
                WebDriverWait(self.browser, 1).until(
                    lambda browser: self('library_search').is_visible)
            except TimeoutException:
                if retry:
                    logging.warning('TimeoutException in show_library')
            else:
                break
        else:
            raise RuntimeError('Too many TimeoutExceptions')

    def set_library_filter(self, filter):
        """ Set the search filter text. """
        for retry in range(3):  # This has had issues...
            try:
                self.library_search = filter + Keys.RETURN
            except StaleElementReferenceException:
                logging.warning('set_library_filter:'
                                ' StaleElementReferenceException')
            else:
                break
        time.sleep(1)  # Wait for display update.

    def clear_library_filter(self):
        """ Clear the search filter via the 'X' button. """
        self('library_clear').click()
        time.sleep(0.5)  # Wait for display update.

    def get_object_types(self):
        """ Return displayed object types. """
        xpath = "//table[(@id='objtypetable')]//td"
        return [element.text for element
                              in self.browser.find_elements(By.XPATH, xpath)]

    def get_library_searches(self):
        """ Return stored library search terms. """
        searches = []

        self.library_search = 'searches'
        for menu in self.browser.find_elements(By.CLASS_NAME, 'ui-autocomplete'):
            items = menu.find_elements(By.CLASS_NAME, 'ui-menu-item')
            searches = [item.text for item in items]
            if len(searches) > 0 and searches[0] == 'In Project':
                break

        self.clear_library_filter()
        return searches

    def get_library_item(self, item_name):
        """ Return element for library item `item_name`. """
        xpath = "//table[(@id='objtypetable')]//td[(@modpath='%s')]" % item_name
        library_item = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_xpath(xpath))
        for retry in range(3):
            try:
                WebDriverWait(self.browser, TMO).until(
                    lambda browser: library_item.is_displayed())
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('get_library_item:'
                                    ' StaleElementReferenceException')
                    library_item = WebDriverWait(self.browser, TMO).until(
                        lambda browser: browser.find_element_by_xpath(xpath))
                else:
                    raise
            else:
                break
        return library_item

    def view_library_item_docs(self, item_name):
        chain = ActionChains(self.browser)
        current_windows = set(self.browser.window_handles)
        self.show_library()
        item = self.get_library_item(item_name)
        chain.context_click(item).perform()
        self("library_item_docs").click()
        new_windows = set(self.browser.window_handles) - current_windows
        docs_window = list(new_windows)[0]

        return docs_window

    def add_library_item_to_dataflow(self, item_name, instance_name,
                                     target_name=None, check=True,
                                     offset=None, prefix=None,
                                     args=None):
        """ Add component `item_name`, with name `instance_name`. """
        library_item = self.get_library_item(item_name)

        if target_name:
            target = self.get_dataflow_figure(target_name).get_drop_targets()[0]
            offset = offset or (2, 2)  # top left corner of target content area
        else:
            xpath = "//*[@id='-dataflow']"
            target = WebDriverWait(self.browser, TMO).until(
                           lambda browser: browser.find_element_by_xpath(xpath))
            offset = offset or (50, 50)

        for retry in range(3):
            try:
                chain = ActionChains(self.browser)
                if False:
                    chain.drag_and_drop(library_item, target)
                else:
                    chain.click_and_hold(library_item)
                    chain.move_to_element_with_offset(target,
                                                      offset[0], offset[1])
                    chain.release(None)
                chain.perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('add_library_item_to_dataflow:'
                                    ' StaleElementReferenceException')
                    library_item = self.get_library_item(item_name)
                    target = WebDriverWait(self.browser, TMO).until(
                           lambda browser: browser.find_element_by_xpath(xpath))
                else:
                    raise
            else:
                break

        page = ArgsPrompt(self.browser, self.port)
        argc = page.argument_count()
        page.set_name(instance_name)
        if argc > 0:
            if args is not None:
                for i, arg in enumerate(args):
                    page.set_argument(i, arg)
            page.click_ok()

        # Check that the prompt is gone so we can distinguish a prompt problem
        # from a dataflow update problem.
        time.sleep(0.25)
        self.browser.implicitly_wait(1)  # We don't expect to find anything.
        try:
            eq(len(self.browser.find_elements(*page('prompt')._locator)), 0)
        finally:
            self.browser.implicitly_wait(TMO)

        retval = None
        if check:  # Check that it's been added.
            retval = WebDriverWait(self.browser, TMO).until(
                        lambda browser: self.get_dataflow_figure(instance_name,
                                                                 prefix))
        return retval

    def fill_slot_from_library(self, slot, classname, args=None):
        """ Fill slot with `classname` instance from library. """
        for retry in range(3):
            try:
                button = self.find_library_button(classname)
                chain = ActionChains(self.browser)
                chain.move_to_element(button)
                chain.click_and_hold(button)
                chain.move_to_element(slot.root)
                chain.release(None)
                chain.perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('fill_slot_from_library:'
                                    'StaleElementReferenceException')
                else:
                    raise
            else:
                break

        # Handle arguments for the slotted class
        page = ArgsPrompt(self.browser, self.port)
        argc = page.argument_count()
        if argc > 0:
            if args is not None:
                for i, arg in enumerate(args):
                    page.set_argument(i, arg)
            page.click_ok()

        # Check that the prompt is gone so we can distinguish a prompt problem
        # from a dataflow update problem.
        time.sleep(0.5)
        self.browser.implicitly_wait(1)  # We don't expect to find anything.
        try:
            eq(len(self.browser.find_elements(*page('prompt')._locator)), 0)
        finally:
            self.browser.implicitly_wait(TMO)

    def get_dataflow_figures(self):
        """ Return dataflow figure elements. """
        return find_dataflow_figures(self)

    def get_dataflow_figure(self, name, prefix=None, retries=5):
        """ Return :class:`DataflowFigure` for `name`. """
        return find_dataflow_figure(self, name, prefix, retries)

    def get_dataflow_component_names(self):
        """ Return names of dataflow components. """
        return find_dataflow_component_names(self)

    def connect(self, src, dst):
        """ Return :class:`ConnectionsPage` for connecting `src` to `dst`. """
        chain = ActionChains(self.browser)
        chain.click_and_hold(src.output_port)
        # Using root rather than input_port since for some reason
        # even using a negative Y offset can select the parent's input.
        chain.move_to_element(dst.input_port)
        chain.release(None)
        chain.perform()
        parent, dot, srcname = src.pathname.rpartition('.')
        parent, dot, dstname = dst.pathname.rpartition('.')
        editor_id = 'ConnectionsFrame-%s' % (parent)
        editor_id = editor_id.replace('.', '-')
        return ConnectionsPage(self.browser, self.port, (By.ID, editor_id))

    def replace(self, name, classname, confirm=True):
        """ Replace `name` with an instance of `classname`. """
        library_item = self.get_library_item(classname)
        target = self.get_dataflow_figure(name).root

        chain = ActionChains(self.browser)
        chain.click_and_hold(library_item)
        chain.move_to_element(target)
        chain.release(None)
        chain.perform()

        dialog = ConfirmationPage(self)
        if confirm:
            dialog.click_ok()
        else:
            dialog.click_cancel()

    def add_object_to_workflow(self, obj_path, target_name):
        """ Add `obj_path` object to `target_name` in workflow. """
        for retry in range(3):
            try:
                items = obj_path.split('.')
                parent = items[:-1]
                comp = items[-1]
                obj = self.get_dataflow_figure(comp, parent)

                target = self.find_object_button(target_name)

                chain = ActionChains(self.browser)
                chain.drag_and_drop(obj.root, target)
                chain.perform()

                #obj = self.find_object_button(obj_path)
                #workflow = self.get_workflow_figure(target_name)
                #flow_fig = workflow.flow
                #chain = ActionChains(self.browser)
                #chain.move_to_element(obj)
                #chain.click_and_hold(obj)
                #chain.move_to_element(flow_fig)
                #chain.move_by_offset(2, 1)
                #chain.release(None)
                #chain.perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('add_object_to_workflow:'
                                    ' StaleElementReferenceException')
                else:
                    raise
            else:
                break

    def add_object_to_workflow_figure(self, obj_path, target_name, target_page=None):
        """ Add `obj_path` object to `target_name` in workflow diagram. """

        if target_page is None:
            target_page = self

        for retry in range(3):
            try:
                items = obj_path.split('.')
                parent = items[:-1]
                comp = items[-1]
                obj = self.get_dataflow_figure(comp, parent)

                workflow = target_page.get_workflow_figure(target_name)
                flow_fig = workflow.flow

                chain = ActionChains(self.browser)
                chain.drag_and_drop(obj.root, flow_fig)
                chain.perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('add_object_to_workflow_figure:'
                                    ' StaleElementReferenceException')
                else:
                    raise
            else:
                break

    def get_workflow_figures(self):
        """ Return workflow figure elements. """
        return find_workflow_figures(self)

    def get_workflow_component_figures(self):
        """ Return workflow component figure elements. """
        return find_workflow_component_figures(self)

    def get_workflow_figure(self, name, prefix=None, retries=5):
        """ Return :class:`WorkflowFigure` for `name`. """
        return find_workflow_figure(self, name, prefix, retries)

    def get_workflow_component_figure(self, name, prefix=None, retries=5):
        """ Return :class:`WorkflowComponentFigure` for `name`. """
        return find_workflow_component_figure(self, name, prefix, retries)

    def show_log(self):
        """ Open log viewer.  Returns :class:`LogViewer`. """
        self('tools_menu').click()
        self('log_button').click()
        return LogViewer.verify(self.browser, self.port)

    def hide_left(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-west-open')
        toggler.click()

    def show_left(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-west-closed')
        toggler.click()

    def hide_right(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-east-open')
        toggler.click()

    def show_right(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-east-closed')
        toggler.click()

    def hide_console(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-south-open')
        toggler.click()

    def show_console(self):
        toggler = self.browser.find_element_by_css_selector('.ui-layout-toggler-south-closed')
        toggler.click()

    def drag_element_to(self, element, drag_to, center):
        '''Drag one element over to another element'''
        chain = ActionChains(self.browser)
        chain.move_to_element(element)
        chain.click_and_hold(element)
        if center:
            # move to center of element
            chain.move_to_element(drag_to)
        else:
            # move to offset from top left of element
            chain.move_to_element_with_offset(drag_to, 2, 2)
        chain.perform()
        return chain

    def release(self, chain):
        '''The drop part of the ActionChain when doing drag and drop'''
        chain.release(on_element=None).perform()
        time.sleep(0.5)  # Pacing for diagram update.

    def replace_driver(self, assembly_name, driver_type):
        ''' find 'driver_type' in the library, drag and drop it on to
            the driver figure of the 'assembly_name'
        '''
        newdriver = self.find_library_button(driver_type)
        assembly = self.get_dataflow_figure(assembly_name)
        driver_element = self.get_dataflow_figure('driver')

        div = driver_element.get_drop_targets()[0]
        chain = self.drag_element_to(newdriver, div, True)
        self.check_highlighting(driver_element('content_area').element, True,
                           "Driver's content_area")
        self.release(chain)

        # brings up a confirm dialog for replacing the existing driver.
        dialog = ConfirmationPage(assembly)
        dialog.click_ok()

    def resize_editor(self, editor):
        '''ensure that the editor is not covering the library
        (or else we cannot drag things from it!)'''
        browser = self.browser

        page_width = browser.get_window_size()['width']

        lib_tab      = self('library_tab').find_element_by_xpath('..')
        lib_width    = lib_tab.size['width']
        lib_position = lib_tab.location['x']

        dialog_title    = editor('dialog_title').find_element_by_xpath('../..')
        dialog_width    = dialog_title.size['width']
        dialog_position = dialog_title.location['x']

        # how much overlap do we have?
        overlap = lib_position - (dialog_position + dialog_width)

        if overlap < 0:  # we are overlapping
            # check to see if we have enough room to move out of the way
            if page_width < dialog_width + lib_width:
                # not enough, need to rezize the editor

                # look for the resize handle
                sibblings = dialog_title.find_elements_by_xpath('../../div')
                handle = None
                for sib in sibblings:
                    if "ui-resizable-se" in sib.get_attribute('class'):
                        handle = sib

                # do the resizing
                chain = ActionChains(browser)
                chain.click_and_hold(handle)
                # we can resize editor down to 425px, any less and we cover drop targets
                chain.move_by_offset(450 - dialog_width, 0).perform()
                # must click because release is not working. why? I do not know.
                chain.click().perform()
                chain.release(None).perform()

                # recalculate the overlap
                dialog_title = editor('dialog_title').find_element_by_xpath('../..')
                dialog_width = dialog_title.size['width']
                dialog_position = dialog_title.location['x']
                overlap = lib_position - (dialog_position + dialog_width)

            # We are good, move out!
            chain = ActionChains(browser)
            chain.click_and_hold(editor('dialog_title').element)
            chain.move_by_offset(overlap, 0).perform()
            # must click because release is not working. why? I do not know.
            chain.click().perform()
            chain.release(None).perform()

            # recalculate the overlap
            dialog_title = editor('dialog_title').find_element_by_xpath('../..')
            dialog_width = dialog_title.size['width']
            dialog_position = dialog_title.location['x']
            overlap = lib_position - (dialog_position + dialog_width)

            if overlap < 0:
                # we still have a problem.
                eq(True, False,
                    "Could not move or rezise the editor dialog so it is not "
                    "overlapping the library. The browser window is too small")

    def get_dataflow_fig_in_globals(self, name):
        '''Find the named dataflow fig in the global dataflow editor'''
        all_figs = self.get_dataflow_figures()
        for fig in all_figs:
            location = fig.get_parent().get_attribute('id')
            if location == "top-dataflow":
                return fig

        return None

    def put_element_on_grid(self, type_str):
        '''find 'type_str' in the Library, drag and drop it onto the grid'''
        browser = self.browser

        grid = browser.find_element_by_xpath('//div[@id="-dataflow"]')

        for retry in range(3):
            try:
                objtype = self.find_library_button(type_str)
                chain = ActionChains(browser)
                chain.click_and_hold(objtype)
                chain.move_to_element_with_offset(grid, 10, 10).perform()
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('put_element_on_grid %s:'
                                    ' StaleElementReferenceException', type_str)
                else:
                    raise
            else:
                break

        self.check_highlighting(grid, True, "Grid")
        self.release(chain)

        # deal with the modal dialog
        name = NameInstanceDialog(self).create_and_dismiss()

        # make sure it is on the grid
        self.ensure_names_in_workspace([name],
            "Dragging '" + type_str +
            "' to grid did not produce a new element on page")

        return name

    def drag_and_drop(self, element, target, should_drop, message='Target'):
        '''Drag and drop an element onto a target'''
        chain = self.drag_element_to(element, target, True)
        chain.move_by_offset(25, 0).perform()
        time.sleep(1.0)  # give it a second to update the figure
        self.check_highlighting(target, should_highlight=should_drop, message=message)
        self.release(chain)

    def ensure_names_in_workspace(self, names, message=None):
        """ensures the list of element names in included in the workspace"""

        allnames = self.get_dataflow_component_names()

        # sometimes does not load all of the names for some reason.
        # Reloading seems to fix the problem
        try_reload = False
        for name in names:
            if not name in allnames:
                try_reload = True
        if try_reload:
            time.sleep(.1)
            allnames = self.get_dataflow_component_names()

        # now we will assert that the elements that we added appear on the page
        for name in names:
            eq(name in allnames, True, '%s: %s' % (message, name))

    def check_highlighting(self, element, should_highlight=True, message='Element'):
        '''check to see that the background-color of the element is highlighted
        '''
        if 'SlotFigure' in element.get_attribute('class'):
            # a slot figure is a div containing a ul element (the context menu) and
            # one or more svg elements, each of which contains a rect and two texts
            # the last rect fill style is what we need to check for highlighting
            rect = element.find_elements_by_css_selector('svg rect')[-1]
            style = rect.get_attribute('style')
        else:
            style = element.get_attribute('style')
        highlighted = ('background-color: rgb(207, 214, 254)' in style) \
                    or ('highlighted.png' in style) \
                    or ('fill: #cfd6fe' in style)
        eq(highlighted, should_highlight, message +
            (' did not highlight (and should have) ' if should_highlight else
             ' highlighed (and should not have) ')
             + 'when dragging a dropable element to it')
