import sys
import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement
from util import ValuePrompt


class UploadPage(BasePageObject):
    """ Pops-up when adding a file. """

    title_prefix = 'OpenMDAO: Add File'

    filename = InputElement((By.NAME, 'myfile'))
    submit = ButtonElement((By.ID, 'add-button'))

    def upload_file(self, path):
        self.filename = path
        self('submit').click()

    def upload_files(self):
        self('submit').click()

    def select_file(self, path):
        self.filename = path

    def select_files(self, paths):
        for path in paths:
            self.select_file(path)


class EditorPage(BasePageObject):
    """ Code editor window. """

    title_prefix = 'OpenMDAO:'

    # Left side.
    files_tab = ButtonElement((By.ID, 'ftree_tab'))
    file_menu = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd/div/nav2/ul/li/a'))
    newfile_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd/div/nav2/ul/li/ul/li[1]/a'))
    newfolder_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd/div/nav2/ul/li/ul/li[2]/a'))
    add_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd/div/nav2/ul/li/ul/li[3]/a'))

    # File context menu.
    file_create = ButtonElement((By.XPATH, "//a[(@rel='createFile')]"))
    file_add    = ButtonElement((By.XPATH, "//a[(@rel='addFile')]"))
    file_folder = ButtonElement((By.XPATH, "//a[(@rel='createFolder')]"))
    file_rename = ButtonElement((By.XPATH, "//a[(@rel='renameFile')]"))
    file_view   = ButtonElement((By.XPATH, "//a[(@rel='viewFile')]"))
    file_edit   = ButtonElement((By.XPATH, "//a[(@rel='editFile')]"))
    file_import = ButtonElement((By.XPATH, "//a[(@rel='importFile')]"))
    file_exec   = ButtonElement((By.XPATH, "//a[(@rel='execFile')]"))
    file_delete = ButtonElement((By.XPATH, "//a[(@rel='deleteFile')]"))
    file_toggle = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))

    # Right side.
    code_tab = ButtonElement((By.ID, 'code_tab'))

    def __init__(self, browser, port):
        super(EditorPage, self).__init__(browser, port)

        self.locators = {}
        self.locators["files"] = (By.XPATH, "//div[@id='ftree']//a[@class='file ui-draggable']")

    def get_files(self):
        """ Return names in the file tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'ftree'))
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

        main_window_handle = self.browser.current_window_handle

        self('file_menu').click()
        self('add_button').click()

        # Switch to the Window that pops up.
        self.browser.switch_to_window('Add File')
        page = UploadPage(self.browser, self.port)
        page.upload_file(file_path)

        # Go back to the main window.
        self.browser.switch_to_window(main_window_handle)

    def add_files(self):
        self('file_menu').click()
        self('add_button').click()
        self.browser.switch_to_window('Add File')
        return UploadPage.verify(self.browser, self.port)

    def new_file_dialog(self):
        """ bring up the new file dialog """
        self('file_menu').click()
        self('newfile_button').click()

        page = ValuePrompt(self.browser, self.port)
        return page

    def new_file(self, filename, code):
        """ Make a new file `filename` with contents `code`. """
        self('file_menu').click()
        self('newfile_button').click()

        page = ValuePrompt(self.browser, self.port)
        page.set_value(filename)

        self.edit_file(filename)

        # Switch to editor textarea
        code_input_element = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_css_selector('textarea'))
# FIXME: absolute delay for editor to get ready.
#        Problem is Firefox sometimes sends arrow key to scrollbar.
#        Sadly this didn't completely fix the issue.
        time.sleep(1)

        # Go to the bottom of the code editor window
        for i in range(4):
            code_input_element.send_keys(Keys.ARROW_DOWN)
        # Type in the code.
        code_input_element.send_keys(code)
        # Control-S to save.
        if sys.platform == 'darwin':
            code_input_element.send_keys(Keys.COMMAND + 's')
        else:
            code_input_element.send_keys(Keys.CONTROL + 's')
# FIXME: absolute delay for save to complete.
        time.sleep(2)

        # Back to main window.
        self.browser.switch_to_default_content()

    def edit_file(self, filename, dclick=True):
        """ Edit `filename` via double-click or context menu. """
        xpath = "//a[(@path='/%s')]" % filename
        element = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_xpath(xpath))
        chain = ActionChains(self.browser)
        if dclick:  # This has had issues...
            for i in range(10):
                try:
                    chain.double_click(element).perform()
                except StaleElementReferenceException:
                    logging.warning('edit_file: StaleElementReferenceException')
                    element = WebDriverWait(self.browser, 1).until(
                        lambda browser: browser.find_element_by_xpath(xpath))
                    chain = ActionChains(self.browser)
                else:
                    break
        else:
            chain.context_click(element).perform()
            self('file_edit').click()

