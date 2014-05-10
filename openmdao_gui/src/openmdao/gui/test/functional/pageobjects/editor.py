import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement, InputElement, TextElement
from util import ValuePrompt, NotifierPage


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
    file_menu = ButtonElement((By.XPATH,
                           '/html/body/div/div/nav2/ul/li/a'))
    newfile_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/nav2/ul/li/ul/li[1]/a'))
    newfolder_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/nav2/ul/li/ul/li[2]/a'))
    add_button = ButtonElement((By.XPATH,
                           '/html/body/div/div/nav2/ul/li/ul/li[3]/a'))

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
    editor_new_button        = ButtonElement((By.ID, 'code_pane-uiBar-new'))
    editor_save_button       = ButtonElement((By.ID, 'code_pane-uiBar-save'))
    editor_find_button       = ButtonElement((By.ID, 'code_pane-uiBar-find'))
    editor_replace_button    = ButtonElement((By.ID, 'code_pane-uiBar-replace'))
    editor_replaceAll_button = ButtonElement((By.ID, 'code_pane-uiBar-replaceAll'))
    editor_undo_button       = ButtonElement((By.ID, 'code_pane-uiBar-undo'))
    editor_overwrite_button  = ButtonElement((By.ID, 'code_pane-overwrite'))
    editor_cancel_button     = ButtonElement((By.ID, 'code_pane-cancel'))

    editor_label = TextElement((By.ID, 'code_pane-label'))

    file_chooser = InputElement((By.ID, 'filechooser'))

    def __init__(self, browser, port):
        super(EditorPage, self).__init__(browser, port)

        self.locators = {}
        self.locators["files"] = (By.XPATH, "//div[@id='file_pane']//a[@class='file ui-draggable']")

    def get_code(self):
        return self.browser.execute_script("return openmdao.frames.code_pane.editor.getValue()")

    def get_tab_label(self):
        label = self.browser.execute_script("return openmdao.frames.code_pane.currentTablabel()")
        return ''.join(label.split('*'))  # ignore changed / unchanged status

    def get_files(self):
        """ Return names in the file tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'file_pane'))
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

        self('file_menu').click()
        self('add_button').click()

        self.file_chooser = file_path

    def add_files(self, *file_paths):
        """ Read in multiple 'file_path's
            FIXME: doesn't work, see elements._InputElement
            Have to use multiple calls to add_file for now
        """
        self('file_menu').click()
        self('add_button').click()
        self('file_chooser').set_values(*file_paths)

    def new_file_dialog(self):
        """ bring up the new file dialog """
        self('file_menu').click()
        self('newfile_button').click()
        return ValuePrompt(self.browser, self.port)

    def find_text(self, text):
        #click the 'find' button, and enter text. Not yet functional
        self('editor_find_button').click()
        alert = self.browser.switch_to_alert()
        chain = ActionChains(alert)
        chain.send_keys(text).perform()
        chain.send_keys(Keys.RETURN).perform()
        return

    def replace_text(self, old_text, new_text, replace_all=False):
        #click the 'replace' or 'replace all 'button,
        # and enter text to find and replace. Not yet functional
        if replace_all:
            self('editor_replace_button').click()
        else:
            self('editor_replaceAll_button').click()
        return

    def undo(self):
        #click the 'undo' button
        self('editor_undo_button').click()
        return

    def new_file(self, filename, code, check=True):
        """ Make a new file `filename` with contents `code`. """
        page = self.new_file_dialog()
        page.set_value(filename)

        NotifierPage.wait(self)  # Wait for creation to complete.

        # Switch to editor textarea
        code_input_element = self.get_text_area()

        # Go to the bottom of the code editor window
        for i in range(4):
            code_input_element.send_keys(Keys.ARROW_DOWN)
        # Type in the code.
        code_input_element.send_keys(code)

        self.save_document(check=check)

    def edit_file(self, filename, dclick=True):
        """ Edit `filename` via double-click or context menu. """
        xpath = "//a[(@path='/%s')]" % filename
        element = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_xpath(xpath))
        chain = ActionChains(self.browser)
        for i in range(10):
            try:
                if dclick:
                    chain.double_click(element).perform()
                else:
                    chain.context_click(element).perform()
                    self('file_edit').click()
            except StaleElementReferenceException:
                logging.warning('edit_file: StaleElementReferenceException')
                element = WebDriverWait(self.browser, 1).until(
                    lambda browser: browser.find_element_by_xpath(xpath))
                chain = ActionChains(self.browser)
            else:
                break

    def get_text_area(self):
        code_input_element = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_css_selector('textarea'))
        # FIXME: absolute delay for editor to get ready.
        #        Problem is Firefox sometimes sends arrow key to scrollbar.
        #        Sadly this didn't completely fix the issue.
        time.sleep(1)
        return code_input_element

    def save_document(self, overwrite=False, check=True, cancel=False):
        # use 'save' button to save code
        self('editor_save_button').click()
        if overwrite:
            self('editor_overwrite_button').click()
        elif cancel:
            self('editor_cancel_button').click()
        if check:
            NotifierPage.wait(self)

    def add_text_to_file(self, text):
        """ Add the given text to the current file.  """
        # Switch to editor textarea
        code_input_element = self.get_text_area()

        # Type in the code.
        code_input_element.send_keys(text)
        return code_input_element

    def append_text_to_file(self, text):
        """ Add the given text to the end of the current file.  """
        # Switch to editor textarea
        code_input_element = self.get_text_area()

        # Type in the code.
        line_count = self.get_code().count('\n')
        code_input_element.send_keys(Keys.ARROW_DOWN * line_count)
        code_input_element.send_keys(text)
        return code_input_element
