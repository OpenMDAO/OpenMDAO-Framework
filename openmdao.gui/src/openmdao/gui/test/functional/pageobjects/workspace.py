import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject
from elements import ButtonElement, InputElement, TextElement


class UploadPage(BasePageObject):
    """ Pops-up when adding a file. """

    filename = InputElement((By.NAME, 'myfile'))
    submit = ButtonElement((By.XPATH, '/html/body/div/div[2]/form/input[2]'))

    def upload_file(self, path):
        self.filename = path
        self('submit').click()


class ComponentNamePrompt(BasePageObject):
    """ Overlay displayed when requesting a name for a component. """

    value = InputElement((By.ID, 'component-prompt'))

    def set_value(self, value):
        self.value = value+Keys.RETURN


class FileNamePrompt(BasePageObject):
    """ Overlay displayed when requesting a name for a file. """

    value = InputElement((By.ID, 'filename-prompt'))

    def set_value(self, value):
        self.value = value+Keys.RETURN


class WorkspacePage(BasePageObject):

    # Top.
    project_menu = ButtonElement((By.ID, 'project-menu'))
    save_button = ButtonElement((By.ID, 'project-save'))
    run_button = ButtonElement((By.ID, 'project-run'))
    reload_button = ButtonElement((By.ID, 'project-reload'))
    close_button = ButtonElement((By.ID, 'project-close'))
    exit_button = ButtonElement((By.ID, 'project-exit'))

    view_menu = ButtonElement((By.ID, 'view-menu'))
    code_button = ButtonElement((By.ID, 'view-code'))
    cmdline_button = ButtonElement((By.ID, 'view-cmdline'))
    console_button = ButtonElement((By.ID, 'view-console'))
    files_button = ButtonElement((By.ID, 'view-files'))
    libraries_button = ButtonElement((By.ID, 'view-libraries'))
    objects_button = ButtonElement((By.ID, 'view-objects'))
    properties_button = ButtonElement((By.ID, 'view-properties'))
    workflow_button = ButtonElement((By.ID, 'view-workflow'))
    structure_button = ButtonElement((By.ID, 'view-structure'))
    refresh_button = ButtonElement((By.ID, 'view-refresh'))

    tools_menu = ButtonElement((By.ID, 'tools-menu'))
    plotter_button = ButtonElement((By.ID, 'tools-plotter'))
    addons_button = ButtonElement((By.ID, 'tools-addons'))
    threedtin_button = ButtonElement((By.ID, 'tools-3dtin'))

    help_menu = ButtonElement((By.ID, 'help-menu'))
    doc_button = ButtonElement((By.ID, 'help-doc'))

    about_button = ButtonElement((By.ID, 'about-item'))

    # Left side.
    objects_tab = ButtonElement((By.ID, 'otree_tab'))

    files_tab = ButtonElement((By.ID, 'ftree_tab'))
    file_menu = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd[2]/div/nav2/ul/li/a'))
    newfile_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd[2]/div/nav2/ul/li/ul/li[1]/a'))
    newfolder_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd[2]/div/nav2/ul/li/ul/li[2]/a'))
    add_button = ButtonElement((By.XPATH,
                           '/html/body/div/dl/dd[2]/div/nav2/ul/li/ul/li[3]/a'))

    # Object context menu.
    obj_properties = ButtonElement((By.XPATH, "//a[(@rel='properties')]"))
    obj_structure = ButtonElement((By.XPATH, "//a[(@rel='show_dataflow')]"))
    obj_workflow = ButtonElement((By.XPATH, "//a[(@rel='show_workflow')]"))
    obj_run = ButtonElement((By.XPATH, "//a[(@rel='run')]"))
    obj_toggle = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))
    obj_remove = ButtonElement((By.XPATH, "//a[(@rel='remove')]"))

    # File context menu.
    file_create = ButtonElement((By.XPATH, "//a[(@rel='create')]"))
    file_add = ButtonElement((By.XPATH, "//a[(@rel='add')]"))
    file_folder = ButtonElement((By.XPATH, "//a[(@rel='createFolder')]"))
    file_rename = ButtonElement((By.XPATH, "//a[(@rel='rename')]"))
    file_view = ButtonElement((By.XPATH, "//a[(@rel='viewFile')]"))
    file_edit = ButtonElement((By.XPATH, "//a[(@rel='editFile')]"))
    file_import = ButtonElement((By.XPATH, "//a[(@rel='importfile')]"))
    file_exec = ButtonElement((By.XPATH, "//a[(@rel='execfile')]"))
    file_delete = ButtonElement((By.XPATH, "//a[(@rel='deleteFile')]"))
    file_toggle = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))

    # Center.
    structure_tab = ButtonElement((By.ID, 'structure_tab'))
    workflow_tab = ButtonElement((By.ID, 'workflow_tab'))
    code_tab = ButtonElement((By.ID, 'code_tab'))

    # Right side.
    properties_tab = ButtonElement((By.ID, 'properties_tab'))

    libraries_tab = ButtonElement((By.ID, 'palette_tab'))
    working_section = ButtonElement((By.XPATH,
                            "//div[(@id='palette')]//div[(@title='working')]"))
    openmdao_section = ButtonElement((By.XPATH,
                            "//div[(@id='palette')]//div[(@title='openmdao')]"))
    # Bottom.
    history = TextElement((By.ID, 'history'))
    command = InputElement((By.ID, 'command'))
    submit = ButtonElement((By.ID, 'command-button'))

    def __init__(self, browser, port):
        super(WorkspacePage, self).__init__(browser, port)

        self.locators = {}
        self.locators["dataflow_component_headers"] = ( By.XPATH, "//div[@class='DataflowComponentFigureHeader']" )
        self.locators["files"] = ( By.XPATH, "//div[@id='ftree']//a[@class='file ui-draggable']" )
        self.locators["objects"] = ( By.XPATH, "//div[@id='otree']//li[@path]" )

        # Locator is relative to the iframe not the top level window.
        self.locators["code_input"] = ( By.XPATH, "/html/body" )
        
        browser.execute_script("var openmdao_test_mode = true;")

    def run(self, timeout=10):
        """ Run current component. """
        old_len = len(self.history)
        self('project_menu').click()
        self('run_button').click()
        WebDriverWait(self.browser, timeout).until(
            lambda browser: self.history[old_len:].endswith('Execution complete.'))

    def close_workspace(self):
        """ Close the workspace page. Returns :class:`ProjectsListPage`. """
        self('project_menu').click()
        self('close_button').click()
        WebDriverWait(self.browser, 10).until(
            lambda browser: browser.title == 'Projects')
        from project import ProjectsListPage
        return ProjectsListPage(self.browser, self.port)

    def save_project(self):
        """ Save current project. """
        self('project_menu').click()
        self('save_button').click()

    def view_refresh(self):
        """ Refresh display. """
        self('view_menu').click()
        self('refresh_button').click()

    def get_files(self):
        '''Warning: the workspace needs to be refreshed to make sure
           this command can see everything'''
        file_items = self.browser.find_elements(*self.locators["files"])
        file_names = []
        for element in file_items:
            file_names.append(element.text[1:])
        return file_names
    
    def get_objects_attribute(self, attribute):
        """ Return list of `attribute` values for all objects. """
        object_elements = self.browser.find_elements(*self.locators["objects"])
        values = []
        for element in object_elements:
            values.append(element.get_attribute(attribute))
        return values

    def add_file(self, file_path):
        """ Read in `file_path` """
        if file_path.endswith('.pyc'):
            file_path = file_path[:-1]

        self('files_tab').click()
        self('file_menu').click()
        self('add_button').click()

        # Switch to the Window that pops up.
        main_window_handle = self.browser.current_window_handle
        self.browser.switch_to_window('Add File')
        page = UploadPage(self.browser, self.port)
        page.upload_file(file_path)

        # Go back to the main window.
        self.browser.switch_to_window(main_window_handle)

    def new_file(self, filename, code):
        """ Make a new file `filename` with contents `code`. """
        self('files_tab').click()
        self('file_menu').click()
        self('newfile_button').click()

        page = FileNamePrompt(self.browser, self.port)
        page.set_value(filename)

        self.edit_file(filename)

        # Switch to editor iframe.
        self.browser.switch_to_frame(0)  # No identifying name or ID.
        code_input_element = WebDriverWait(self.browser, 10).until(
            lambda browser: browser.find_element(*self.locators['code_input']))

        # Go to the bottom of the code editor window
        for i in range(4):
            code_input_element.send_keys(Keys.ARROW_DOWN)
        # Type in the code.
        code_input_element.send_keys(code)
        # Control-S to save.
        code_input_element.send_keys(Keys.CONTROL+'s')
#FIXME: absolute delay.
        time.sleep(2)
        
        # Back to main window.
        self.browser.switch_to_default_content()

    def import_file(self, filename):
        """ Import from `filename`. """
        xpath = "//a[(@path='/%s')]" % filename
        element = self.browser.find_element_by_xpath(xpath)
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        self('file_import').click()
#FIXME: absolute delay.
        time.sleep(2)

    def edit_file(self, filename, dclick=True):
        """ Edit `filename` via double-click or context menu. """
        xpath = "//a[(@path='/%s')]" % filename
        element = self.browser.find_element_by_xpath(xpath)
        chain = ActionChains(self.browser)
        if dclick:
            chain.double_click(element).perform()
        else:
            chain.context_click(element).perform()
            self('file_edit').click()

    def show_structure(self, component_name):
        """ Show structure of `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree']//li[(@path='%s')]//a" % component_name
        element = WebDriverWait(self.browser, 10).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        self('obj_structure').click()

    def add_library_item_to_structure(self, item_name, instance_name):
        """ Add component `item_name`, with name `instance_name`. """
        xpath = "//div[(@id='palette')]//div[(@path='%s')]" % item_name
        library_item = self.browser.find_element_by_xpath(xpath)
        target = self.browser.find_element_by_xpath("//*[@id='-dataflow']")
        chain = ActionChains(self.browser)
        #chain = chain.drag_and_drop(library_item, target)
        chain = chain.click_and_hold(library_item)
        chain = chain.move_to_element_with_offset(target, 100, 100)
        chain = chain.release(None)
        chain.perform()

        page = ComponentNamePrompt(self.browser, self.port)
        page.set_value(instance_name)
        WebDriverWait(self.browser, 10).until(
            lambda browser: instance_name in self.get_dataflow_component_names())

    def get_dataflow_figures(self):
        """ Return dataflow figures. """
        return self.browser.find_elements_by_class_name('DataflowComponentFigure')

    def get_dataflow_component_names(self):
        """ Return names of dataflow components. """
        dataflow_component_headers = \
            self.browser.find_elements(*self.locators["dataflow_component_headers"])
        names = []
        for header in dataflow_component_headers:
            names.append(header.text)
        return names

