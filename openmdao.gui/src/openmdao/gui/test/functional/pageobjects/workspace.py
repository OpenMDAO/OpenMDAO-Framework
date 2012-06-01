import logging
import threading
import time

from nose import SkipTest

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from editor import EditorPage
from elements import ButtonElement, InputElement, TextElement
from util import abort, ValuePrompt, NotifierPage


class DataflowFigure(BasePageObject):
    """ Represents elements within a dataflow figure. """

    props_button  = ButtonElement((By.XPATH, "//a[text()='Properties']"))
    run_button    = ButtonElement((By.XPATH, "//a[text()='Run']"))
    remove_button = ButtonElement((By.XPATH, "//a[text()='Remove']"))

    @property
    def border(self):
        """ Figure border property. """
        return self.root.value_of_css_property('border')

    @property
    def name(self):
        """ Figure name. """
        return self.root.find_elements_by_class_name('DataflowFigureHeader')[0].text

    @property
    def top_right(self):
        """ Figure maximize/minimize button. """
        return self.root.find_elements_by_class_name('DataflowFigureTopRight')[0]

    def remove(self):
        """ Remove this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('remove_button').click()


class WorkspacePage(BasePageObject):

    title_prefix = 'OpenMDAO:'

    # Top.
    project_menu      = ButtonElement((By.ID, 'project-menu'))
    save_button       = ButtonElement((By.ID, 'project-save'))
    run_button        = ButtonElement((By.ID, 'project-run'))
    reload_button     = ButtonElement((By.ID, 'project-reload'))
    close_button      = ButtonElement((By.ID, 'project-close'))
    exit_button       = ButtonElement((By.ID, 'project-exit'))

    view_menu         = ButtonElement((By.ID, 'view-menu'))
    cmdline_button    = ButtonElement((By.ID, 'view-cmdline'))
    console_button    = ButtonElement((By.ID, 'view-console'))
    libraries_button  = ButtonElement((By.ID, 'view-libraries'))
    objects_button    = ButtonElement((By.ID, 'view-objects'))
    properties_button = ButtonElement((By.ID, 'view-properties'))
    workflow_button   = ButtonElement((By.ID, 'view-workflow'))
    dataflow_button   = ButtonElement((By.ID, 'view-dataflow'))
    refresh_button    = ButtonElement((By.ID, 'view-refresh'))

    tools_menu        = ButtonElement((By.ID, 'tools-menu'))
    editor_button     = ButtonElement((By.ID, 'tools-editor'))
    plotter_button    = ButtonElement((By.ID, 'tools-plotter'))
    addons_button     = ButtonElement((By.ID, 'tools-addons'))

    help_menu         = ButtonElement((By.ID, 'help-menu'))
    doc_button        = ButtonElement((By.ID, 'help-doc'))

    about_button      = ButtonElement((By.ID, 'about-item'))

    # Left side.
    objects_tab = ButtonElement((By.ID, 'otree_tab'))

    # Object context menu.
    obj_properties = ButtonElement((By.XPATH, "//a[(@rel='properties')]"))
    obj_dataflow   = ButtonElement((By.XPATH, "//a[(@rel='show_dataflow')]"))
    obj_workflow   = ButtonElement((By.XPATH, "//a[(@rel='show_workflow')]"))
    obj_run        = ButtonElement((By.XPATH, "//a[(@rel='run')]"))
    obj_toggle     = ButtonElement((By.XPATH, "//a[(@rel='toggle')]"))
    obj_remove     = ButtonElement((By.XPATH, "//a[(@rel='remove')]"))

    # Center.
    dataflow_tab = ButtonElement((By.ID, 'dataflow_tab'))
    workflow_tab = ButtonElement((By.ID, 'workflow_tab'))
    code_tab     = ButtonElement((By.ID, 'code_tab'))

    # Right side.
    properties_tab  = ButtonElement((By.ID, 'properties_tab'))

    libraries_tab   = ButtonElement((By.ID, 'palette_tab'))
    working_section = ButtonElement((By.XPATH,
                            "//div[(@id='palette')]//div[(@title='working')]"))
    openmdao_section = ButtonElement((By.XPATH,
                            "//div[(@id='palette')]//div[(@title='openmdao')]"))
    # Bottom.
    history = TextElement((By.ID, 'history'))
    command = InputElement((By.ID, 'command'))
    submit  = ButtonElement((By.ID, 'command-button'))

    def __init__(self, browser, port):
        super(WorkspacePage, self).__init__(browser, port)

        self.locators = {}
        self.locators["objects"] = ( By.XPATH, "//div[@id='otree']//li[@path]" )

        # Wait for bulk of page to load.
        WebDriverWait(self.browser, 2*TMO).until(
            lambda browser: len(self.get_dataflow_figures()) > 0)
        # Now wait for WebSockets.
# FIXME: absolute delay before polling sockets.
        time.sleep(2)
        browser.execute_script('openmdao.Util.webSocketsReady(2);')
        NotifierPage.wait(browser, port)

    def run(self, timeout=TMO):
        """ Run current component. """
        self('project_menu').click()
        self('run_button').click()
        NotifierPage.wait(self.browser, self.port, timeout)

    def do_command(self, cmd, timeout=TMO):
        """ Execute a command. """
        self.command = cmd
        self('submit').click()
        NotifierPage.wait(self.browser, self.port, timeout)

    def close_workspace(self):
        """ Close the workspace page. Returns :class:`ProjectsListPage`. """
        self.browser.execute_script('openmdao.Util.closeWebSockets();')
        NotifierPage.wait(self.browser, self.port)
        self('project_menu').click()

        # Sometimes chromedriver hangs here, so we click in separate thread.
        # It's a known issue on the chromedriver site.
        closer = threading.Thread(target=self._closer)
        closer.daemon = True
        closer.start()
        closer.join(60)
        if closer.is_alive():
            abort(True)
            raise SkipTest("Can't close workspace, driver hung :-(")

        from project import ProjectsListPage
        return ProjectsListPage.verify(self.browser, self.port)

    def _closer(self):
        """ Clicks the close button. """
        self('close_button').click()

    def open_editor(self):
        """ Open code editor.  Returns :class:`EditorPage`. """
        self('tools_menu').click()
        self('editor_button').click()
        self.browser.switch_to_window('Code Editor')
        return EditorPage.verify(self.browser, self.port)

    def save_project(self):
        """ Save current project. """
        self('project_menu').click()
        self('save_button').click()
        NotifierPage.wait(self.browser, self.port)

    def get_objects_attribute(self, attribute):
        """ Return list of `attribute` values for all objects. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'otree'))
        object_elements = self.browser.find_elements(*self.locators["objects"])
        values = []
        for element in object_elements:
            values.append(element.get_attribute(attribute))
        return values

    def show_dataflow(self, component_name):
        """ Show dataflow of `component_name`. """
        self('objects_tab').click()
        xpath = "//div[@id='otree']//li[(@path='%s')]//a" % component_name
        element = WebDriverWait(self.browser, TMO).until(
                      lambda browser: browser.find_element_by_xpath(xpath))
        chain = ActionChains(self.browser)
        chain.context_click(element).perform()
        self('obj_dataflow').click()

    def add_library_item_to_dataflow(self, item_name, instance_name):
        """ Add component `item_name`, with name `instance_name`. """
        xpath = "//div[(@id='palette')]//div[(@path='%s')]" % item_name
        library_item = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_xpath(xpath))
        WebDriverWait(self.browser, TMO).until(
            lambda browser: library_item.is_displayed())
# FIXME: absolute delay to wait for 'slide' to complete.
        time.sleep(1)

        target = WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element_by_xpath("//*[@id='-dataflow']"))

        chain = ActionChains(self.browser)
        if False:
            chain = chain.drag_and_drop(library_item, target)
        else:
            chain = chain.click_and_hold(library_item)
            chain = chain.move_to_element_with_offset(target, 100, 100)
            chain = chain.release(None)
        chain.perform()

        page = ValuePrompt(self.browser, self.port)
        page.set_value(instance_name)
        WebDriverWait(self.browser, TMO).until(
            lambda browser: instance_name in self.get_dataflow_component_names())

    def get_dataflow_figures(self):
        """ Return dataflow figure elements. """
        return self.browser.find_elements_by_class_name('DataflowFigure')

    def get_dataflow_figure(self, name, retries=5):
        """ Return :class:`DataflowFigure` for `name`. """
        for retry in range(retries):
            figures = self.browser.find_elements_by_class_name('DataflowFigure')
            fig_name = None
            for figure in figures:
                try:
                    fig_name = figure.find_elements_by_class_name('DataflowFigureHeader')[0].text
                except StaleElementReferenceException:
                    logging.warning('get_dataflow_figure: StaleElementReferenceException')
                else:
                    if fig_name == name:
                        return DataflowFigure(self.browser, self.port, figure)
            time.sleep(0.5)
        return None

    def get_dataflow_component_names(self):
        """ Return names of dataflow components. """
        dataflow_component_headers = \
            self.browser.find_elements_by_class_name('DataflowFigureHeader')
        names = []

        for i in range(len(dataflow_component_headers)):
            for retry in range(10):  # This has had issues...
                try:
                    names.append(self.browser.find_elements_by_class_name('DataflowFigureHeader')[i].text)
                except StaleElementReferenceException:
                    logging.warning('get_dataflow_component_names: StaleElementReferenceException')
                else:
                    break

        if len(names) != len(dataflow_component_headers):
            logging.error('get_dataflow_component_names:'
                          ' expecting %d names, got %s',
                          len(dataflow_component_headers), names)
        return names

