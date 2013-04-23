import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement


class WorkflowFigure(BasePageObject):
    """ Represents elements within a workflow figure. """

    # Context menu.
    flip_button  = ButtonElement((By.XPATH, "./ul/li[text()='Flip Workflow']"))
    clear_button = ButtonElement((By.XPATH, "./ul/li[text()='Clear Workflow']"))

    def __init__(self, browser, port, root):
        super(WorkflowFigure, self).__init__(browser, port, root)
        self._pathname = None

    @property
    def pathname(self):
        """ Pathname of this component. """
        return self._pathname

    @pathname.setter
    def pathname(self, path):
        self._pathname = path

    @property
    def driver(self):
        """ Driver figure for this workflow. """
        children = self.root.find_elements_by_xpath('./*')
        return children[0]

    @property
    def flow(self):
        """ Flow div for this workflow. """
        children = self.root.find_elements_by_xpath('./*')
        return children[1]

    @property
    def components(self):
        """ Component figures for this workflow. """
        components = self.flow.find_elements_by_xpath('./*')
        return components

    @property
    def component_names(self):
        """ Names of the Component in this workflow. """
        components = self.flow.find_elements_by_xpath('./*')
        names = []
        for component in components:
            svg = component.find_elements_by_css_selector('svg')
            if len(svg) > 0:
                # WorkflowComponentFigure, get name from svg text
                names.append(svg[0].find_element_by_css_selector('text').text)
            else:
                # WorkflowFigure, get driver name
                children = component.find_elements_by_css_selector('.WorkflowComponentFigure')
                if len(children) > 0:
                    driver_fig = children[0]
                    driver_name = driver_fig.find_element_by_css_selector('svg text').text
                    names.append(driver_name)
        return names

    def collapse(self):
        """ Collapse the flow div. """
        self.flow.find_element_by_css_selector('.ui-icon-maximized').click()

    @property
    def collapsed(self):
        """ True if the flow div is collapsed. """
        return len(self.flow.find_elements_by_css_selector('.ui-icon-minimized')) > 0

    def expand(self):
        """ Expand the flow div. """
        self.flow.find_element_by_css_selector('.ui-icon-minimized').click()

    @property
    def expanded(self):
        """ True if the flow div is expanded. """
        return len(self.flow.find_elements_by_css_selector('.ui-icon-maximized')) > 0

    @property
    def highlighted(self):
        """ True if the flow div background is highlighted. """
        bg_image = self.flow.value_of_css_property('background-image')
        return (bg_image.find('highlight') > 0)

    @property
    def horizontal(self):
        """ True if the components in the flow div are arranged horizontally. """
        for component in self.components:
            svg = component.find_elements_by_css_selector('svg')
            if len(svg) > 0:
                if not 'clear: none' in component.get_attribute('style').lower():
                    return False
        return True

    def flip(self):
        """ Flip the workflow from horizontal to vertical or vice versa. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.flow, 5, 5)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self('flip_button').click()

    def clear(self):
        """ Clear the workflow. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.flow, 5, 5)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self('clear_button').click()


def find_workflow_figures(page):
    """ Returns workflow figure elements in `page`. """
    time.sleep(0.5)  # Pause for stable display.
    root = page.root or page.browser
    return root.find_elements_by_class_name('WorkflowFigure')


def find_workflow_figure(page, name, prefix=None, retries=5):
    """ Return :class:`WorkflowFigure` for `name`. """
    root = page.root or page.browser
    for retry in range(retries):
        time.sleep(0.5)  # Pause for stable display.
        figures = root.find_elements_by_class_name('WorkflowFigure')
        if not figures:
            continue
        driver_name = None
        for figure in figures:
            page.browser.implicitly_wait(1)
            try:
                children = figure.find_elements_by_xpath('./*')
                driver_fig = children[0]
                driver_name = driver_fig.find_element_by_css_selector('svg text').text
            except StaleElementReferenceException:
                logging.warning('get_workflow_figure:'
                                ' StaleElementReferenceException')
            else:
                if driver_name == name:
                    fig = WorkflowFigure(page.browser, page.port, figure)
                    if prefix is not None:
                        if prefix:
                            fig.pathname = '%s.%s' % (prefix, name)
                        else:
                            fig.pathname = name
                    return fig
            finally:
                page.browser.implicitly_wait(TMO)
    return None


class WorkflowComponentFigure(BasePageObject):
    """ Represents elements within a workflow figure. """

    # Context menu.
    edit_button       = ButtonElement((By.XPATH, "./ul/li[text()='Edit']"))
    properties_button = ButtonElement((By.XPATH, "./ul/li[text()='Properties']"))
    run_button        = ButtonElement((By.XPATH, "./ul/li[text()='Run']"))
    remove_button     = ButtonElement((By.XPATH, "./ul/li[text()='Remove from Workflow']"))

    @property
    def pathname(self):
        """ Pathname of this component. """
        return self._pathname

    @pathname.setter
    def pathname(self, path):
        self._pathname = path

    @property
    def state(self):
        """ Exec state of this component. """
        rect = self.root.find_element_by_css_selector('rect')
        style = rect.get_attribute('style').lower()
        if ('stroke: #ff0000' in style):
            return 'INVALID'
        elif ('stroke: #00ff00' in style):
            return 'VALID'
        elif ('stroke: #0000ff' in style):
            return 'RUNNING'
        else:
            return 'UNKNOWN'

    def run(self):
        """ Run this component. """
        rect = self.root.find_element_by_css_selector('rect')
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(rect, 15, 15)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self('run_button').click()


def find_workflow_component_figures(page):
    """ Returns workflow component figure elements in `page`. """
    time.sleep(0.5)  # Pause for stable display.
    root = page.root or page.browser
    return root.find_elements_by_class_name('WorkflowComponentFigure')


def find_workflow_component_figure(page, name, prefix=None, retries=5):
    """ Returns :class:`WorkflowComponentFigure` for `name`. """
    root = page.root or page.browser
    for retry in range(retries):
        time.sleep(0.5)  # Pause for stable display.
        figures = root.find_elements_by_class_name('WorkflowComponentFigure')
        if not figures:
            continue
        component_name = None
        for figure in figures:
            page.browser.implicitly_wait(1)
            component_name = figure.find_element_by_css_selector('svg text').text
            if component_name == name:
                fig = WorkflowComponentFigure(page.browser, page.port, figure)
                if prefix is not None:
                    if prefix:
                        fig.pathname = '%s.%s' % (prefix, name)
                    else:
                        fig.pathname = name
                return fig
    return None
