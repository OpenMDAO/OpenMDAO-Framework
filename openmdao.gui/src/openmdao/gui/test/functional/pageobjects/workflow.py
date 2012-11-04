import logging
import time

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO


class WorkflowFigure(BasePageObject):
    """ Represents elements within a workflow figure. """

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

    @property
    def highlighted(self):
        """ True if the flow div background is highlighted. """
        bg_image = self.flow.value_of_css_property('background-image')
        return (bg_image.find('highlight') > 0)


def find_workflow_figures(page):
    """ Returns workflow figure elements in `page`. """
    time.sleep(0.5)  # Pause for stable display.
    root = page.root or page.browser
    return root.find_elements_by_class_name('WorkflowFigure')


def find_workflow_component_figures(page):
    """ Returns workflow component figure elements in `page`. """
    time.sleep(0.5)  # Pause for stable display.
    root = page.root or page.browser
    return root.find_elements_by_class_name('WorkflowComponentFigure')


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
