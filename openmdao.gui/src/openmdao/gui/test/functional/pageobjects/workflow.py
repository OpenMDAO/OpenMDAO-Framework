import logging
import time

from selenium.webdriver.common.by import By

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import TextElement


class WorkflowFigure(BasePageObject):
    """ Represents elements within a workflow figure. """

    # parts of the WorkflowFigure div. Nothing here yet. Not sure
    #   if needed
    title_bar = TextElement((By.CLASS_NAME, 'WorkflowFigureTitleBar'))

    # Context menu. Not needed yet but will need later

    @property
    def pathname(self):
        """ Pathname of this component. """
        return self._pathname

    @pathname.setter
    def pathname(self, path):
        self._pathname = path

    @property
    def background_color(self):
        """ Figure background-color property. """
        return self.root.value_of_css_property('background-color')


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
        fig_name = None
        for figure in figures:
            page.browser.implicitly_wait(1)
            try:
                # figure name is the text of the only child of the WorkflowFigure div
                children = figure.find_elements_by_xpath('./*')
                fig_name = children[0].text
                #   could also try figure.childNodes[0].text
            except StaleElementReferenceException:
                logging.warning('get_workflow_figure:'
                                ' StaleElementReferenceException')
            else:
                if fig_name == name:
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

