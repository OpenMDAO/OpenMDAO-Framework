import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import GenericElement, ButtonElement, TextElement
from component import ComponentPage, DriverPage, PropertiesPage, AssemblyPage
from connections import ConnectionsPage


class DataflowFigure(BasePageObject):
    """ Represents elements within a dataflow figure. """

    name = TextElement((By.CLASS_NAME, 'DataflowFigureHeader'))

    top_left     = GenericElement((By.CLASS_NAME, 'DataflowFigureTopLeft'))
    header       = GenericElement((By.CLASS_NAME, 'DataflowFigureHeader'))
    top_right    = ButtonElement((By.CLASS_NAME,  'DataflowFigureTopRight'))
    content_area = GenericElement((By.CLASS_NAME, 'DataflowFigureContentArea'))

    bottom_left  = GenericElement((By.CLASS_NAME, 'DataflowFigureBottomLeft'))
    bottom_right = GenericElement((By.CLASS_NAME, 'DataflowFigureBottomRight'))
    footer       = GenericElement((By.CLASS_NAME, 'DataflowFigureFooter'))

    # Context menu.
    edit_button        = ButtonElement((By.XPATH, "../div/a[text()='Edit']"))
    properties_button  = ButtonElement((By.XPATH, "../div/a[text()='Properties']"))
    run_button         = ButtonElement((By.XPATH, "../div/a[text()='Run']"))
    connections_button = ButtonElement((By.XPATH, "../div/a[text()='Edit Data Connections']"))
    show_dataflows     = ButtonElement((By.XPATH, "../div/a[text()='Show Data Connections']"))
    hide_dataflows     = ButtonElement((By.XPATH, "../div/a[text()='Hide Data Connections']"))
    show_driverflows   = ButtonElement((By.XPATH, "../div/a[text()='Show Driver Connections']"))
    hide_driverflows   = ButtonElement((By.XPATH, "../div/a[text()='Hide Driver Connections']"))
    disconnect_button  = ButtonElement((By.XPATH, "../div/a[text()='Disconnect']"))
    remove_button      = ButtonElement((By.XPATH, "../div/a[text()='Remove']"))

    # Port context menus.
    edit_connections   = ButtonElement((By.XPATH, "../div/a[text()='Edit Connections']"))
    edit_passthroughs  = ButtonElement((By.XPATH, "../div/a[text()='Edit Passthroughs']"))
    edit_driver        = ButtonElement((By.XPATH, "../div/a[text()='Edit Driver']"))

    def __init__(self, browser, port, root):
        super(DataflowFigure, self).__init__(browser, port, root)
        self._pathname = None

    @property
    def pathname(self):
        """ Pathname of this component. """
        if self._pathname is None:
            # Much slower than if explicitly set.
            parent = self('header').find_element_by_xpath('..')
            fig_id = parent.get_attribute('id')
            script = "return jQuery('#" + fig_id + "').data('pathname')"
            self._pathname = self.browser.execute_script(script)
        return self._pathname

    @pathname.setter
    def pathname(self, path):
        self._pathname = path

    @property
    def input_port(self):
        """ Input port element, `pathname` must be set previously. """
        return self.root.find_element_by_id(self.pathname + '-input')

    @property
    def output_port(self):
        """ Output port element, `pathname` must be set previously. """
        return self.root.find_element_by_id(self.pathname + '-output')

    @property
    def border(self):
        """ Figure border property. """
        return self.root.value_of_css_property('border')

    @property
    def background_color(self):
        """ Figure background-color property. """
        return self.root.value_of_css_property('background-color')

    @property
    def coords(self):
        """ Figure (left, top). """
        left = self.root.value_of_css_property('left')
        left = int(left[0:-2])  # Drop 'px'.
        top = self.root.value_of_css_property('top')
        top = int(top[0:-2])  # Drop 'px'.
        return (left, top)

    def editor_page(self, double_click=True, base_type='Component', version=ComponentPage.Version.OLD):
        """ Return :class:`ComponentPage` for this component. """
        chain = ActionChains(self.browser)
        if double_click:
            chain.double_click(self.root).perform()
        else:
            self._context_click('edit_button')
        editor_id = 'ObjectFrame_%s' % self.pathname.replace('.', '-')
        chain.release(None).perform()
        if base_type == 'Assembly':
            return AssemblyPage(self.browser, self.port, (By.ID, editor_id))
        elif base_type == 'Driver':
            return DriverPage(self.browser, self.port, (By.ID, editor_id))
        else:
            return ComponentPage(self.browser, self.port, (By.ID, editor_id), version=version)

    def properties_page(self):
        """ Return :class:`PropertiesPage` for this component. """
        self._context_click('properties_button')
        props_id = '%s-properties' % self.pathname.replace('.', '-')
        return PropertiesPage(self.browser, self.port, (By.ID, props_id))

    def connections_page(self):
        """ Return :class:`ConnectionsPage` for this component. """
        self._context_click('connections_button')
        frame_id = 'ConnectionsFrame-%s' % self.pathname.replace('.', '-')
        return ConnectionsPage(self.browser, self.port, (By.ID, frame_id))

    def input_edit_driver(self, driver_pathname):
        """ Return :class:`DriverPage` associated with the input port. """
        chain = ActionChains(self.browser)
        chain.context_click(self.input_port).perform()
        time.sleep(0.5)
        self('edit_driver').click()
        editor_id = 'ObjectFrame_%s' % driver_pathname.replace('.', '-')
        return DriverPage(self.browser, self.port, (By.ID, editor_id))

    def output_edit_driver(self, driver_pathname):
        """ Return :class:`DriverPage` associated with the output port. """
        # FIXME: can't get response from context click.
        chain = ActionChains(self.browser)
        chain.context_click(self.output_port).perform()
        time.sleep(0.5)
        self('edit_driver').click()
        editor_id = 'ObjectFrame_%s' % driver_pathname.replace('.', '-')
        return DriverPage(self.browser, self.port, (By.ID, editor_id))

    def run(self):
        """ Run this component. """
        self._context_click('run_button')

    def disconnect(self):
        """ Disconnect this component. """
        self._context_click('disconnect_button')

    def remove(self):
        """ Remove this component. """
        self._context_click('remove_button')

    def display_dataflows(self, show):
        """ Show/hide data flows. """
        if show:
            self._context_click('show_dataflows')
        else:
            self._context_click('hide_dataflows')

    def display_driverflows(self, show):
        """ Show/hide driver flows. """
        if show:
            self._context_click('show_driverflows')
        else:
            self._context_click('hide_driverflows')

    def _context_click(self, name):
        """ Display context menu. """
        chain = ActionChains(self.browser)
        # Default is centered which causes problems in some contexts.
        # Offset is apparently limited, (20, 20) had problems.
        chain.move_to_element_with_offset(self.root, 15, 15)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self(name).click()

    def get_pathname(self):
        '''Get the OpenMDAO pathname for a DataflowFigure'''
        figid = self.root.get_attribute('id')  # get the ID of the element here
        script = "return jQuery('#" + figid + "').data('pathname')"
        return self.browser.execute_script(script)

    def get_parent(self):
        '''get the parent element of this DataflowFigure'''
        return self.root.find_element_by_xpath("..")

    def get_drop_targets(self):
        '''Dataflow figures are made of many subelements. This function
        returns a list of them so that we can try dropping on any one
        of the elements
        '''
        # return [self(area).element for area in \
        #        ['top_left','header','top_right', 'content_area',
        #         'bottom_left', 'footer', 'bottom_right']]

        # add back 'top_left' 'bottom_left' at some point. right now that test fails
        arr = ['content_area', 'header', 'footer', 'bottom_right', 'top_right']
        return [self(area).element for area in arr]


def find_dataflow_figures(page):
    """ Return dataflow figure elements in `page`. """
    root = page.root or page.browser
    time.sleep(0.5)  # Pause for stable display.
    elements = root.find_elements_by_class_name('DataflowFigure')
    figs = [DataflowFigure(page.browser, page.port, element) for element in elements]
    return figs


def find_dataflow_figure(page, name, prefix=None, retries=5):
    """ Return :class:`DataflowFigure` for `name` in `page`. """
    root = page.root or page.browser
    for retry in range(retries):
        time.sleep(0.5)  # Pause for stable display.
        figures = root.find_elements_by_class_name('DataflowFigure')
        if not figures:
            continue
        fig_name = None
        for figure in figures:
            page.browser.implicitly_wait(1)
            try:
                header = figure.find_elements_by_class_name('DataflowFigureHeader')
                if len(header) == 0:
                    # the outermost figure (globals) has no header or name
                    if name == '' and prefix is None:
                        fig = DataflowFigure(page.browser, page.port, figure)
                        return fig
                    else:
                        continue
                fig_name = figure.find_elements_by_class_name('DataflowFigureHeader')[0].text
            except StaleElementReferenceException:
                logging.warning('get_dataflow_figure:'
                                ' StaleElementReferenceException')
            else:
                if fig_name == name:
                    fig = DataflowFigure(page.browser, page.port, figure)
                    if prefix is not None:
                        if prefix:
                            fig.pathname = '%s.%s' % (prefix, name)
                        else:
                            fig.pathname = name
                    return fig
            finally:
                page.browser.implicitly_wait(TMO)
    return None


def find_dataflow_component_names(page):
    """ Return names of dataflow components in `page`. """
    root = page.root or page.browser
    names = []

    # Assume there should be at least 1, wait for number to not change.
    n_found = 0
    for retry in range(10):
        time.sleep(0.5)  # Pause for stable display.
        dataflow_component_headers = \
            root.find_elements_by_class_name('DataflowFigureHeader')
        if dataflow_component_headers:
            n_headers = len(dataflow_component_headers)
            if n_found:
                if n_headers == n_found:
                    return [h.text for h in dataflow_component_headers]
            n_found = n_headers
    else:
        logging.error('get_dataflow_component_names: n_found %s', n_found)
        return names
