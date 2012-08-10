import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from basepageobject import BasePageObject, TMO
from elements import GenericElement, ButtonElement, TextElement
from component import ComponentPage, DriverPage, PropertiesPage, AssemblyPage
from connections import ConnectionsPage

class DataflowFigure(BasePageObject):
    """ Represents elements within a dataflow figure. """

    name = TextElement((By.CLASS_NAME, 'DataflowFigureHeader'))

    top_left = GenericElement((By.CLASS_NAME, 'DataflowFigureTopLeft'))
    header = GenericElement((By.CLASS_NAME, 'DataflowFigureHeader'))
    top_right = ButtonElement((By.CLASS_NAME, 'DataflowFigureTopRight'))
    
    content_area = GenericElement((By.CLASS_NAME, 'DataflowFigureContentArea'))

    bottom_left = GenericElement((By.CLASS_NAME, 'DataflowFigureBottomLeft'))
    bottom_right = GenericElement((By.CLASS_NAME, 'DataflowFigureBottomRight'))
    footer = GenericElement((By.CLASS_NAME, 'DataflowFigureFooter'))



    # Context menu.
    edit_button        = ButtonElement((By.XPATH, "../div/a[text()='Edit']"))
    properties_button  = ButtonElement((By.XPATH, "../div/a[text()='Properties']"))
    connections_button = ButtonElement((By.XPATH, "../div/a[text()='Connections']"))
    disconnect_button  = ButtonElement((By.XPATH, "../div/a[text()='Disconnect']"))
    show_dataflows     = ButtonElement((By.XPATH, "../div/a[text()='Show Data Flows']"))
    hide_dataflows     = ButtonElement((By.XPATH, "../div/a[text()='Hide Data Flows']"))
    show_driverflows   = ButtonElement((By.XPATH, "../div/a[text()='Show Driver Flows']"))
    hide_driverflows   = ButtonElement((By.XPATH, "../div/a[text()='Hide Driver Flows']"))
    run_button         = ButtonElement((By.XPATH, "../div/a[text()='Run']"))
    remove_button      = ButtonElement((By.XPATH, "../div/a[text()='Remove']"))

    # Port context menus.
    edit_connections   = ButtonElement((By.XPATH, "../div/a[text()='Edit Connections']"))
    edit_driver        = ButtonElement((By.XPATH, "../div/a[text()='Edit Driver']"))


    @property
    def pathname(self):
        """ Pathname of this component. """
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

    def editor_page(self, double_click=True, is_assembly=False):
        """ Return :class:`ComponentPage` for this component. """
        chain = ActionChains(self.browser)
        if double_click:
            chain.double_click(self.root).perform()
        else:
            self._context_click('edit_button')
        editor_id = 'CE-%s' % self.pathname.replace('.', '-')
        if is_assembly:
            return AssemblyPage(self.browser, self.port, (By.ID, editor_id))
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

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
        editor_id = 'CE-%s' % driver_pathname.replace('.', '-')
        return DriverPage(self.browser, self.port, (By.ID, editor_id))

    def output_edit_driver(self, driver_pathname):
        """ Return :class:`DriverPage` associated with the output port. """
# FIXME: can't get response from context click.
        chain = ActionChains(self.browser)
        chain.context_click(self.output_port).perform()
        time.sleep(0.5)
        self('edit_driver').click()
        editor_id = 'CE-%s' % driver_pathname.replace('.', '-')
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
        chain = chain.move_to_element_with_offset(self.root, 15, 15)
        chain = chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self(name).click()

