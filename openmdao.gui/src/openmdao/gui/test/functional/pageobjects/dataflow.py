from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By

from basepageobject import BasePageObject
from elements import ButtonElement, TextElement
from component import ComponentPage, PropertiesPage
from connections import ConnectionsPage


class DataflowFigure(BasePageObject):
    """ Represents elements within a dataflow figure. """

    name = TextElement((By.CLASS_NAME, 'DataflowFigureHeader'))
    top_right = ButtonElement((By.CLASS_NAME, 'DataflowFigureTopRight'))

    # Context menu.
    edit_button        = ButtonElement((By.XPATH, "../div/a[text()='Edit']"))
    properties_button  = ButtonElement((By.XPATH, "../div/a[text()='Properties']"))
    connections_button = ButtonElement((By.XPATH, "../div/a[text()='Connections']"))
    disconnect_button  = ButtonElement((By.XPATH, "../div/a[text()='Disconnect']"))
    run_button         = ButtonElement((By.XPATH, "../div/a[text()='Run']"))
    remove_button      = ButtonElement((By.XPATH, "../div/a[text()='Remove']"))

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

    def editor_page(self, double_click=True):
        """ Return :class:`ComponentPage` for this component. """
        chain = ActionChains(self.browser)
        if double_click:
            chain.double_click(self.root).perform()
        else:
            chain.context_click(self.root).perform()
            self('edit_button').click()
        editor_id = 'CE-%s' % self.pathname.replace('.', '-')
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

    def properties_page(self):
        """ Return :class:`PropertiesPage` for this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('properties_button').click()
        props_id = '%s-properties' % self.pathname.replace('.', '-')
        return PropertiesPage(self.browser, self.port, (By.ID, props_id))

    def connections_page(self):
        """ Return :class:`ConnectionsPage` for this component. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.root,15,15).context_click(None).perform()
        self('connections_button').click()
        frame_id = 'ConnectionsFrame-%s' % self.pathname.replace('.', '-')
        return ConnectionsPage(self.browser, self.port, (By.ID, frame_id))

    def run(self):
        """ Run this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('run_button').click()

    def disconnect(self):
        """ Disconnect this component. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.root,2,2).context_click(None).perform()
        self('disconnect_button').click()

    def remove(self):
        """ Remove this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('remove_button').click()
