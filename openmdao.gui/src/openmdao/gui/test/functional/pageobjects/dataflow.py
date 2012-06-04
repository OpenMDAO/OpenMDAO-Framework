from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import ButtonElement
from component import ComponentPage


class DataflowFigure(BasePageObject):
    """ Represents elements within a dataflow figure. """

    properties_button = ButtonElement((By.XPATH, "//a[text()='Properties']"))
    run_button        = ButtonElement((By.XPATH, "//a[text()='Run']"))
    disconnect_button = ButtonElement((By.XPATH, "//a[text()='Disconnect']"))
    remove_button     = ButtonElement((By.XPATH, "//a[text()='Remove']"))

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
        return self.root.find_element_by_id(self.pathname+'-input')

    @property
    def output_port(self):
        """ Output port element, `pathname` must be set previously. """
        return self.root.find_element_by_id(self.pathname+'-output')

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

    def editor_page(self):
        """ Return :class:`ComponentPage` for this component. """
        chain = ActionChains(self.browser)
        chain.double_click(self.root).perform()
        editor_id = 'CE-%s' % self.pathname.replace('.', '-')
        return ComponentPage(self.browser, self.port, (By.ID, editor_id))

    def properties_page(self):
        """ Return :class:`PropertiesPage` for this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
#FIXME: for some reason the button isn't found.
        self('properties_button').click()
        return PropertiesPage(self.browser, self.port)

    def remove(self):
        """ Remove this component. """
        chain = ActionChains(self.browser)
        chain.context_click(self.root).perform()
        self('remove_button').click()

