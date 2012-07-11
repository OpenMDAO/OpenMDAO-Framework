from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, InputElement


class ConnectionsPage(DialogPage):
    """ Connection editor page. """

    src_component = InputElement((By.ID, 'src_cmp_list'))
    dst_component = InputElement((By.ID, 'dst_cmp_list'))

    src_variable = InputElement((By.ID, 'src_var_list'))
    dst_variable = InputElement((By.ID, 'dst_var_list'))

    connect_button = ButtonElement((By.ID, 'connect'))

    @property
    def source_component(self):
        """ The name of the source component. """
        return  self('src_component').value

    @source_component.setter
    def source_component(self, name):
        """ Set the name of the source component. """
        self.src_component = name

    @property
    def destination_component(self):
        """ The name of the destination component. """
        return  self('dst_component').value

    @destination_component.setter
    def destination_component(self, name):
        """ Set the name of the destination component. """
        self.dst_component = name

    @property
    def source_variable(self):
        """ The name of the source variable. """
        return self('src_variable').value

    @source_variable.setter
    def source_variable(self, name):
        """ Set the name of the source variable. """
        self.src_variable = name

    @property
    def destination_variable(self):
        """ The name of the destination variable. """
        return  self('dst_variable').value

    @destination_variable.setter
    def destination_variable(self, name):
        """ Set the name of the destination variable. """
        self.dst_variable = name

    def get_variable_figures(self):
        """ Return variable figure elements. """
        return self.browser.find_elements_by_class_name('VariableFigureHeader')

    def connect(self):
        """ Connect the selected variables. """
        self('connect_button').click()

    def connect_vars(self, src, dst):
        """ Connect `src` to `dst`. """
        self.src_variable = src + '\n'
        self.dst_variable = dst + '\n'
        self('connect_button').click()
