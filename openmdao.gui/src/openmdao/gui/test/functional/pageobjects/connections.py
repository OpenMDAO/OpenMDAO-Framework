import logging

from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import TMO
from dialog import DialogPage
from elements import ButtonElement, InputElement


class ConnectionsPage(DialogPage):
    """ Connection editor page. """

    source_component = InputElement((By.ID, 'src_cmp_list'))
    target_component = InputElement((By.ID, 'dst_cmp_list'))

    source_variable = InputElement((By.ID, 'src_var_list'))
    target_variable = InputElement((By.ID, 'dst_var_list'))

    connect_button = ButtonElement((By.ID, 'connect'))

    def set_source_component(self, comp_name):
        """ Set the source component. """
        self.source_component = comp_name + '\n'

    def set_target_component(self, comp_name):
        """ Set the target component. """
        self.target_component = comp_name + '\n'

    def connect(self):
        """ Connect the selected variables. """
        self('connect_button').click()

    def connect_vars(self, src, dst):
        """ Connect `src` to `dst`. """
        self.source_variable = src + '\n'
        self.target_variable = dst + '\n'
        self('connect_button').click()

    def check_variable_figures(self):
        """ Return number of variable figure elements, assume zero. """
        self.browser.implicitly_wait(1)
        try:
            figs = self.browser.find_elements_by_class_name('VariableFigureHeader')
            count = len(figs)
        finally:
            self.browser.implicitly_wait(TMO)
        return count

    def get_variable_figures(self):
        """ Return variable figure elements. """
        return self.browser.find_elements_by_class_name('VariableFigureHeader')

    def get_variable_names(self):
        """ Return names of variables. """
        # shameful copy/paste from workspace.get_dataflow_component_names()
        names = []

        # Assume there should be at least 1, wait for number to not change.
        n_found = 0
        for retry in range(10):
            variable_figure_headers = \
                self.browser.find_elements_by_class_name('VariableFigureHeader')
            if variable_figure_headers:
                n_headers = len(variable_figure_headers)
                if n_found:
                    if n_headers == n_found:
                        break
                n_found = n_headers
        else:
            logging.error('get_variable_names: n_found %s', n_found)
            return names

        for i in range(len(variable_figure_headers)):
            for retry in range(10):  # This has had issues...
                try:
                    names.append(self.browser.find_elements_by_class_name('VariableFigureHeader')[i].text)
                except StaleElementReferenceException:
                    logging.warning('get_variable_names:'
                                    ' StaleElementReferenceException')
                except IndexError:
                    logging.warning('get_variable_names:'
                                    ' IndexError for i=%s, headers=%s',
                                    i, len(variable_figure_headers))
                else:
                    break

        if len(names) != len(variable_figure_headers):
            logging.error('get_variable_names:'
                          ' expecting %d names, got %s',
                          len(variable_figure_headers), names)
        return names
