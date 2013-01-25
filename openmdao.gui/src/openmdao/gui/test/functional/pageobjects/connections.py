import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import TMO
from dialog import DialogPage
from elements import ButtonElement, InputElement


class ConnectionsPage(DialogPage):
    """ Connection editor page. """

    source_component = InputElement((By.ID, 'src_cmp_input'))
    target_component = InputElement((By.ID, 'dst_cmp_input'))

    source_variable = InputElement((By.ID, 'src_var_input'))
    target_variable = InputElement((By.ID, 'dst_var_input'))

    connect_button = ButtonElement((By.ID, 'connect'))

    # Context menu.
    show_all_button       = ButtonElement((By.XPATH, "./div/ul/li[text()='Show All Variables']"))
    show_connected_button = ButtonElement((By.XPATH, "./div/ul/li[text()='Show Connected Variables Only']"))

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

    @property
    def components_pane(self):
        """ Component selector pane. """
        children = self.root.find_elements_by_xpath('./div')
        return children[0]

    @property
    def connections_pane(self):
        """ Connections pane. """
        children = self.root.find_elements_by_xpath('./div')
        return children[1]

    @property
    def variables_pane(self):
        """ Variable selector pane. """
        children = self.root.find_elements_by_xpath('./div')
        return children[2]

    def show_all_variables(self):
        """ Show all variables. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.connections_pane, 5, 5)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self('show_all_button').click()

    def show_connected_variables(self):
        """ Show only the connected variables. """
        chain = ActionChains(self.browser)
        chain.move_to_element_with_offset(self.connections_pane, 5, 5)
        chain.context_click(None)
        chain.perform()
        time.sleep(0.5)
        self('show_connected_button').click()

    def count_variable_figures(self):
        """ Return number of variable figure elements, assume zero. """
        self.browser.implicitly_wait(1)
        try:
            figs = self.browser.find_elements_by_class_name('variable-figure')
            count = len(figs)
        finally:
            self.browser.implicitly_wait(TMO)
        return count

    def count_variable_connections(self):
        """ Return number of variable figure elements, assume zero. """
        self.browser.implicitly_wait(1)
        try:
            figs = self.browser.find_elements_by_class_name('variable-connection')
            count = len(figs)
        finally:
            self.browser.implicitly_wait(TMO)
        return count

    def get_variable_figures(self):
        """ Return variable figure elements. """
        return self.browser.find_elements_by_class_name('variable-figure')

    def get_variable_names(self):
        """ Return names of variable figures. """
        # shameful copy/paste from workspace.get_dataflow_component_names()
        names = []

        # Assume there should be at least 1, wait for number to not change.
        n_found = 0
        for retry in range(10):
            variable_names = \
                self.browser.find_elements_by_class_name('variable-name')
            if variable_names:
                n_names = len(variable_names)
                if n_found:
                    if n_names == n_found:
                        break
                n_found = n_names
        else:
            logging.error('get_variable_names: n_found %s', n_found)
            return names

        for i in range(len(variable_names)):
            for retry in range(10):  # This has had issues...
                try:
                    names.append(self.browser.find_elements_by_class_name('variable-name')[i].text)
                except StaleElementReferenceException:
                    logging.warning('get_variable_names:'
                                    ' StaleElementReferenceException')
                except IndexError:
                    logging.warning('get_variable_names:'
                                    ' IndexError for i=%s, headers=%s',
                                    i, len(variable_names))
                else:
                    break

        if len(names) != len(variable_names):
            logging.error('get_variable_names:'
                          ' expecting %d names, got %s',
                          len(variable_names), names)
        return names

    def find_variable_name(self, name):
        """ Return the variable name elements containing the name `name`. """
        names = []

        # Assume there should be at least 1, wait for number to not change.
        n_found = 0
        for retry in range(10):
            variable_names = \
                self.browser.find_elements_by_class_name('variable-name')
            if variable_names:
                n_names = len(variable_names)
                if n_found:
                    if n_names == n_found:
                        break
                n_found = n_names
        else:
            logging.error('find_variable_name: n_found %s', n_found)
            return names

        for i in range(len(variable_names)):
            for retry in range(10):  # This has had issues...
                try:
                    elm = self.browser.find_elements_by_class_name('variable-name')[i]
                    if elm.text == name:
                        names.append(elm)
                except StaleElementReferenceException:
                    logging.warning('find_variable_name:'
                                    ' StaleElementReferenceException')
                except IndexError:
                    logging.warning('find_variable_name:'
                                    ' IndexError for i=%s, headers=%s',
                                    i, len(variable_names))
                else:
                    break

        return names
