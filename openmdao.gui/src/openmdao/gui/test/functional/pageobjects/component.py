from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement


class Grid(object):
    """ Represents a SlickGrid at `root`. """

    def __init__(self, root):
        self.grid = root

    def get_data(self):
        """ Returns a list of rows, each row is a list of columns. """
        rows = self.grid.find_elements(By.CLASS_NAME, 'slick-row')
        data = []
        for row in rows:
            cols = row.find_elements(By.CLASS_NAME, 'slick-cell')
            row_data = []
            for col in cols:
                row_data.append(col.text)
            data.append(row_data)
        return data


class ComponentPage(DialogPage):
    """ Component editor page. """

    inputs_tab   = ButtonElement((By.XPATH, 'dl/dt[1]'))
    slots_tab    = ButtonElement((By.XPATH, 'dl/dt[2]'))
    outputs_tab  = ButtonElement((By.XPATH, 'dl/dt[3]'))

    def get_inputs(self):
        """ Return ``(name, type, value, units, desc, connected)``. """
        self('inputs_tab').click()
        grid = Grid(self.root.find_element_by_id('Inputs_props'))
        return grid.get_data()

    def set_input(row, value):
        """ Set input on `row` to `value`. """
        self('inputs_tab').click()

    def get_outputs(self):
        """ Return ``(name, type, value, units, desc, connected)``. """
        self('outputs_tab').click()
        grid = Grid(self.root.find_element_by_id('Outputs_props'))
        return grid.get_data()

