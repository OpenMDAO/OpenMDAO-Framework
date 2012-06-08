from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, GridElement, TextElement


class ComponentPage(DialogPage):
    """ Component editor page. """

    dataflow_tab = ButtonElement((By.XPATH, "dl/dt[text()='Dataflow']"))
    inputs_tab   = ButtonElement((By.XPATH, "dl/dt[text()='Inputs']"))
    slots_tab    = ButtonElement((By.XPATH, "dl/dt[text()='Slots']"))
    outputs_tab  = ButtonElement((By.XPATH, "dl/dt[text()='Outputs']"))

    inputs  = GridElement((By.ID, 'Inputs_props'))
    outputs = GridElement((By.ID, 'Outputs_props'))

    def get_inputs(self):
        """ Return inputs grid. """
        self('inputs_tab').click()
        return self.inputs

    def set_input(self, name, value):
        """ Set input `name` to `value`. """
        self('inputs_tab').click()
        grid = self.inputs
        found = []
        for row in grid.rows:
            if row[0] == name:
                row[2] = value
                return
            found.append(row[0])
        raise RuntimeError('%r not found in inputs %s' % (name, found))

    def get_outputs(self):
        """ Return outputs grid. """
        self('outputs_tab').click()
        return self.outputs


class PropertiesPage(DialogPage):
    """ Component properties page. """

    header  = TextElement((By.XPATH, 'h3[1]'))
    inputs  = GridElement((By.ID, 'Inputs_props'))
    outputs = GridElement((By.ID, 'Outputs_props'))

    def set_input(self, name, value):
        """ Set input `name` to `value`. """
        self('inputs_tab').click()
        grid = self.inputs
        found = []
        for row in grid.rows:
            if row[0] == name:
                row[1] = value
                return
            found.append(row[0])
        raise RuntimeError('%r not found in inputs %s' % (name, found))

