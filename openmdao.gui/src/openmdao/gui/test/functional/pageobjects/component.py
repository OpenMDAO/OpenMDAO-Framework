from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, GridElement, TextElement
from util import NotifierPage


class ComponentPage(DialogPage):
    """ Component editor page. """

    inputs_tab   = ButtonElement((By.XPATH, "dl/dt[text()='Inputs']"))
    slots_tab    = ButtonElement((By.XPATH, "dl/dt[text()='Slots']"))
    outputs_tab  = ButtonElement((By.XPATH, "dl/dt[text()='Outputs']"))

    inputs  = GridElement((By.ID, 'Inputs_props'))
    outputs = GridElement((By.ID, 'Outputs_props'))

    def __init__(self, browser, port, locator):
        super(ComponentPage, self).__init__(browser, port, locator)
        # It takes a while for the full load to complete.
        NotifierPage.wait(browser, port)

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


class DriverPage(ComponentPage):
    """ Driver editor page. """

    parameters_tab   = ButtonElement((By.XPATH, "dl/dt[text()='Parameters']"))
    workflow_tab     = ButtonElement((By.XPATH, "dl/dt[text()='Workflow']"))
    objectives_tab   = ButtonElement((By.XPATH, "dl/dt[text()='Objectives']"))
    equalities_tab   = ButtonElement((By.XPATH, "dl/dt[text()='EqConstr]"))
    inequalities_tab = ButtonElement((By.XPATH, "dl/dt[text()='IneqConstr]"))

    parameters   = GridElement((By.ID, 'Parameters_parms'))
    objectives   = GridElement((By.ID, 'Objectives_objectives'))
    equalities   = GridElement((By.ID, 'EqConstraints_constraints'))
    inequalities = GridElement((By.ID, 'IneqConstraints_constraints'))

    def get_parameters(self):
        """ Return parameters grid. """
        self('parameters_tab').click()
        return self.parameters

    def get_objectives(self):
        """ Return objectives grid. """
        self('objectives_tab').click()
        return self.objectives

    def get_eq_constraints(self):
        """ Return equality constraints grid. """
        self('equalities_tab').click()
        return self.equalities

    def get_ineq_constraints(self):
        """ Return constraints grid. """
        self('inequalities_tab').click()
        return self.inequalities


class AssemblyPage(ComponentPage):
    """ Assembly editor page. """

    dataflow_tab = ButtonElement((By.XPATH, "dl/dt[text()='Dataflow']"))


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

