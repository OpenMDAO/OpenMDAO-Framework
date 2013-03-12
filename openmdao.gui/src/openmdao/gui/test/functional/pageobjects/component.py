import random
import string

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys

from dialog import DialogPage
from elements import ButtonElement, GridElement, TextElement, InputElement
from workflow import find_workflow_component_figures
from util import ArgsPrompt, NotifierPage


class ComponentPage(DialogPage):
    """ Component editor page. """

    inputs_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Inputs']"))
    slots_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='Slots']"))
    outputs_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Outputs']"))
    events_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Events']"))

    inputs  = GridElement((By.ID, 'Inputs_props'))
    outputs = GridElement((By.ID, 'Outputs_props'))
    inputs_filter = InputElement((By.ID, 'Inputs_variableFilter'))
    outputs_filter = InputElement((By.ID, 'Outputs_variableFilter'))

    def __init__(self, browser, port, locator):
        super(ComponentPage, self).__init__(browser, port, locator)
        # It takes a while for the full load to complete.
        NotifierPage.wait(self)

    def get_tab_labels(self):
        """ Return a list of the tab labels. """
        elements = self.root.find_elements_by_class_name('ui-tabs-anchor')
        labels = [element.text for element in elements]
        return labels

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

    def filter_inputs(self, filter_text):
        self.inputs_filter = filter_text

    def filter_outputs(self, filter_text):
        self.outputs_filter = filter_text

    def clear_inputs_filter(self):
        self.inputs_filter = ""

    def clear_outputs_filter(self):
        self.outputs_filter = ""

    def get_events(self):
        """ Return events grid. """
        self('events_tab').click()
        return self.events

    def get_outputs(self):
        """ Return outputs grid. """
        self('outputs_tab').click()
        return self.outputs
    
    def show_inputs(self):
        """switch to inputs tab"""
        self('inputs_tab').click()

    def show_outputs(self):
        """switch to outputs tab"""
        self('outputs_tab').click()

    def show_slots(self):
        """switch to slots tab"""
        self('slots_tab').click()


class DriverPage(ComponentPage):
    """ Driver editor page. """

    parameters_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Parameters']"))
    workflow_tab    = ButtonElement((By.XPATH, "div/ul/li/a[text()='Workflow']"))
    objectives_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Objectives']"))
    constraints_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Constraints']"))
    triggers_tab    = ButtonElement((By.XPATH, "div/ul/li/a[text()='Triggers']"))

    parameters  = GridElement((By.ID, 'Parameters_parms'))
    objectives  = GridElement((By.ID, 'Objectives_objectives'))
    constraints = GridElement((By.ID, 'Constraints_constraints'))
    triggers    = GridElement((By.ID, 'Triggers_triggers'))

    add_parameter  = ButtonElement((By.XPATH, "//span[text()='Add Parameter']"))
    add_objective  = ButtonElement((By.XPATH, "//span[text()='Add Objective']"))
    add_constraint = ButtonElement((By.XPATH, "//span[text()='Add Constraint']"))
    add_trigger    = ButtonElement((By.XPATH, "//span[text()='Add Event']"))

    def get_parameters(self):
        """ Return parameters grid. """
        self('parameters_tab').click()
        return self.parameters

    def get_objectives(self):
        """ Return objectives grid. """
        self('objectives_tab').click()
        return self.objectives

    def get_constraints(self):
        """ Return constraints grid. """
        self('constraints_tab').click()
        return self.constraints

    def get_triggers(self):
        """ Return triggers grid. """
        self('triggers_tab').click()
        return self.triggers

    def new_parameter(self):
        """ Return :class:`ParameterDialog`. """
        self('add_parameter').click()
        return ParameterDialog(self.browser, self.port,
                               (By.XPATH, "//div[@id='parameter-dialog']/.."))

    def new_objective(self):
        """ Return :class:`ObjectiveDialog`. """
        self('add_objective').click()
        return ObjectiveDialog(self.browser, self.port,
                               (By.XPATH, "//div[@id='objective-dialog']/.."))

    def new_constraint(self):
        """ Return :class:`ConstraintDialog`. """
        self('add_constraint').click()
        return ConstraintDialog(self.browser, self.port,
                                (By.XPATH, "//div[@id='constraint-dialog']/.."))

    def new_trigger(self):
        """ Return :class:`EventDialog`. """
        self('add_trigger').click()
        return EventDialog(self.browser, self.port,
                           (By.XPATH, "//div[@id='event-dialog']/.."))

    def show_workflow(self):
        """switch to workflow tab"""
        self('workflow_tab').click()

    def get_workflow_component_figures(self):
        """ Return workflow component figure elements. """
        return find_workflow_component_figures(self)


class ParameterDialog(DialogPage):
    """ Dialog for adding a new parameter. """

    target = InputElement((By.ID, 'parameter-target'))
    low    = InputElement((By.ID, 'parameter-low'))
    high   = InputElement((By.ID, 'parameter-high'))
    scaler = InputElement((By.ID, 'parameter-scaler'))
    adder  = InputElement((By.ID, 'parameter-adder'))
    name   = InputElement((By.ID, 'parameter-name'))
    ok     = ButtonElement((By.ID, 'parameter-ok'))
    cancel = ButtonElement((By.ID, 'parameter-cancel'))


class ObjectiveDialog(DialogPage):
    """ Dialog for adding a new objective. """

    expr   = InputElement((By.ID, 'objective-expr'))
    name   = InputElement((By.ID, 'objective-name'))
    ok     = ButtonElement((By.ID, 'objective-ok'))
    cancel = ButtonElement((By.ID, 'objective-cancel'))


class ConstraintDialog(DialogPage):
    """ Dialog for adding a new constraint. """

    expr   = InputElement((By.ID, 'constraint-expr'))
    scaler = InputElement((By.ID, 'constraint-scaler'))
    adder  = InputElement((By.ID, 'constraint-adder'))
    name   = InputElement((By.ID, 'constraint-name'))
    ok     = ButtonElement((By.ID, 'constraint-ok'))
    cancel = ButtonElement((By.ID, 'constraint-cancel'))


class EventDialog(DialogPage):
    """ Dialog for adding a new event. """

    target = InputElement((By.ID, 'event-target'))
    ok     = ButtonElement((By.ID, 'event-ok'))
    cancel = ButtonElement((By.ID, 'event-cancel'))


class AssemblyPage(ComponentPage):
    """ Assembly editor page. """

    dataflow_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Dataflow']"))

    def show_dataflow(self):
        self('dataflow_tab').element.click()


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


class NameInstanceDialog(ArgsPrompt):
    """ Adds :meth:`create_and_dismiss` to :class:`ArgsPrompt`. """

    def __init__(self, parent):
        super(NameInstanceDialog, self).__init__(parent.browser, parent.port)

    def create_and_dismiss(self, name=None):
        """Names the instance. Returns the name. Force a name with the name argument"""
        chars = string.ascii_uppercase
        name = name or ''.join(random.choice(chars).strip() for x in range(8))

        self.name = name
        self.click_ok()

        return name
