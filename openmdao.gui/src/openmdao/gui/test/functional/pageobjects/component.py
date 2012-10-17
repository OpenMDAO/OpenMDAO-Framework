import random 
import string 
 
from selenium.webdriver.common.by import By 
 
from dialog import DialogPage 
from elements import ButtonElement, GridElement, TextElement, InputElement 
from workflow import find_workflow_component_figures
from util import NotifierPage, ValuePrompt
 
 
class ComponentPage(DialogPage): 
    """ Component editor page. """ 
 
    inputs_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Inputs']")) 
    slots_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='Slots']")) 
    outputs_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Outputs']")) 
 
    inputs  = GridElement((By.ID, 'Inputs_props')) 
    outputs = GridElement((By.ID, 'Outputs_props')) 
 
    def __init__(self, browser, port, locator): 
        super(ComponentPage, self).__init__(browser, port, locator) 
        # It takes a while for the full load to complete. 
        NotifierPage.wait(self)
 
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
 
    def show_slots(self): 
        """switch to slots tab""" 
        self('slots_tab').click() 
 

 
class DriverPage(ComponentPage): 
    """ Driver editor page. """ 
 
    parameters_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Parameters']")) 
    workflow_tab    = ButtonElement((By.XPATH, "div/ul/li/a[text()='Workflow']")) 
    objectives_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Objectives']")) 
    constraints_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Constraints']")) 
 
    parameters  = GridElement((By.ID, 'Parameters_parms')) 
    objectives  = GridElement((By.ID, 'Objectives_objectives')) 
    constraints = GridElement((By.ID, 'Constraints_constraints')) 
 
    add_parameter  = ButtonElement((By.XPATH, "//div[text()='Add Parameter']"))
    add_objective  = ButtonElement((By.XPATH, "//div[text()='Add Objective']"))
    add_constraint = ButtonElement((By.XPATH, "//div[text()='Add Constraint']"))

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

    def show_workflow(self): 
        """switch to workflow tab""" 
        self('workflow_tab').click() 
 
    def get_workflow_component_figures(self):
        """ Return workflow component figure elements. """
        return find_workflow_component_figures(self)
 

class ParameterDialog(DialogPage):
    """ Dialog for adding a new parameter. """

    target =  InputElement((By.ID, 'parameter-target'))
    low    =  InputElement((By.ID, 'parameter-low'))
    high   =  InputElement((By.ID, 'parameter-high'))
    scaler =  InputElement((By.ID, 'parameter-scaler'))
    adder  =  InputElement((By.ID, 'parameter-adder'))
    name   =  InputElement((By.ID, 'parameter-name'))
    ok     = ButtonElement((By.ID, 'parameter-ok'))
    cancel = ButtonElement((By.ID, 'parameter-cancel'))


class ObjectiveDialog(DialogPage):
    """ Dialog for adding a new objective. """

    expr   =  InputElement((By.ID, 'objective-expr'))
    name   =  InputElement((By.ID, 'objective-name'))
    ok     = ButtonElement((By.ID, 'objective-ok'))
    cancel = ButtonElement((By.ID, 'objective-cancel'))


class ConstraintDialog(DialogPage):
    """ Dialog for adding a new constraint. """

    expr   =  InputElement((By.ID, 'constraint-expr'))
    scaler =  InputElement((By.ID, 'constraint-scaler'))
    adder  =  InputElement((By.ID, 'constraint-adder'))
    name   =  InputElement((By.ID, 'constraint-name'))
    ok     = ButtonElement((By.ID, 'constraint-ok'))
    cancel = ButtonElement((By.ID, 'constraint-cancel'))


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
 
 
class NameInstanceDialog(ValuePrompt): 
    """ Adds :meth:`create_and_dismiss` to :class:`ValuePrompt`. """ 
 
    def __init__(self, parent):
        super(NameInstanceDialog, self).__init__(parent.browser, parent.port)

    def create_and_dismiss(self, name=None): 
        """Names the instance. Returns the name. Force a name with the name argument""" 
        chars = string.ascii_uppercase 
        name = name or ''.join(random.choice(chars).strip() for x in range(8)) 
 
        self.value = name 
        self.click_ok() 
 
        return name 

