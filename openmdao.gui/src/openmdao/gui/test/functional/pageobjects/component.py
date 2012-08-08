<<<<<<< HEAD
import random 
import string 
 
from selenium.webdriver.common.by import By 
 
from dialog import DialogPage 
from elements import ButtonElement, GridElement, TextElement, InputElement 
from util import NotifierPage 
from basepageobject import TMO 
 
 
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
        self('slots_tab').element.click() 
 
 
class DriverPage(ComponentPage): 
    """ Driver editor page. """ 
 
    parameters_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='Parameters']")) 
    workflow_tab     = ButtonElement((By.XPATH, "div/ul/li/a[text()='Workflow']")) 
    objectives_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='Objectives']")) 
    equalities_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='EqConstr]")) 
    inequalities_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='IneqConstr]")) 
 
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
 
 
class NameInstanceDialog(DialogPage): 
    """The dialog that appears to name a Component when its dragged onto the workspace""" 
 
    textInput = InputElement((By.ID, 'get-value-input')) 
    okButton = ButtonElement((By.ID, 'get-value-ok')) 
    cancelButton = ButtonElement((By.ID, 'get-value-cancel')) 
 
     
    def __init__(self, browser, port): 
        super(NameInstanceDialog, self).__init__(browser, port, (By.XPATH, '//div[@id="get-value"]/..')) 
 
 
 
    def create_and_dismiss(self, name = None): 
        """Names the instance. Returns the name. Force a name with the name argument""" 
        chars = string.ascii_uppercase 
        name = name or ''.join(random.choice(chars).strip() for x in range(8)) 
 
        self('textInput').value = name 
        self('okButton').click() 
 
        return name 
 
    def clickOK(): 
        okButton.click() 
 
    def clickCancel(): 
        cancelButton.click() 
 

