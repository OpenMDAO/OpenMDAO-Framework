import random
import string

from functools import partial

from selenium.webdriver.common.by import By

from dialog import DialogPage
from elements import ButtonElement, GridElement, TextElement, InputElement
from workflow import find_workflow_figure, find_workflow_figures, \
                     find_workflow_component_figures, \
                     find_workflow_component_figure
from util import ArgsPrompt, NotifierPage
from grid import GridColumnPicker


class ComponentPage(DialogPage):

    class Variable(object):
        def __init__(self, row, headers):
            self._cells = row.cells

            def getter(cls, index=0):
                return cls._cells[index]

            def setter(cls, value, index=0):
                if not cls._cells[index].editable:
                    raise AttributeError("can't set attribute")
                else:
                    cls._cells[index].value = value

            for index in range(len(headers)):
                if headers[index].value != "":
                    setattr(
                        self.__class__,
                        headers[index].value.lower(),
                        property(
                            partial(getter, index=index),
                            partial(setter, index=index)
                        )
                    )

    """ Component editor page. """

    Version = type('Enum', (), {"OLD": 1, "NEW": 2})
    As = type('Enum', (), {"GRID": 0, "ROW": 1, "VARIABLE": 2})
    SortOrder = type('Enum', (), {"ASCENDING": 0, "DESCENDING": 1})

    inputs_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Inputs']"))
    slots_tab   = ButtonElement((By.XPATH, "div/ul/li/a[text()='Slots']"))
    outputs_tab = ButtonElement((By.XPATH, "div/ul/li/a[text()='Outputs']"))
    events_tab  = ButtonElement((By.XPATH, "div/ul/li/a[text()='Events']"))

    inputs  = GridElement((By.ID, 'Inputs_props'))
    outputs = GridElement((By.ID, 'Outputs_props'))

    inputs_filter = InputElement((By.ID, 'Inputs_variableFilter'))
    inputs_clear = ButtonElement((By.ID, 'Inputs_clear'))

    outputs_filter = InputElement((By.ID, 'Outputs_variableFilter'))
    outputs_clear = ButtonElement((By.ID, 'Outputs_clear'))

    def __init__(self, browser, port, locator, version=Version.OLD):
        super(ComponentPage, self).__init__(browser, port, locator)
        # It takes a while for the full load to complete.
        NotifierPage.wait(self)
        self.version = version
        self._sort_order = {"inputs": 0, "outputs": 0}
        self._column_picker = None

    def get_tab_labels(self):
        """ Return a list of the tab labels. """
        elements = self.root.find_elements_by_class_name('ui-tabs-anchor')
        labels = [element.text for element in elements]
        return labels

    def set_input(self, name, value):
        """ Set input `name` to `value`. """
        self('inputs_tab').click()
        grid = self.inputs
        found = []
        for row in grid.rows:
            if row[1] == name:
                row[2] = value
                return
            found.append(row[1])
        raise RuntimeError('%r not found in inputs %s' % (name, found))

    def filter_inputs(self, filter_text):
        """ Filter out input variables from grid using `filter_text`. """
        self.inputs_filter = filter_text

    def filter_outputs(self, filter_text):
        """ Filter out output variables from grid using `filter_text`. """
        self.outputs_filter = filter_text

    def clear_inputs_filter(self):
        """ Clear input variable filter. """
        self('inputs_clear').click()

    def clear_outputs_filter(self):
        """ Clear output variable filter. """
        self('outputs_clear').click()

    def _sort_column(self, grid, column_name, sort_order, tab):
        """ Sorts the variables in column `column_name`"""
        header = [header for header in grid.headers if header.value == column_name]
        if(not header):
            raise Exception("Grid has no column named %s" % column_name)

        header = header[0]
        if (sort_order == self.SortOrder.ASCENDING):
            while((self._sort_order[tab] % 2) == 0):
                self._sort_order[tab] = self._sort_order[tab] + 1
                header.click()
        else:
            while((self._sort_order[tab] % 2) != 0):
                self._sort_order[tab] = self._sort_order[tab] + 1
                header.click()

    def sort_inputs_column(self, column_name, sort_order=SortOrder.ASCENDING):
        """ Sort `column_name` in inputs grid in `sort_order` """
        self("inputs_tab").click()
        self._sort_column(self.inputs, column_name, sort_order, "inputs")

    def sort_outputs_column(self, column_name, sort_order=SortOrder.ASCENDING):
        """ Sort `column_name` in outputs grid in `sort_order` """
        self("outputs_tab").click()
        self._sort_column(self.outputs, column_name, sort_order, "outputs")

    def get_events(self):
        """ Return events grid. """
        self('events_tab').click()
        return self.events

    def show_inputs(self):
        """switch to inputs tab"""
        self('inputs_tab').click()

    def show_outputs(self):
        """switch to outputs tab"""
        self('outputs_tab').click()

    def show_slots(self):
        """switch to slots tab"""
        self('slots_tab').click()

    def toggle_column_visibility(self, column_name):
        self._toggle_column_visibility(self._active_grid, column_name)

    @property
    def _active_grid(self):
        if self.inputs.displayed:
            return self.inputs

        elif self.outputs.displayed:
            return self.outputs

    def _toggle_column_visibility(self, grid, column_name):
        if not self._column_picker:
            self._column_picker = self._get_column_picker(grid)

        elif not self._column_picker.displayed:
            self._column_picker = self._get_column_picker(grid)

        self._column_picker.get_option(column_name).click()

    def _get_column_picker(self, grid):
        grid.headers[0].context_click()
        column_pickers = [GridColumnPicker(self.browser, element)
            for element in self.browser.find_elements(By. CLASS_NAME, "slick-columnpicker")]
        for column_picker in column_pickers:
            if column_picker.displayed:
                return column_picker

    def get_inputs(self, return_type=None):
        """ Return inputs grid. """
        self('inputs_tab').click()
        return self._get_variables(self.inputs, return_type)

    def get_outputs(self, return_type=None):
        """ Return outputs grid. """
        self('outputs_tab').click()
        return self._get_variables(self.outputs, return_type)

    def get_input(self, name, return_type=None):
        """ Return first input variable with `name`. """
        self('inputs_tab').click()
        return self._get_variable(name, self.inputs, return_type)

    def get_output(self, name, return_type=None):
        """ Return first output variable with `name`. """
        self('outputs_tab').click()
        return self._get_variable(name, self.outputs, return_type)

    def _get_variables(self, grid, return_type):
        if (return_type == self.As.GRID or self.version == self.Version.OLD):
            return grid

        headers = grid.headers
        rows = grid.rows

        return [self.Variable(row, headers) for row in rows]

    def _get_variable(self, name, grid, return_type):
        found = []
        for row in grid.rows:
            if row[1] == name:
                if (return_type == self.As.ROW or self.version == self.Version.OLD):
                    return row
                else:
                    return self.Variable(row, grid.headers)
            found.append(row[1])
        raise RuntimeError('%r not found in inputs %s' % (name, found))


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

    def get_workflow_figures(self):
        """ Return workflow figure elements. """
        return find_workflow_figures(self)

    def get_workflow_component_figures(self):
        """ Return workflow component figure elements. """
        return find_workflow_component_figures(self)

    def get_workflow_figure(self, name, prefix=None, retries=5):
        """ Return :class:`WorkflowFigure` for `name`. """
        return find_workflow_figure(self, name, prefix, retries)

    def get_workflow_component_figure(self, name, prefix=None, retries=5):
        """ Return :class:`WorkflowComponentFigure` for `name`. """
        return find_workflow_component_figure(self, name, prefix, retries)


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
