""" Base class for all workflows. """

from fnmatch import fnmatch

# pylint: disable=E0611,F0401
from openmdao.main.case import Case
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudocomp import PseudoComponent

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, parent=None, scope=None, members=None):
        """Create a Workflow.

        parent: Driver (optional)
            The Driver that contains this Workflow.  This option is normally
            passed instead of scope because scope usually isn't known at
            initialization time.  If scope is not provided, it will be
            set to parent.parent, which should be the Assembly that contains
            the parent Driver.

        scope: Component (optional)
            The scope can be explicitly specified here, but this is not
            typically known at initialization time.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
        """
        self._iterator = None
        self._stop = False
        self._parent = parent
        self._scope = scope
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.

        self._rec_required = None  # Case recording configuration.
        self._rec_parameters = None
        self._rec_objectives = None
        self._rec_responses = None
        self._rec_constraints = None
        self._rec_inputs = None
        self._rec_outputs = None

        if members:
            for member in members:
                if not isinstance(member, basestring):
                    raise TypeError("Components must be added to a workflow by name.")
                self.add(member)

    @property
    def scope(self):
        """The scoping Component that is used to resolve the Component names in
        this Workflow.
        """
        if self._scope is None and self._parent is not None:
            self._scope = self._parent.get_expr_scope()
        if self._scope is None:
            raise RuntimeError("workflow has no scope!")
        return self._scope

    @scope.setter
    def scope(self, scope):
        self._scope = scope
        self.config_changed()

    @property
    def itername(self):
        return self._iterbase()

    def check_config(self):
        """Perform any checks that we need prior to run. Specific workflows
        should override this."""
        pass

    def set_initial_count(self, count):
        """
        Set initial value for execution count.  Only needed if the iteration
        coordinates must be initialized, such as for CaseIterDriverBase.

        count: int
            Initial value for workflow execution count.
        """
        self._initial_count = count - 1  # run() and step() will increment.

    def reset(self):
        """ Reset execution count. """
        self._exec_count = self._initial_count

    def run(self, ffd_order=0, case_uuid=None):
        """ Run the Components in this Workflow. """

        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0

        iterbase = self._iterbase()

        if case_uuid is None:
            # We record the case and are responsible for unique case ids.
            record_case = True
            case_uuid = Case.next_uuid()
        else:
            record_case = False

        for comp in self._iterator:
            if isinstance(comp, PseudoComponent):
                comp.run(ffd_order=ffd_order)
            else:
                self._comp_count += 1
                comp.set_itername('%s-%d' % (iterbase, self._comp_count))
                comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
            if self._stop:
                raise RunStopped('Stop requested')
        self._iterator = None

        if record_case and self._rec_required:
            self._record_case(case_uuid=case_uuid)

    def configure_recording(self, includes, excludes):
        """ Called at start of top-level run to configure case recording. """
        driver = self._parent
        scope = driver.parent
        top = scope
        while top.parent is not None:
            top = top.parent
        if not top.recorders:
            self._rec_required = False
            return

        prefix = scope.get_pathname()
        if prefix:
            prefix += '.'
        inputs = []
        outputs = []

        # Parameters
        self._rec_parameters = []
        if hasattr(driver, 'get_parameters'):
            for name, param in driver.get_parameters().items():
                if isinstance(name, tuple):
                    name = name[0]
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_parameters.append(param)
                    inputs.append(path)

        # Objectives
        self._rec_objectives = []
        if hasattr(driver, 'eval_objective'):
            key = driver.get_objectives().keys()[0]
            name = 'Objective'
            path = prefix+name
            if self._check_path(path, includes, excludes):
                self._rec_objectives.append(key)
                outputs.append(path)
        elif hasattr(driver, 'eval_objectives'):
            for j, key in enumerate(driver.get_objectives()):
                name = 'Objective_%d' % j
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_objectives.append(key)
                    outputs.append(path)

        # Responses
        self._rec_responses = []
        if hasattr(driver, 'eval_responses'):
            for j, key in enumerate(driver.get_responses()):
                name = 'Response_%d' % j
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_responses.append(key)
                    outputs.append(path)

        # Constraints
        self._rec_constraints = []
        if hasattr(driver, 'get_ineq_constraints'):
            for name, con in driver.get_ineq_constraints().items():
                name = 'Constraint ( %s )' % name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(path)

        if hasattr(driver, 'get_eq_constraints'):
            for name, con in driver.get_eq_constraints().items():
                name = 'Constraint ( %s )' % name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(path)

        # Variables
        case_inputs, case_outputs = top.get_case_variables()

        self._rec_inputs = []
        for path, value in case_inputs:
            if self._check_path(path, includes, excludes):
                self._rec_inputs.append(path)
                inputs.append(path)

        self._rec_outputs = []
        for path, value in case_outputs:
            if self._check_path(path, includes, excludes):
                self._rec_outputs.append(path)
                outputs.append(path)

        path = '%s.workflow.itername' % driver.get_pathname()
        if self._check_path(path, includes, excludes):
            self._rec_outputs.append(path)
            outputs.append(path)

        self._rec_required = bool(inputs or outputs)
        if self._rec_required:
            # Register paths in recorders.
            for recorder in top.recorders:
                recorder.register(id(self), inputs, outputs)

    @staticmethod
    def _check_path(path, includes, excludes):
        """ Return True if `path` should be recorded. """
        for pattern in includes:
            if fnmatch(path, pattern):
                break
        else:
            return False

        for pattern in excludes:
            if fnmatch(path, pattern):
                return False
        return True

    def _record_case(self, case_uuid):
        """ Record case in all recorders. """
        driver = self._parent
        scope = driver.parent
        top = scope
        while top.parent is not None:
            top = top.parent

        inputs = []
        outputs = []

        # Parameters.
        for param in self._rec_parameters:
            value = param.evaluate(scope)
            if param.size == 1:  # evaluate() always returns list.
                value = value[0]
            inputs.append(value)

        # Objectives.
        outputs.extend(driver.eval_named_objective(key)
                       for key in self._rec_objectives)
        # Responses.
        outputs.extend(driver.eval_response(key)
                       for key in self._rec_responses)
        # Constraints.
        outputs.extend(con.evaluate(scope)
                       for con in self._rec_constraints)
        # Variables.
        if self._rec_inputs or self._rec_outputs:
            case_inputs, case_outputs = top.get_case_variables()
            inputs.extend(val for name, val in case_inputs
                                            if name in self._rec_inputs)
            outputs.extend(val for name, val in case_outputs
                                             if name in self._rec_outputs)
            name = '%s.workflow.itername' % driver.get_pathname()
            if name in self._rec_outputs:
                outputs.append(self.itername)

        # Record.
        for recorder in top.recorders:
            recorder.record(id(self), inputs, outputs,
                            case_uuid, self._parent._case_uuid)

    def _iterbase(self):
        """ Return base for 'iteration coordinates'. """
        if self._parent is None:
            return str(self._exec_count)  # An unusual case.
        else:
            prefix = self._parent.get_itername()
            if prefix:
                prefix += '.'
            return '%s%d' % (prefix, self._exec_count)

    def step(self, ffd_order=0):
        """Run a single component in this Workflow."""
        if self._iterator is None:
            self._iterator = self.__iter__()
            self._exec_count += 1
            self._comp_count = 0

        comp = self._iterator.next()
        self._comp_count += 1
        iterbase = self._iterbase()
        comp.set_itername('%s-%d' % (iterbase, self._comp_count))
        try:
            comp.run(ffd_order=ffd_order)
        except StopIteration, err:
            self._iterator = None
            raise err
        raise RunStopped('Step complete')

    def stop(self):
        """
        Stop all Components in this Workflow.
        We assume it's OK to to call stop() on something that isn't running.
        """
        for comp in self.get_components(full=True):
            comp.stop()
        self._stop = True

    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the workflow by name."""
        raise NotImplementedError("This Workflow has no 'add' function")

    def config_changed(self):
        """Notifies the Workflow that workflow configuration
        (dependencies, etc.) has changed.
        """
        pass

    def remove(self, comp):
        """Remove a component from this Workflow by name."""
        raise NotImplementedError("This Workflow has no 'remove' function")

    def get_names(self, full=False):
        """Return a list of component names in this workflow."""
        raise NotImplementedError("This Workflow has no 'get_names' function")

    def get_components(self, full=False):
        """Returns a list of all component objects in the workflow. No ordering
        is assumed.
        """
        scope = self.scope
        return [getattr(scope, name) for name in self.get_names(full)]

    def __iter__(self):
        """Returns an iterator over the components in the workflow in
        some order.
        """
        raise NotImplementedError("This Workflow has no '__iter__' function")

    def __len__(self):
        raise NotImplementedError("This Workflow has no '__len__' function")
