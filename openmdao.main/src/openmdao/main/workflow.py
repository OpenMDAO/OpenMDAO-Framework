""" Base class for all workflows. """

from fnmatch import fnmatch
from traceback import format_exc

# pylint: disable=E0611,F0401
from openmdao.main.case import Case
from openmdao.main.depgraph import _get_inner_connections
from openmdao.main.exceptions import RunStopped, TracedError
from openmdao.main.pseudocomp import PseudoComponent

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, parent, members=None):
        """Create a Workflow.

        parent: Driver
            The Driver that contains this Workflow.  This option is normally
            passed instead of scope because scope usually isn't known at
            initialization time.  If scope is not provided, it will be
            set to parent.parent, which should be the Assembly that contains
            the parent Driver.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
        """
        self._stop = False
        self._parent = parent
        self._scope = None
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._var_graph = None

        self._rec_required = None  # Case recording configuration.
        self._rec_parameters = None
        self._rec_objectives = None
        self._rec_responses = None
        self._rec_constraints = None
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

    def check_config(self, strict=False):
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
        self._exec_count += 1

        iterbase = self._iterbase()

        if case_uuid is None:
            # We record the case and are responsible for unique case ids.
            record_case = True
            case_uuid = Case.next_uuid()
        else:
            record_case = False

        err = None
        scope = self.scope
        try:
            for comp in self:
                # before the workflow runs each component, update that
                # component's inputs based on the graph
                scope.update_inputs(comp.name, graph=self._var_graph)
                if isinstance(comp, PseudoComponent):
                    comp.run(ffd_order=ffd_order)
                else:
                    comp.set_itername('%s-%s' % (iterbase, comp.name))
                    comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
                if self._stop:
                    raise RunStopped('Stop requested')
        except Exception as exc:
            err = TracedError(exc, format_exc())

        if record_case and self._rec_required:
            self._record_case(case_uuid, err)

        if err is not None:
            err.reraise(with_traceback=False)

    def configure_recording(self, includes, excludes):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""
        driver = self._parent
        scope = driver.parent
        top = scope
        while top.parent is not None:
            top = top.parent
        if not top.recorders:
            self._rec_required = False
            return (set(), dict())

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
                    inputs.append(name)

        # Objectives
        self._rec_objectives = []
        if hasattr(driver, 'eval_objective'):
            key = driver.get_objectives().keys()[0]
            name = 'Objective'
            path = prefix+name
            if self._check_path(path, includes, excludes):
                self._rec_objectives.append(key)
                outputs.append(name)
        elif hasattr(driver, 'eval_objectives'):
            for j, key in enumerate(driver.get_objectives()):
                name = 'Objective_%d' % j
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_objectives.append(key)
                    outputs.append(name)

        # Responses
        self._rec_responses = []
        if hasattr(driver, 'eval_responses'):
            for j, key in enumerate(driver.get_responses()):
                name = 'Response_%d' % j
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_responses.append(key)
                    outputs.append(name)

        # Constraints
        self._rec_constraints = []
        if hasattr(driver, 'get_ineq_constraints'):
            for name, con in driver.get_ineq_constraints().items():
                name = 'Constraint ( %s )' % name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(name)

        if hasattr(driver, 'get_eq_constraints'):
            for name, con in driver.get_eq_constraints().items():
                name = 'Constraint ( %s )' % name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(name)

        # Other outputs.
        self._rec_outputs = []
        srcs = scope.list_inputs()
        if hasattr(driver, 'get_parameters'):
            srcs.extend(param.target
                        for param in driver.get_parameters().values())
        dsts = scope.list_outputs()
        if hasattr(driver, 'get_objectives'):
            dsts.extend(objective.pcomp_name+'.out0'
                        for objective in driver.get_objectives().values())
        if hasattr(driver, 'get_responses'):
            dsts.extend(response.pcomp_name+'.out0'
                        for response in driver.get_responses().values())
        if hasattr(driver, 'get_ineq_constraints'):
            dsts.extend(constraint.pcomp_name+'.out0'
                        for constraint in driver.get_ineq_constraints().values())
        if hasattr(driver, 'get_eq_constraints'):
            dsts.extend(constraint.pcomp_name+'.out0'
                        for constraint in driver.get_eq_constraints().values())
        for src, dst in _get_inner_connections(scope._depgraph, srcs, dsts):
            path = prefix+src
            if src not in inputs and src not in outputs and \
               self._check_path(path, includes, excludes):
                self._rec_outputs.append(src)
                outputs.append(src)

        name = '%s.workflow.itername' % driver.name
        path = prefix+name
        if self._check_path(path, includes, excludes):
            self._rec_outputs.append(name)
            outputs.append(name)

        # If recording required, register names in recorders.
        self._rec_required = bool(inputs or outputs)
        if self._rec_required:
            for recorder in top.recorders:
                recorder.register(driver, inputs, outputs)

        return (set(prefix+name for name in inputs), dict())

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

    def _record_case(self, case_uuid, err):
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
            try:
                value = param.evaluate(scope)
            except Exception as exc:
                driver.raise_exception("Can't evaluate '%s' for recording: %s"
                                       % (param, exc), RuntimeError)

            if param.size == 1:  # evaluate() always returns list.
                value = value[0]
            inputs.append(value)

        # Objectives.
        for key in self._rec_objectives:
            try:
                outputs.append(driver.eval_named_objective(key))
            except Exception as exc:
                driver.raise_exception("Can't evaluate '%s' for recording: %s"
                                       % (key, exc), RuntimeError)
        # Responses.
        for key in self._rec_responses:
            try:
                outputs.append(driver.eval_response(key))
            except Exception as exc:
                driver.raise_exception("Can't evaluate '%s' for recording: %s"
                                       % (key, exc), RuntimeError)
        # Constraints.
        for con in self._rec_constraints:
            try:
                value = con.evaluate(scope)
            except Exception as exc:
                driver.raise_exception("Can't evaluate '%s' for recording: %s"
                                       % (con, exc), RuntimeError)
            if len(value) == 1:  # evaluate() always returns list.
                value = value[0]
            outputs.append(value)

        # Other outputs.
        for name in self._rec_outputs:
            try:
                outputs.append(scope.get(name))
            except Exception as exc:
                scope.raise_exception("Can't get '%s' for recording: %s"
                                      % (name, exc), RuntimeError)
        # Record.
        for recorder in top.recorders:
            recorder.record(driver, inputs, outputs, err,
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
        self._var_graph = None

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
