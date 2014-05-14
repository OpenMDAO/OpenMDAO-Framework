""" Base class for all workflows. """

# pylint: disable-msg=E0611,F0401
from openmdao.main.case import Case
from openmdao.main.exceptions import RunStopped
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

    def run(self, ffd_order=0, case_label='', case_uuid=None):
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

        scope = self.scope

        for comp in self:
            # before the workflow runs each component, update that
            # component's inputs based on the graph
            scope.update_inputs(comp.name)
            if isinstance(comp, PseudoComponent):
                comp.run(ffd_order=ffd_order)
            else:
                comp.set_itername('%s-%s' % (iterbase, comp.name))
                comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
            if self._stop:
                raise RunStopped('Stop requested')

        if record_case:
            self._record_case(label=case_label, case_uuid=case_uuid)

    def _record_case(self, label, case_uuid):
        """ Record case in all recorders. """
        top = self._parent
        while top.parent is not None:
            top = top.parent
        recorders = top.recorders
        if not recorders:
            return

        inputs = []
        outputs = []
        driver = self._parent
        scope = driver.parent

        # Parameters
        if hasattr(driver, 'get_parameters'):
            for name, param in driver.get_parameters().iteritems():
                if isinstance(name, tuple):
                    name = name[0]
                value = param.evaluate(scope)
                if param.size == 1:  # evaluate() always returns list.
                    value = value[0]
                inputs.append((name, value))

        # Objectives
        if hasattr(driver, 'eval_objective'):
            outputs.append(('Objective', driver.eval_objective()))
        elif hasattr(driver, 'eval_objectives'):
            for j, obj in enumerate(driver.eval_objectives()):
                outputs.append(('Objective_%d' % j, obj))

        # Responses
        if hasattr(driver, 'eval_responses'):
            for j, response in enumerate(driver.eval_responses()):
                outputs.append(("Response_%d" % j, response))

        # Constraints
        if hasattr(driver, 'get_ineq_constraints'):
            for name, con in driver.get_ineq_constraints().iteritems():
                val = con.evaluate(scope)
                outputs.append(('Constraint ( %s )' % name, val))

        if hasattr(driver, 'get_eq_constraints'):
            for name, con in driver.get_eq_constraints().iteritems():
                val = con.evaluate(scope)
                outputs.append(('Constraint ( %s )' % name, val))

        # Other
        case_inputs, case_outputs = top.get_case_variables()
        inputs.extend(case_inputs)
        outputs.extend(case_outputs)
        outputs.append(('%s.workflow.itername' % driver.get_pathname(),
                        self.itername))

        case = Case(inputs, outputs, label=label,
                    case_uuid=case_uuid, parent_uuid=self._parent._case_uuid)

        for recorder in recorders:
            recorder.record(case)

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
