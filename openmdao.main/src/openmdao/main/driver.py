""" Driver class definition """

#public symbols
__all__ = ["Driver"]

import fnmatch

# pylint: disable-msg=E0611,F0401

from openmdao.main.interfaces import IDriver, ICaseRecorder, IHasEvents, \
                                     implements
from openmdao.main.exceptions import RunStopped
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.component import Component
from openmdao.main.workflow import Workflow
from openmdao.main.case import Case
from openmdao.main.dataflow import Dataflow
from openmdao.main.hasevents import HasEvents
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.util.decorators import add_delegate
from openmdao.main.mp_support import is_instance, has_interface
from openmdao.main.rbac import rbac
from openmdao.main.datatypes.api import List, Slot, Str


@add_delegate(HasEvents)
class Driver(Component):
    """ A Driver iterates over a workflow of Components until some condition
    is met. """

    implements(IDriver, IHasEvents)

    recorders = List(Slot(ICaseRecorder, required=False),
                     desc='Case recorders for iteration data.')

    # Extra variables for adding to CaseRecorders
    printvars = List(Str, iotype='in',
                     desc='List of extra variables to output in the recorders.')

    # set factory here so we see a default value in the docs, even
    # though we replace it with a new Dataflow in __init__
    workflow = Slot(Workflow, allow_none=True, required=True,
                    factory=Dataflow, hidden=True)

    def __init__(self):
        self._iter = None
        super(Driver, self).__init__()
        self.workflow = Dataflow(self)
        self.force_execute = True

        # This flag is triggered by adding or removing any parameters,
        # constraints, or objectives.
        self._invalidated = False

    def _workflow_changed(self, oldwf, newwf):
        if newwf is not None:
            newwf._parent = self

    def get_expr_scope(self):
        """Return the scope to be used to evaluate ExprEvaluators."""
        return self.parent

    def _invalidate(self):
        """ Method for delegates to declare that the driver is in an invalid
        state so that isvalid() returns false. Presently, this is called when
        a constraint/objective/parameter is set, removed, or cleared.
        """
        self._invalidated = True
        self._set_exec_state('INVALID')

    def is_valid(self):
        """Return False if any Component in our workflow(s) is invalid,
        if any of our variables is invalid, or if the parameters,
        constraints, or objectives have changed.
        """
        if super(Driver, self).is_valid() is False:
            return False

        # force exection if any param, obj, or constraint has changed.
        if self._invalidated:
            return False

        # force execution if any component in the workflow is invalid
        for comp in self.workflow.get_components():
            if not comp.is_valid():
                return False
        return True

    def check_config(self):
        """Verify that our workflow is able to resolve all of its components."""

        # workflow will raise an exception if it can't resolve a Component
        super(Driver, self).check_config()
        self._update_workflow()

        self.workflow.check_config()

    def _update_workflow(self):
        """Updates workflow contents based on driver dependencies."""
        # if workflow is not defined, or if it contains only Drivers, try to
        # use parameters, objectives and/or constraint expressions to
        # determine the necessary workflow members
        try:
            iterset = set(c.name for c in self.iteration_set())
            alldrivers = all([isinstance(c, Driver)
                                for c in self.workflow.get_components()])
            if len(self.workflow) == 0:
                pass
            elif alldrivers is True:
                reqcomps = self._get_required_compnames()
                self.workflow.add([name for name in reqcomps
                                        if name not in iterset])
            # calling get_components() here just makes sure that all of the
            # components can be resolved
            comps = self.workflow.get_components()
        except Exception as err:
            self.raise_exception(str(err), type(err))

    def iteration_set(self):
        """Return a set of all Components in our workflow(s) and
        recursively in any workflow in any Driver in our workflow(s).
        """
        allcomps = set()
        if len(self.workflow) == 0:
            for compname in self._get_required_compnames():
                self.workflow.add(compname)
        for child in self.workflow.get_components():
            allcomps.add(child)
            if has_interface(child, IDriver):
                allcomps.update(child.iteration_set())
        return allcomps

    @rbac(('owner', 'user'))
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name,
        dest_comp_name) for each dependency introduced by any ExprEvaluators
        in this Driver, ignoring any dependencies on components that are
        inside of this Driver's iteration set.
        """
        iternames = set([c.name for c in self.iteration_set()])
        conn_list = super(Driver, self).get_expr_depends()
        new_list = []
        for src, dest in conn_list:
            if src not in iternames and dest not in iternames:
                new_list.append((src, dest))
        return new_list

    def _get_required_compnames(self):
        """Returns a set of names of components that are required by
        this Driver in order to evaluate parameters, objectives
        and constraints.  This list will include any intermediate
        components in the data flow between components referenced by
        parameters and those referenced by objectives and/or constraints.
        """
        setcomps = set()
        getcomps = set()

        if hasattr(self, '_delegates_'):
            for name, dclass in self._delegates_.items():
                inst = getattr(self, name)
                if isinstance(inst, HasParameters):
                    setcomps = inst.get_referenced_compnames()
                elif isinstance(inst, (HasConstraints, HasEqConstraints,
                                       HasIneqConstraints, HasObjective, HasObjectives)):
                    getcomps.update(inst.get_referenced_compnames())

        full = set(setcomps)

        if self.parent:
            graph = self.parent._depgraph
            for end in getcomps:
                for start in setcomps:
                    full.update(graph.find_all_connecting(start, end))
        return full

    def get_references(self, name):
        """Return parameter, constraint, and objective references to component
        `name` in preparation for subsequent :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        refs = {}
        if hasattr(self, '_delegates_'):
            for dname, dclass in self._delegates_.items():
                inst = getattr(self, dname)
                if isinstance(inst, (HasParameters, HasConstraints,
                                     HasEqConstraints, HasIneqConstraints,
                                     HasObjective, HasObjectives)):
                    refs[inst] = inst.get_references(name)
        return refs

    def remove_references(self, name):
        """Remove parameter, constraint, and objective references to component
        `name`.

        name: string
            Name of component being removed.
        """
        if hasattr(self, '_delegates_'):
            for dname, dclass in self._delegates_.items():
                inst = getattr(self, dname)
                if isinstance(inst, (HasParameters, HasConstraints,
                                     HasEqConstraints, HasIneqConstraints,
                                     HasObjective, HasObjectives)):
                    inst.remove_references(name)

    def restore_references(self, refs, name):
        """Restore parameter, constraint, and objective references to component
        `name` from `refs`.

        name: string
            Name of component being removed.

        refs: object
            Value returned by :meth:`get_references`.
        """
        if hasattr(self, '_delegates_'):
            for dname, dclass in self._delegates_.items():
                inst = getattr(self, dname)
                if isinstance(inst, (HasParameters, HasConstraints,
                                     HasEqConstraints, HasIneqConstraints,
                                     HasObjective, HasObjectives)):
                    inst.restore_references(refs[inst], name)

    @rbac('*', 'owner')
    def run(self, force=False, ffd_order=0, case_id=''):
        """Run this object. This should include fetching input variables if necessary,
        executing, and updating output variables. Do not override this function.

        force: bool
            If True, force component to execute even if inputs have not
            changed. (Default is False)

        ffd_order: int
            Order of the derivatives to be used when finite differencing (1 for first
            derivatives, 2 for second derivativse). During regular execution,
            ffd_order should be 0. (Default is 0)

        case_id: str
            Identifier for the Case that is associated with this run. (Default is '')
            If applied to the top-level assembly, this will be prepended to
            all iteration coordinates.
        """

        for recorder in self.recorders:
            recorder.startup()

        # Override just to reset the workflow :-(
        self.workflow.reset()
        super(Driver, self).run(force, ffd_order, case_id)
        self._invalidated = False

    def execute(self):
        """ Iterate over a workflow of Components until some condition
        is met. If you don't want to structure your driver to use *pre_iteration*,
        *post_iteration*, etc., just override this function. As a result, none
        of the ``<start/pre/post/continue>_iteration()`` functions will be called.
        """
        self._iter = None
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()

    def step(self):
        """Similar to the 'execute' function, but this one only
        executes a single Component from the workflow each time
        it's called.
        """
        if self._iter is None:
            self.start_iteration()
            self._iter = self._step()
        try:
            self._iter.next()
        except StopIteration:
            self._iter = None
            raise
        raise RunStopped('Step complete')

    def _step(self):
        while self.continue_iteration():
            self.pre_iteration()
            for junk in self._step_workflow():
                yield
            self.post_iteration()
        self._iter = None
        raise StopIteration()

    def _step_workflow(self):
        while True:
            try:
                self.workflow.step()
            except RunStopped:
                pass
            yield

    def stop(self):
        self._stop = True
        self.workflow.stop()

    def start_iteration(self):
        """Called just prior to the beginning of an iteration loop. This can
        be overridden by inherited classes. It can be used to perform any
        necessary pre-iteration initialization.
        """
        self._continue = True

    def continue_iteration(self):
        """Return False to stop iterating."""
        return self._continue

    def pre_iteration(self):
        """Called prior to each iteration.  This is where iteration events are set."""
        self.set_events()

    def run_iteration(self):
        """Runs workflow."""
        wf = self.workflow
        if len(wf) == 0:
            self._logger.warning("'%s': workflow is empty!" % self.get_pathname())
        wf.run(ffd_order=self.ffd_order, case_id=self._case_id)

    def calc_derivatives(self, first=False, second=False, savebase=False):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""
        self.workflow.calc_derivatives(first, second, savebase)

    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """ Check derivatives for all components in this workflow."""
        self.workflow.check_derivatives(order, driver_inputs, driver_outputs)

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration

    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed or dependencies may have
        changed.
        """
        super(Driver, self).config_changed(update_parent)
        if self.workflow is not None:
            self.workflow.config_changed()

    def record_case(self):
        """ A driver can call this function to record the current state of the
        current iteration as a Case into all slotted case recorders. Generally,
        the driver should call this function once per iteration and may also
        need to call it at the conclusion.

        All parameters, objectives, and constraints are included in the Case
        output, along with all extra variables listed in self.printvars.
        """

        if not self.recorders:
            return

        case_input = []
        case_output = []
        iotypes = {}

        # Parameters
        if hasattr(self, 'get_parameters'):
            for name, param in self.get_parameters().iteritems():
                if isinstance(name, tuple):
                    name = name[0]
                case_input.append([name, param.evaluate(self.parent)])
                iotypes[name] = 'in'

        # Objectives
        if hasattr(self, 'eval_objective'):
            case_output.append(["Objective", self.eval_objective()])

        # Constraints
        if hasattr(self, 'get_ineq_constraints'):
            for name, con in self.get_ineq_constraints().iteritems():
                val = con.evaluate(self.parent)
                if '>' in val[2]:
                    case_output.append(["Constraint ( %s )" % name,
                                                              val[0] - val[1]])
                else:
                    case_output.append(["Constraint ( %s )" % name,
                                                              val[1] - val[0]])

        if hasattr(self, 'get_eq_constraints'):
            for name, con in self.get_eq_constraints().iteritems():
                val = con.evaluate(self.parent)
                case_output.append(["Constraint ( %s )" % name, val[1] - val[0]])

        tmp_printvars = self.printvars[:]
        tmp_printvars.append('%s.workflow.itername' % self.name)
        iotypes[tmp_printvars[-1]] = 'out'

        # Additional user-requested variables
        for printvar in tmp_printvars:

            if '*' in printvar:
                printvars = self._get_all_varpaths(printvar)
            else:
                printvars = [printvar]

            for var in printvars:
                iotype = iotypes.get(var)
                if iotype is None:
                    iotype = self.parent.get_metadata(var, 'iotype')
                    iotypes[var] = iotype
                if iotype == 'in':
                    val = ExprEvaluator(var, scope=self.parent).evaluate()
                    case_input.append([var, val])
                elif iotype == 'out':
                    val = ExprEvaluator(var, scope=self.parent).evaluate()
                    case_output.append([var, val])
                else:
                    msg = "%s is not an input or output" % var
                    self.raise_exception(msg, ValueError)

        case = Case(case_input, case_output, parent_uuid=self._case_id)

        for recorder in self.recorders:
            recorder.record(case)

    def _get_all_varpaths(self, pattern, header=''):
        ''' Return a list of all varpaths in the driver's workflow that
        match the specified pattern.

        Used by record_case.'''

        # assume we don't want this in driver's imports
        from openmdao.main.assembly import Assembly

        # Start with our driver's settings
        all_vars = []
        for var in self.list_vars():
            all_vars.append('%s.%s' % (self.name, var))

        for comp in self.workflow.__iter__():

            # All variables from components in workflow
            for var in comp.list_vars():
                all_vars.append('%s%s.%s' % (header, comp.name, var))

            # Recurse into assemblys
            if isinstance(comp, Assembly):

                assy_header = '%s%s.' % (header, comp.name)
                assy_vars = comp.driver._get_all_varpaths(pattern, assy_header)
                all_vars = all_vars + assy_vars

        # Match pattern in our var names
        matched_vars = []
        if pattern == '*':
            matched_vars = all_vars
        else:
            matched_vars = fnmatch.filter(all_vars, pattern)

        return matched_vars

    def get_workflow(self):
        """ Get the driver info and the list of components that make up the
            driver's workflow; recurse on nested drivers.
        """
        from openmdao.main.assembly import Assembly
        ret = {}
        ret['pathname'] = self.get_pathname()
        ret['type'] = type(self).__module__ + '.' + type(self).__name__
        ret['workflow'] = []
        ret['valid'] = self.is_valid()
        for comp in self.workflow:
            pathname = comp.get_pathname()
            if is_instance(comp, Assembly) and comp.driver:
                ret['workflow'].append({
                    'pathname': pathname,
                    'type':     type(comp).__module__ + '.' + type(comp).__name__,
                    'driver':   comp.driver.get_workflow(),
                    'valid':    comp.is_valid()
                  })
            elif is_instance(comp, Driver):
                ret['workflow'].append(comp.get_workflow())
            else:
                ret['workflow'].append({
                    'pathname': pathname,
                    'type':     type(comp).__module__ + '.' + type(comp).__name__,
                    'valid':    comp.is_valid()
                  })
        return ret


class Run_Once(Driver):
    """An assembly starts with a bare driver that just executes the workflow
    a single time. The only difference between this and the Driver base class
    is that `record_case` is called at the conclusion of the workflow execution.
    """

    def execute(self):
        ''' Call parent, then record cases.'''

        super(Run_Once, self).execute()
        self.record_case()
