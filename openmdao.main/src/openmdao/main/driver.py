""" Driver class definition """

#public symbols
__all__ = ["Driver"]

import fnmatch

from zope.interface import implementedBy

# pylint: disable-msg=E0611,F0401

from openmdao.main.component import Component
from openmdao.main.dataflow import Dataflow
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Slot, VarTree
from openmdao.main.depgraph import find_all_connecting
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasevents import HasEvents
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.main.interfaces import IDriver, IHasEvents, ISolver, \
                                     implements
from openmdao.main.mp_support import is_instance, has_interface
from openmdao.main.rbac import rbac
from openmdao.main.vartree import VariableTree
from openmdao.main.workflow import Workflow
from openmdao.util.decorators import add_delegate


class GradientOptions(VariableTree):
    ''' Options for calculation of the gradient by the driver's workflow. '''

    # Finite Difference
    fd_form = Enum('forward', ['forward', 'backward', 'central', 'complex_step'],
                   desc="Finite difference mode. (forward, backward, central) "
                   "You can also set to 'complex_step' to peform the complex "
                   "step method if your components support it.",
                   framework_var=True)
    fd_step = Float(1.0e-6, desc='Default finite difference stepsize',
                    framework_var=True)
    fd_step_type = Enum('absolute',
                        ['absolute', 'relative', 'bounds_scaled'],
                        desc='Set to absolute, relative, '
                        'or scaled to the bounds (high-low) step sizes',
                        framework_var=True)

    force_fd = Bool(False, desc="Set to True to force finite difference "
                                "of this driver's entire workflow in a"
                                "single block.",
                           framework_var=True)

    # KTM - story up for this one.
    #fd_blocks = List([], desc='User can specify nondifferentiable blocks '
    #                          'by adding sets of component names.')

    # Analytic solution with GMRES
    gmres_tolerance = Float(1.0e-9, desc='Tolerance for GMRES',
                            framework_var=True)
    gmres_maxiter = Int(100, desc='Maximum number of iterations for GMRES',
                        framework_var=True)
    derivative_direction = Enum('auto',
                                ['auto', 'forward', 'adjoint'],
                                desc = "Direction for derivative calculation. "
                                "Can be 'forward', 'adjoint', or 'auto'. "
                                "Auto is the default setting. "
                                "When set to auto, OpenMDAO automatically "
                                "figures out the best direction based on the "
                                "number of parameters and responses. "
                                "When the number of parameters and responses "
                                "are equal, then forward direction is used.",
                                framework_var=True)

@add_delegate(HasEvents)
class Driver(Component):
    """ A Driver iterates over a workflow of Components until some condition
    is met. """

    implements(IDriver, IHasEvents)

    # set factory here so we see a default value in the docs, even
    # though we replace it with a new Dataflow in __init__
    workflow = Slot(Workflow, allow_none=True, required=True,
                    factory=Dataflow, hidden=True)

    gradient_options = VarTree(GradientOptions(), iotype='in',
                               framework_var=True)

    def __init__(self):
        self._iter = None
        super(Driver, self).__init__()

        self.workflow = Dataflow(self)
        self.force_execute = True

        self._required_compnames = None

        # This flag is triggered by adding or removing any parameters,
        # constraints, or objectives.
        self._invalidated = False

        # clean up unwanted trait from Component
        self.remove_trait('missing_deriv_policy')

        self._evaluators = {}  # Used to evaluate variables to e recorded.

    def _workflow_changed(self, oldwf, newwf):
        """callback when new workflow is slotted"""
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
        self.workflow.check_config()

    def iteration_set(self, solver_only=False):
        """Return a set of all Components in our workflow and
        recursively in any workflow in any Driver in our workflow.

        solver_only: Bool
            Only recurse into solver drivers. These are the only kinds
            of drivers whose derivatives get absorbed into the parent
            driver's graph.
        """
        allcomps = set()
        for child in self.workflow.get_components(full=True):
            allcomps.add(child)
            if has_interface(child, IDriver):
                if solver_only and not has_interface(child, ISolver):
                    continue
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

    @rbac(('owner', 'user'))
    def get_expr_var_depends(self, recurse=True):
        """Returns a tuple of sets of the form (src_set, dest_set)
        containing all dependencies introduced by any parameters,
        objectives, or constraints in this Driver.  If recurse is True,
        include any refs from subdrivers.
        """
        srcset = set()
        destset = set()
        if hasattr(self, '_delegates_'):
            for dname, dclass in self._delegates_.items():
                delegate = getattr(self, dname)
                if isinstance(delegate, HasParameters):
                    destset.update(delegate.get_referenced_varpaths())
                elif isinstance(delegate, (HasConstraints,
                                     HasEqConstraints, HasIneqConstraints,
                                     HasObjective, HasObjectives)):
                    srcset.update(delegate.get_referenced_varpaths())

            if recurse:
                for sub in self.subdrivers():
                    srcs, dests = sub.get_expr_var_depends(recurse)
                    srcset.update(srcs)
                    destset.update(dests)

        return srcset, destset

    @rbac(('owner', 'user'))
    def subdrivers(self):
        """Returns a generator of of subdrivers of this driver."""
        for d in self.iteration_set():
            if has_interface(d, IDriver):
                yield d

    def _get_required_compnames(self):
        """Returns a set of names of components that are required by
        this Driver in order to evaluate parameters, objectives
        and constraints.  This list will include any intermediate
        components in the data flow between components referenced by
        parameters and those referenced by objectives and/or constraints.
        """
        if self._required_compnames is None:
            conns = super(Driver, self).get_expr_depends()
            getcomps = set([u for u, v in conns if u != self.name])
            setcomps = set([v for u, v in conns if v != self.name])

            full = set(setcomps)

            compgraph = self.parent._depgraph.component_graph()

            for end in getcomps:
                for start in setcomps:
                    full.update(find_all_connecting(compgraph, start, end))

            self._required_compnames = full

        return self._required_compnames

    @rbac(('owner', 'user'))
    def list_pseudocomps(self):
        """Return a list of names of pseudocomps resulting from
        our objectives, and constraints.
        """
        pcomps = []
        if hasattr(self, '_delegates_'):
            for name, dclass in self._delegates_.items():
                delegate = getattr(self, name)
                if hasattr(delegate, 'list_pseudocomps'):
                    pcomps.extend(delegate.list_pseudocomps())
        return pcomps

    def get_references(self, name):
        """Return a dict of parameter, constraint, and objective
        references to component `name` in preparation for
        subsequent :meth:`restore_references` call.

        name: string
            Name of component being referenced.
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
        """Remove parameter, constraint, objective  and workflow
        references to component `name`.

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
        self.workflow.remove(name)

    def restore_references(self, refs):
        """Restore parameter, constraint, and objective references to component
        `name` from `refs`.

        refs: object
            Value returned by :meth:`get_references`.
        """
        for inst, inst_refs in refs.items():
            inst.restore_references(inst_refs)

    @rbac('*', 'owner')
    def run(self, force=False, ffd_order=0, case_id='', case_uuid=''):
        """Run this object. This should include fetching input variables if
        necessary, executing, and updating output variables. Do not override
        this function.

        force: bool
            If True, force component to execute even if inputs have not
            changed. (Default is False)

        ffd_order: int
            Order of the derivatives to be used when finite differencing (1
            for first derivatives, 2 for second derivatives). During regular
            execution, ffd_order should be 0. (Default is 0)

        case_id: str
            Identifier for the Case that is associated with this run.
            If applied to the top-level assembly, this will be prepended to
            all iteration coordinates. (Default is '')
        """

        # (Re)configure parameters.
        if hasattr(self, 'config_parameters'):
            self.config_parameters()

        # force param pseudocomps to get updated values to start
        # KTM1 - probably don't need this anymore
        self.update_parameters()

        # Reset the workflow.
        self.workflow.reset()
        super(Driver, self).run(force, ffd_order, case_id, case_uuid)
        self._invalidated = False

    def update_parameters(self):
        if hasattr(self, 'get_parameters'):
            for param in self.get_parameters().values():
                param.initialize(self.get_expr_scope())

    def execute(self):
        """ Iterate over a workflow of Components until some condition
        is met. If you don't want to structure your driver to use
        *pre_iteration*, *post_iteration*, etc., just override this function.
        As a result, none of the ``<start/pre/post/continue>_iteration()``
        functions will be called.
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
        '''Step through a single workflow comp and then return control'''
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
        """Stop the workflow."""
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
        """Called prior to each iteration.
        This is where iteration events are set."""
        self.set_events()

    def run_iteration(self):
        """Runs workflow."""
        wf = self.workflow
        if len(wf) == 0:
            self._logger.warning("'%s': workflow is empty!"
                                 % self.get_pathname())

        wf.run(ffd_order=self.ffd_order, case_id=self._case_id)

    def calc_derivatives(self, first=False, second=False, savebase=False,
                         required_inputs=None, required_outputs=None):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""
        self.workflow.calc_derivatives(first, second, savebase,
                                       required_inputs, required_outputs)

    def calc_gradient(self, inputs=None, outputs=None):
        """Returns the gradient of the passed outputs with respect to
        all passed inputs. The basic driver behavior is to call calc_gradient
        on its workflow. However, some driver (optimizers in particular) may
        want to define their own behavior.
        """
        return self.workflow.calc_gradient(inputs, outputs, upscope=True)

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration

    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed or dependencies may have
        changed.
        """
        super(Driver, self).config_changed(update_parent)
        self._required_compnames = None
        self._invalidate()
        if self.workflow is not None:
            self.workflow.config_changed()

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
        comps = [comp for comp in self.workflow]
        for comp in comps:

            # Skip pseudo-comps
            if hasattr(comp, '_pseudo_type'):
                continue

            pathname = comp.get_pathname()
            if is_instance(comp, Assembly) and comp.driver:
                inames = [cls.__name__
                          for cls in list(implementedBy(comp.__class__))]
                ret['workflow'].append({
                    'pathname':   pathname,
                    'type':       type(comp).__module__ + '.' + type(comp).__name__,
                    'interfaces': inames,
                    'driver':     comp.driver.get_workflow(),
                    'valid':      comp.is_valid()
                })
            elif is_instance(comp, Driver):
                ret['workflow'].append(comp.get_workflow())
            else:
                inames = [cls.__name__
                          for cls in list(implementedBy(comp.__class__))]
                ret['workflow'].append({
                    'pathname':   pathname,
                    'type':       type(comp).__module__ + '.' + type(comp).__name__,
                    'interfaces': inames,
                    'valid':      comp.is_valid()
                })
        return ret

