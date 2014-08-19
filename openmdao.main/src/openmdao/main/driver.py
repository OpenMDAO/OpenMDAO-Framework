""" Driver class definition """

#public symbols
__all__ = ["Driver"]

from zope.interface import implementedBy

# pylint: disable=E0611,F0401

from openmdao.main.mpiwrap import MPI, mpiprint
from openmdao.main.component import Component
from openmdao.main.dataflow import Dataflow
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Slot, \
                                        List, VarTree
from openmdao.main.depgraph import find_all_connecting, \
                                   collapse_driver, get_reduced_subgraph
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

    directional_fd = Bool(False, desc="Set to True to do a directional "
                                       "finite difference for each GMRES "
                                       "iteration instead of pre-computing "
                                       "the full fd space.",
                                       framework_var=True)

    fd_blocks = List([], desc="List of lists that contain comps which "
                              "should be finite-differenced together.",
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
                                desc="Direction for derivative calculation. "
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
        self._required_compnames = None

        # clean up unwanted trait from Component
        self.remove_trait('missing_deriv_policy')

    def __deepcopy__(self, memo):
        """For some reason `missing_deriv_policy` gets resurrected."""
        result = super(Driver, self).__deepcopy__(memo)
        result.remove_trait('missing_deriv_policy')
        return result

    def _workflow_changed(self, oldwf, newwf):
        """callback when new workflow is slotted"""
        if newwf is not None:
            newwf.parent = self

    def get_expr_scope(self):
        """Return the scope to be used to evaluate ExprEvaluators."""
        return self.parent

    def _collapse_subdrivers(self, g):
        """collapse subdriver iteration sets into single nodes."""
        # collapse all subdrivers in our graph
        wfnames = set(self.workflow.get_names(full=True))
        for child_drv in self.subdrivers(recurse=False):
            collapse_driver(g, child_drv, wfnames)

    def get_depgraph(self):
        return self.parent._depgraph  # May change this to use a smaller graph later

    def get_reduced_graph(self):
        nodes = [c.name for c in self.iteration_set()]
        nodes.append(self.name)
        return get_reduced_subgraph(self.parent.get_reduced_graph(), nodes)

    def check_config(self, strict=False):
        """Verify that our workflow is able to resolve all of its components."""

        # workflow will raise an exception if it can't resolve a Component
        super(Driver, self).check_config(strict=strict)
        self.workflow.check_config(strict=strict)

    @rbac(('owner', 'user'))
    def get_itername(self):
        """Return current 'iteration coordinates'."""
        if self.parent._top_driver is self:
            return self.parent.get_itername()

        return self.itername

    def iteration_set(self, solver_only=False):
        """Return a set of all Components in our workflow and
        recursively in any workflow in any Driver in our workflow.

        solver_only: Bool
            Only recurse into solver drivers. These are the only kinds
            of drivers whose derivatives get absorbed into the parent
            driver's graph.
        """
        allcomps = set()
        for child in self.workflow:
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
        deps = []
        for src, dest in super(Driver, self).get_expr_depends():
            if src not in iternames and dest not in iternames:
                deps.add((src, dest))
        return list(deps)

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
            for dname in self._delegates_:
                delegate = getattr(self, dname)
                if isinstance(delegate, HasParameters):
                    destset.update(delegate.get_referenced_varpaths(refs=True))
                elif isinstance(delegate, (HasConstraints,
                                     HasEqConstraints, HasIneqConstraints)):
                    srcset.update(delegate.list_constraint_targets())
                elif isinstance(delegate,
                                 (HasObjective, HasObjectives)):
                    srcset.update(delegate.list_objective_targets())

            if recurse:
                for sub in self.subdrivers(recurse=True):
                    srcs, dests = sub.get_expr_var_depends(recurse=True)
                    srcset.update(srcs)
                    destset.update(dests)

        return srcset, destset

    @rbac(('owner', 'user'))
    def subdrivers(self, recurse=False):
        """Returns a generator of all subdrivers
        contained in this driver's workflow.  If recurse is True,
        include all subdrivers in our entire iteration set.
        """
        if recurse:
            itercomps = self.iteration_set()
        else:
            itercomps = list(self.workflow)

        for comp in itercomps:
            if has_interface(comp, IDriver):
                yield comp

    def _get_required_compnames(self):
        """Returns a set of names of components that are required by
        this Driver in order to evaluate parameters, objectives
        and constraints.  This list will include any intermediate
        components in the data flow between components referenced by
        parameters and those referenced by objectives and/or constraints.
        """
        if self._required_compnames is None:
            # call base class version of get_expr_depends so we don't filter out
            # comps in our iterset.  We want required names to be everything between
            # and including comps that we reference in any parameter, objective, or
            # constraint.
            conns = super(Driver, self).get_expr_depends()
            
            getcomps = set([u for u, v in conns if u != self.name])
            setcomps = set([v for u, v in conns if v != self.name])

            full = set(setcomps)
            full.update(getcomps)
            full.update(self.list_pseudocomps())

            compgraph = self.get_depgraph().component_graph()

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
            for name in self._delegates_:
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
            for dname in self._delegates_:
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
            for dname in self._delegates_:
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
    def run(self, force=False, ffd_order=0, case_uuid=''):
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

        case_uuid: str
            Identifier for the Case that is associated with this run.
        """
        # (Re)configure parameters.
        if hasattr(self, 'config_parameters'):
            self.config_parameters()

        # force param pseudocomps to get updated values to start
        # KTM1 - probably don't need this anymore
        self.update_parameters()
        
        # Reset the workflow.
        self.workflow.reset()
        super(Driver, self).run(ffd_order, case_uuid)

    @rbac(('owner', 'user'))
    def configure_recording(self, includes, excludes):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""
        return self.workflow.configure_recording(includes, excludes)

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
        self.end_iteration()

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

    def end_iteration(self):
        """Called at the end of the iteraton loop.  Override this in 
        inherited classes to perform some action after iteration is complete.
        """
        pass

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

        wf.run(ffd_order=self.ffd_order)

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
        self._depgraph = None
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
                })
        return ret

    def _get_param_constraint_pairs(self):
        """Returns a list of tuples of the form (param, constraint)."""
        pairs = []
        if hasattr(self, 'list_param_group_targets'):
            pgroups = self.list_param_group_targets()
            for key, cnst in self.get_eq_constraints().iteritems():
                for params in pgroups:
                    if params[0] == cnst.rhs.text:
                        pairs.append((params[0], cnst.pcomp_name+'.out0'))
                    elif params[0] == cnst.lhs.text:
                        pairs.append((params[0], cnst.pcomp_name+'.out0'))
        return pairs

    @rbac(('owner', 'user'))
    def setup_systems(self):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.workflow.setup_systems()

    #### MPI related methods ####

    @rbac(('owner', 'user'))
    def get_req_cpus(self):
        """Return requested_cpus."""
        #mpiprint("driver %s reports %s cpus" % (self.name,self.workflow.get_req_cpus()))
        return self.workflow.get_req_cpus()

    def setup_communicators(self, comm):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.workflow.setup_communicators(comm)

    def setup_scatters(self):
        self.workflow.setup_scatters()

    @rbac(('owner', 'user'))
    def get_full_nodeset(self):
        """Return the full set of nodes in the depgraph
        belonging to this driver (includes full iteration set).
        """
        names = super(Driver, self).get_full_nodeset()
        names.update(self.workflow.get_full_nodeset())
        return names

    def get_iteration_tree(self):
        """Return a list of lists indicating the iteration
        hierarchy for this driver.  This recurses down into
        sub-Drivers but NOT into sub-Assemblies.
        """
        tree = [self.get_pathname(), []]
        for comp in self.workflow:
            if IDriver.providedBy(comp):
                tree[1].append(comp.get_iteration_tree())
            else:
                tree[1].append(comp.get_pathname())
        return tree
