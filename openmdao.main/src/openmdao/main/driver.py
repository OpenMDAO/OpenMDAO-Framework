""" Driver class definition """

#public symbols
__all__ = ["Driver"]

# pylint: disable=E0611,F0401

from networkx.algorithms.components import strongly_connected_components

from openmdao.main.mpiwrap import PETSc
from openmdao.main.component import Component
from openmdao.main.dataflow import Dataflow
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Slot, \
                                        List, VarTree
from openmdao.main.depgraph import find_all_connecting, \
                                   collapse_driver
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasevents import HasEvents
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasresponses import HasResponses
from openmdao.main.interfaces import IDriver, IHasEvents, ISolver, \
                                     implements
from openmdao.main.mp_support import has_interface
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

    derivative_direction = Enum('auto',
                                ['auto', 'forward', 'adjoint'],
                                desc="Direction for derivative calculation. "
                                "Can be 'forward', 'adjoint', or 'auto'. "
                                "'auto' is the default setting. "
                                "When set to auto, OpenMDAO automatically "
                                "figures out the best direction based on the "
                                "number of parameters and responses. "
                                "When the number of parameters and responses "
                                "are equal, then forward direction is used.",
                                framework_var=True)

    # KTM - story up for this one.
    #fd_blocks = List([], desc='User can specify nondifferentiable blocks '
    #                          'by adding sets of component names.')

    # Linear Solver settings
    lin_solver = Enum('scipy_gmres', ['scipy_gmres', 'petsc_ksp', 'linear_gs'],
                      desc='Method to use for gradient calculation',
                      framework_var=True)

    atol = Float(1.0e-9, desc='Absolute tolerance for the linear solver.',
                 framework_var=True)
    rtol = Float(1.0e-9, desc='Relative tolerance for the linear solver. '
                               '(Not supported by scipy.gmres)',
                               framework_var=True)
    maxiter = Int(100, desc='Maximum number of iterations for the linear solver.',
                  framework_var=True)

    iprint = Enum(0, [0, 1], desc="Set to 1 to print out residual of the linear solver",
                  framework_var=True)


    def _lin_solver_changed(self, oldls, newls):
        # if PETSc has been imported prior to the creation of a remote object using
        # the multiprocessing package, we get errors due to broken socket connections
        # from PETSc, so use this flag to prevent PETSc from being imported unless it's
        # actually used.
        if newls == 'petsc_ksp':
            PETSc.needs_ksp = True


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

    # flag to determine partitioning of our workflow's System
    system_type = Enum('auto',
                       ['auto', 'serial', 'parallel'],
                       desc="Determines the partitioning of this driver's "
                            "workflow components into Systems. Default is "
                            "'auto', where a hierarchy of serial and parallel "
                            "systems is automatically determined. 'serial' "
                            "and 'parallel' may be specified to force the"
                            "workflow components into a single serial or "
                            "parallel System.  Note that when not running "
                            "under MPI, this option is ignored and the "
                            "resulting System will always be serial.",
                       framework_var=True)

    def __init__(self):
        self._iter = None
        super(Driver, self).__init__()

        self.workflow = Dataflow(self)
        self._required_compnames = None
        self._reduced_graph = None

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

    def requires_derivs(self):
        return False

    def get_expr_scope(self):
        """Return the scope to be used to evaluate ExprEvaluators."""
        return self.parent

    def _collapse_subdrivers(self, g):
        """collapse subdriver iteration sets into single nodes."""
        # collapse all subdrivers in our graph
        itercomps = {}
        itercomps['#parent'] = self.workflow.get_names(full=True)

        for child_drv in self.subdrivers(recurse=False):
            itercomps[child_drv.name] = [c.name for c in child_drv.iteration_set()]

        for child_drv in self.subdrivers(recurse=False):
            excludes = set()
            for name, comps in itercomps.items():
                if name != child_drv.name:
                    for cname in comps:
                        if cname not in itercomps[child_drv.name]:
                            excludes.add(cname)

            collapse_driver(g, child_drv, excludes)

        # now remove any comps that are shared by subdrivers but are not found
        # in our workflow
        to_remove = set()
        for name, comps in itercomps.items():
            if name != '#parent':
                for comp in comps:
                    if comp not in itercomps['#parent']:
                        to_remove.add(comp)

        g.remove_nodes_from(to_remove)

    def get_depgraph(self):
        return self.parent._depgraph  # May change this to use a smaller graph later

    def get_reduced_graph(self):
        if self._reduced_graph is None:
            parent_graph = self.parent.get_reduced_graph()

            # copy parent graph
            g = parent_graph.subgraph(parent_graph.nodes_iter())

            nodes = set([c.name for c in self.workflow])
            g.collapse_subdrivers(nodes, self.workflow.subdrivers())

            nodes.add(self.name)

            g = g.full_subgraph(nodes)

            nodes.remove(self.name)

            # create fake edges to/from the driver and each of its
            # components so we can get everything that's relevant
            # by getting all nodes that are strongly connected to the
            # driver in the graph.
            to_add = []
            for name in nodes:
                if not g.has_edge(self.name, name):
                    to_add.append((self.name, name))
                if not g.has_edge(name, self.name):
                    to_add.append((name, self.name))
            g.add_edges_from(to_add)
            comps = []
            for comps in strongly_connected_components(g):
                if self.name in comps:
                    break
            g.remove_edges_from(to_add)
            self._reduced_graph = g.subgraph(comps)

        return self._reduced_graph

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
                                     HasObjective, HasObjectives, HasResponses)):
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
                                     HasObjective, HasObjectives, HasResponses)):
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
    def run(self, force=False, case_uuid=''):
        """Run this object. This should include fetching input variables if
        necessary, executing, and updating output variables. Do not override
        this function.

        force: bool
            If True, force component to execute even if inputs have not
            changed. (Default is False)

        case_uuid: str
            Identifier for the Case that is associated with this run.
        """
        # (Re)configure parameters.
        if hasattr(self, 'config_parameters'):
            self.config_parameters()

        # force param pseudocomps to get updated values to start
        self.update_parameters()

        # Reset the workflow.
        self.workflow.reset()
        super(Driver, self).run(case_uuid)

    @rbac(('owner', 'user'))
    def configure_recording(self, recording_options=None):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""
        return self.workflow.configure_recording(recording_options)

    def update_parameters(self):
        if hasattr(self, 'get_parameters'):
            params = self.get_parameters()
            for param in params.values():
                param.initialize(self.get_expr_scope(), self)
            if 'u' in self.workflow._system.vec:
                self.workflow._system.vec['u'].set_to_scope(self.parent,
                                                            params.keys())

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

        wf.run()

    def calc_derivatives(self, first=False, second=False):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""
        self.workflow.calc_derivatives(first, second)

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
        """Set up system trees from here down to all of our
        child Components.
        """
        if self.name in self.parent._reduced_graph:
            self._system = self.parent._reduced_graph.node[self.name]['system']
            self.workflow.setup_systems(self.system_type)

    def print_norm(self, driver_string, iteration, res, res0, msg=None, indent=0, solver='NL'):
        """ Prints out the norm of the residual in a neat readable format.
        """

        # Find indentation level
        if self.itername == '-driver':
            level = 0 + indent
        else:
            level = self.itername.count('.') + 1 + indent
            
        indent = '   ' * level
        if msg is not None:
            form = indent + '[%s] %s: %s   %d | %s'
            print form % (self.name, solver, driver_string, iteration, msg)
            return

        form = indent + '[%s] %s: %s   %d | %.9g %.9g'
        print form % (self.name, solver, driver_string, iteration, res, res/res0)

    #### MPI related methods ####

    @rbac(('owner', 'user'))
    def get_req_cpus(self):
        """Return requested_cpus."""
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

    def calc_gradient(self, inputs=None, outputs=None, mode='auto',
                      return_format='array', force_regen=True):
        """Returns the Jacobian of derivatives between inputs and outputs.

        inputs: list of strings
            List of OpenMDAO inputs to take derivatives with respect to.

        outputs: list of strings
            Lis of OpenMDAO outputs to take derivatives of.

        mode: string in ['forward', 'adjoint', 'auto', 'fd']
            Mode for gradient calculation. Set to 'auto' to let OpenMDAO choose
            forward or adjoint based on problem dimensions. Set to 'fd' to
            finite difference the entire workflow.

        return_format: string in ['array', 'dict']
            Format for return value. Default is array, but some optimizers may
            want a dictionary instead.

        force_regen: boolean
            Set to True to force a regeneration of the system hierarchy. This
            is set to True because this function is meant for manual testing.
        """

        return self.workflow.calc_gradient(inputs=inputs, outputs=outputs,
                                           mode=mode, return_format=return_format,
                                           force_regen=force_regen)

    @rbac(('owner', 'user'))
    def setup_depgraph(self, dgraph):
        self._reduced_graph = None
        # # add connections for params, constraints, etc.
        # pass

        if self.workflow._calc_gradient_inputs is not None:
            for param in self.workflow._calc_gradient_inputs:
                dgraph.add_param(self.name, param)
        else:  # add connections for our params/constraints/objectives
            # if hasattr(self, 'list_param_group_targets'):
            #     params = self.list_param_group_targets()

            # for now do nothing here because params are already
            # in the depgraph
            pass

        # add connections for calc gradient outputs
        if self.workflow._calc_gradient_outputs is not None:
            for vname in self.workflow._calc_gradient_outputs:
                dgraph.add_driver_input(self.name, vname)
        else:
            pass # for now, do nothing

    @rbac(('owner', 'user'))
    def pre_setup(self):
        self._reduced_graph = None
        self.workflow.pre_setup()
