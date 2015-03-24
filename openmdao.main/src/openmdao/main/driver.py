""" Driver class definition """

#public symbols
__all__ = ["Driver"]

import sys
from cStringIO import StringIO
from math import isnan

# pylint: disable=E0611,F0401

from networkx.algorithms.components import strongly_connected_components
import networkx as nx

from openmdao.main.mpiwrap import PETSc
from openmdao.main.component import Component
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Slot, \
                                        List, VarTree
from openmdao.main.depgraph import find_all_connecting, \
                                   collapse_driver, gsort
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
from openmdao.main.workflow import Workflow, get_cycle_vars
from openmdao.main.case import Case

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

    #fd_blocks = List([], desc='User can specify nondifferentiable blocks '
    #                          'by adding sets of component names.',
    #                          framework_var=True)

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
    # though we replace it with a new Workflow in __init__
    workflow = Slot(Workflow, allow_none=True, required=True,
                    factory=Workflow, hidden=True)

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

        self.workflow = Workflow(self)
        self._required_compnames = None
        self._reduced_graph = None
        self._iter_set = None
        self._full_iter_set = None

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

        for child_drv in self.subdrivers(recurse=False):
            itercomps[child_drv.name] = [c.name for c in child_drv.iteration_set()]

        for child_drv in self.subdrivers(recurse=False):
            excludes = set(self._iter_set)
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
            for comp in comps:
                if comp not in self._iter_set:
                    to_remove.add(comp)

        g.remove_nodes_from(to_remove)

    def get_reduced_graph(self):
        if self._reduced_graph is None:
            parent_graph = self.parent._reduced_graph

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

        # duplicate entries in the workflow are not allowed
        names = self.workflow._explicit_names
        dups = list(set([x for x in names if names.count(x) > 1]))
        if len(dups) > 0:
            raise RuntimeError("%s workflow has duplicate entries: %s" %
                                (self.get_pathname(), str(dups)))

        # workflow will raise an exception if it can't resolve a Component
        super(Driver, self).check_config(strict=strict)
        self.workflow.check_config(strict=strict)

    @rbac(('owner', 'user'))
    def get_itername(self):
        """Return current 'iteration coordinates'."""
        if self.parent._top_driver is self:
            return self.parent.get_itername()

        return self.itername

    def compute_itersets(self, cgraph):
        """Return a list of all components required to run a full
        iteration of this driver.
        """
        self._full_iter_set = set()

        comps = [getattr(self.parent, n) for n in self.workflow._explicit_names]
        subdrivers = [c for c in comps if has_interface(c, IDriver)]
        subnames = [s.name for s in subdrivers]

        allcomps = [getattr(self.parent, n) for n in cgraph if not n == self.name]
        alldrivers = [c.name for c in allcomps if has_interface(c, IDriver)]

        # make our own copy of the graph to play with
        cgraph = cgraph.subgraph([n for n in cgraph
                                if n not in alldrivers or n in subnames])

        myset = set(self.workflow._explicit_names +
                    self.list_pseudocomps())

        # First, have all of our subdrivers (recursively) determine
        # their iteration sets, because we need those to determine
        # our full set.
        subcomps = set()
        for comp in subdrivers:
            cgcopy = cgraph.subgraph(cgraph.nodes_iter())
            comp.compute_itersets(cgcopy)
            subcomps.update(comp._full_iter_set)

        # create fake edges to/from the driver and each of its
        # components so we can get everything that's relevant
        # by getting all nodes that are strongly connected to the
        # driver in the graph.
        for drv in subdrivers:
            for name in drv._full_iter_set:
                cgraph.add_edge(drv.name, name)
                cgraph.add_edge(name, drv.name)

        # add predecessors to my pseudocomps if they aren't
        # already in the itersets of my subdrivers
        for pcomp in self.list_pseudocomps():
            for pred in cgraph.predecessors(pcomp):
                if pred not in subcomps:
                    myset.add(pred)

        # now create fake edges from us to all of the comps
        # that we know about in our iterset
        for name in myset:
            cgraph.add_edge(self.name, name)
            cgraph.add_edge(name, self.name)

        # collapse our explicit subdrivers
        self._iter_set = self.workflow._explicit_names
        self._collapse_subdrivers(cgraph)

        comps = []
        for comps in strongly_connected_components(cgraph):
            if self.name in comps:
                break

        self._iter_set = set(comps)
        self._iter_set.remove(self.name)

        # the following fixes a test failure when using DOEdriver with
        # an empty workflow.  This adds any comps that own DOEdriver
        # parameters to the DOEdriver's iteration set.
        conns = self.get_expr_depends()
        self._iter_set.update([u for u,v in conns if u != self.name and u not in subcomps])
        self._iter_set.update([v for u,v in conns if v != self.name and v not in subcomps])

        old_iter = self._iter_set.copy()

        # remove any drivers that were not explicitly specified in our worklow
        self._iter_set = set([c for c in self._iter_set if c not in alldrivers
                                or c in subnames])

        diff = old_iter - self._iter_set
        if diff:
            self._logger.warning("Driver '%s' had the following subdrivers removed"
                                 " from its workflow because they were not explicity"
                                 " added: %s" % (self.name, list(diff)))

        self._full_iter_set.update(self._iter_set)
        self._full_iter_set.update(subcomps)

    def compute_ordering(self, cgraph):
        """Given a component graph, each driver can determine its iteration
        set and the ordering of its workflow.
        """
        cgraph = cgraph.subgraph(self._full_iter_set)

        # call compute_ordering on all subdrivers
        for name in self._iter_set:
            obj = getattr(self.parent, name)
            if has_interface(obj, IDriver):
                obj.compute_ordering(cgraph)

        self._collapse_subdrivers(cgraph)

        # now figure out the order of our iter_set
        self._ordering = self.workflow._explicit_names + \
                         [n for n in self._iter_set
                           if n not in self.workflow._explicit_names]

        # remove any nodes that got collapsed into subdrivers
        self._ordering = [n for n in self._ordering if n in cgraph]

        self._ordering = gsort(cgraph, self._ordering)

        self.workflow._ordering = self._ordering

    def iteration_set(self):
        """Return a set of all Components in our workflow and
        recursively in any workflow in any Driver in our workflow.
        """
        return set([getattr(self.parent, n) for n in self._full_iter_set])

    @rbac(('owner', 'user'))
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name,
        dest_comp_name) for each dependency introduced by any ExprEvaluators
        in this Driver, ignoring any dependencies on components that are
        inside of this Driver's iteration set.
        """
        iternames = set([c.name for c in self.iteration_set()])
        deps = set()
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
            itercomps = list([getattr(self.parent,n) for n in self._iter_set])

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

            compgraph = self.parent._depgraph.component_graph()

            for end in getcomps:
                for start in setcomps:
                    full.update(find_all_connecting(compgraph, start, end))

            if self.name in full:
                full.remove(self.name)
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

    def name_changed(self, old, new):
        """Change any workflows or delegates that reference the old
        name of an object that has now been changed to a new name.

        old: string
            Original name of the object

        new: string
            New name of the object
        """

        # alert any delegates of the name change
        if hasattr(self, '_delegates_'):
            for dname in self._delegates_:
                inst = getattr(self, dname)
                if isinstance(inst, (HasParameters, HasConstraints,
                                     HasEqConstraints, HasIneqConstraints,
                                     HasObjective, HasObjectives, HasResponses)):
                    inst.name_changed(old, new)

        # update our workflow
        for i, name in enumerate(self.workflow._explicit_names):
            if name == old:
                self.workflow._explicit_names[i] = new

        # force update of workflow full names
        self.workflow.config_changed()

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

    def run_iteration(self, case_uuid=None):
        """Runs workflow."""
        wf = self.workflow
        if not wf._ordering:
            self._logger.warning("'%s': workflow is empty!"
                                 % self.get_pathname())

        if not wf._system.is_active():
            return

        self._stop = False
        self.workflow._exec_count += 1

        iterbase = wf._iterbase()

        if not case_uuid:
            # We record the case and are responsible for unique case ids.
            record_case = True
            case_uuid = Case.next_uuid()
        else:
            record_case = False

        err = None
        try:
            uvec = wf._system.vec['u']
            fvec = wf._system.vec['f']

            if wf._need_prescatter:
                wf._system.scatter('u', 'p')

            # save old value of u to compute resids
            for node in wf._cycle_vars:
                fvec[node][:] = uvec[node][:]

            wf._system.run(iterbase=iterbase, case_uuid=case_uuid)

            # update resid vector for cyclic vars
            for node in wf._cycle_vars:
                fvec[node][:] -= uvec[node][:]

            if self._stop:
                raise RunStopped('Stop requested')
        except Exception:
            err = sys.exc_info()

        if record_case and wf._rec_required:
            try:
                wf._record_case(case_uuid, err)
            except Exception as exc:
                if err is None:
                    err = sys.exc_info()
                self._logger.error("Can't record case: %s", exc)

        # reraise exception with proper traceback if one occurred
        if err is not None:
            # NOTE: cannot use 'raise err' here for some reason.  Must separate
            # the parts of the tuple.
            raise err[0], err[1], err[2]

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
        self._iter_set = None
        self._full_iter_set = None
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

    def setup_init(self):
        super(Driver, self).setup_init()

        self._required_compnames = None
        self._iter_set = None
        self._full_iter_set = None
        self._depgraph = None
        self._reduced_graph = None

        self.workflow.setup_init()

    @rbac(('owner', 'user'))
    def setup_systems(self):
        """Set up system trees from here down to all of our
        child Components.
        """
        if self.name in self.parent._reduced_graph:
            self._system = self.parent._reduced_graph.node[self.name]['system']
            self.workflow.setup_systems(self.system_type)

    def print_norm(self, driver_string, iteration, res, res0, msg=None,
                   indent=0, solver='NL'):
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

        # FIXME: move this somewhere else...
        if hasattr(self.workflow._system, 'graph'):
            self.workflow._cycle_vars = get_cycle_vars(self.workflow._system.graph,
                                                       self.parent._var_meta)
        else:
            self.workflow._cycle_vars = []

    @rbac(('owner', 'user'))
    def get_full_nodeset(self):
        """Return the full set of nodes in the depgraph
        belonging to this driver (includes full iteration set).
        """
        names = super(Driver, self).get_full_nodeset()
        names.update(self._full_iter_set)

        srcvars, destvars = self.get_expr_var_depends()
        ours = srcvars
        ours.update(destvars)

        # check for any VarSystems that correspond to our params/constraints/obj
        # because they should also 'belong' to us
        if self.parent._reduced_graph:
            cgraph = self.parent._reduced_graph.component_graph()
            for node in cgraph:
                if node in ours:
                    names.add(node)
        return names

    def calc_gradient(self, inputs=None, outputs=None, mode='auto',
                      return_format='array'):
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
        """

        return self._calc_gradient(inputs=inputs, outputs=outputs,
                                   mode=mode, return_format=return_format,
                                   force_regen=True)

    def _calc_gradient(self, inputs, outputs, mode='auto',
                       return_format='array', options=None, force_regen=False):
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
            Set to True to force a regeneration of the system hierarchy.
        """

        # if inputs aren't specified, use parameters
        if inputs is None:
            if hasattr(self, 'list_param_group_targets'):
                inputs = self.list_param_group_targets()
            if not inputs:
                msg = "No inputs given for derivatives."
                self.raise_exception(msg, RuntimeError)

        # If outputs aren't specified, use the objectives and constraints
        if outputs is None:
            outputs = []
            if hasattr(self, 'list_objective_targets'):
                outputs.extend(self.list_objective_targets())
            if hasattr(self, 'list_constraint_targets'):
                outputs.extend(self.list_constraint_targets())
            if not outputs:
                msg = "No outputs given for derivatives."
                self.raise_exception(msg, RuntimeError)

        inputs  = [_fix_tups(x) for x in inputs]
        outputs = [_fix_tups(x) for x in outputs]

        self.workflow._calc_gradient_inputs = inputs[:]
        self.workflow._calc_gradient_outputs = outputs[:]

        try:
            if force_regen:
                top = self
                while top.parent is not None:
                    top = top.parent

                top._setup(inputs=inputs, outputs=outputs, drvname=self.name)

            if options is None:
                options = self.gradient_options

            J = self.workflow.calc_gradient(inputs, outputs, mode, return_format,
                                            options=options)

            # Finally, we need to untransform the jacobian if any parameters have
            # scalers.
            if not hasattr(self, 'get_parameters'):
                return J

            params = self.get_parameters()

            if len(params) == 0:
                return J

            i = 0
            for group in inputs:

                if isinstance(group, str):
                    pname = name = group
                else:
                    pname = tuple(group)
                    name = group[0]

                # Note: 'dict' is the only valid return_format for MPI runs.
                if return_format == 'dict':
                    if pname in params:
                        scaler = params[pname].scaler
                        if scaler != 1.0:
                            for okey in J.keys():
                                J[okey][name] = J[okey][name]*scaler

                else:
                    width = len(self._system.vec['u'][name])

                    if pname in params:
                        scaler = params[pname].scaler
                        if scaler != 1.0:
                            J[:, i:i+width] = J[:, i:i+width]*scaler

                    i += width

        finally:
            self.workflow._calc_gradient_inputs = None
            self.workflow._calc_gradient_outputs = None

        return J

    def check_gradient(self, inputs=None, outputs=None, stream=sys.stdout, mode='auto'):
        """Compare the OpenMDAO-calculated gradient with one calculated
        by straight finite-difference. This provides the user with a way
        to validate his derivative functions (apply_deriv and provideJ.)

        inputs: (optional) iter of str or None
            Names of input variables. The calculated gradient will be
            the matrix of values of the output variables with respect
            to these input variables. If no value is provided for inputs,
            they will be determined based on the parameters of
            this Driver.

        outputs: (optional) iter of str or None
            Names of output variables. The calculated gradient will be
            the matrix of values of these output variables with respect
            to the input variables. If no value is provided for outputs,
            they will be determined based on the objectives and constraints
            of this Driver.

        stream: (optional) file-like object or str
            Where to write to, default stdout. If a string is supplied,
            that is used as a filename. If None, no output is written.

        mode: (optional) str
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode,
            or 'auto' to let OpenMDAO determine the correct mode.
            Defaults to 'auto'.

        Returns the finite difference gradient, the OpenMDAO-calculated
        gradient, and a list of suspect inputs/outputs.
        """
        # tuples cause problems
        if inputs:
            inputs = list(inputs)
        if outputs:
            outputs = list(outputs)

        if isinstance(stream, basestring):
            stream = open(stream, 'w')
            close_stream = True
        else:
            close_stream = False
            if stream is None:
                stream = StringIO()

        J = self.calc_gradient(inputs, outputs, mode=mode)
        Jbase = self.calc_gradient(inputs, outputs, mode='fd')

        print >> stream, 24*'-'
        print >> stream, 'Calculated Gradient'
        print >> stream, 24*'-'
        print >> stream, J
        print >> stream, 24*'-'
        print >> stream, 'Finite Difference Comparison'
        print >> stream, 24*'-'
        print >> stream, Jbase

        # This code duplication is needed so that we print readable names for
        # the constraints and objectives.

        if inputs is None:
            if hasattr(self, 'list_param_group_targets'):
                inputs = self.list_param_group_targets()
                input_refs = []
                for item in inputs:
                    if len(item) < 2:
                        input_refs.append(item[0])
                    else:
                        input_refs.append(item)
            # Should be caught in calc_gradient()
            else:  # pragma no cover
                msg = "No inputs given for derivatives."
                self.raise_exception(msg, RuntimeError)
        else:
            input_refs = inputs

        if outputs is None:
            outputs = []
            output_refs = []
            if hasattr(self, 'get_objectives'):
                obj = ["%s.out0" % item.pcomp_name for item in
                       self.get_objectives().values()]
                outputs.extend(obj)
                output_refs.extend(self.get_objectives().keys())
            if hasattr(self, 'get_constraints'):
                con = ["%s.out0" % item.pcomp_name for item in
                       self.get_constraints().values()]
                outputs.extend(con)
                output_refs.extend(self.get_constraints().keys())

            if len(outputs) == 0:  # pragma no cover
                msg = "No outputs given for derivatives."
                self.raise_exception(msg, RuntimeError)
        else:
            output_refs = outputs

        out_width = 0

        for output, oref in zip(outputs, output_refs):
            out_val = self.parent.get(output)
            out_names = _flattened_names(oref, out_val)
            out_width = max(out_width, max([len(out) for out in out_names]))

        inp_width = 0
        for input_tup, iref in zip(inputs, input_refs):
            if isinstance(input_tup, str):
                input_tup = [input_tup]
            inp_val = self.parent.get(input_tup[0])
            inp_names = _flattened_names(str(iref), inp_val)
            inp_width = max(inp_width, max([len(inp) for inp in inp_names]))

        label_width = out_width + inp_width + 4

        print >> stream
        print >> stream, label_width*' ', \
              '%-18s %-18s %-18s' % ('Calculated', 'FiniteDiff', 'RelError')
        print >> stream, (label_width+(3*18)+3)*'-'

        suspect_limit = 1e-5
        error_n = error_sum = 0
        error_max = error_loc = None
        suspects = []
        i = -1

        io_pairs = []

        for output, oref in zip(outputs, output_refs):
            out_val = self.parent.get(output)
            for out_name in _flattened_names(oref, out_val):
                i += 1
                j = -1
                for input_tup, iref in zip(inputs, input_refs):
                    if isinstance(input_tup, basestring):
                        input_tup = (input_tup,)

                    inp_val = self.parent.get(input_tup[0])
                    for inp_name in _flattened_names(iref, inp_val):
                        j += 1
                        calc = J[i, j]
                        finite = Jbase[i, j]
                        if finite and calc:
                            error = (calc - finite) / finite
                        else:
                            error = calc - finite
                        error_n += 1
                        error_sum += abs(error)
                        if error_max is None or abs(error) > abs(error_max):
                            error_max = error
                            error_loc = (out_name, inp_name)
                        if abs(error) > suspect_limit or isnan(error):
                            suspects.append((out_name, inp_name))
                        print >> stream, '%*s / %*s: %-18s %-18s %-18s' \
                              % (out_width, out_name, inp_width, inp_name,
                                 calc, finite, error)
                        io_pairs.append("%*s / %*s"
                                        % (out_width, out_name,
                                           inp_width, inp_name))
        print >> stream
        if error_n:
            print >> stream, 'Average RelError:', error_sum / error_n
            print >> stream, 'Max RelError:', error_max, 'for %s / %s' % error_loc
        if suspects:
            print >> stream, 'Suspect gradients (RelError > %s):' % suspect_limit
            for out_name, inp_name in suspects:
                print >> stream, '%*s / %*s' \
                      % (out_width, out_name, inp_width, inp_name)
        print >> stream

        if close_stream:
            stream.close()

        # return arrays and suspects to make it easier to check from a test
        return Jbase.flatten(), J.flatten(), io_pairs, suspects

    @rbac(('owner', 'user'))
    def setup_depgraph(self, dgraph):
        self._reduced_graph = None
        if self.workflow._calc_gradient_inputs is not None:
            for param in self.workflow._calc_gradient_inputs:
                dgraph.add_param(self.name, param)

        # add connections for calc gradient outputs
        if self.workflow._calc_gradient_outputs is not None:
            for vname in self.workflow._calc_gradient_outputs:
                dgraph.add_driver_input(self.name, vname)

    @rbac(('owner', 'user'))
    def init_var_sizes(self):
        for cname in self._ordering:
            getattr(self.parent, cname).init_var_sizes()

    @rbac(('owner', 'user'))
    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this Component.
        """
        if self.force_fd:
            return False

        return ISolver.providedBy(self) or self.__class__ == Driver

def _flattened_names(name, val, names=None):
    """ Return list of names for values in `val`.
    Note that this expands arrays into an entry for each index!.
    """
    from numpy import ndarray
    if names is None:
        names = []
    if isinstance(val, float):
        names.append(name)
    elif isinstance(val, ndarray):
        for i in range(len(val)):
            value = val[i]
            _flattened_names('%s[%s]' % (name, i), value, names)
    elif IVariableTree.providedBy(val):
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            _flattened_names('.'.join((name, key)), value, names)
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))
    return names


def _fix_tups(x):
    """Return x[0] if x is a single element tuple, else return x."""
    if isinstance(x, tuple) and len(x) == 1:
        return x[0]
    return x
