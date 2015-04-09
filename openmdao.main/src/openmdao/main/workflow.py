""" Base class for all workflows. """

from fnmatch import fnmatch
from math import isnan
import sys
from types import NoneType

import weakref
from StringIO import StringIO

from numpy import ndarray
from networkx.algorithms.dag import is_directed_acyclic_graph
from networkx.algorithms.components import strongly_connected_components

# pylint: disable-msg=E0611,F0401
from openmdao.main.mp_support import has_interface
from openmdao.main.case import Case
from openmdao.main.mpiwrap import MPI, MPI_info
from openmdao.main.systems import SerialSystem, ParallelSystem, \
                                  OpaqueSystem, VarSystem, CompoundSystem, \
                                  partition_subsystems, ParamSystem, \
                                  get_comm_if_active, collapse_to_system_node
from openmdao.main.depgraph import _get_inner_connections, get_nondiff_groups, \
                                   collapse_nodes, simple_node_iter, CollapsedGraph
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import IVariableTree, IDriver
from openmdao.main.depgraph import is_connection
from openmdao.util.decorators import method_accepts


__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order during a single iteration of a Driver.
    """

    def __init__(self, parent, members=None):
        """Create a Workflow.

        parent: Driver
            The Driver that contains this Workflow.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
        """
        self._parent = None
        self.parent = parent
        self._stop = False
        self._scope = None
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._system = None
        self._reduced_graph = None

        self._explicit_names = []  # names the user adds explicitly

        self._rec_required = None  # Case recording configuration.
        self._rec_parameters = None
        self._rec_objectives = None
        self._rec_responses = None
        self._rec_constraints = None
        self._rec_outputs = None

        self._need_prescatter = False

        self._ordering = None

        self._calc_gradient_inputs = None
        self._calc_gradient_outputs = None

        if members:
            for member in members:
                if not isinstance(member, basestring):
                    raise TypeError("Components must be added to a workflow by name.")
                self.add(member)

        self.mpi = MPI_info()

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = None if self._parent is None else self._parent()
        state['_scope'] = None if self._scope is None else self._scope()
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        self.parent = state['_parent']
        self.scope = state['_scope']

    def __contains__(self, comp):
        return comp in self.parent._iter_set

    @property
    def parent(self):
        """ This workflow's driver. """
        return None if self._parent is None else self._parent()

    @parent.setter
    def parent(self, parent):
        self._parent = None if parent is None else weakref.ref(parent)

    @property
    def scope(self):
        """The scoping Component that is used to resolve the Component names in
        this Workflow.
        """
        if self._scope is None and self.parent is not None:
            self._scope = weakref.ref(self.parent.get_expr_scope())
        if self._scope is None:
            raise RuntimeError("workflow has no scope!")
        return self._scope()

    @scope.setter
    def scope(self, scope):
        self._scope = None if scope is None else weakref.ref(scope)
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
        self._initial_count = count - 1  # run() will increment.

    def reset(self):
        """ Reset execution count. """
        self._exec_count = self._initial_count

    def calc_gradient(self, inputs=None, outputs=None, mode='auto',
                      return_format='array', force_regen=False, options=None):
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

        options: Gradient_Options Vartree
            Override this driver's Gradient_Options with others.
        """

        return self._system.calc_gradient(inputs, outputs, mode=mode,
                                       options=options if options else self.parent.gradient_options,
                                       iterbase=self._iterbase(),
                                       return_format=return_format)

    def calc_newton_direction(self):
        """ Solves for the new state in Newton's method and leaves it in the
        df vector."""

        self._system.calc_newton_direction(options=self.parent.gradient_options,
                                           iterbase=self._iterbase())

    def configure_recording(self, recording_options=None):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""

        if recording_options:
            includes = recording_options.includes
            excludes = recording_options.excludes
            save_problem_formulation = recording_options.save_problem_formulation
        else:
            includes = excludes = save_problem_formulation = None

        if not recording_options or not (save_problem_formulation or includes):
            self._rec_required = False
            return (set(), dict())

        driver = self.parent
        scope = driver.parent
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
                if save_problem_formulation or \
                    self._check_path(path, includes, excludes):
                    self._rec_parameters.append(param)
                    inputs.append(name)

        # Objectives
        self._rec_objectives = []
        if hasattr(driver, 'eval_objectives'):
            for key, objective in driver.get_objectives().items():
                name = objective.pcomp_name
                if key != objective.text:
                    name = key

                #name = objective.pcomp_name
                path = prefix+name
                if save_problem_formulation or \
                   self._check_path(path, includes, excludes):
                    self._rec_objectives.append(key)
                    if key != objective.text:
                        outputs.append(name)
                    else:
                        outputs.append(name + '.out0')

        # Responses
        self._rec_responses = []
        if hasattr(driver, 'get_responses'):
            for key, response in driver.get_responses().items():
                name = response.pcomp_name
                path = prefix+name
                if save_problem_formulation or \
                   self._check_path(path, includes, excludes):
                    self._rec_responses.append(key)
                    outputs.append(name + '.out0')

        # Constraints
        self._rec_constraints = []
        if hasattr(driver, 'get_eq_constraints'):
            for con in driver.get_eq_constraints().values():
                name = con.pcomp_name
                path = prefix+name
                if save_problem_formulation or \
                   self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(name + '.out0')

        if hasattr(driver, 'get_ineq_constraints'):
            for con in driver.get_ineq_constraints().values():
                name = con.pcomp_name
                path = prefix+name
                if save_problem_formulation or \
                   self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(name + '.out0')

        self._rec_outputs = []
        for comp in self:
            try:
                successors = driver.get_reduced_graph().successors(comp.name)
            except:
                err = sys.exc_info()
                import traceback
                print traceback.format_exc()
                raise err[0], err[1], err[2]

            for output_name, aliases in successors:

                # From Bret: it does make sense to skip subdrivers like you said, except for the
                #      case where a driver has actual outputs of its own.  So you may have to keep
                #  subdriver successors if the edge between the subdriver and the successor
                #  is an actual data connection.
                # look at the edge metadata to see if there's maybe a 'conn' in there for real connections.
                if has_interface(comp, IDriver):
                    if not is_connection(driver._reduced_graph, comp.name, output_name):
                        continue

                if '.in' in output_name: # look for something that is not a pseudo input
                    for n in aliases:
                        if not ".in" in n:
                            output_name = n
                            break
                #output_name = prefix + output_name
                if output_name not in outputs and self._check_path(prefix + output_name, includes, excludes) :
                    self._rec_outputs.append(output_name)
                    outputs.append(output_name)
                    #self._rec_all_outputs.append(output_name)

        for cname in driver._ordering:
            comp = getattr(self.scope, cname)
            for output_name in scope._depgraph.list_outputs(comp.name):
                if has_interface(comp, IDriver): # Only record outputs from drivers if they are framework variables
                    metadata = scope.get_metadata(output_name)
                    if not ('framework_var' in metadata and metadata[ 'framework_var'] ):
                        continue

                #output_name = prefix + output_name
                if output_name not in outputs and self._check_path(prefix + output_name, includes, excludes) :
                    outputs.append(output_name)
                    self._rec_outputs.append(output_name)

        name = '%s.workflow.itername' % driver.name
        path = prefix+name
        if self._check_path(path, includes, excludes):
            self._rec_outputs.append(name)
            outputs.append(name)

        # If recording required, register names in recorders.
        self._rec_required = bool(inputs or outputs)
        if self._rec_required:
            top = scope
            while top.parent is not None:
                top = top.parent
            for recorder in top.recorders:
                recorder.register(driver, inputs, outputs)

        return (set(prefix+name for name in inputs), dict())

    @staticmethod
    def _check_path(path, includes, excludes):
        """ Return True if `path` should be recorded. """
        record = False

        # first see if it's included
        for pattern in includes:
            if fnmatch(path, pattern):
                record = True

        # if it passes include filter, check exclude filter
        if record:
            for pattern in excludes:
                if fnmatch(path, pattern):
                    record = False

        return record

    def _record_case(self, case_uuid, err):
        """ Record case in all recorders. """
        driver = self.parent
        scope = driver.parent
        top = scope
        while top.parent is not None:
            top = top.parent

        inputs = []
        outputs = []

        # print "recording case"
        # if MPI:
        #     print 'workflow', self
        #     print 'workflow comm',self._system.mpi.comm
        #     print 'workflow rank',self._system.mpi.rank


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
                            case_uuid, self.parent._case_uuid)

    def _iterbase(self):
        """ Return base for 'iteration coordinates'. """
        if self.parent is None:
            return str(self._exec_count)  # An unusual case.
        else:
            prefix = self.parent.get_itername()
            if prefix:
                prefix += '.'
            return '%s%d' % (prefix, self._exec_count)

    def stop(self):
        """
        Stop all Components in this Workflow.
        We assume it's OK to to call stop() on something that isn't running.
        """
        self._system.stop()
        self._stop = True

    @method_accepts(TypeError,
                    compnames=(str, list, tuple),
                    index=(int, NoneType),
                    check=bool)
    def add(self, compnames, index=None, check=False):
        """
        add(self, compnames, index=None, check=False)
        Add new component(s) to the end of the workflow by name.
        """

        if isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            nodes = compnames

        try:
            iter(nodes)
        except TypeError:
            raise TypeError("Components must be added by name to a workflow.")

        # workflow deriv graph, etc. must be recalculated
        self.config_changed()

        for node in nodes:
            if isinstance(node, basestring):

                if check:
                    # check whether each node is valid and if not then
                    # construct a useful error message.
                    parent = self.parent
                    name = parent.parent.name
                    if not name:
                        name = "the top assembly."

                    # Components in subassys are never allowed.
                    if '.' in node:
                        msg = "Component '%s' is not" % node + \
                              " in the scope of %s" % name
                        raise AttributeError(msg)

                    # Does the component really exist?
                    try:
                        target = parent.parent.get(node)
                    except AttributeError:
                        msg = "Component '%s'" % node + \
                              " does not exist in %s" % name
                        raise AttributeError(msg)

                    # Don't add yourself to your own workflow
                    if target == parent:
                        msg = "You cannot add a driver to its own workflow"
                        raise AttributeError(msg)

                if index is None:
                    self._explicit_names.append(node)
                else:
                    self._explicit_names.insert(index, node)
                    index += 1
            else:
                msg = "Components must be added by name to a workflow."
                raise TypeError(msg)

    def config_changed(self):
        """Notifies the Workflow that workflow configuration
        (dependencies, etc.) has changed.
        """
        self._system = None
        self._ordering = None

    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            msg = "Components must be removed by name from a workflow."
            raise TypeError(msg)
        try:
            self._explicit_names.remove(compname)
        except ValueError:
            pass
        self.config_changed()

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter([getattr(self.scope, n) for n in self.parent._ordering])

    def __len__(self):
        raise NotImplementedError("This Workflow has no '__len__' function")

    def setup_init(self):
        self._system = None

        self._rec_required = None  # Case recording configuration.
        self._rec_parameters = None
        self._rec_objectives = None
        self._rec_responses = None
        self._rec_constraints = None
        self._rec_outputs = None

        self._need_prescatter = False

        self._ordering = None

    def setup_systems(self, system_type):
        """Get the subsystem for this workflow. Each
        subsystem contains a subgraph of this workflow's component
        graph, which contains components and/or other subsystems.
        """

        scope = self.scope
        drvname = self.parent.name

        parent_graph = self.scope._reduced_graph
        reduced = parent_graph.subgraph(parent_graph.nodes_iter())

        # collapse driver iteration sets into a single node for
        # the driver.
        reduced.collapse_subdrivers(self.parent._iter_set,
                                    self.subdrivers())

        reduced = reduced.full_subgraph(self.parent._iter_set)

        params = set()
        for s in parent_graph.successors(drvname):
            if parent_graph[drvname][s].get('drv_conn') == drvname:
                if reduced.in_degree(s):
                    continue
            params.add(s)
            reduced.add_node(s[0], comp='param')
            reduced.add_edge(s[0], s)

        # we need to connect a param comp node to all param nodes
        for node in params:
            param = node[0]
            reduced.node[param]['system'] = \
                ParamSystem(scope, reduced, param)

        #outs = []
        #for p in parent_graph.predecessors(drvname):
        #    if parent_graph[p][drvname].get('drv_conn') == drvname:
        #        outs.append(p)

        #for out in outs:
        #    vname = out[1][0]
        #    if reduced.out_degree(vname) == 0:
        #        reduced.add_node(vname, comp='dumbvar')
        #        reduced.add_edge(out, vname)
        #        reduced.node[vname]['system'] = \
        #                   VarSystem(scope, reduced, vname)

        cgraph = reduced.component_graph()

        opaque_map = {} # map of all internal comps to collapsed
                                # name of opaque system
        if self.scope._derivs_required:
            # collapse non-differentiable system groups into
            # opaque systems
            systems = {}
            for group in get_nondiff_groups(reduced, cgraph, self.scope):
                gtup = tuple(sorted(group))
                system = OpaqueSystem(scope, parent_graph,
                                      cgraph.subgraph(group),
                                      str(gtup))
                systems[gtup] = system

            for gtup, system in systems.items():
                collapse_to_system_node(cgraph, system, gtup)
                reduced.add_node(gtup, comp=True)
                collapse_nodes(reduced, gtup, reduced.internal_nodes(gtup))
                reduced.node[gtup]['comp'] = True
                reduced.node[gtup]['system'] = system
                for c in gtup:
                    opaque_map[c] = gtup

            # get rid of any back edges for opaque boundary nodes that
            # originate inside of the opaque system
            to_remove = []
            for node in systems:
                for s in reduced.successors(node):
                    if node in reduced.predecessors(s):
                        to_remove.append((s, node))
            reduced.remove_edges_from(to_remove)

        self._reduced_graph = reduced

        if system_type == 'auto' and MPI:
            self._auto_setup_systems(scope, reduced, cgraph)
        elif MPI and system_type == 'parallel':
            self._system = ParallelSystem(scope, reduced, cgraph,
                                          str(tuple(sorted(cgraph.nodes()))))
        else:
            self._system = SerialSystem(scope, reduced, cgraph,
                                        str(tuple(sorted(cgraph.nodes()))))

        self._system.set_ordering([p[0] for p in params]+self._ordering,
                                  opaque_map)

        self._system._parent_system = self.parent._system

        for comp in self:
            comp.setup_systems()

    def _auto_setup_systems(self, scope, reduced, cgraph):
        """
        Collapse the graph into nodes representing parallel
        and serial subsystems.
        """
        cgraph = partition_subsystems(scope, reduced, cgraph)

        if len(cgraph) > 1:
            if len(cgraph.edges()) > 0:
                self._system = SerialSystem(scope, reduced,
                                            cgraph, tuple(cgraph.nodes()))
            else:
                self._system = ParallelSystem(scope, reduced,
                                              cgraph, str(tuple(cgraph.nodes())))
        elif len(cgraph) == 1:
            name = cgraph.nodes()[0]
            self._system = cgraph.node[name].get('system')
        else:
            raise RuntimeError("setup_systems called on %s.workflow but component graph is empty!" %
                               self.parent.get_pathname())

    def get_req_cpus(self):
        """Return requested_cpus"""
        if self._system is None:
            return (1, 1)
        else:
            return self._system.get_req_cpus()

    def setup_communicators(self, comm):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.mpi.comm = get_comm_if_active(self, comm)
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._system.setup_communicators(self.mpi.comm)

    def setup_scatters(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._system.setup_scatters()

    def get_full_nodeset(self):
        """Return the set of nodes in the depgraph
        belonging to this driver (inlcudes full iteration set).
        """
        return set([c.name for c in self.parent.iteration_set()])

    def subdrivers(self):
        """Return a list of direct subdrivers in this workflow."""
        return [c for c in self if has_interface(c, IDriver)]

    def clear(self):
        """Remove all components from this workflow."""
        self._explicit_names = []
        self.config_changed()

    def mimic(self, src):
        '''Mimic capability'''
        self.clear()
        par = self.parent.parent
        if par is not None:
            self._explicit_names = [n for n in src._explicit_names
                                            if hasattr(par, n)]
        else:
            self._explicit_names = src._explicit_names[:]

def get_cycle_vars(graph, varmeta):
    # examine the graph to see if we have any cycles that we need to
    # deal with
    cycle_vars = []

    # make a copy of the graph since we don't want to modify it
    g = graph.subgraph(graph.nodes_iter())

    sizes = []

    while not is_directed_acyclic_graph(g):
        if not sizes:
            # get total data sizes for subsystem connections
            for u,v,data in g.edges_iter(data=True):
                sz = 0
                for node in data['varconns']:
                    sz += varmeta[node].get('size', 0)
                data['conn_size'] = sz
                sizes.append((sz, (u,v)))

            sizes = sorted(sizes)

        strong = list(strongly_connected_components(g))[0]
        if len(strong) == 1:
            break

        # find the connection with the smallest data xfer
        for sz, (src, dest) in sizes:
            if src in strong and dest in strong:
                cycle_vars.extend(g[src][dest]['varconns'])
                g.remove_edge(src, dest)
                sizes.remove((sz, (src, dest)))
                break

    return cycle_vars
