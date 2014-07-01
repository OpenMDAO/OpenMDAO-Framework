""" Base class for all workflows. """

from fnmatch import fnmatch
from traceback import format_exc
import weakref

# pylint: disable-msg=E0611,F0401
from openmdao.main.case import Case
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint
from openmdao.main.systems import SerialSystem, ParallelSystem, \
                                  partition_mpi_subsystems, \
                                  get_comm_if_active, _create_simple_sys, \
                                  get_full_nodeset
from openmdao.main.depgraph import _get_inner_connections
from openmdao.main.exceptions import RunStopped, TracedError

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order during a single iteration of a Driver.
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
        self._parent = None
        self.parent = parent
        self._stop = False
        self._scope = None
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._wf_comp_graph = None
        self._var_graph = None
        self._system = None

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
        try:
            self._system.run(iterbase=iterbase, ffd_order=ffd_order,
                                case_uuid=case_uuid)

            if self._stop:
                raise RunStopped('Stop requested')
        except Exception as exc:
            err = TracedError(exc, format_exc())

        if record_case and self._rec_required:
            try:
                self._record_case(case_uuid, err)
            except Exception as exc:
                if err is None:
                    err = TracedError(exc, format_exc())
                self.parent._logger.error("Can't record case: %s", exc)

        if err is not None:
            err.reraise(with_traceback=False)

    def calc_gradient(self, inputs=None, outputs=None,
                      upscope=False, mode='forward'):

        # TODO - Support automatic determination of mode

        return self._system.calc_gradient(inputs, outputs, mode)


    def configure_recording(self, includes, excludes):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""
        if not includes:
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
                if self._check_path(path, includes, excludes):
                    self._rec_parameters.append(param)
                    inputs.append(name)

        # Objectives
        self._rec_objectives = []
        if hasattr(driver, 'eval_objectives'):
            for key, objective in driver.get_objectives().items():
                name = objective.pcomp_name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_objectives.append(key)
                    outputs.append(name)

        # Responses
        self._rec_responses = []
        if hasattr(driver, 'get_responses'):
            for key, response in driver.get_responses().items():
                name = response.pcomp_name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_responses.append(key)
                    outputs.append(name)

        # Constraints
        self._rec_constraints = []
        if hasattr(driver, 'get_eq_constraints'):
            for con in driver.get_eq_constraints().values():
                name = con.pcomp_name
                path = prefix+name
                if self._check_path(path, includes, excludes):
                    self._rec_constraints.append(con)
                    outputs.append(name)
        if hasattr(driver, 'get_ineq_constraints'):
            for con in driver.get_ineq_constraints().values():
                name = con.pcomp_name
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
        if hasattr(driver, 'get_eq_constraints'):
            dsts.extend(constraint.pcomp_name+'.out0'
                        for constraint in driver.get_eq_constraints().values())
        if hasattr(driver, 'get_ineq_constraints'):
            dsts.extend(constraint.pcomp_name+'.out0'
                        for constraint in driver.get_ineq_constraints().values())

        graph = scope._depgraph
#        graph = scope._depgraph.full_subgraph(self.get_names(full=True))
        for src, dst in _get_inner_connections(graph, srcs, dsts):
            if scope.get_metadata(src)['iotype'] == 'in':
                continue
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
            top = scope
            while top.parent is not None:
                top = top.parent
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
        driver = self.parent
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

    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the workflow by name."""
        raise NotImplementedError("This Workflow has no 'add' function")

    def config_changed(self):
        """Notifies the Workflow that workflow configuration
        (dependencies, etc.) has changed.
        """
        self._wf_comp_graph = None
        self._var_graph = None
        self._system = None

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

    ## MPI stuff ##

    def setup_systems(self):
        """Get the subsystem for this workflow. Each
        subsystem contains a subgraph of this workflow's component
        graph, which contains components and/or other subsystems.
        """
        scope = self.scope
        depgraph = self.parent.get_depgraph()

        cgraph = depgraph.component_graph()
        cgraph = cgraph.subgraph(self.get_names(full=True))

        # collapse driver iteration sets into a single node for
        # the driver, except for nodes from their iteration set
        # that are in the iteration set of their parent driver.
        self.parent._collapse_subdrivers(cgraph)

        # create systems for all simple components
        for node, data in cgraph.nodes_iter(data=True):
            if isinstance(node, basestring):
                data['system'] = _create_simple_sys(depgraph, scope, getattr(scope, node))

        # collapse the graph (recursively) into nodes representing
        # subsystems
        if MPI:
            cgraph = partition_mpi_subsystems(depgraph, cgraph, scope)

            if len(cgraph) > 1:
                if len(cgraph.edges()) > 0:
                    #mpiprint("creating serial top: %s" % cgraph.nodes())
                    self._system = SerialSystem(scope, depgraph, cgraph, tuple(cgraph.nodes()))
                else:
                    #mpiprint("creating parallel top: %s" % cgraph.nodes())
                    self._system = ParallelSystem(scope, depgraph, cgraph, str(tuple(cgraph.nodes())))
            elif len(cgraph) == 1:
                name = cgraph.nodes()[0]
                self._system = cgraph.node[name].get('system')
            else:
                raise RuntimeError("setup_systems called on %s.workflow but component graph is empty!" %
                                    self.parent.get_pathname())
        else:
            self._system = SerialSystem(scope, depgraph, cgraph, str(tuple(cgraph.nodes())))

        self._system.set_ordering([c.name for c in self])

        for comp in self:
            comp.setup_systems()

    def get_req_cpus(self):
        """Return requested_cpus"""
        if self._system is None:
            return 1
        else:
            return self._system.get_req_cpus()

    def setup_communicators(self, comm):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.mpi.comm = get_comm_if_active(self, comm)
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        #mpiprint("workflow for %s: setup_comms" % self.parent.name)
        self._system.setup_communicators(self.mpi.comm)

    def setup_variables(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        return self._system.setup_variables()

    def setup_sizes(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        return self._system.setup_sizes()

    def setup_vectors(self, arrays=None):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._system.setup_vectors(arrays)

    def setup_scatters(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._system.setup_scatters()

    def get_full_nodeset(self):
        """Return the set of nodes in the depgraph
        belonging to this driver (inlcudes full iteration set).
        """
        return set([c.name for c in self.parent.iteration_set()])
