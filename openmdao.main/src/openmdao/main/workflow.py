""" Base class for all workflows. """

#from networkx.algorithms.components import strongly_connected_components

# pylint: disable-msg=E0611,F0401
from openmdao.main.case import Case

from openmdao.main.mpiwrap import MPI, MPI_info
from openmdao.main.systems import SerialSystem, ParallelSystem, \
                                  partition_mpi_subsystems, partition_subsystems, \
                                  collapse_subdrivers, get_comm_if_active, _create_simple_sys
from openmdao.util.nameutil import partition_names_by_comp

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
        self._stop = False
        self._parent = parent
        self._scope = None
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._wf_comp_graph = None
        self._var_graph = None
        self._subsystem = None

        if members:
            for member in members:
                if not isinstance(member, basestring):
                    raise TypeError("Components must be added to a workflow by name.")
                self.add(member)

        self.mpi = MPI_info()

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

    # def run(self, ffd_order=0, case_label='', case_uuid=None):
    #     """ Run the Components in this Workflow. """
    #     subsys = self._subsystem
    #     # if self._subsystem is not None:
    #     #     return self._subsystem.run()#self.scope, ffd_order, case_id, self._iterbase(case_id))

    #     self._stop = False
    #     self._exec_count += 1

    #     #iterbase = self._iterbase()

    #     if case_uuid is None:
    #         # We record the case and are responsible for unique case ids.
    #         record_case = True
    #         case_uuid = Case.next_uuid()
    #     else:
    #         record_case = False

    #     #scope = self.scope

    #     # for comp in self:
    #     #     # before the workflow runs each component, update that
    #     #     # component's inputs based on the graph
    #     #     scope.update_inputs(comp.name, graph=self._var_graph)
    #     #     if isinstance(comp, PseudoComponent):
    #     #         comp.run(ffd_order=ffd_order)
    #     #     else:
    #     #         comp.set_itername('%s-%s' % (iterbase, comp.name))
    #     #         comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
    #     #     if self._stop:
    #     #         raise RunStopped('Stop requested')

    #     subsys.run()

    #     if record_case:
    #         self._record_case(label=case_label, case_uuid=case_uuid)

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
        self._wf_comp_graph = None
        self._var_graph = None
        self._subsystem = None

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

    def get_comp_graph(self):
        """Returns the subgraph of the component graph that contains
        the components in this workflow, including additional connections
        needed due to subdriver iterations.
        """
        if self._wf_comp_graph is None:
            cgraph = self.scope._depgraph.component_graph().copy()

            topdrv = self.scope._top_driver
            srcs, dests = topdrv.get_expr_var_depends(recurse=True,
                                                      refs=True)

            #mpiprint("DRIVER SRCS: %s" % srcs)
            #mpiprint("DRIVER DESTS: %s" % dests)
            wfnames = set(self.get_names(full=True))
            self._wf_comp_graph = wfgraph = cgraph.subgraph(wfnames)

            # get ALL driver inputs and outputs and label the
            # appropriate graph nodes so we can use that during
            # decomposition
            comp_ins = partition_names_by_comp(dests)
            comp_outs = partition_names_by_comp(srcs)
            for cname, inputs in comp_ins.items():
                if cname in wfnames:
                    drvins = wfgraph.node[cname].setdefault('drv_inputs',set())
                    for inp in inputs:
                        drvins.add('.'.join((cname,inp)))
            for cname, outputs in comp_outs.items():
                if cname in wfnames:
                    drvouts = wfgraph.node[cname].setdefault('outputs',set())
                    for out in outputs:
                        drvouts.add('.'.join((cname,out)))

        return self._wf_comp_graph
     
    ## MPI stuff ##

    def setup_systems(self):
        """Get the subsystem for this workflow. Each
        subsystem contains a subgraph of this workflow's component 
        graph, which contains components and/or other subsystems.
        """
        scope = self.scope

        #mpiprint("depgraph = %s" % self.scope._depgraph.edges())
        #drvgraph = self.get_driver_graph()

        # first, get the component subgraph that is limited to 
        # the components in this workflow, but has extra edges
        # due to driver dependencies.
        cgraph = self.get_comp_graph().copy()

        #mpiprint("cgraph1 = %s" % cgraph.edges())
        # collapse driver iteration sets into a single node for
        # the driver, except for nodes from their iteration set
        # that are in the iteration set of their parent.
        collapse_subdrivers(cgraph, self._parent)
        #cgraph.remove_node(self._parent.name)

        #mpiprint("cgraph2 = %s" % cgraph.edges())
        #mpiprint("**** %s: cgraph edges (pre-xform) = %s" % (self._parent.name,cgraph.edges()))

        # collapse the graph (recursively) into nodes representing
        # serial and parallel subsystems
        if MPI:
            partition_mpi_subsystems(cgraph, scope, self)
            #mpiprint("**** %s: cgraph nodes (post-xform) = %s" % (self._parent.name,cgraph.nodes()))
            #mpiprint("**** %s: cgraph edges (post-xform) = %s" % (self._parent.name,cgraph.edges()))
            
            #mpiprint("cgraph3 = %s" % cgraph.edges())

            if len(cgraph) > 1:
                if len(cgraph.edges()) > 0:
                    #mpiprint("creating serial top: %s" % cgraph.nodes())
                    self._subsystem = SerialSystem(cgraph, scope, self,
                                                   tuple(sorted(cgraph.nodes())))
                else:
                    #mpiprint("creating parallel top: %s" % cgraph.nodes())
                    self._subsystem = ParallelSystem(cgraph, scope, self,
                                                     tuple(sorted(cgraph.nodes())))
            elif len(cgraph) == 1:
                name = cgraph.nodes()[0]
                self._subsystem = cgraph.node[name].get('system')
                if self._subsystem is None:
                    _create_simple_sys(cgraph, scope, name)
                    self._subsystem = cgraph.node[name]['system']
            else:
                raise RuntimeError("get_subsystem called on %s.workflow but component graph is empty!" %
                                    self._parent.get_pathname())
        else:
            partition_subsystems(cgraph, scope, self)
            self._subsystem = SerialSystem(cgraph, scope, self,
                                           tuple(sorted(cgraph.nodes())))
            
        for comp in self:
            comp.setup_systems()
            
    def get_req_cpus(self):
        """Return requested_cpus"""
        if self._subsystem is None:
            return 1
        else:
            return self._subsystem.get_req_cpus()

    def setup_communicators(self, comm, scope):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.mpi.comm = get_comm_if_active(self, comm)
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._subsystem.setup_communicators(self.mpi.comm, scope)

    def setup_variables(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        return self._subsystem.setup_variables()

    def setup_sizes(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        return self._subsystem.setup_sizes()

    def setup_vectors(self, arrays=None):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._subsystem.setup_vectors(arrays)

    def setup_scatters(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        self._subsystem.setup_scatters()
