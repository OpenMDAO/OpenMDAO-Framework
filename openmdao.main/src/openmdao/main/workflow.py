""" Base class for all workflows. """

from networkx import topological_sort

# pylint: disable-msg=E0611,F0401
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, parent=None, scope=None, members=None):
        """Create a Workflow.

        parent: Driver (optional)
            The Driver that contains this Workflow.  This option is normally
            passed instead of scope because scope usually isn't known at
            initialization time.  If scope is not provided, it will be
            set to parent.parent, which should be the Assembly that contains
            the parent Driver.

        scope: Component (optional)
            The scope can be explicitly specified here, but this is not
            typically known at initialization time.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
        """
        self._iterator = None
        self._stop = False
        self._parent = parent
        self._scope = scope
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._wf_graph = None
        self._wf_comp_graph = None
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
        return self._iterbase('')

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

    def _run_comp(self, comp, ffd_order, case_id, iterbase):
        if isinstance(comp, PseudoComponent):
            comp.run(ffd_order=ffd_order, case_id=case_id)
        else:
            self._comp_count += 1
            comp.set_itername('%s-%d' % (iterbase, self._comp_count))
            comp.run(ffd_order=ffd_order, case_id=case_id)
        if self._stop:
            raise RunStopped('Stop requested')
        
    def _mixed_run(self, ffd_order=0, case_id=''):
        """Run the components in ths workflow in a mixed serial/parallel
        mode.
        """
        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)
        scope = self.scope

        for cname in self._sorted_comps:
            if isinstance(cname, tuple):
                for comp in self._collapsed_cgraph.node[cname]['local_comps']:
                    self._run_comp(comp, ffd_order, case_id, iterbase)
            else:
                self._run_comp(getattr(scope, cname), ffd_order, case_id, iterbase)

        self._iterator = None

    def _serial_run(self, ffd_order=0, case_id=''):
        """ Run the Components in this Workflow in serial. """

        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)

        for comp in self._iterator:
            self._run_comp(comp, ffd_order, case_id, iterbase)
        self._iterator = None

    run = _serial_run

    def _iterbase(self, case_id):
        """ Return base for 'iteration coordinates'. """
        if self._parent is None:
            return str(self._exec_count)  # An unusual case.
        else:
            prefix = self._parent.get_itername()
            if not prefix:
                prefix = case_id
            if prefix:
                prefix += '.'
            return '%s%d' % (prefix, self._exec_count)

    def step(self, ffd_order=0, case_id=''):
        """Run a single component in this Workflow."""
        if self._iterator is None:
            self._iterator = self.__iter__()
            self._exec_count += 1
            self._comp_count = 0

        comp = self._iterator.next()
        self._comp_count += 1
        iterbase = self._iterbase(case_id)
        comp.set_itername('%s-%d' % (iterbase, self._comp_count))
        try:
            comp.run(ffd_order=ffd_order, case_id=case_id)
        except StopIteration, err:
            self._iterator = None
            raise err
        raise RunStopped('Step complete')

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
        self._wf_graph = None
        self._wf_comp_graph = None

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

    def get_graph(self):
        """Returns the subgraph of the full depgraph that is
        relevant to this workflow.
        """
        if self._wf_graph is None:
            dgraph = self.scope._depgraph
            self.wf_graph = dgraph.full_subgraph(
                                      self.get_names(full=True))
        return self.wf_graph

    def get_comp_graph(self):
        """Returns the subgraph of the component graph that contains
        the components in this workflow.
        """
        if self._wf_comp_graph is None:
            cgraph = self.scope._depgraph.component_graph()
            self._wf_comp_graph = cgraph.subgraph(self.get_names(full=True))
        return self._wf_comp_graph

    ## MPI stuff ##

    def setup_sizes(self):
        for comp in self:
            comp.setup_sizes()

    # def setup_communicators(self):
    #     """Allocate communicators from here down to all of our
    #     child Components.
    #     """
    #     comm = self.mpi.comm

    #     for comp in self:
    #         comp.mpi.comm = comm
    #         comp.setup_communicators()

    def setup_communicators(self):
        """Allocate communicators from here down to all of our
        child Components.
        """
        # algorithm: 
        # break the full workflow subgraph down into a serial wflow
        # of parallel chunks by starting with the set of nodes with
        # in degree of 0 and put them in a parallel wflow. Then collapse
        # those into a single node and create a new prallel wflow of 
        # the set of successors that are only dependent on the nodes
        # already visited.

        # at the serial workflow level, all components use all of the
        # available processors, while in a parallel workflow, available
        # processors are shared between the components in the workflow.
        
        # change our run function to allow for parallel execution
        self.run = self._mixed_run

        scope = self.scope
        comm = self.mpi.comm

        # first, get the component subgraph that is limited to 
        # the components in this workflow.
        cgraph = self.get_comp_graph().copy()

        remaining = set(cgraph.nodes_iter())

        # start with nodes having in degree of 0
        nodes = [n for n in cgraph.nodes_iter() 
                            if cgraph.in_degree(n)==0]   

        assert len(nodes) > 0

        while remaining:
            newnodes = set()
            for node in nodes:
                newnodes.update(cgraph.successors_iter(node))
            if len(nodes) > 1:
                _collapse(cgraph, nodes)
            remaining.difference_update(nodes)
            nodes = [n for n in newnodes 
                      if not set(cgraph.predecessors(n)).intersection(remaining)]

        # now we have a collapsed graph with the parallel
        # comps collapsed into single nodes with tuple names
        self._collapsed_cgraph = cgraph
        self._sorted_comps = topological_sort(cgraph)

        for node in self._sorted_comps:
            if isinstance(node, tuple):
                # it's a parallel group, so divide up the processors
                self._setup_parallel_comms(node)
            else: # it's serial, so give all processors to this comp
                getattr(scope, node).mpi.comm = comm

    def _setup_parallel_comms(self, nodes):
        mpiprint("_setup_parallel_comms: %s" % list(nodes))
        local_comps = []

        scope = self.scope
        cgraph = self._collapsed_cgraph
        comm = self.mpi.comm

        size = comm.size
        child_comps = [getattr(scope, n) for n in nodes]
        
        cpus = [c.get_cpu_range() for c in child_comps]
        requested_procs = [c[0] for c in cpus]
        assigned_procs = [0 for c in cpus]
        #max_procs = [c[1] for c in cpus]

        # # if get_max_cpus() returns None, it means that comp can use
        # # as many cpus as we can give it
        # if None in max_procs:
        #     max_usable = size
        # else:
        #     max_usable = sum(max_procs)

        assigned = 0 #sum(assigned_procs)
        #unassigned = size - assigned
        # if unassigned < 0:
        #     raise RuntimeError("Allocated CPUs is short by %d" % -unassigned)

        requested = sum(requested_procs)
        limit = min(size, requested)

        mpiprint("requested = %d" % requested)
        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        while assigned < limit:
            mpiprint("assigned, limit = %d, %d" % (assigned, limit))
            for i, comp in enumerate(child_comps):
                if requested_procs[i] == 0: # skip and deal with these later
                    continue
                if assigned_procs[i] < requested_procs[i]:
                    assigned_procs[i] += 1
                    assigned += 1
                    if assigned == limit:
                        break

        if requested:
            # now, if we have any procs left after assigning all the requested 
            # ones, allocate any remaining ones to any comp that can use them
            limit = size # min(size, max_usable)
            while assigned < limit:
                mpiprint("assigned, limit = %d, %d" % (assigned, limit))
                for i, comp in enumerate(child_comps):
                    if requested_procs[i] == 0: # skip and deal with these later
                        continue
                    #if assigned_procs[i] < max_procs[i] or max_procs[i] is None:
                    assigned_procs[i] += 1
                    assigned += 1
                    if assigned == limit:
                        break

        color = []
        for i, procs in enumerate([p for p in assigned_procs if p != 0]):
            color.extend([i]*procs)

        if size > assigned:
            color.extend([MPI.UNDEFINED]*(size-assigned))

        rank = self.mpi.comm.rank
        mpiprint("splitting")
        sub_comm = comm.Split(color[rank])

        if sub_comm == MPI.COMM_NULL:
            mpiprint("null comm")
        else:
            mpiprint("subcomm size = %d" % sub_comm.size)

        rank_color = color[rank]
        for i,c in enumerate(child_comps):
            if i == rank_color:
                c.mpi.comm = sub_comm
                c.mpi.cpus = assigned_procs[i]
                local_comps.append(c)
            elif assigned_procs[i] == 0:  # comp is duplicated everywhere
                c.mpi.comm = sub_comm  # TODO: make sure this is the right comm
                local_comps.append(c)

        for comp in local_comps:
            if hasattr(c, 'setup_communicators'):
                c.setup_communicators()

        # store local_comps in the graph for later use during run()
        cgraph.node[nodes]['local_comps'] = local_comps


def _collapse(g, nodes):
    """Collapse the named nodes in g into a single node
    with a name that is a tuple of nodes.
    """
    # combine node names into a single tuple
    newname = tuple(nodes)
    g.add_node(newname)
    for node in nodes:
        for u,v in g.edges(node):
            g.add_edge(newname, v)
        g.remove_node(node)

def get_branch(g, node, visited=None):
    """Return the full set of nodes that branch *exclusively*
    from the given node.
    """
    if visited is None:
        visited = set()
    visited.add(node)
    branch = []
    for succ in g.successors(node):
        preds = g.predecessors(succ)
        for p in preds:
            if p not in visited:
                break
        else:
            branch.append(succ)
            branch.extend(get_branch(g, succ, visited))
    return branch


# import networkx
# g = networkx.DiGraph()
# g.add_edges_from([(1,2),(2,3),(2,4),(4,5),(3,5),(3,6),(5,7),(6,7),
#                   (7,8),(9,10),(10,11),(11,12),(11,13),(12,14),(13,14),(14,8)])
# print "branch 1 = %s" % get_branch(g, 1)
# print "branch 9 = %s" % get_branch(g, 9)
# print "branch 3 = %s" % get_branch(g, 3)
# print "branch 11 = %s" % get_branch(g, 11)
