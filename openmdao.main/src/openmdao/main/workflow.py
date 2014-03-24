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
        self._nested_wflow = None
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

    def run(self, ffd_order=0, case_id=''):
        """ Run the Components in this Workflow in serial. """
        if self._nested_wflow is not None:
            self._nested_wflow.run(ffd_order, case_id)
            return

        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)

        for comp in self._iterator:
            self._run_comp(comp, ffd_order, case_id, iterbase)
        self._iterator = None

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

    def setup_communicators(self, scope):
        """Allocate communicators from here down to all of our
        child Components.
        """
        # at the serial workflow level, all components use all of the
        # available processors, while in a parallel workflow, available
        # processors are shared between the components in the workflow.
        # The max needed processors for a serial flow will be the max
        # required by any component in that flow, while the cpus
        # required for a parallel flow will be the sum of the required
        # processors for the parallel components.
        
        comm = self.mpi.comm

        # first, get the component subgraph that is limited to 
        # the components in this workflow.
        cgraph = self.get_comp_graph().copy()

        for node, data in cgraph.nodes_iter(data=True):
            data['req_cpus'] = getattr(scope, node).get_cpu_range()[0]

        transform_graph(cgraph)

        # now we have a collapsed graph with the parallel
        # comps collapsed into single nodes with tuple names
        if len(cgraph.edges()) > 0:
            self._system = SerialSystem(cgraph)
        else:
            self._system = ParallelSystem(cgraph)

        self._system.mpi.comm = comm
        self._system.setup_communicators(scope)

        # for node in cgraph.graph['topo']:
        #     if isinstance(node, tuple):
        #         # it's a parallel group, so divide up the processors
        #         self._setup_parallel_comms(node)
        #     else: # it's serial, so give all processors to this comp
        #         getattr(scope, node).mpi.comm = comm

    # def _setup_parallel_comms(self, nodes):
    #     mpiprint("_setup_parallel_comms: %s" % list(nodes))
    #     local_comps = []

    #     scope = self.scope
    #     cgraph = self._system.graph
    #     comm = self.mpi.comm

    #     size = comm.size
    #     child_comps = [getattr(scope, n) for n in nodes]
        
    #     cpus = [c.get_cpu_range() for c in child_comps]
    #     requested_procs = [c[0] for c in cpus]
    #     assigned_procs = [0 for c in cpus]

    #     assigned = 0

    #     requested = sum(requested_procs)
    #     limit = min(size, requested)

    #     mpiprint("requested = %d" % requested)

    #     # first, just use simple round robin assignment of requested CPUs
    #     # until everybody has what they asked for or we run out
    #     while assigned < limit:
    #         mpiprint("assigned, limit = %d, %d" % (assigned, limit))
    #         for i, comp in enumerate(child_comps):
    #             if requested_procs[i] == 0: # skip and deal with these later
    #                 continue
    #             if assigned_procs[i] < requested_procs[i]:
    #                 assigned_procs[i] += 1
    #                 assigned += 1
    #                 if assigned == limit:
    #                     break

    #     if requested:
    #         # now, if we have any procs left after assigning all the requested 
    #         # ones, allocate any remaining ones to any comp that can use them
    #         limit = size
    #         while assigned < limit:
    #             for i, comp in enumerate(child_comps):
    #                 if requested_procs[i] == 0: # skip and deal with these later
    #                     continue
    #                 #if assigned_procs[i] < max_procs[i] or max_procs[i] is None:
    #                 assigned_procs[i] += 1
    #                 assigned += 1
    #                 if assigned == limit:
    #                     break

    #     color = []
    #     for i, procs in enumerate([p for p in assigned_procs if p != 0]):
    #         color.extend([i]*procs)

    #     if size > assigned:
    #         color.extend([MPI.UNDEFINED]*(size-assigned))

    #     rank = self.mpi.comm.rank

    #     sub_comm = comm.Split(color[rank])

    #     if sub_comm == MPI.COMM_NULL:
    #         mpiprint("null comm")
    #         return
    #     else:
    #         mpiprint("subcomm size = %d" % sub_comm.size)

    #     rank_color = color[rank]
    #     for i,c in enumerate(child_comps):
    #         if i == rank_color:
    #             c.mpi.comm = sub_comm
    #             c.mpi.cpus = assigned_procs[i]
    #             local_comps.append(c)
    #         elif assigned_procs[i] == 0:  # comp is duplicated everywhere
    #             c.mpi.comm = sub_comm  # TODO: make sure this is the right comm
    #             local_comps.append(c)

    #     for comp in local_comps:
    #         if hasattr(c, 'setup_communicators'):
    #             c.setup_communicators()

    #     # store local_comps in the graph for later use during run()
    #     cgraph.node[nodes]['local_comps'] = local_comps

class System(object):
    def __init__(self, graph):
        self.graph = graph
        self.local_comps = []
        self.mpi = MPI_info()
        
class SerialSystem(System):
    def __init__(self, graph):
        super(SerialSystem, self).__init__(graph)
        self.req_cpus = max([graph.node[n]['req_cpus'] for n in graph])

    def setup_communicators(self, scope):
        comm = self.mpi.comm
        for name in topological_sort(self.graph):
            data = self.graph.node[name]
            if 'system' in data: # nested workflow
                comp = data['system']
            else:
                comp = getattr(scope, name)
            comp.mpi.comm = comm
            self.local_comps.append(comp)
            if hasattr(comp, 'setup_communicators'):
                comp.setup_communicators(scope)

    def run(self, force=False, ffd_order=0, case_id=''):
        pass

class ParallelSystem(System):
    def __init__(self, graph):
        super(ParallelSystem, self).__init__(graph)
        self.req_cpus = sum([graph.node[n]['req_cpus'] for n in graph])

    def setup_communicators(self, scope):
        local_comps = []

        comm = self.mpi.comm
        size = comm.size
        rank = comm.rank

        child_comps = []
        requested_procs = []
        for name, data in self.graph.nodes_iter(data=True):
            if 'system' in data: # nested workflow
                child_comps.append(data['system'])
                requested_procs.append(child_comps[-1].req_cpus)
            else:
                child_comps.append(getattr(scope, name))
                requested_procs.append(child_comps[-1].get_cpu_range()[0])
        
        assigned_procs = [0]*len(requested_procs)

        assigned = 0

        requested = sum(requested_procs)
        limit = min(size, requested)

        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        while assigned < limit:
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
            limit = size
            while assigned < limit:
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

        sub_comm = comm.Split(color[rank])

        if sub_comm == MPI.COMM_NULL:
            mpiprint("null comm")
            return
        else:
            mpiprint("subcomm size = %d for graph(%s)" % (sub_comm.size,self.graph.nodes()))

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
                c.setup_communicators(scope)

        self.local_comps = local_comps
        

def transform_graph(g):
    """Return a nested graph with metadata for parallel
    and serial subworkflows.
    """
    if len(g) <= 1:
        return g

    gcopy = g.copy()

    while len(gcopy) > 1:
        # find all nodes with in degree 0. If we find 
        # more than one, we can execute them in parallel
        zero_in_nodes = [n for n in gcopy.nodes_iter() 
                            if gcopy.in_degree(n)==0]

        if len(zero_in_nodes) > 1: # start of parallel chunk
            parallel_group = []
            for node in zero_in_nodes:
                brnodes = [node]
                brnodes.extend(get_branch(gcopy, node))
                if len(brnodes) > 1:
                    parallel_group.append(tuple(brnodes))
                else:
                    parallel_group.append(brnodes[0])

            for branch in parallel_group:
                if isinstance(branch, tuple):
                    _collapse(g, branch)
                    gcopy.remove_nodes_from(branch)
                else:
                    gcopy.remove_node(branch)

            parallel_group = tuple(parallel_group)
            _collapse(g, parallel_group, serial=False)
        else:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)
    

def _collapse(g, nodes, serial=True):
    """Collapse the named nodes in g into a single node
    with a name that is a tuple of nodes and put the
    subgraph containing those nodes into the new node's
    metadata.

    If serial is True, also transform the new subgraph by
    locating any internal parallel branches.
    """
    # combine node names into a single tuple
    newname = tuple(nodes)

    # create a subgraph containing all of the collapsed nodes
    # inside of the new node
    subg = g.subgraph(nodes).copy()

    new_edges = []
    for node in nodes:
        new_edges.extend([(newname,v) for v in g.successors_iter(node)])
        new_edges.extend([(u,newname) for u in g.predecessors_iter(node)])
        
    g.add_node(newname)
    g.add_edges_from(new_edges)
    g.remove_nodes_from(nodes) # must do this after adding edges because otherwise we 
                               # get some edges to removed nodes that we don't want

    if serial:
        transform_graph(subg)
        subsys = SerialSystem(subg)
    else:
        subsys = ParallelSystem(subg)

    g.node[newname]['system'] = subsys
    g.node[newname]['req_cpus'] = subsys.req_cpus
 

def get_branch(g, node, visited=None):
    """Return the full list of nodes that branch *exclusively*
    from the given node.  The starting node is not included in 
    the list.
    """
    if visited is None:
        visited = set()
    visited.add(node)
    branch = []
    for succ in g.successors(node):
        for p in g.predecessors(succ):
            if p not in visited:
                break
        else:
            branch.append(succ)
            branch.extend(get_branch(g, succ, visited))
    return branch

# def find_graph_partitions(g, parallels=None):#, serials=None, current_serial=None):
#     """Chops the graph up into parallel branches and serial
#     parts.  Does not recursively partition subgraphs.  Returns
#     a tuple of tuples of parallel branches. All nodes not included
#     in a parallel branch are serial.
#     """
#     if parallels is None:
#         parallels = []
#     # if serials is None:
#     #     serials = []

#     # find all nodes with in degree 0. If we find 
#     # more than one, we can execute them in parallel
#     zero_in_nodes = [n for n in g.nodes_iter() 
#                         if g.in_degree(n)==0]

#     if len(zero_in_nodes) > 1: # start of parallel chunk
#         parallel_group = []
#         to_remove = []
#         for node in zero_in_nodes:
#             brnodes = [node]
#             brnodes.extend(get_branch(g, node))
#             parallel_group.append(tuple(brnodes))
#             to_remove.extend(brnodes)
#         parallels.append(tuple(parallel_group))
#         g.remove_nodes_from(to_remove)
#         # if current_serial:
#         #     serials.append(tuple(current_serial))
#         #     current_serial = None
#     else:  # serial
#         # if current_serial is None:
#         #     current_serial = zero_in_nodes[:]
#         # else:
#         #     current_serial.append(zero_in_nodes[0])
#         g.remove_nodes_from(zero_in_nodes)

#     if len(g) > 1:
#         return find_graph_partitions(g, parallels)#, serials, current_serial)

#     return parallels #, serials
    
import networkx
g = networkx.DiGraph()
nodes = range(1,15)
g.add_edges_from([(1,2),(2,3),(2,4),(4,5),(3,5),(3,6),(5,7),(6,7),
                  (7,8),(9,10),(10,11),(11,12),(11,13),(12,14),(13,14),(14,8)])
for n in nodes:
    g.node[n]['req_cpus'] = 1
    
print "branch 1 = %s" % get_branch(g, 1)
print "branch 9 = %s" % get_branch(g, 9)
print "branch 3 = %s" % get_branch(g, 3)
print "branch 11 = %s" % get_branch(g, 11)
transform_graph(g)
print g.nodes()

