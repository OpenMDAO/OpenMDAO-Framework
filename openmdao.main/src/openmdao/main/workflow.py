""" Base class for all workflows. """

from networkx import topological_sort

# pylint: disable-msg=E0611,F0401
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint
from openmdao.main.interfaces import obj_has_interface, IComponent

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

    def run(self, ffd_order=0, case_id=''):
        """ Run the Components in this Workflow. """
        if self._subsystem is not None:
            return self._subsystem.run(self.scope, 
                                       ffd_order, 
                                       case_id, self._iterbase(case_id))

        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)

        for comp in self._iterator:
            if isinstance(comp, PseudoComponent):
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                self._comp_count += 1
                comp.set_itername('%s-%d' % (iterbase, self._comp_count))
                comp.run(ffd_order=ffd_order, case_id=case_id)
            if self._stop:
                raise RunStopped('Stop requested')
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

    def setup_communicators(self, scope):
        """Allocate communicators from here down to all of our
        child Components.
        """
        # at the serial workflow level, all components use all of the
        # available processors, while in a parallel workflow, available
        # processors are shared between the components in the workflow.
        # The requested processors for a serial flow will be the max
        # requested by any component in that flow, while the cpus
        # requested for a parallel flow will be the sum of the requested
        # processors for the parallel components.
        
        comm = self.mpi.comm

        # first, get the component subgraph that is limited to 
        # the components in this workflow.
        cgraph = self.get_comp_graph().copy()

        for node, data in cgraph.nodes_iter(data=True):
            data['req_cpus'] = getattr(scope, node).get_cpu_range()[0]

        # collapse the graph (recursively) with the parallel
        # branches collapsed into single nodes with tuple names
        transform_graph(cgraph)

        if len(cgraph.edges()) > 0:
            mpiprint("creating serial top: %s" % cgraph.nodes())
            self._subsystem = SerialSystem(cgraph)
        else:
            mpiprint("creating parallel top: %s" % cgraph.nodes())
            self._subsystem = ParallelSystem(cgraph)

        self._subsystem.mpi.comm = comm
        self._subsystem.setup_communicators(scope)


class System(object):
    def __init__(self, graph):
        self.graph = graph
        self.name = str(tuple(sorted(graph.nodes())))
        self.local_comps = []
        self.mpi = MPI_info()
        
class SerialSystem(System):
    def __init__(self, graph):
        super(SerialSystem, self).__init__(graph)
        self.req_cpus = max([graph.node[n]['req_cpus'] for n in graph])

    def run(self, scope, ffd_order, case_id, iterbase):
        #mpiprint("running serial system %s: %s" % (self.name, [c.name for c in self.local_comps]))
        for i, comp in enumerate(self.local_comps):
            # scatter(...)
            if isinstance(comp, System):
                comp.run(scope, ffd_order, case_id, iterbase)
            elif isinstance(comp, PseudoComponent):
                mpiprint("running %s" % comp.name)
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                comp.set_itername('%s-%d' % (iterbase, i))
                mpiprint("running %s" % comp.name)
                comp.run(ffd_order=ffd_order, case_id=case_id)

    def setup_communicators(self, scope):
        comm = self.mpi.comm
        self.local_comps = []
        #mpiprint("setting up comms for serial %s (%s)" % (self.name, id(self)))
        # if comm != MPI.COMM_NULL:
        #     mpiprint("size=%d" % comm.size)
        # else:
        #     mpiprint("null comm!")
        for name in topological_sort(self.graph):
            data = self.graph.node[name]
            if 'system' in data: # nested workflow
                comp = data['system']
            else:
                comp = getattr(scope, name)
            #mpiprint("*** serial child %s" % comp.name)
            comp.mpi.comm = comm
            self.local_comps.append(comp)
            if hasattr(comp, 'setup_communicators'):
                comp.setup_communicators(scope)


class ParallelSystem(System):
    def __init__(self, graph):
        super(ParallelSystem, self).__init__(graph)
        self.req_cpus = sum([graph.node[n]['req_cpus'] for n in graph])

    def run(self, scope, ffd_order, case_id, iterbase):
        #mpiprint("running parallel system %s: %s" % (self.name, [c.name for c in self.local_comps]))
        # don't scatter unless we contain something that's actually 
        # going to run
        if self.local_comps: 
            pass # scatter(...)

        for i, comp in enumerate(self.local_comps):
            if isinstance(comp, System):
                comp.run(scope, ffd_order, case_id, iterbase)
            elif isinstance(comp, PseudoComponent):
                mpiprint("running %s" % comp.name)
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                mpiprint("running %s" % comp.name)
                comp.set_itername('%s-%d' % (iterbase, i))
                comp.run(ffd_order=ffd_order, case_id=case_id)

    def setup_communicators(self, scope):
        comm = self.mpi.comm
        size = comm.size
        rank = comm.rank

        child_comps = []
        requested_procs = []
        for name, data in self.graph.nodes_iter(data=True):
            system = data.get('system')
            if system is not None: # nested workflow
                child_comps.append(system)
                requested_procs.append(system.req_cpus)
                #mpiprint("!! system %s requests %d cpus" % (system.name, system.req_cpus))
            else:
                child_comps.append(getattr(scope, name))
                requested_procs.append(getattr(scope, name).get_cpu_range()[0])
        
        assigned_procs = [0]*len(requested_procs)

        assigned = 0

        requested = sum(requested_procs)
        limit = min(size, requested)

        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        if requested:
            while assigned < limit:
                for i, comp in enumerate(child_comps):
                    if requested_procs[i] == 0: # skip and deal with these later
                        continue
                    if assigned_procs[i] < requested_procs[i]:
                        assigned_procs[i] += 1
                        assigned += 1
                        if assigned == limit:
                            break

            # # now, if we have any procs left after assigning all the requested 
            # # ones, allocate any remaining ones to any comp that can use them
            # limit = size
            # while assigned < limit:
            #     for i, comp in enumerate(child_comps):
            #         if requested_procs[i] == 0: # skip and deal with these later
            #             continue
            #         assigned_procs[i] += 1
            #         assigned += 1
            #         if assigned == limit:
            #             break

        mpiprint("comm size = %d" % comm.size)
        mpiprint("child_comps: %s" % [c.name for c in child_comps])
        mpiprint("requested_procs: %s" % requested_procs)
        mpiprint("assigned_procs: %s" % assigned_procs)

        color = []
        for i, procs in enumerate([p for p in assigned_procs if p > 0]):
            color.extend([i]*procs)

        if size > assigned:
            color.extend([MPI.UNDEFINED]*(size-assigned))

        #mpiprint("color=%s" % color)
        rank_color = color[rank]
        sub_comm = comm.Split(rank_color)

        if sub_comm == MPI.COMM_NULL:
            #mpiprint("null comm")
            return
        # else:
        #     mpiprint("subcomm size = %d for graph(%s)" % (sub_comm.size,self.graph.nodes()))

        local_comps = []

        for i,c in enumerate(child_comps):
            if i == rank_color:
                c.mpi.comm = sub_comm
                c.mpi.cpus = assigned_procs[i]
                #mpiprint(" %s has the right color! (%d)" % (c.name, rank_color))
                local_comps.append(c)
            elif assigned_procs[i] == 0:  # comp is duplicated everywhere
                #mpiprint("0 procs assigned to %s" % c.name)
                c.mpi.comm = sub_comm  # TODO: make sure this is the right comm
                local_comps.append(c)

        self.local_comps = local_comps

        for comp in local_comps:
            if hasattr(comp, 'setup_communicators'):
                #mpiprint("setting up comms for child %s (%s)" % (comp.name, id(comp)))
                comp.setup_communicators(scope)


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
                    parallel_group.append(tuple(sorted(brnodes)))
                else:
                    parallel_group.append(brnodes[0])

            for branch in parallel_group:
                if isinstance(branch, tuple):
                    _collapse(g, branch, serial=True)
                    gcopy.remove_nodes_from(branch)
                else:
                    gcopy.remove_node(branch)

            parallel_group = tuple(sorted(parallel_group))
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
    newname = tuple(sorted(nodes))

    # create a subgraph containing all of the collapsed nodes
    # inside of the new node
    subg = g.subgraph(nodes).copy()

    #mpiprint("collapsing %s" % list(nodes))

    new_edges = []
    for node in nodes:
        new_edges.extend([(newname,v) for v in g.successors_iter(node)])
        new_edges.extend([(u,newname) for u in g.predecessors_iter(node)])
        
    g.add_node(newname)
    g.add_edges_from(new_edges)

    # must do this after adding edges because otherwise we 
    # get some edges to removed nodes that we don't want
    g.remove_nodes_from(nodes) 

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

def dump_parallel_graph(system, nest=0):
    
    print "{%d}" % MPI.COMM_WORLD.rank,
    print " "*nest,
    print system.name,
    typ = "serial" if isinstance(system, SerialSystem) else "parallel"
    print " [%s](req=%d)(rank=%d)" % (typ, system.req_cpus, MPI.COMM_WORLD.rank)

    nest += 3
    for comp in system.local_comps:
        if isinstance(comp, System):
            dump_parallel_graph(comp, nest)
        else:
            print "{%d}" % MPI.COMM_WORLD.rank,
            print " "*nest,
            print comp.name

# import networkx
# g = networkx.DiGraph()
# nodes = range(1,15)
# g.add_edges_from([(1,2),(2,3),(2,4),(4,5),(3,5),(3,6),(5,7),(6,7),
#                   (7,8),(9,10),(10,11),(11,12),(11,13),(12,14),(13,14),(14,8)])
# for n in nodes:
#     g.node[n]['req_cpus'] = 1
    
# print "branch 1 = %s" % get_branch(g, 1)
# print "branch 9 = %s" % get_branch(g, 9)
# print "branch 3 = %s" % get_branch(g, 3)
# print "branch 11 = %s" % get_branch(g, 11)
# transform_graph(g)
# print g.nodes()
# dump_parallel_graph(g)


