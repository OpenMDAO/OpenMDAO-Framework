""" Base class for all workflows. """
import sys
from StringIO import StringIO
from networkx import topological_sort
#from collections import OrderedDict

from networkx import edge_boundary

# pylint: disable-msg=E0611,F0401
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint
#from openmdao.util.nameutil import partition_names_by_comp

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
        the components in this workflow, including additional connections
        needed due to subdriver iterations.
        """
        if self._wf_comp_graph is None:
            cgraph = self.scope._depgraph.component_graph().copy()
            self.add_driver_connections(cgraph)
            self._wf_comp_graph = cgraph.subgraph(self.get_names(full=True))
        return self._wf_comp_graph

    def add_driver_connections(self, g):
        """Add edges in the workflow component graph to represent dependencies
        between a comp and a driver if that comp is connected to any comps 
        inside of the driver's iteration set.
        """
        scope = self.scope
        dconns = []
        for node in g:
            comp = getattr(scope, node)
            if has_interface(comp, IDriver):
                iterset = [c.name for c in comp.iteration_set()]
                for itercomp in iterset:
                    for u,v in g.edges_iter(itercomp):
                        if v not in iterset:
                            dconns.append((node, v))
                    for u,v in g.in_edges_iter(itercomp):
                        if u not in iterset:
                            dconns.append((u, node))

        g.add_edges_from(dconns)

        # now add metadata to indicate which vars are set or retrieved from
        # our parent driver
        srcset, destset = self._parent.get_expr_var_depends()

        # now add variables that are connected inputs or outputs
        wfgraph = self.get_graph()

        conns = wfgraph.list_connections()

        srcset.update([src for src,dest in conns])
        destset.update([dest for src,dest in conns])

        for name in srcset:
            parts = name.split('.', 1)
            if len(parts) > 1 and parts[0] in g:
                g.node[parts[0]].setdefault('inputs', set()).add(parts[1])
                
        for name in destset:
            parts = name.split('.', 1)
            if len(parts) > 1 and parts[0] in g:
                g.node[parts[0]].setdefault('outputs', set()).add(parts[1])
     
    ## MPI stuff ##

    def _get_subsystem(self):
        """Get the serial/parallel subsystem for this workflow. Each
        subsystem contains a subgraph of this workflow's component 
        graph, which contains components and/or other subsystems.
        """
        if self._subsystem is None:
            scope = self.scope

            # first, get the component subgraph that is limited to 
            # the components in this workflow.
            cgraph = self.get_comp_graph().copy()

            # collapse the graph (recursively) with the parallel
            # branches collapsed into single nodes with tuple names
            transform_graph(cgraph, scope)

            if len(cgraph) > 1:
                if len(cgraph.edges()) > 0:
                    #mpiprint("creating serial top: %s" % cgraph.nodes())
                    self._subsystem = SerialSystem(cgraph, scope)
                else:
                    #mpiprint("creating parallel top: %s" % cgraph.nodes())
                    self._subsystem = ParallelSystem(cgraph, scope)
            else:
                self._subsystem = cgraph.node[cgraph.nodes()[0]]['system']

        return self._subsystem

    def get_req_cpus(self):
        """Return requested_cpus"""
        return self._get_subsystem().get_req_cpus()

    # def get_vector_vars(self):
    #     """Assemble an ordereddict of names of variables needed by this
    #     workflow, which includes any that its parent driver(s) reference
    #     in parameters, constraints, or objectives, as well as any used to 
    #     connect any components in this workflow, AND any returned from
    #     calling get_vector_vars on any subsystems in this workflow.
    #     """
    #     return self._get_subsystem().get_vector_vars(self.scope)

    def setup_sizes(self, variables):
        self._get_subsystem().setup_sizes(variables)

    def setup_communicators(self, comm, scope):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.mpi.comm = comm
        self._get_subsystem().setup_communicators(comm, scope)


class System(object):
    def __init__(self, graph):
        self.graph = graph
        self.name = str(tuple(sorted(graph.nodes())))
        self.local_comps = []
        self.mpi = MPI_info()
        self.mpi.req_cpus = None

    def get_req_cpus(self):
        return self.mpi.req_cpus

    def setup_sizes(self, variables):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """
        comps = dict([(c.name, c) for c in self.local_comps])

        for name in variables.keys():
            parts = name.split('.', 1)
            if len(parts) > 1:
                cname, vname = parts
                comp = comps.get(cname)
                if comp is not None:
                    sz = comp.get_float_var_size(vname)
                    if sz is not None:
                        vdict = variables[name]
                        sz, flat_idx, base = sz
                        vdict['size'] = sz
                        if flat_idx is not None:
                            vdict['flat_idx'] = flat_idx

        # pass the call down to any subdrivers/subsystems
        # and subassemblies. subassemblies will ignore the
        # variables passed into them in this case.
        for comp in self.local_comps:
            comp.setup_sizes(variables)
            
    def dump_parallel_graph(self, nest=0, stream=sys.stdout):
        """Prints out a textual representation of the collapsed
        execution graph (with groups of component nodes collapsed
        into SerialSystems and ParallelSystems).  It shows which
        components run on the current processor.
        """
        if not self.local_comps:
            return

        if stream is None:
            getval = True
            stream = StringIO()
        else:
            getval = False

        stream.write(" "*nest)
        stream.write(self.name)
        stream.write(" [%s](req=%d)(rank=%d)\n" % (self.__class__.__name__, 
                                                   self.get_req_cpus(), 
                                                   MPI.COMM_WORLD.rank))

        nest += 3
        for comp in self.local_comps:
            if isinstance(comp, System):
                comp.dump_parallel_graph(nest, stream)
            else:
                stream.write(" "*nest)
                stream.write("%s\n" % comp.name)

        if getval:
            return stream.getvalue()

    # def _add_vars_from_comp(self, comp, vecvars, scope):
    #     if isinstance(comp, System):
    #         vecvars.update(comp.get_vector_vars(scope))
    #     else:
    #         vecvars.update(comp.get_vector_vars())
    #         # 'inputs' and 'outputs' metadata have been added
    #         # to the comp nodes from drivers that iterate
    #         # over them.
    #         for name in self.graph.node[comp.name].get('inputs',()):
    #             vecvars['.'.join([comp.name,name])] = \
    #                                        comp.get_vector_var(name)
    #         for name in self.graph.node[comp.name].get('outputs',()):
    #             vecvars['.'.join([comp.name,name])] = \
    #                                        comp.get_vector_var(name)
        
        
class SerialSystem(System):
    def __init__(self, graph, scope):
        super(SerialSystem, self).__init__(graph)
        cpus = []
        for node, data in graph.nodes_iter(data=True):
            if isinstance(node, tuple):
                cpus.append(data['system'].get_req_cpus())
            else:
                cpus.append(getattr(scope, node).get_req_cpus())
        self.mpi.req_cpus = max(cpus)

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

    def setup_communicators(self, comm, scope):
        self.mpi.comm = comm
        self.local_comps = []

        for name in topological_sort(self.graph):
            if isinstance(name, tuple): # it's a subsystem
                comp = self.graph.node[name]['system']
            else:
                comp = getattr(scope, name)
            #mpiprint("*** serial child %s" % comp.name)
            comp.mpi.comm = comm
            self.local_comps.append(comp)
            comp.setup_communicators(comm, scope)

    # def get_vector_vars(self, scope):
    #     """Assemble an ordereddict of names of variables needed by this
    #     workflow, which includes any that its parent driver(s) reference
    #     in parameters, constraints, or objectives, as well as any used to 
    #     connect any components in this workflow, AND any returned from
    #     calling get_vector_vars on any subsystems in this workflow.
    #     """
    #     self.vector_vars = OrderedDict()
    #     for comp in self.local_comps:
    #         self._add_vars_from_comp(comp, self.vector_vars, scope)
    #     return self.vector_vars


class ParallelSystem(System):
    def __init__(self, graph, scope):
        super(ParallelSystem, self).__init__(graph)
        cpus = 0
        # in a parallel system, the required cpus is the sum of
        # the required cpus of the members
        for node, data in graph.nodes_iter(data=True):
            if isinstance(node, tuple):
                cpus += data['system'].get_req_cpus()
            else:
                cpus += getattr(scope, node).get_req_cpus()
        self.mpi.req_cpus = cpus
 
    def run(self, scope, ffd_order, case_id, iterbase):
        #mpiprint("running parallel system %s: %s" % (self.name, [c.name for c in self.local_comps]))
        # don't scatter unless we contain something that's actually 
        # going to run
        if not self.local_comps:
            return

        # scatter(...)

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

    def setup_communicators(self, comm, scope):
        self.mpi.comm = comm
        size = comm.size
        rank = comm.rank

        child_comps = []
        requested_procs = []
        for name, data in self.graph.nodes_iter(data=True):
            system = data.get('system')
            if system is not None: # nested workflow
                child_comps.append(system)
                requested_procs.append(system.get_req_cpus())
                #mpiprint("!! system %s requests %d cpus" % (system.name, system.req_cpus))
            else:
                comp = getattr(scope, name)
                child_comps.append(comp)
                requested_procs.append(comp.get_req_cpus())

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

        #mpiprint("comm size = %d" % comm.size)
        #mpiprint("child_comps: %s" % [c.name for c in child_comps])
        mpiprint("requested_procs: %s" % requested_procs)
        mpiprint("assigned_procs: %s" % assigned_procs)

        for i,comp in enumerate(child_comps):
            if requested_procs[i] > 0 and assigned_procs[i] == 0:
                raise RuntimeError("parallel group %s requested %d processors but got 0" %
                                   (child_comps[i].name, requested_procs[i]))

        color = []
        for i, procs in enumerate([p for p in assigned_procs if p > 0]):
            color.extend([i]*procs)

        if size > assigned:
            color.extend([MPI.UNDEFINED]*(size-assigned))

        rank_color = color[rank]
        sub_comm = comm.Split(rank_color)

        self.local_comps = []

        if sub_comm == MPI.COMM_NULL:
            return

        for i,c in enumerate(child_comps):
            if i == rank_color:
                c.mpi.cpus = assigned_procs[i]
                self.local_comps.append(c)
            elif requested_procs[i] == 0:  # comp is duplicated everywhere
                self.local_comps.append(c)

        for comp in self.local_comps:
            comp.setup_communicators(sub_comm, scope)

    #def get_vector_vars(self, scope):
        # """Assemble an ordereddict of names of variables needed by this
        # workflow, which includes any that its parent driver(s) reference
        # in parameters, constraints, or objectives, as well as any used to 
        # connect any components in this workflow, AND any returned from
        # calling get_vector_vars on any subsystems in this workflow.
        # """
        # self.vector_vars = OrderedDict()
        # vector_vars = OrderedDict()
        # if self.local_comps:
        #     comp = self.local_comps[0]
        #     self._add_vars_from_comp(comp, vector_vars, scope)
        # vnames = self.mpi.comm.allgather(vector_vars.keys())

        # fullnamelst = []
        # seen = set()
        # for names in vnames:
        #     for name in names:
        #         if name not in seen:
        #             seen.add(name)
        #             fullnamelst.append(name)

        # # group names (in order) by component
        # compdct = OrderedDict()
        # partition_names_by_comp(fullnamelst, compdct)

        # # TODO: may need to mess with ordering once we add in
        # # derivative calculations, so that order of vars
        # # within a comp matches order returned by list_deriv_vars...
        # for cname, vname in compdct:
        #     if cname is None:
        #         self.vector_vars[cname] = None
        #     else:
        #         self.vector_vars['.'.join((cname, vname))] = None

        # self.vector_vars.update(vector_vars)

        # for comp in self.local_comps:
        #     self._add_vars_from_comp(comp, self.vector_vars, scope)
        # return self.vector_vars
                

def transform_graph(g, scope):
    """Return a nested graph with metadata for parallel
    and serial subworkflows.
    """
    if len(g) <= 1:
        return g

    # first, create connections between any subdrivers and anything
    # that connects to their iteration set.  Otherwise we get parallel
    # workflows when we really need sequential ones, e.g., if a subdriver
    # iterates over C1 which feeds another component C2, then that
    # subdriver should run BEFORE C2 in the workflow.

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
                    _collapse(scope, g, branch, serial=True)
                    gcopy.remove_nodes_from(branch)
                else:
                    gcopy.remove_node(branch)

            parallel_group = tuple(sorted(parallel_group))
            _collapse(scope, g, parallel_group, serial=False)
        else:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)
    
def _expand_tuples(nodes):
    lst = []
    stack = list(nodes)
    while stack:
        node = stack.pop()
        if isinstance(node, tuple):
            stack.extend(node)
        else:
            lst.append(node)
    return lst

def _collapse(scope, g, nodes, serial=True):
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

    # collect all of the edges that have been collapsed
    g.node[newname]['xfers'] = edge_boundary(scope._depgraph, 
                                    scope._depgraph.find_prefixed_nodes(_expand_tuples(nodes)))

    #mpiprint("*** edge boundary = %s for %s" % (g.node[newname]['xfers'],newname))

    if serial:
        transform_graph(subg, scope)
        subsys = SerialSystem(subg, scope)
    else:
        subsys = ParallelSystem(subg, scope)

    g.node[newname]['system'] = subsys
 

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

