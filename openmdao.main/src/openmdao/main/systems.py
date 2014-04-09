import sys
from StringIO import StringIO
from networkx import topological_sort
from collections import OrderedDict
from itertools import chain

from networkx import edge_boundary

import numpy

# pylint: disable-msg=E0611,F0401
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint
#from openmdao.util.nameutil import partition_edges_by_comp
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver

class System(object):
    def __init__(self, graph):
        self.graph = graph
        self.name = str(tuple(sorted(graph.nodes())))
        self.local_comps = []
        self.mpi = MPI_info()
        self.mpi.req_cpus = None
        self.all_variables = OrderedDict()

    def get_req_cpus(self):
        return self.mpi.req_cpus

    def setup_variables(self):
        for comp in self.local_comps:
            comp.setup_variables()

        g = self.graph.graph
        for vname in chain(g.get('inputs',()), g.get('outputs',())):
            self.all_variables[vname] = { 'size': 0 }         

    def setup_sizes(self, scope):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """

        comps = dict([(c.name, c) for c in self.local_comps])

        # for name in variables.keys():
        #     parts = name.split('.', 1)
        #     if len(parts) > 1:
        #         cname, vname = parts
        #         comp = comps.get(cname)
        #         if comp is not None:
        #             sz = comp.get_float_var_size(vname)
        #             vdict = variables[name]
        #             if sz is not None:
        #                 sz, flat_idx, base = sz
        #                 vdict['size'] = sz
        #                 if flat_idx is not None:
        #                     vdict['flat_idx'] = flat_idx

        for i, (name, vdict) in enumerate(self.all_variables.items()):
            cname, vname = name.split('.',1)
            comp = comps.get(cname)
            if comp is not None:
                info = comp.get_float_var_size(vname)
                if info is not None:
                    sz, flat_idx, base = info
                    vdict['size'] = sz
                    if flat_idx is not None:
                        vdict['flat_idx'] = flat_idx

        comm = self.mpi.comm

        sizes_add, sizes_noadd = _partition_subvars(scope._depgraph,
                                                    self.all_variables.keys(),
                                                    self.all_variables)

        # create an (nproc x numvars) var size vector containing 
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((comm.size, len(sizes_add)), 
                                           int)

        self.variables = OrderedDict()
        for name, var in self.all_variables.items():
            self.variables[name] = var
        
        rank = comm.rank
        for i, (name, var) in enumerate(self.variables.items()):
            self.local_var_sizes[rank, i] = var['size']

        # collect local var sizes from all of the processes in our comm
        # these sizes will be the same in all processes except in cases
        # where a variable belongs to a multiprocessor component.  In that
        # case, the part of the component that runs in a given process will
        # only have a slice of each of the component's variables.
        comm.Allgather(self.local_var_sizes[rank,:], 
                       self.local_var_sizes)

        mpiprint("local sizes = %s" % self.local_var_sizes[rank,:])

        # create a (1 x nproc) vector for the sizes of all of our 
        # local inputs
        self.input_sizes = numpy.zeros(comm.size, int)

        # pass the call down to any subdrivers/subsystems
        # and subassemblies. components will ignore the
        # scope passed into them in this case.
        for comp in self.local_comps:
            comp.setup_sizes(scope)
            
    def setup_vectors(self):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        self.uVec = VecWrapper(self, self.variables.keys())
        #self.pVec = VecWrapper(self, ???)

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
        for v, data in self.variables.items():
            stream.write(" "*(nest+2))
            stream.write("%s (%d)\n" % (v,data.get('size',0)))

        nest += 3
        for comp in self.local_comps:
            if isinstance(comp, System):
                comp.dump_parallel_graph(nest, stream)
            else:
                stream.write(" "*nest)
                stream.write("%s\n" % comp.name)
                if has_interface(comp, IDriver):
                    comp.workflow._subsystem.dump_parallel_graph(nest, stream)

        if getval:
            return stream.getvalue()

        
class SerialSystem(System):
    def __init__(self, graph, scope):
        super(SerialSystem, self).__init__(graph)
        cpus = []
        for node, data in graph.nodes_iter(data=True):
            if isinstance(node, tuple):
                #mpiprint("%d getting system for %s %s" % (id(graph), type(node),str(node)))
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
                mpiprint("seq running %s" % comp.name)
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                comp.set_itername('%s-%d' % (iterbase, i))
                mpiprint("seq running %s" % comp.name)
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
                mpiprint("parallel running %s" % comp.name)
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                mpiprint("parallel running %s" % comp.name)
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

        self.local_comps = []

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
                

def transform_graph(g, scope):
    """Return a nested graph with metadata for parallel
    and serial subworkflows.  Graph must have no cycles.
    """
    if len(g) < 2:
        return g

    gcopy = g.copy()

    to_remove = []

    #mpiprint("transforming graph %d: %s" % (id(g),g.nodes()))
    while len(gcopy) > 1:
        # find all nodes with in degree 0. If we find 
        # more than one, we can execute them in parallel
        zero_in_nodes = [n for n in gcopy.nodes_iter() 
                            if gcopy.in_degree(n)==0]

        if len(zero_in_nodes) > 1: # start of parallel chunk
            parallel_group = []
            for node in zero_in_nodes:
                brnodes = get_branch(gcopy, node)
                if len(brnodes) > 1:
                    parallel_group.append(tuple(sorted(brnodes)))
                else:
                    parallel_group.append(brnodes[0])

            for branch in parallel_group:
                if isinstance(branch, tuple):
                    to_remove.extend(branch)
                    subg = _precollapse(scope, g, branch)
                    transform_graph(subg, scope)
                    #mpiprint("%d adding system for %s %s" % (id(g),type(branch),str(branch)))
                    g.node[branch]['system'] = SerialSystem(subg, scope)
                    gcopy.remove_nodes_from(branch)
                else:
                    gcopy.remove_node(branch)

            parallel_group = tuple(sorted(parallel_group))
            to_remove.extend(parallel_group)
            subg = _precollapse(scope, g, parallel_group)
            #mpiprint("%d adding system for %s %s" % (id(g),type(parallel_group),str(parallel_group)))
            g.node[parallel_group]['system'] = ParallelSystem(subg, scope)
        else:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)

    # Now remove all of the old nodes
    g.remove_nodes_from(to_remove)
    #mpiprint("graph %d post transform: %s" % (id(g),g.nodes()))
    
def collapse_subdrivers(g, driver):
    """collapse subdriver iteration sets into single nodes."""
    # collapse all subdrivers (recursively) 
    scope = driver.parent
    wfnames = driver.workflow.get_names(full=True)
    for child_drv in driver.subdrivers():
        iterset = [c.name for c in child_drv.iteration_set()
                    if c.name not in wfnames]
        iterset.append(child_drv.name)
        #mpiprint("%s: iterset = %s" % (child_drv.name,iterset))
        _precollapse(scope, g, iterset, newname=child_drv.name)
        iterset.remove(child_drv.name)
        g.remove_nodes_from(iterset)
        #mpiprint("post-collapse: %s" % g.nodes())

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

def _precollapse(scope, g, nodes, newname=None):
    """Update all metadata and create new combined nodes based
    on the named nodes, but don't actually remove the old nodes.
    Returns a subgraph containing only the specified nodes.
    """
    if newname is None:
        # combine node names into a single tuple if new name not given
        newname = tuple(nodes)

    # create a subgraph containing all of the collapsed nodes
    # inside of the new node
    subg = g.subgraph(nodes).copy()
    #mpiprint("%d precollapse: subgraph %s" % (id(subg),subg.nodes()))

    #mpiprint("collapsing %s" % list(nodes))

    # the component graph connection edges contain 'var_edges' metadata
    # that contains all variable connections that were collapsed into
    # each component connection.
    nset = set(nodes)
    opp = set(g.nodes_iter())-nset

    # get all incoming and outgoing boundary edges
    out_edges = edge_boundary(g, nodes)
    in_edges = edge_boundary(g, opp)

    g.add_node(newname)

    #mpiprint("collapsing edges: %s" % collapsing_edges)
    xfers = {}
    subg.graph['inputs'] = inputs = []
    subg.graph['outputs'] = outputs = []
    for u,v in out_edges:
        var_edges = g.edge[u][v].get('var_edges', ())
        #mpiprint("adding edge (%s,%s)" % (newname, v))
        g.add_edge(newname, v)
        xfers.setdefault((newname, v), []).extend(var_edges)
        outputs.extend([u for u,v in var_edges])

    for u,v in in_edges:
        var_edges = g.edge[u][v].get('var_edges', ())
        #mpiprint("adding edge (%s,%s)" % (u, newname))
        g.add_edge(u, newname)
        xfers.setdefault((u, newname), []).extend(var_edges)
        inputs.extend([v for u,v in var_edges])

    # save the collapsed edges in the metadata of the new edges
    # so each subsystem knows what its inputs and outputs are
    for edge, var_edges in xfers.items():
        g[edge[0]][edge[1]]['var_edges'] = var_edges

    return subg


def get_branch(g, node, visited=None):
    """Return the full list of nodes that branch *exclusively*
    from the given node.  The starting node is included in 
    the list.
    """
    if visited is None:
        visited = set()
    visited.add(node)
    branch = [node]
    for succ in g.successors(node):
        for p in g.predecessors(succ):
            if p not in visited:
                break
        else:
            branch.extend(get_branch(g, succ, visited))
    return branch


def _partition_subvars(graph, names, vardict):
    """If a subvar has a basevar that is also included in a
    var vector, then the size of the subvar does not add
    to the total size of the var vector because it's size
    is already included in its basevar size.

    This method returns (sizes, nosizes), where sizes is a list 
    of vars/subvars that add to the size of the var vector and 
    nosizes is a list of subvars that do not.

    names are assumed to be sorted such that a basevar will
    always be found in the list before any of its subvars.
    """
    nameset = set()
    nosizes = []
    sizes = []
    for name in names:
        base = graph.node[name].get('basevar')
        if base and base in nameset:
            nosizes.append(name)
        else:
            sizes.append(name)
        nameset.add(name)

    return (sizes, nosizes)
