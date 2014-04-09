import sys
from StringIO import StringIO
from networkx import topological_sort
from collections import OrderedDict

from networkx import edge_boundary

import numpy

# pylint: disable-msg=E0611,F0401
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, get_petsc_vec
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start
from openmdao.util.nameutil import partition_names_by_comp

class System(object):
    def __init__(self, graph):
        self.graph = graph
        self.name = str(tuple(sorted(graph.nodes())))
        self.local_comps = []
        self.mpi = MPI_info()
        self.mpi.req_cpus = None
        self.all_variables = OrderedDict()
        self.vec = {}

    def get_req_cpus(self):
        return self.mpi.req_cpus

    def setup_variables(self):
        for comp in self.local_comps:
            comp.setup_variables()

        g = self.graph.graph
        inputs = sorted(g.get('inputs',()))
        outputs = sorted(g.get('outputs',()))
        compdict = partition_names_by_comp(inputs, OrderedDict())
        partition_names_by_comp(outputs, compdict)
        allcomps = sorted(compdict.keys())
        for cname in allcomps:
            for vname in compdict[cname]:
                self.all_variables['.'.join((cname,vname))] = { 'size': 0 }         

    def setup_sizes(self, scope):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """

        comps = dict([(c.name, c) for c in self.local_comps])

        for i, (name, vdict) in enumerate(self.all_variables.items()):
            cname, vname = name.split('.',1)
            comp = comps.get(cname)
            if comp is not None:
                info = comp.get_float_var_info(vname)
                if info is not None:
                    sz, flat_idx, base = info
                    vdict['size'] = sz
                    if flat_idx is not None:
                        vdict['flat_idx'] = flat_idx
                    if base is not None:
                        vdict['basevar'] = base

        comm = self.mpi.comm

        sizes_add, sizes_noadd = _partition_subvars(self.all_variables.keys(),
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
            
    def setup_vectors(self, vecs):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        mpiprint("**** setting up vectors for %s" % self.name)
        self.vec['u'] = VecWrapper(self.mpi.comm, self.variables.keys(),
                        self.all_variables)
        #self.pVec = VecWrapper(self, ???)
        for comp in self.local_comps:
            comp.setup_vectors(self.vec)

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
    subg.graph['inputs'] = inputs = set()
    subg.graph['outputs'] = outputs = set()
    for u,v in out_edges:
        var_edges = g.edge[u][v].get('var_edges', ())
        #mpiprint("adding edge (%s,%s)" % (newname, v))
        g.add_edge(newname, v)
        xfers.setdefault((newname, v), []).extend(var_edges)
        outputs.update([u for u,v in var_edges])

    for u,v in in_edges:
        var_edges = g.edge[u][v].get('var_edges', ())
        #mpiprint("adding edge (%s,%s)" % (u, newname))
        g.add_edge(u, newname)
        xfers.setdefault((u, newname), []).extend(var_edges)
        inputs.update([v for u,v in var_edges])

    # save the collapsed edges in the metadata of the new edges
    # so each subsystem knows what its inputs and outputs are
    for edge, var_edges in xfers.items():
        g[edge[0]][edge[1]]['var_edges'] = var_edges

    # add any driver inputs/outputs to our input/output list
    drv_inputs = set()
    drv_outputs = set()
    for node, data in subg.nodes_iter(data=True):
        drv_inputs.update(data.get('drv_inputs', ()))
        drv_outputs.update(data.get('drv_outputs', ()))

    g.node[newname]['drv_inputs'] = drv_inputs
    g.node[newname]['drv_outputs'] = drv_outputs

    inputs.update(drv_inputs)
    outputs.update(drv_outputs)

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


def _partition_subvars(names, vardict):
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
        base = vardict[name].get('basevar')
        if base and base in nameset:
            nosizes.append(name)
        else:
            sizes.append(name)
        nameset.add(name)

    return (sizes, nosizes)


class VecWrapper(object):
    """A wrapper object for a local vector, a distributed PETSc vector,
    and info about what var maps to what range within the distributed
    vector.
    """
    def __init__(self, comm, varlist, allvars):
        varsizes_added, varsizes_noadd = _partition_subvars(varlist, 
                                                            allvars)

        self._info = {} # dict of (start_idx, view)

        size = sum([allvars[name]['size'] for name in varsizes_added])

        self.array = numpy.zeros(size)

        # first, add views for vars whose sizes are added to the total,
        # i.e., their basevars are not included in the vector.
        start, end = 0, 0
        for name in varsizes_added:
            sz = allvars[name].get('size')
            if sz:
                end += sz
                self._info[name] = (self.array[start:end], start)
                mpiprint("*** view for %s is %s" % (name, [start,end]))
                start += sz

        self.petsc_vec = get_petsc_vec(comm, self.array)

        # now add views for subvars that are subviews of their
        # basevars
        for name in varsizes_noadd:
            varinfo = allvars[name]
            sz = varinfo['size']
            if sz:
                idx = varinfo['flat_idx']
                basestart = self.start(varinfo['basevar'])
                sub_idx = offset_flat_index(idx, basestart)
                substart = get_flat_index_start(sub_idx)
                #mpiprint("size,basestart,substart,sub_idx = (%d, %d, %d, %d)" % 
                #            (size,basestart, substart, sub_idx))
                self._info[name] = (self.array[sub_idx], substart)
                mpiprint("*** view for %s is %s" % (name, list(self.bounds(name))))

    def view(self, name):
        """Return the array view into the larger array for the
        given name.  name may contain array indexing.
        """
        return self._info[name][0]

    def start(self, name):
        """Return the starting index for the array view belonging
        to the given name. name may contain array indexing.
        """
        return self._info[name][1]

    def bounds(self, name):
        """Return the bounds corresponding to the slice occupied
        by the named variable within the flattened array.
        name may contain array indexing.
        """
        view, start = self._info[name]
        return (start, start + view.size)

    def scatter(self):
        pass  # see if we can do scatter functionality here...



# def _linspace(self, start, end):
#     """ Return a linspace vector of the right int type for PETSc """
#     return numpy.array(numpy.linspace(start, end-1, end-start), 'i')


