import sys
from StringIO import StringIO
from networkx import topological_sort
from collections import OrderedDict
from itertools import chain

from networkx import edge_boundary

import numpy

# pylint: disable-msg=E0611,F0401
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, get_petsc_vec
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start
#from openmdao.util.nameutil import partition_names_by_comp

class System(object):
    def __init__(self, graph):
        self.graph = graph
        if len(graph) > 1:
            self.name = str(tuple(sorted(graph.nodes())))
        else:
            self.name = graph.nodes()[0]
        self.subsystems = []
        self.mpi = MPI_info()
        self.mpi.requested_cpus = None
        self.all_variables = OrderedDict()
        self.vec = {}
        #self._dump_graph()

    def get_inputs(self):
        # the full set of inputs is stored in the 
        # metadata of this System's graph.
        return self.graph.graph.get('inputs',set())
        
    def get_outputs(self):
        # the full set of outputs is stored in the 
        # metadata of this System's graph.
        return self.graph.graph.get('outputs',set())
        
    def get_req_cpus(self):
        return self.mpi.requested_cpus

    def setup_variables(self):
        self.all_variables = OrderedDict()

        for sub in self.subsystems:
            sub.setup_variables()
            for name, var in sub.all_variables.items():
                self.all_variables[name] = var

        for vname in chain(sorted(self.get_inputs()), 
                           sorted(self.get_outputs())):
            if vname not in self.all_variables:
                self.all_variables[vname] = { 'size': 0 }

        mpiprint("AFTER setup_variables, %s has %s" % (self.name,
                                                       self.all_variables.keys()))
        
    def setup_sizes(self, scope):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """

        subs = dict([(c.name, c) for c in self.subsystems])

        for i, (name, vdict) in enumerate(self.all_variables.items()):
            cname, vname = name.split('.',1)
            sub = subs.get(cname)
            if sub is not None: # only SimpleSystems will match cname
                comp = sub._comp
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

        mpiprint("in %s, add=%s, noadd = %s" % (self.name,sizes_add,sizes_noadd))

        # create an (nproc x numvars) var size vector containing 
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((comm.size, len(sizes_add)), 
                                           int)

        self.variables = OrderedDict()
        for name in sizes_add:
            self.variables[name] = self.all_variables[name]
        
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

        #mpiprint("vars and local sizes for %s " % self.name)
        #for name, sz in zip(self.variables.keys(), self.local_var_sizes[rank, :]):
        #    mpiprint("%s ( %d )" % (name, sz))

        # create a (1 x nproc) vector for the sizes of all of our 
        # local inputs
        self.input_sizes = numpy.zeros(comm.size, int)

        inputs = self.get_inputs()

        self.input_sizes[rank] = sum([v['size'] 
                                        for n,v in self.variables.items() 
                                           if n in inputs])

        comm.Allgather(self.input_sizes[rank], self.input_sizes)

        mpiprint("input sizes for %s = %s" % (self.name, self.input_sizes))

        # pass the call down to any subdrivers/subsystems
        # and subassemblies. components will ignore the
        # scope passed into them in this case.
        for sub in self.subsystems:
            sub.setup_sizes(scope)

        mpiprint("AFTER setup_sizes in %s, variables=%s" % (self.name, self.variables.keys()))
        mpiprint("AFTER setup_sizes in %s, local_sizes=%s" % (self.name, self.local_var_sizes[rank]))
            
    def setup_vectors(self, arrays):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        rank = self.mpi.comm.rank
        if arrays is None:  # we're the top level System in our Assembly
            arrays = {}
            # create top level vectors            
            #mpiprint("**** setting up vectors for %s" % self.name)
            size = numpy.sum(self.local_var_sizes[rank, :])
            mpiprint("TOTAL LOCAL SIZE for %s = %d" % (self.name, size))
            for name in ['u']: #, 'f', 'du', 'df']:
                self.vec[name] = VecWrapper(self, numpy.zeros(size))
                arrays[name] = self.vec[name].array
        else: # if we're the top level, we don't need input arrays

            for name, vec in arrays.items():
                mpiprint("TOTAL LOC SIZE for %s = %d" % (self.name, vec.size))
                self.vec[name] = VecWrapper(self, vec)

            insize = self.input_sizes[rank]
            # for name in ['p', 'dp']:
            #     self.vec[name] = VecWrapper(self, numpy.zeros(insize))

        start, end = 0, 0
        for sub in self.subsystems:
            sz = numpy.sum(sub.local_var_sizes[sub.mpi.comm.rank, :])
            end += sz
            mpiprint("%s: passing [%d,%d] view of size %d array to %s" % 
                        (self.name,start,end,arrays['u'].size,sub.name))
            sub.setup_vectors(dict([(n,arrays[n][start:end]) for n in
                                        ['u']])) #, 'f', 'du', 'df']]))
            start += sz

    def dump_subsystem_tree(self, nest=0, stream=sys.stdout):
        """Prints out a textual representation of the collapsed
        execution graph (with groups of component nodes collapsed
        into SerialSystems and ParallelSystems).  It shows which
        components run on the current processor.
        """
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
        #for v, data in self.all_variables.items():
        for v, (arr, start) in self.vec['u']._info.items():
            stream.write(" "*(nest+2))
            stream.write("%s (%s)\n" % (v, list(self.vec['u'].bounds(v))))

        nest += 3
        for sub in self.subsystems:
            sub.dump_subsystem_tree(nest, stream)

        if getval:
            return stream.getvalue()

    def _dump_graph(self):
        mpiprint("GRAPH DUMP for %s" % self.name)
        for node, data in self.graph.nodes_iter(data=True):
            mpiprint("%s: %s" % (str(node), {'inputs':data['inputs'],'outputs':data['outputs']}))
        for u,v,data in self.graph.edges_iter(data=True):
            mpiprint("(%s,%s): %s" % (u,v,{'var_edges':data['var_edges']}))

class SimpleSystem(System):
    """A System for a single Component."""
    def __init__(self, graph, scope):
        super(SimpleSystem, self).__init__(graph)
        self.graph.graph.setdefault('inputs',set()).update(self.graph.node[self.name]['inputs'])
        self.graph.graph.setdefault('outputs',set()).update(self.graph.node[self.name]['outputs'])
        self._comp = getattr(scope, graph.nodes()[0])
        self.mpi.requested_cpus = self._comp.get_req_cpus()

    def run(self, scope, ffd_order, case_id, iterbase):
        comp = self._comp
        #mpiprint("running simple system %s: %s" % (self.name, self._comp.name))
        if not isinstance(comp, PseudoComponent):
            comp.set_itername('%s-%d' % (iterbase, 1))

        mpiprint("simple running %s" % comp.name)
        comp.run(ffd_order=ffd_order, case_id=case_id)

    def setup_communicators(self, comm, scope):
        mpiprint("setting up comms for %s" % self.name)
        self.mpi.comm = comm
        self.subsystems = []


class DriverSystem(SimpleSystem):
    """A System for a Driver component."""

    def setup_communicators(self, comm, scope):
        mpiprint("setting up comms for %s" % self.name)
        self.mpi.comm = comm
        self.subsystems = [self._comp.workflow.get_subsystem()]

    
class CompoundSystem(System):
    def __init__(self, graph, scope):
        super(CompoundSystem, self).__init__(graph)
        for node, data in self.graph.nodes_iter(data=True):
            if isinstance(node, basestring):
                _create_simple_sys(graph, scope, node)

class SerialSystem(CompoundSystem):

    def get_req_cpus(self):
        cpus = []
        for node, data in self.graph.nodes_iter(data=True):
            cpus.append(data['system'].get_req_cpus())
        self.mpi.requested_cpus = max(cpus)
        return self.mpi.requested_cpus

    def run(self, scope, ffd_order, case_id, iterbase):
        #mpiprint("running serial system %s: %s" % (self.name, [c.name for c in self.subsystems]))
        for i, comp in enumerate(self.subsystems):
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
        self.subsystems = []

        for name in topological_sort(self.graph):
            sub = self.graph.node[name]['system']
            sub.mpi.comm = comm
            self.subsystems.append(sub)
            sub.setup_communicators(comm, scope)


class ParallelSystem(CompoundSystem):

    def get_req_cpus(self):
        cpus = 0
        # in a parallel system, the required cpus is the sum of
        # the required cpus of the members
        for node, data in self.graph.nodes_iter(data=True):
            try:
                cpus += data['system'].get_req_cpus()
            except KeyError:
                mpiprint("NO SYSTEM for %s" % str(node))
        self.mpi.requested_cpus = cpus
        return cpus
 
    def run(self, scope, ffd_order, case_id, iterbase):
        #mpiprint("running parallel system %s: %s" % (self.name, [c.name for c in self.subsystems]))
        # don't scatter unless we contain something that's actually 
        # going to run
        if not self.subsystems:
            return

        # scatter(...)

        for i, sub in enumerate(self.subsystems):
            sub.run(scope, ffd_order, case_id, iterbase)

    def setup_communicators(self, comm, scope):
        self.mpi.comm = comm
        size = comm.size
        rank = comm.rank

        subsystems = []
        requested_procs = []
        for name, data in self.graph.nodes_iter(data=True):
            system = data['system']
            subsystems.append(system)
            requested_procs.append(system.get_req_cpus())

        assigned_procs = [0]*len(requested_procs)

        assigned = 0

        requested = sum(requested_procs)

        limit = min(size, requested)

        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        if requested:
            while assigned < limit:
                for i, system in enumerate(subsystems):
                    if requested_procs[i] == 0: # skip and deal with these later
                        continue
                    if assigned_procs[i] < requested_procs[i]:
                        assigned_procs[i] += 1
                        assigned += 1
                        if assigned == limit:
                            break

        #mpiprint("comm size = %d" % comm.size)
        #mpiprint("subsystems: %s" % [c.name for c in subsystems])
        mpiprint("requested_procs: %s" % requested_procs)
        mpiprint("assigned_procs: %s" % assigned_procs)

        self.subsystems = []

        for i,sub in enumerate(subsystems):
            if requested_procs[i] > 0 and assigned_procs[i] == 0:
                raise RuntimeError("parallel group %s requested %d processors but got 0" %
                                   (sub.name, requested_procs[i]))

        color = []
        for i, procs in enumerate([p for p in assigned_procs if p > 0]):
            color.extend([i]*procs)

        if size > assigned:
            color.extend([MPI.UNDEFINED]*(size-assigned))

        rank_color = color[rank]
        sub_comm = comm.Split(rank_color)

        if sub_comm == MPI.COMM_NULL:
            return

        for i,sub in enumerate(subsystems):
            if i == rank_color:
                sub.mpi.cpus = assigned_procs[i]
                self.subsystems.append(sub)
            elif requested_procs[i] == 0:  # sub is duplicated everywhere
                self.subsystems.append(sub)

        for sub in self.subsystems:
            sub.setup_communicators(sub_comm, scope)
             
    def setup_variables(self):
        """ Determine variables from local subsystems """
        for sub in self.subsystems:
            sub.setup_variables()

        self.all_variables = OrderedDict()

        sub = self.subsystems[0]
        varkeys_list = self.mpi.comm.allgather(sub.all_variables.keys())
        for varkeys in varkeys_list:
            for name in varkeys:
                self.all_variables[name] = { 'size': 0 }
        for name, var in sub.all_variables.items():
            self.all_variables[name] = var


   
def _create_simple_sys(g, scope, name):
    comp = getattr(scope, name)
    subg = _precollapse(scope, g, (name,), newname=name)
    if has_interface(comp, IDriver):
        g.node[name]['system'] = DriverSystem(subg, scope)
    else:
        g.node[name]['system'] = SimpleSystem(subg, scope)

def partition_subsystems(g, scope):
    """Return a nested graph with metadata for parallel
    and serial subworkflows.  Graph must acyclic.
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
                    partition_subsystems(subg, scope)
                    #mpiprint("%d adding system for %s %s" % (id(g),type(branch),str(branch)))
                    g.node[branch]['system'] = SerialSystem(subg, scope)
                    gcopy.remove_nodes_from(branch)
                else: # single comp system
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
        #outputs.update([u for u,v in var_edges])

    for u,v in in_edges:
        var_edges = g.edge[u][v].get('var_edges', ())
        #mpiprint("adding edge (%s,%s)" % (u, newname))
        g.add_edge(u, newname)
        xfers.setdefault((u, newname), []).extend(var_edges)
        #inputs.update([v for u,v in var_edges])

    # save the collapsed edges in the metadata of the new edges
    # so each subsystem knows what its inputs and outputs are
    for edge, var_edges in xfers.items():
        g[edge[0]][edge[1]]['var_edges'] = var_edges

    # update our inputs/outputs with inputs/outputs from child nodes
    for node, data in subg.nodes_iter(data=True):
        inputs.update(data['inputs'])
        outputs.update(data['outputs'])

    g.node[newname]['inputs'] = inputs
    g.node[newname]['outputs'] = outputs

    inputs.update(inputs)
    outputs.update(outputs)

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

    The items in each list will have the same ordering as they
    had in the original list of names.
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
    def __init__(self, system, array):
        self.array = array

        rank = system.mpi.comm.rank
        allvars = system.all_variables
        variables = system.variables
        sizes = system.local_var_sizes
        
        self._info = OrderedDict() # dict of (start_idx, view)

        # first, add views for vars whose sizes are added to the total,
        # i.e., their basevars are not included in the vector.
        start, end = 0, 0
        for i, (name, var) in enumerate(variables.items()):
            sz = sizes[rank][i]
            if sz > 0:
                end += sz
                if self.array.size < (end-start):
                    raise RuntimeError("in subsystem %s, can't create a view of [%d,%d] from a %d size array" %
                                         (system.name,start,end,self.array.size))
                self._info[name] = (self.array[start:end], start)
                mpiprint("*** %s: view for %s is %s, size=%d" % (system.name,name, [start,end],self._info[name][0].size))
                start += sz

        # now add views for subvars that are subviews of their
        # basevars
        for name, var in allvars.items():
            if name not in variables:
                sz = var['size']
                if sz > 0:
                    mpiprint("FOUND A SUBVAR: %s, size=%d" % (name, sz))
                    idx = var['flat_idx']
                    basestart = self.start(var['basevar'])
                    sub_idx = offset_flat_index(idx, basestart)
                    substart = get_flat_index_start(sub_idx)
                    #mpiprint("size,basestart,substart,sub_idx = (%d, %d, %d, %d)" % 
                    #            (size,basestart, substart, sub_idx))
                    self._info[name] = (self.array[sub_idx], substart)
                    mpiprint("*** %s: view for %s is %s" % (system.name, name, list(self.bounds(name))))

        # create the PETSc vector
        self.petsc_vec = get_petsc_vec(system.mpi.comm, self.array)

    def view(self, name):
        """Return the array view into the larger array for the
        given name.  name may contain array indexing.
        """
        return self._info[name][0]

    def views(self):
        return self._info.keys()

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


    