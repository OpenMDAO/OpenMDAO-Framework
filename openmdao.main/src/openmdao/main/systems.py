import sys
from StringIO import StringIO
from networkx import topological_sort
from collections import OrderedDict
from itertools import chain

from networkx import edge_boundary

import numpy

# pylint: disable-msg=E0611,F0401
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, get_petsc_vec, PETSc
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start
from openmdao.main.vecwrapper import VecWrapper

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
        self.app_ordering = None
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

            #mpiprint("%s for SUB %s, adding vars %s" % (self.name,sub.name,sub.all_variables.keys()))
            for name, var in sub.all_variables.items():
                self.all_variables[name] = var

        for vname in chain(sorted(self.get_inputs()), 
                           sorted(self.get_outputs())):
            if vname not in self.all_variables:
                #mpiprint("%s ADDING zero size for %s" % (self.name, vname))
                self.all_variables[vname] = { 'size': 0 }

        #mpiprint("AFTER setup_variables, %s has %s" % (self.name,
        #                                               self.all_variables.keys()))
        
    def setup_sizes(self):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """
        comm = self.mpi.comm

        # pass the call down to any subdrivers/subsystems
        # and subassemblies. 
        for sub in self.subsystems:
            sub.setup_sizes()

        sizes_add, sizes_noadd = _partition_subvars(self.all_variables.keys(),
                                                    self.all_variables)

        #mpiprint("in %s, add=%s, noadd = %s" % (self.name,sizes_add,sizes_noadd))

        # create an (nproc x numvars) var size vector containing 
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((comm.size, len(sizes_add)), 
                                           int)

        self.variables = OrderedDict()
        for name in sizes_add:
            self.variables[name] = self.all_variables[name]

        #mpiprint("%s setup_sizes: vars = %s" % (self.name, self.variables.keys()))
        
        rank = comm.rank
        for i, (name, var) in enumerate(self.variables.items()):
            self.local_var_sizes[rank, i] = var['size']

        #mpiprint("%s before ALLGATHER: local_var_sizes[%d] = %s" % (self.name,rank,self.local_var_sizes[rank]))
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

        mpiprint("%s local_var_sizes:\n %s" % (self.name, self.local_var_sizes))

        # create a (1 x nproc) vector for the sizes of all of our 
        # local inputs
        self.input_sizes = numpy.zeros(comm.size, int)

        inputs = self.get_inputs()

        self.input_sizes[rank] = sum([v['size'] 
                                        for n,v in self.variables.items() 
                                           if n in inputs])

        #mpiprint("%s before ALLGATHER: input_sizes[%d] = %s" % (self.name,rank,self.input_sizes[rank]))
        comm.Allgather(self.input_sizes[rank], self.input_sizes)

        #mpiprint("inputs = %s for %s" % ([(n,v['size'])
        #                                     for n,v in self.variables.items() 
        #                                   if n in inputs], self.name))
        #mpiprint("input sizes for %s = %s" % (self.name,self.input_sizes[rank]))

        #mpiprint("AFTER setup_sizes in %s, variables=%s" % (self.name, self.variables.keys()))
        #mpiprint("AFTER setup_sizes in %s, local_sizes=%s" % (self.name, self.local_var_sizes[rank]))
            
    def setup_vectors(self, arrays):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        rank = self.mpi.comm.rank
        if arrays is None:  # we're the top level System in our Assembly
            insize = 0
            inputs = []
            arrays = {}
            # create top level vectors            
            #mpiprint("**** setting up vectors for %s" % self.name)
            size = numpy.sum(self.local_var_sizes[rank, :])
            #mpiprint("TOTAL LOCAL SIZE for %s (rank=%d) = %d" % (self.name, rank,size))
            for name in ['u']: #, 'f', 'du', 'df']:
                #mpiprint("creating top level Vec in %s, size=%d" % (self.name,size))
                arrays[name] = numpy.zeros(size)
        else: # if we're the top level, we don't need input arrays
            insize = self.input_sizes[rank]
            inputs = self.get_inputs()

        for name in ['u']: #, 'f', 'du', 'df']:
            #mpiprint("ARRAY %s.%s is size %d" % (self.name, name, arr.size))
            #mpiprint("creating Vec %s in %s, size=%d" % (name, self.name,arrays[name].size))
            self.vec[name] = VecWrapper(self, arrays[name])

        for name in ['p']:#, 'dp']:
            #mpiprint("creating input Vec %s in %s, size=%d" % (name, self.name,insize))
            self.vec[name] = VecWrapper(self, numpy.zeros(insize), inputs=inputs)

        start, end = 0, 0
        for sub in self.subsystems:
            sz = numpy.sum(sub.local_var_sizes[sub.mpi.comm.rank, :])
            #mpiprint("SUB %s says it has size %d" % (sub.name,sz))
            end += sz
            if end-start > arrays['u'][start:end].size:
                mpiprint("PASSING [%d,%d] view of size %d array from %s to %s" % 
                            (start,end,arrays['u'][start:end].size,self.name,sub.name))
            sub.setup_vectors(dict([(n,arrays[n][start:end]) for n in
                                        ['u']])) #, 'f', 'du', 'df']]))
            start += sz

        return self.vec

    def get_all_subsystems(self):
        for node, data in self.graph.nodes_iter(data=True):
            yield data['system']

    def get_simple_subsystems(self):
        for sub in self.get_all_subsystems():
            if isinstance(sub, SimpleSystem):
                yield sub

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

        name_map = { 'SerialSystem': 'ser', 'ParallelSystem': 'par',
                     'SimpleSystem': 'simp', 'DriverSystem': 'drv',
                     'AssemblySystem': 'asm' }
        stream.write(" "*nest)
        stream.write(str(self.name).replace(' ','').replace("'",""))
        stream.write(" [%s](req=%d)(rank=%d)(vsize=%d)(isize=%d)\n" % 
                                          (name_map[self.__class__.__name__], 
                                           self.get_req_cpus(), 
                                           MPI.COMM_WORLD.rank,
                                           self.vec['u'].array.size,
                                           self.input_sizes[self.mpi.comm.rank]))
        inputs = self.get_inputs()

        #for v, data in self.all_variables.items():
        for v, (arr, start) in self.vec['u']._info.items():
            stream.write(" "*(nest+2))
            if v in inputs:
                stream.write("%s (%s) *input*\n" % (v, list(self.vec['u'].bounds(v))))
            else:
                stream.write("%s (%s)\n" % (v, list(self.vec['u'].bounds(v))))

        nest += 3
        for sub in self.subsystems:
            sub.dump_subsystem_tree(nest, stream)

        if getval:
            return stream.getvalue()

    def create_scatter(self, var_idxs, input_idxs):
        """ Concatenates lists of indices and creates a PETSc Scatter """
        #mpiprint("src_idxs = %s, dest_idxs = %s" % (var_idxs,input_idxs))
        merge = lambda x: numpy.concatenate(x) if len(x) > 0 else []
        var_idx_set = PETSc.IS().createGeneral(merge(var_idxs), comm=self.mpi.comm)
        input_idx_set = PETSc.IS().createGeneral(merge(input_idxs), comm=self.mpi.comm)
        if self.app_ordering is not None:
            var_idx_set = self.app_ordering.app2petsc(var_idx_set)

        if len(merge(var_idxs)) != len(merge(input_idxs)):
            mpiprint("creating scatter: (%d != %d) srcs: %s,  dest: %s in %s" % (len(merge(var_idxs)),len(merge(input_idxs)),merge(var_idxs),merge(input_idxs),self.name))
        return PETSc.Scatter().create(self.vec['u'].petsc_vec, var_idx_set,
                                      self.vec['p'].petsc_vec, input_idx_set)

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
        mpiprint("%s simple inputs = %s" % (self.name, self.get_inputs()))

    def run(self, scope, ffd_order, case_id, iterbase):
        comp = self._comp
        #mpiprint("running simple system %s: %s" % (self.name, self._comp.name))
        if not isinstance(comp, PseudoComponent):
            comp.set_itername('%s-%d' % (iterbase, 1))

        mpiprint("simple running %s" % comp.name)
        comp.run(ffd_order=ffd_order, case_id=case_id)

    def setup_communicators(self, comm, scope):
        mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
        self.mpi.comm = comm
        self.subsystems = []

    def setup_variables(self):
        super(SimpleSystem, self).setup_variables()
        comp = self._comp
        for name, vdict in self.all_variables.items():
            cname, vname = name.split('.',1)
            info = comp.get_float_var_info(vname)
            if info is not None:
                sz, flat_idx, base = info
                vdict['size'] = sz
                if flat_idx is not None:
                    vdict['flat_idx'] = flat_idx
                if base is not None:
                    vdict['basevar'] = '.'.join((cname, base))

    def setup_scatters(self):
        rank = self.mpi.comm.rank
        start = numpy.sum(self.input_sizes[:rank])
        end = numpy.sum(self.input_sizes[:rank+1])
        dest_idxs = [petsc_linspace(start, end)]
        src_idxs = []
        varkeys = self.variables.keys()
        for dest in self.get_inputs():
            if dest in self.variables:
                ivar = varkeys.index(dest)
                # FIXME: currently just using the local var size for input size
                src_idxs.append(numpy.sum(self.local_var_sizes[:, :ivar]) + # ??? args[arg] - user really needs to be able to define size for multi-proc comps
                                      numpy.arange(0, self.local_var_sizes[rank,ivar], dtype='i'))
        # if len(src_idxs) != len(dest_idxs):
        #     mpiprint("setting up scatter: (%d != %d) srcs: %s,  dest: %s in %s" % (len(src_idxs),len(dest_idxs),src_idxs,dest_idxs,self.name))
        self.scatter_full = self.create_scatter(src_idxs, dest_idxs)


class DriverSystem(SimpleSystem):
    """A System for a Driver component."""

    def setup_communicators(self, comm, scope):
        mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
        self.mpi.comm = comm
        self.subsystems = [self._comp.workflow.get_subsystem()]


class AssemblySystem(SimpleSystem):
    """A System to handle an Assembly."""

    def setup_communicators(self, comm, scope):
        super(AssemblySystem, self).setup_communicators(comm, None)
        self._comp.setup_communicators(comm)

    def setup_variables(self):
        self._comp.setup_variables()

    def setup_sizes(self):
        self._comp.setup_sizes()

    def setup_vectors(self, arrays):
        self._comp.setup_vectors()

    def setup_scatters(self):
        self._comp.setup_scatters()


class CompoundSystem(System):
    def __init__(self, graph, scope):
        super(CompoundSystem, self).__init__(graph)
        for node, data in self.graph.nodes_iter(data=True):
            if isinstance(node, basestring):
                _create_simple_sys(graph, scope, node)

    def setup_scatters(self):
        """ Defines a scatter for args at this system's level """
        var_sizes = self.local_var_sizes
        input_sizes = self.input_sizes
        rank = self.mpi.comm.rank

        app_indices = []
        for ivar in xrange(len(self.variables)):
            start = numpy.sum(var_sizes[:, :ivar]) + numpy.sum(var_sizes[:rank, ivar])
            end = start + var_sizes[rank, ivar]
            app_indices.append(petsc_linspace(start, end))
        app_indices = numpy.concatenate(app_indices)

        start = numpy.sum(var_sizes[:rank, :])
        end = numpy.sum(var_sizes[:rank+1, :])
        petsc_indices = petsc_linspace(start, end)

        app_ind_set = PETSc.IS().createGeneral(app_indices, comm=self.mpi.comm)
        petsc_ind_set = PETSc.IS().createGeneral(petsc_indices, comm=self.mpi.comm)
        self.app_ordering = PETSc.AO().createBasic(app_ind_set, petsc_ind_set, 
                                                   comm=self.mpi.comm)

        src_full = []
        dest_full = []
        start = end = numpy.sum(input_sizes[:rank])
        varkeys = self.variables.keys()

        #for subsystem in self.get_all_subsystems():
        for subsystem in self.subsystems:
            src_partial = []
            dest_partial = []
            for inp in subsystem.get_inputs():
                if not hasattr(subsystem, 'variables'):
                    mpiprint("****** %s HAS NO VARIABLES" % subsystem.name)
                    continue
                if inp not in subsystem.all_variables and inp in self.variables:
                    ivar = varkeys.index(inp)
                    dest_size = self.local_var_sizes[rank, ivar]
                    src_idxs = numpy.sum(var_sizes[:, :ivar]) + numpy.arange(0, dest_size) #args[arg]

                    end += dest_size #args[arg].shape[0]
                    dest_idxs = petsc_linspace(start, end)
                    start += dest_size #args[arg].shape[0]

                    src_partial.append(src_idxs)
                    dest_partial.append(dest_idxs)
                    src_full.append(src_idxs)
                    dest_full.append(dest_idxs)

            subsystem.scatter_partial = subsystem.create_scatter(src_partial, dest_partial)

        self.scatter_full = self.create_scatter(src_full, dest_full)

        for sub in self.subsystems:
            sub.setup_scatters()


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
        mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
        self.mpi.comm = comm
        self.subsystems = []

        for name in topological_sort(self.graph):
            sub = self.graph.node[name]['system']
            self.subsystems.append(sub)
            sub.setup_communicators(comm, scope)


class ParallelSystem(CompoundSystem):

    def get_req_cpus(self):
        cpus = 0
        # in a parallel system, the required cpus is the sum of
        # the required cpus of the members
        for node, data in self.graph.nodes_iter(data=True):
            cpus += data['system'].get_req_cpus()
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
        mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
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

        #mpiprint("RANKCOLOR: %d,  COLOR: %s, comm.size: %d, subcomm.size: %d" % (rank_color, color,comm.size,sub_comm.size))
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
        self.all_variables = OrderedDict()

        for sub in self.subsystems:
            sub.setup_variables()

        if self.subsystems:
            sub = self.subsystems[0]
            names = sub.all_variables.keys()
        else:
            sub = None
            names = []
        #mpiprint("%s before ALLGATHER, varkeys=%s" % (self.name, names))
        varkeys_list = self.mpi.comm.allgather(names)
        #mpiprint("%s after ALLGATHER, varkeys = %s" % (self.name,varkeys_list))
        for varkeys in varkeys_list:
            for name in varkeys:
                self.all_variables[name] = { 'size': 0 }

        for sub in self.subsystems:
            for name, var in sub.all_variables.items():
                self.all_variables[name] = var


   
def _create_simple_sys(g, scope, name):
    comp = getattr(scope, name)
    subg = _precollapse(scope, g, (name,), newname=name)
    if has_interface(comp, IDriver):
        g.node[name]['system'] = DriverSystem(subg, scope)
    elif has_interface(comp, IAssembly):
        g.node[name]['system'] = AssemblySystem(subg, scope)
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

    The items in each list will have the same ordering as they
    had in the original list of names.
    """
    nosizes = []
    sizes = []
    nameset = set(names)
    for name in names:
        if '[' in name:
            base = name.split('[', 1)[0]
            if base in nameset:
                nosizes.append(name)
                continue
        else:
            base = name
        if base.rsplit('.', 1)[0] in nameset:
            nosizes.append(name)
        else:
            sizes.append(name)

    return (sizes, nosizes)


def petsc_linspace(start, end):
    """ Return a linspace vector of the right int type for PETSc """
    return numpy.arange(start, end, dtype='i')

