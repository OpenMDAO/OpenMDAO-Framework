import sys
from StringIO import StringIO
from collections import OrderedDict
from itertools import chain

import numpy
import networkx as nx

# pylint: disable-msg=E0611,F0401
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, PETSc
from openmdao.main.exceptions import RunStopped
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly, IImplicitComponent
from openmdao.main.vecwrapper import VecWrapper, DataTransfer, idx_merge, petsc_linspace
from openmdao.main.depgraph import break_cycles, get_graph_partition, \
                                   is_driver_node, get_all_deps, gsort

def call_if_found(obj, fname, *args, **kwargs):
    """If the named function exists in the object, call it
    with the provided args.
    """
    if hasattr(obj, fname):
        return getattr(obj, fname)(*args, **kwargs)

class System(object):
    def __init__(self, scope, depgraph, nodes, name):
        self.scope = scope
        self.name = name

        # get our I/O edges from the depgraph
        self.in_edges, self.out_edges = \
                         get_graph_partition(depgraph, nodes)

        # if a src or dest is a driver, rename it to the
        # variable on the other side. Scatters for driver
        # params/obj/constraints use same var name in u and
        # p vector.
        for i,(u,v) in enumerate(self.in_edges):
            if is_driver_node(depgraph, u):
                u = v
            elif is_driver_node(depgraph, v):
                v = u
            self.in_edges[i] = (u,v)

        for i,(u,v) in enumerate(self.out_edges):
            if is_driver_node(depgraph, u):
                u = v
            elif is_driver_node(depgraph, v):
                v = u
            self.out_edges[i] = (u,v)

        #mpiprint("%s %s: in_edges = %s" % (self.__class__.__name__, self.name, self.in_edges))
        #mpiprint("%s %s: out_edges = %s" % (self.__class__.__name__, self.name, self.out_edges))

        self.mpi = MPI_info()
        self.mpi.requested_cpus = None
        self.all_variables = OrderedDict() # dict of all vars used in data xfer
        self.vector_vars = OrderedDict() # all vars used in vectors
        self.vector_subvars = OrderedDict() # all vars that are subvars of vector vars
        self.noflat_vars = OrderedDict() # all vars that are not flattenable to float arrays
        self.vec = {}
        self.app_ordering = None
        self.scatter_full = None
        self.scatter_partial = None

    def get_inputs(self, local=False):
        return [v for u,v in self.in_edges]

    def get_outputs(self, local=False):
        return [u for u,v in self.out_edges]

    def set_ordering(self, ordering):
        pass

    def local_subsystems(self):
        return ()

    def all_subsystems(self):
        return ()

    def get_req_cpus(self):
        return self.mpi.requested_cpus

    def _get_var_info(self, name):
        vdict = { 'size': 0 }
        
        parts = name.split('.',1)
        if len(parts) > 1:
            cname, vname = parts
            child = getattr(self.scope, cname)
        else:
            cname, vname = '', name
            child = self.scope
        info = child.get_float_var_info(vname)
        if info is None:
            vdict['flat'] = False
        else:  # variable is flattenable to a float array
            sz, flat_idx, base = info
            vdict['size'] = sz
            vdict['flat'] = True
            if flat_idx is not None:
                vdict['flat_idx'] = flat_idx
            if base is not None:
                if cname:
                    bname = '.'.join((cname, base))
                else:
                    bname = base
                vdict['basevar'] = bname
                
        return vdict

    def setup_variables(self):
        #mpiprint("setup_variables: %s" % self.name)

        self.all_variables = OrderedDict()

        for sub in self.local_subsystems():
            sub.setup_variables()
            #mpiprint("%s for SUB %s, adding vars %s" % (self.name,sub.name,sub.all_variables.keys()))
            self.all_variables.update(sub.all_variables)

        #mpiprint("%s: inputs = %s" % (self.name, self.get_inputs(local=True)))
        #mpiprint("%s: outputs = %s" % (self.name, self.get_outputs()))

        for vname in chain(sorted(self.get_inputs(local=True)), 
                           sorted(self.get_outputs())):
            if vname not in self.all_variables:
                #mpiprint("%s ADDING zero size for %s" % (self.name, vname))
                self.all_variables[vname] = self._get_var_info(vname)

        # check both ends of each connection.  If either side is not flattenable,
        # then leave both vars out of the scatter vectors by marking them as flat=False
        myvars = self.all_variables
        for sub in self.local_subsystems():
            for u,v in chain(sub.in_edges, sub.out_edges):
                if u in myvars:
                    if not myvars[u].get('flat') and v in myvars:
                        myvars[v]['flat'] = False
                        
                if v in myvars:
                    if not myvars[v].get('flat') and u in myvars:
                        myvars[u]['flat'] = False

        #mpiprint("=== %s: all_variables = %s" % (self.name, self.all_variables.keys()))

    def setup_sizes(self):
        """Given a dict of variables, set the sizes for 
        those that are local.
        """
        #mpiprint("setup_sizes: %s" % self.name)
        comm = self.mpi.comm

        if MPI and comm == MPI.COMM_NULL:
            self.local_var_sizes = numpy.zeros((0,0), int)
            self.input_sizes = numpy.zeros(0, int)
            return

        size = self.mpi.size
        rank = self.mpi.rank

        self.vector_vars = OrderedDict()

        # create a (1 x nproc) vector for the sizes of all of our 
        # local inputs
        self.input_sizes = numpy.zeros(size, int)

        # pass the call down to any subdrivers/subsystems
        # and subassemblies. 
        for sub in self.local_subsystems():
            sub.setup_sizes()

        sizes_add, sizes_noadd, noflats = self._partition_vars()

        #mpiprint("in %s, add=%s, noadd = %s, noflat=%s" % (self.name,sizes_add,sizes_noadd,noflats))

        # create an (nproc x numvars) var size vector containing 
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((size, len(sizes_add)), int)

        for name in sizes_add:
            self.vector_vars[name] = self.all_variables[name]

        for name in sizes_noadd:
            self.vector_subvars[name] = self.all_variables[name]

        for name in noflats:
            self.noflat_vars[name] = self.all_variables[name]

        #mpiprint("%s setup_sizes: vars = %s" % (self.name, self.vector_vars.keys()))
        
        for i, (name, var) in enumerate(self.vector_vars.items()):
            self.local_var_sizes[rank, i] = var['size']

        # collect local var sizes from all of the processes in our comm
        # these sizes will be the same in all processes except in cases
        # where a variable belongs to a multiprocessor component.  In that
        # case, the part of the component that runs in a given process will
        # only have a slice of each of the component's variables.
        #mpiprint("setup_sizes Allgather (var sizes) %s: %d" % (self.name,comm.size))
        if MPI:
            comm.Allgather(self.local_var_sizes[rank,:], 
                           self.local_var_sizes)

        inputs = self.get_inputs(local=True)

        self.input_sizes[rank] = sum([v['size'] 
                                        for n,v in self.vector_vars.items() 
                                           if n in inputs])

        #mpiprint("setup_sizes Allgather (input sizes)")
        if MPI:
            comm.Allgather(self.input_sizes[rank], self.input_sizes)

        #mpiprint("%s input_sizes: %s" % (self.name, self.input_sizes))

    def setup_vectors(self, arrays):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        #mpiprint("setup_vectors: %s" % self.name)
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return

        rank = self.mpi.rank
        if arrays is None:  # we're the top level System in our Assembly
            arrays = {}
            # create top level vectors
            size = numpy.sum(self.local_var_sizes[rank, :])
            for name in ['u', 'f']: #, 'du', 'df']:
                arrays[name] = numpy.zeros(size)

        insize = self.input_sizes[rank]
        inputs = self.get_inputs(local=True)

        for name in ['u', 'f']: #, 'du', 'df']:
            self.vec[name] = VecWrapper(self, arrays[name])

        for name in ['p']:#, 'dp']:
            self.vec[name] = VecWrapper(self, numpy.zeros(insize), 
                                        inputs=inputs)

        #mpiprint("UVEC for %s" % self.name)
        #self.vec['u'].dump('u')
        #mpiprint("PVEC for %s" % self.name)
        #self.vec['p'].dump('p')

        start, end = 0, 0
        for sub in self.local_subsystems():
            sz = numpy.sum(sub.local_var_sizes[sub.mpi.rank, :])
            end += sz
            if end-start > arrays['u'][start:end].size:
                raise RuntimeError("size mismatch: passing [%d,%d] view of size %d array from %s to %s" % 
                            (start,end,arrays['u'][start:end].size,self.name,sub.name))
            sub.setup_vectors(dict([(n,arrays[n][start:end]) for n in
                                        ['u', 'f']])) #,'du', 'df']]))
            start += sz

        return self.vec

    def scatter(self, srcvecname, destvecname, subsystem=None):
        """ Perform data transfer (partial or full scatter or
        send/receive for data that isn't flattenable to a 
        float array.
        """
        if subsystem is None:
            scatter = self.scatter_full
        else:
            scatter = subsystem.scatter_partial

        if scatter is None:
            mpiprint("NO scatter for %s" % self.name)
        #if not scatter is None:
        else:   
            if subsystem is None:
                mpiprint("full scatter for %s" % self.name)
            else:
                mpiprint("scatter %s --> %s" % (self.name, subsystem.name))

            srcvec = self.vec[srcvecname]
            destvec = self.vec[destvecname]

            #mpiprint("scatter_conns = %s" % scatter.scatter_conns)
            scatter(self, srcvec, destvec) #, reverse=??)

        return scatter

    def dump_subsystem_tree(self, nest=0, stream=sys.stdout):
        """Prints out a textual representation of the collapsed
        execution graph (with groups of component nodes collapsed
        into SerialSystems and ParallelSystems).  It shows which
        components run on the current processor.
        """
        #mpiprint("dump_subsystem_tree: %s" % self.name)
        if stream is None:
            getval = True
            stream = StringIO()
        else:
            getval = False

        if MPI and self.mpi.comm == MPI.COMM_NULL:
            mpiprint("returning early for %s" % self.name)
            return stream.getvalue() if getval else None
        
        if MPI is None:
            world_rank = 0
        else:
            world_rank = MPI.COMM_WORLD.rank

        name_map = { 'SerialSystem': 'ser', 'ParallelSystem': 'par',
                     'SimpleSystem': 'simp', 'DriverSystem': 'drv',
                     'AssemblySystem': 'asm', 'InnerAssemblySystem': 'inner' }
        stream.write(" "*nest)
        stream.write(str(self.name).replace(' ','').replace("'",""))
        stream.write(" [%s](req=%d)(rank=%d)(vsize=%d)(isize=%d)\n" % 
                                          (name_map[self.__class__.__name__], 
                                           self.get_req_cpus(), 
                                           world_rank,
                                           self.vec['u'].array.size,
                                           self.input_sizes[self.mpi.rank]))
        inputs = self.get_inputs(local=True)

        for v, (arr, start) in self.vec['u']._info.items():
            stream.write(" "*(nest+2))
            if v in inputs:
                stream.write("u['%s'] (%s)   p['%s'] (%s)\n" % 
                                 (v, list(self.vec['u'].bounds(v)),
                                  v, list(self.vec['p'].bounds(v))))
            else:
                stream.write("u['%s'] (%s)\n" % (v, list(self.vec['u'].bounds(v))))

        if self.scatter_partial:
            noflats = self.scatter_partial.noflat_vars
        elif self.scatter_full:
            noflats = self.scatter_full.noflat_vars
        else:
            noflats = ()
        if self.noflats:
            stream.write(' '*(nest+2) + "= noflats =\n")

        for src, dest in noflats:
            stream.write(" "*(nest+2))
            stream.write("%s --> %s\n" % (src, dest))

        nest += 4
        for sub in self.local_subsystems():
            sub.dump_subsystem_tree(nest, stream)

        return stream.getvalue() if getval else None

    def _partition_vars(self):
        """If a subvar has a basevar that is also included in a
        var vector, then the size of the subvar does not add
        to the total size of the var vector because it's size
        is already included in its basevar size. Also, unflattenable
        vars must be handled separately from the var vector.

        This method returns (sizes, nosizes, noflat), where sizes is a list 
        of vars/subvars that add to the size of the var vector and 
        nosizes is a list of subvars that are flattenable but do not, 
        and noflat is a list of vars/subvars that are not flattenable.

        The items in each list will have the same ordering as they
        had in the original list of names.
        """
        nosizes = []
        sizes = []
        noflats = []
        vardict = self.all_variables
        nameset = set(vardict.keys())

        for name, info in vardict.items():
            if not info.get('flat', True):
                noflats.append(name)
            elif '[' in name:
                base = name.split('[', 1)[0]
                if base in nameset:
                    nosizes.append(name)
                    #mpiprint("adding %s to nosizes, %s" % (name, nameset))
                else:
                    sizes.append(name)
            else:
                base = name
                if '.' in name and base.rsplit('.', 1)[0] in nameset:
                    nosizes.append(name)
                    #mpiprint("adding %s to nosizes, %s" % (name, nameset))
                else:
                    sizes.append(name)

        return (sizes, nosizes, noflats)



class SimpleSystem(System):
    """A System for a single Component."""
    def __init__(self, depgraph, scope, name):
        if isinstance(name, basestring):
            comp = getattr(scope, name)
            super(SimpleSystem, self).__init__(scope, depgraph, 
                          depgraph.find_prefixed_nodes(comp.get_full_nodeset()), 
                          name)
        else: # system for a bunch of boundary vars
            super(SimpleSystem, self).__init__(scope, depgraph, name, str(name))
            comp = None

        if has_interface(comp, IImplicitComponent):
            self._apply_F = self._implicit_apply_F
        else:
            self._apply_F = self._explicit_apply_F

        self._comp = comp
        if comp is None:
            self.mpi.requested_cpus = 1
        else:
            self.mpi.requested_cpus = self._comp.get_req_cpus()
        #mpiprint("%s simple inputs = %s" % (self.name, self.get_inputs()))

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        comp = self._comp
        mpiprint("running simple system %s" % self.name)
        # if not isinstance(comp, PseudoComponent):
        #     comp.set_itername('%s-%d' % (iterbase, 1))

        #mpiprint("%s.run  (system)" % comp.name)
        self.scatter('u','p')
        if comp is not None:
            if self._comp.parent is not None and 'p' in self.vec:
                self.vec['p'].set_to_scope(comp.parent)
                #mpiprint("=== P vector for %s before: %s" % (comp.name, self.vec['p'].items()))
            comp.set_itername('%s-%s' % (iterbase, comp.name))
            comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
            if self._comp.parent is not None and 'u' in self.vec:
                self.vec['u'].set_from_scope(comp.parent)
            #mpiprint("=== U vector for %s after: %s" % (comp.name,self.vec['u'].items()))
            # for vname in chain(comp.list_inputs(connected=True), comp.list_outputs(connected=True)):
            #     mpiprint("%s.%s = %s" % (comp.name,vname,getattr(comp,vname)))

    def stop(self):
        if self._comp is not None:
            self._comp.stop()

    def setup_communicators(self, comm):
        #size = comm.size if MPI else 1
        #mpiprint("setup_communicators (size=%d): %s" % (size,self.name))
        self.mpi.comm = comm

    #def setup_variables(self):
        ##mpiprint("setup_variables: %s" % self.name)

        #super(SimpleSystem, self).setup_variables()

        #for name, vdict in self.all_variables.items():
            #parts = name.split('.',1)
            #if len(parts) > 1:
                #cname, vname = parts
                #child = getattr(self.scope, cname)
            #else:
                #cname, vname = '', name
                #child = self.scope
            #info = child.get_float_var_info(vname)
            #if info is None:
                #vdict['flat'] = False
            #else:  # variable is flattenable to a float array
                #sz, flat_idx, base = info
                #vdict['size'] = sz
                #if flat_idx is not None:
                    #vdict['flat_idx'] = flat_idx
                #if base is not None:
                    #if cname:
                        #bname = '.'.join((cname, base))
                    #else:
                        #bname = base
                    #vdict['basevar'] = bname

    def setup_scatters(self):
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        #mpiprint("setup_scatters: %s" % self.name)
        rank = self.mpi.rank
        start = numpy.sum(self.input_sizes[:rank])
        end = numpy.sum(self.input_sizes[:rank+1])
        dest_idxs = [petsc_linspace(start, end)]
        src_idxs = []
        ukeys = self.vec['u'].keys()
        pkeys = self.vec['p'].keys()
        scatter_conns = []
        other_conns = []
        for dest in pkeys:
            ivar = ukeys.index(dest)
            scatter_conns.append((dest,dest))
            # FIXME: currently just using the local var size for input size
            src_idxs.append(numpy.sum(self.local_var_sizes[:, :ivar]) + # ??? args[arg] - user really needs to be able to define size for multi-proc comps
                                  petsc_linspace(0, self.local_var_sizes[rank,ivar]))
        if len(idx_merge(src_idxs)) != len(idx_merge(dest_idxs)):
            raise RuntimeError("ERROR: setting up scatter: (%d != %d) srcs: %s,  dest: %s in %s" % 
                                (len(src_idxs), len(dest_idxs), src_idxs, dest_idxs, self.name))
        
        other_conns = [(n,n) for n in self.get_inputs() if n not in pkeys]
        
        if scatter_conns or other_conns:
            self.scatter_full = DataTransfer(self, src_idxs, dest_idxs, 
                                             scatter_conns, other_conns)

    def apply_F(self):
        return self._apply_F()

    def _implicit_apply_F(self):
        self.scatter('u','p')
        comp = self._comp
        if self._comp.parent is not None:
            self.vec['p'].set_to_scope(self._comp.parent)
            #mpiprint("=== P vector for %s before: %s" % (comp.name, self.vec['p'].items()))
        comp.evaluate()
        if self._comp.parent is not None:
            self.vec['u'].set_from_scope(self._comp.parent)
        #vec['f'].array[:] = vec['u'].array

    def _explicit_apply_F(self):
        """ F_i(p_i,u_i) = u_i - G_i(p_i) = 0 """
        #mpiprint("%s.apply_F" % self.name)
        vec = self.vec
        self.scatter('u','p')
        comp = self._comp
        vec['f'].array[:] = vec['u'].array[:]
        if self._comp.parent is not None:
            self.vec['p'].set_to_scope(self._comp.parent)
            #mpiprint("=== P vector for %s before: %s" % (comp.name, self.vec['p'].items()))
        comp.run()
        if self._comp.parent is not None:
            self.vec['u'].set_from_scope(self._comp.parent)
        #mpiprint("=== U vector for %s after: %s" % (comp.name,self.vec['u'].items()))
        #mpiprint("=== F vector for %s after: %s" % (comp.name,self.vec['f'].items()))
        vec['f'].array[:] -= vec['u'].array[:]
        vec['u'].array[:] += vec['f'].array[:]
        #mpiprint("after apply_F, f = %s" % self.vec['f'].array)



class AssemblySystem(SimpleSystem):
    """A System to handle an Assembly."""

    def setup_communicators(self, comm):
        super(AssemblySystem, self).setup_communicators(comm)
        self._comp.setup_communicators(comm)

    def setup_variables(self):
        super(AssemblySystem, self).setup_variables()
        self._comp.setup_variables()

    def setup_sizes(self):
        super(AssemblySystem, self).setup_sizes()
        self._comp.setup_sizes()

    def setup_vectors(self, arrays):
        super(AssemblySystem, self).setup_vectors(arrays)
        # internal Assembly will create new vectors
        self._comp.setup_vectors() 

    def setup_scatters(self):
        super(AssemblySystem, self).setup_scatters()
        self._comp.setup_scatters()


class CompoundSystem(System):
    """A System that has subsystems."""

    def __init__(self, scope, depgraph, subg, name=None):        
        super(CompoundSystem, self).__init__(scope, depgraph, 
                                             get_full_nodeset(depgraph, scope, subg.nodes()), name)
        self.driver = None
        self.graph = subg
        self._local_subsystems = []  # subsystems in the same process
        self._ordering = None

    def get_inputs(self, local=False):
        if local:
            systems = self.local_subsystems()
        else:
            systems = self.all_subsystems()

        inputs = set()
        for sub in systems:
            inputs.update(sub.get_inputs())
        return inputs

    def get_outputs(self, local=False):
        # the full set of outputs is stored in the 
        # metadata of this System's graph.
        if local:
            systems = self.local_subsystems()
        else:
            systems = self.all_subsystems()

        outputs = set()
        for sub in systems:
            outputs.update(sub.get_outputs())
        return outputs
        
    def local_subsystems(self):
        if MPI:
            return self._local_subsystems
        else:
            return self.all_subsystems()

    def all_subsystems(self):
        for node, data in self.graph.nodes_iter(data=True):
            yield data['system']

    def setup_scatters(self):
        """ Defines a scatter for args at this system's level """
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return
        #mpiprint("setup_scatters: %s" % self.name)
        var_sizes = self.local_var_sizes
        input_sizes = self.input_sizes
        rank = self.mpi.rank

        if MPI:
            start = numpy.sum(var_sizes[:rank, :])
            end = numpy.sum(var_sizes[:rank+1, :])
            petsc_idxs = petsc_linspace(start, end)
    
            app_idxs = []
            for ivar in xrange(len(self.vector_vars)):
                start = numpy.sum(var_sizes[:, :ivar]) + numpy.sum(var_sizes[:rank, ivar])
                end = start + var_sizes[rank, ivar]
                app_idxs.append(petsc_linspace(start, end))
            app_idxs = numpy.concatenate(app_idxs)
    
            app_ind_set = PETSc.IS().createGeneral(app_idxs, comm=self.mpi.comm)
            petsc_ind_set = PETSc.IS().createGeneral(petsc_idxs, comm=self.mpi.comm)
            self.app_ordering = PETSc.AO().createBasic(app_ind_set, petsc_ind_set, 
                                                       comm=self.mpi.comm)

        #mpiprint("app indices:   %s\npetsc indices: %s" %
                  #(app_ind_set.getIndices(), petsc_ind_set.getIndices()))
        src_full = []
        dest_full = []
        scatter_conns_full = []
        other_conns_full = []
        noflats = set([k for k,v in self.all_variables.items()
                           if not v.get('flat',True)])

        start = end = numpy.sum(input_sizes[:rank])
        varkeys = self.vector_vars.keys()
        
        if varkeys != self.vec['u'].keys():
            raise RuntimeError("varkeys != u vector!")

        # FIXME: not sure why this loops over all subsystems and not
        #        just local...
        for subsystem in self.all_subsystems():
            if MPI and subsystem.mpi.comm == MPI.COMM_NULL:
                continue
            #mpiprint("setting up scatters from %s to %s" % (self.name, subsystem.name))
            src_partial = []
            dest_partial = []
            scatter_conns = []
            other_conns = []  # non-flattenable vars
            if subsystem in self.local_subsystems():
                for src, dest in subsystem.in_edges:
                    if dest in noflats:
                        other_conns.append((src, dest))
                        other_conns_full.append((src, dest))
                    else:
                        try:
                            isrc = varkeys.index(src)
                        except (ValueError, KeyError):
                            # other_conns.append((src, dest))
                            # other_conns_full.append((src, dest))
                            pass # scatter should have already happened at higher level
                        else:
                            dest_idxs = self.vec['p'].indices(dest)
                            #mpiprint("dest indices of %s = %s" % (dest, dest_idxs))
                            src_idxs = numpy.sum(var_sizes[:, :isrc]) + \
                                              petsc_linspace(0, dest_idxs.shape[0]) #args[arg]
                            #mpiprint("src indices of %s = %s" % (src, src_idxs))
                            scatter_conns.append((src,dest))
                            scatter_conns_full.append((src,dest))
                            src_partial.append(src_idxs)
                            dest_partial.append(dest_idxs)

                src_full.extend(src_partial)
                dest_full.extend(dest_partial)

            #mpiprint("PARTIAL scatter setup: %s to %s: %s\n%s" % (self.name, subsystem.name,
            #                                                  src_partial, dest_partial))
            if scatter_conns or other_conns:
                subsystem.scatter_partial = DataTransfer(self, src_partial, 
                                                         dest_partial, 
                                                         scatter_conns, other_conns)

        if scatter_conns_full or other_conns_full:
            self.scatter_full = DataTransfer(self, src_full, dest_full, 
                                             scatter_conns_full, other_conns_full)

        for sub in self.local_subsystems():
            sub.setup_scatters()

    def apply_F(self):
        """ Delegate to subsystems """
        #mpiprint("%s.apply_F" % self.name)
        self.scatter('u','p')
        for subsystem in self.local_subsystems():
            subsystem.apply_F()
        #mpiprint("=== U vector for %s after: %s" % (self.name,self.vec['u'].items()))
        #mpiprint("=== F vector for %s after: %s" % (self.name,self.vec['f'].items()))

    def stop(self):
        for s in self.all_subsystems():
            s.stop()


class SerialSystem(CompoundSystem):

    def __init__(self, scope, depgraph, subg, name=None):
        super(SerialSystem, self).__init__(scope, depgraph, subg, name)
        self._ordering = None

    def all_subsystems(self):
        for node in self._ordering:
            yield self.graph.node[node]['system']

    def set_ordering(self, ordering):
        """Return the execution order of our subsystems."""
        self._ordering = [n for n in ordering if n in self.graph]
        for node in self.graph.nodes_iter():
            if node not in self._ordering:
                self._ordering.append(node)

        if nx.is_directed_acyclic_graph(self.graph):
            g = self.graph
        else:
            # don't modify real graph
            g = self.graph.subgraph(self.graph.nodes())
            break_cycles(g)

        self._ordering = gsort(get_all_deps(g), self._ordering)

        for s in self.all_subsystems():
            s.set_ordering(ordering)

    def get_req_cpus(self):
        cpus = []
        for sub in self.all_subsystems():
            cpus.append(sub.get_req_cpus())
        self.mpi.requested_cpus = max(cpus+[1])
        return self.mpi.requested_cpus

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        mpiprint("running serial system %s: %s" % (self.name, [c.name for c in self.local_subsystems()]))
        self._stop = False
        for sub in self.local_subsystems():
            self.scatter('u', 'p', sub)
            sub.run(iterbase, ffd_order, case_label, case_uuid)
            if self._stop:
                raise RunStopped('Stop requested')

    def setup_communicators(self, comm):
        if comm is not None:
            mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
        self._local_subsystems = []

        #mpiprint("setup_comms Split (serial)")
        self.mpi.comm = comm #get_comm_if_active(self, comm)
        #if self.mpi.comm == MPI.COMM_NULL:
        #    return

        for sub in self.all_subsystems():
            self._local_subsystems.append(sub)
            sub.setup_communicators(self.mpi.comm)


class ParallelSystem(CompoundSystem):

    def get_req_cpus(self):
        cpus = 0
        # in a parallel system, the required cpus is the sum of
        # the required cpus of the members
        for node, data in self.graph.nodes_iter(data=True):
            cpus += data['system'].get_req_cpus()
        self.mpi.requested_cpus = cpus
        return cpus
 
    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        #mpiprint("running parallel system %s: %s" % (self.name, [c.name for c in self.local_subsystems()]))
        # don't scatter unless we contain something that's actually 
        # going to run
        if not self.local_subsystems():
            return

        self.scatter('u', 'p')

        for sub in self.local_subsystems():
            sub.run(iterbase, ffd_order, case_label, case_uuid)

    def setup_communicators(self, comm):
        #mpiprint("setting up comms for %s (size=%d)" % (self.name,comm.size))
        self.mpi.comm = comm
        size = comm.size
        rank = comm.rank

        subsystems = []
        requested_procs = []
        for system in self.all_subsystems():
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
        #mpiprint("requested_procs: %s" % requested_procs)
        #mpiprint("assigned_procs: %s" % assigned_procs)

        self._local_subsystems = []

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
        #mpiprint("setup_comms Split (par)")
        sub_comm = comm.Split(rank_color)

        if sub_comm == MPI.COMM_NULL:
            return

        #mpiprint("RANKCOLOR: %d,  COLOR: %s, comm.size: %d, subcomm.size: %d" % (rank_color, color,comm.size,sub_comm.size))
        for i,sub in enumerate(subsystems):
            if i == rank_color:
                self._local_subsystems.append(sub)
            elif requested_procs[i] == 0:  # sub is duplicated everywhere
                self._local_subsystems.append(sub)

        for sub in self.local_subsystems():
            sub.setup_communicators(sub_comm)
             
    def setup_variables(self):
        """ Determine variables from local subsystems """
        #mpiprint("setup_variables: %s" % self.name)
        self.all_variables = OrderedDict()
        if MPI and self.mpi.comm == MPI.COMM_NULL:
            return

        for sub in self.local_subsystems():
            sub.setup_variables()

        if self.local_subsystems():
            sub = self.local_subsystems()[0]
            names = sub.all_variables.keys()
        else:
            sub = None
            names = []
        #mpiprint("%s before ALLGATHER, varkeys=%s" % (self.name, names))
        #mpiprint("setup_variables Allgather")
        varkeys_list = self.mpi.comm.allgather(names)
        #mpiprint("%s after ALLGATHER, varkeys = %s" % (self.name,varkeys_list))
        for varkeys in varkeys_list:
            for name in varkeys:
                self.all_variables[name] = self._get_var_info(name)

        for sub in self.local_subsystems():
            for name, var in sub.all_variables.items():
                self.all_variables[name] = var

class DriverSystem(SimpleSystem):
#class DriverSystem(SerialSystem):
    """A System for a Driver component."""

    def __init__(self, driver):
        driver.setup_systems()
        depgraph = driver.get_depgraph()
        scope = driver.parent
        #subg = depgraph.subgraph(driver.name).copy()
        #subg.node[driver.name]['system'] = driver.workflow._system
        #super(DriverSystem, self).__init__(scope, depgraph, subg, driver.name)
        super(DriverSystem, self).__init__(depgraph, scope, driver.name)
        driver._system = self
        
    #def set_ordering(self, ordering):
        #self._ordering = [self.name]  
        ## our workflow will set the order of its subsystem

    def setup_communicators(self, comm):
        #size = comm.size if MPI else 1
        #mpiprint("setup_communicators (size=%d): %s" % (size,self.name))
        self._comp.setup_communicators(self.mpi.comm)

    def setup_variables(self):
        super(DriverSystem, self).setup_variables()
        self._comp.setup_variables()

    def setup_sizes(self):
        super(DriverSystem, self).setup_sizes()
        self._comp.setup_sizes()

    def setup_vectors(self, arrays):
        super(DriverSystem, self).setup_vectors(arrays)
        self._comp.setup_vectors(arrays)

    def setup_scatters(self):
        super(DriverSystem, self).setup_scatters()
        self._comp.setup_scatters()
      
    def local_subsystems(self):
        return self.all_subsystems()

    def all_subsystems(self):
        return (self._comp.workflow._system,)


class InnerAssemblySystem(SerialSystem):
    """A system to handle data transfer to an Assembly
    boundary from its inner components. It splits the entire
    graph into three pieces, a boundary input system, a top
    driver system, and a boundary out system.
    """
    def __init__(self, scope):
        drvname = scope._top_driver.name
        depgraph = scope.get_depgraph()

        conns = scope.list_connections()
        srcset = set([u for u,v in conns])
        destset = set([v for u,v in conns])
        
        bndry_outs = [v for v in scope.list_outputs() if v in destset]
        bndry_ins = [v for v in scope.list_inputs() if v in srcset]

        g = nx.DiGraph()
        g.add_node(drvname)
        g.node[drvname]['system'] = DriverSystem(scope._top_driver)

        self.bins = bins = tuple(bndry_ins)
        self.bouts = bouts = tuple(bndry_outs)

        ordering = []

        if bins:
            g.add_node(bins)
            g.add_edge(bins, drvname)
            g.node[bins]['system'] = SimpleSystem(depgraph, scope, bins)
            ordering.append(bins)
        
        ordering.append(drvname)

        if bouts:
            g.add_node(bouts)
            g.add_edge(drvname, bouts)       
            g.node[bouts]['system'] = SimpleSystem(depgraph, scope, bouts)
            ordering.append(bouts)

        super(InnerAssemblySystem, self).__init__(scope, depgraph, g, '_inner_asm')
        self.set_ordering(ordering)

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        self.vec['u'].set_from_scope(self.scope, self.bins)
        super(InnerAssemblySystem, self).run(iterbase, ffd_order, 
                                             case_label, case_uuid)
        self.vec['u'].set_to_scope(self.scope, self.bouts)

def _create_simple_sys(depgraph, scope, name):
    comp = getattr(scope, name)

    if has_interface(comp, IDriver):
        sub = DriverSystem(comp)
    elif has_interface(comp, IAssembly):
        sub = AssemblySystem(depgraph, scope, name)
    else:
        sub = SimpleSystem(depgraph, scope, name)
    return sub

def partition_mpi_subsystems(depgraph, cgraph, scope):
    """Return a nested system graph with metadata for parallel
    and serial subworkflows.  Graph must acyclic. All subdriver
    iterations sets must have already been collapsed.
    
    """
    if len(cgraph) < 2:
        return cgraph

    gcopy = cgraph.copy()

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
                    #parallel_group.append(tuple(sorted(brnodes)))
                    parallel_group.append(tuple(brnodes))
                else:
                    parallel_group.append(brnodes[0])

            for branch in parallel_group:
                if isinstance(branch, tuple):
                    to_remove.extend(branch)
                    subg = cgraph.subgraph(branch)  #_precollapse(scope, g, branch)
                    partition_mpi_subsystems(depgraph, subg, scope)
                    #mpiprint("%d adding system for %s %s" % (id(g),type(branch),str(branch)))
                    cgraph.add_node(branch, system=SerialSystem(scope, depgraph, subg, str(branch)))
                    gcopy.remove_nodes_from(branch)
                else: # single comp system
                    gcopy.remove_node(branch)

            #parallel_group = tuple(sorted(parallel_group))
            parallel_group = tuple(parallel_group)
            to_remove.extend(parallel_group)
            subg = cgraph.subgraph(parallel_group)  #_precollapse(scope, g, parallel_group)
            #mpiprint("%d adding system for %s %s" % (id(g),type(parallel_group),str(parallel_group)))
            cgraph.add_node(parallel_group, 
                       system=ParallelSystem(scope, depgraph, subg, str(parallel_group)))
        elif len(zero_in_nodes) == 1:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)
        else: # circular - no further splitting
            break

    # Now remove all of the old nodes
    cgraph.remove_nodes_from(to_remove)

    return cgraph

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


def get_comm_if_active(obj, comm):
    if comm is None or comm == MPI.COMM_NULL:
        return comm

    req = obj.get_req_cpus()
    #mpiprint("rank+1: %d,  req: %d" % (comm.rank+1,req))
    if comm.rank+1 > req:
        color = MPI.UNDEFINED
    else:
        color = 1

    newcomm = comm.Split(color)
    # if isinstance(obj, System):
    #     name = obj.name
    # else:
    #     name = obj._parent.name
    # if newcomm == MPI.COMM_NULL:
    #     mpiprint("NULL COMM for %s" % name)
    # else:
    #     mpiprint("active COMM (size %d) for %s" %(newcomm.size, name))
    return newcomm


def simple_node_iter(nodes):
    """Return individual nodes from an iterator containing nodes and 
    iterators of nodes.
    """
    if isinstance(nodes, basestring):
        nodes = (nodes,)
        
    for node in nodes:
        if isinstance(node, basestring):
            yield node
        else:
            for n in simple_node_iter(node):
                yield n

def get_full_nodeset(depgraph, scope, group):
    names = set()
    for name in simple_node_iter(group):
        obj = getattr(scope, name)
        if hasattr(obj, 'get_full_nodeset'):
            names.update(obj.get_full_nodeset())
        else:
            names.update(name)
    return depgraph.find_prefixed_nodes(names)
