import sys
from StringIO import StringIO
from collections import OrderedDict
from itertools import chain

import numpy
import networkx as nx

# pylint: disable-msg=E0611,F0401
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, PETSc
from openmdao.main.exceptions import RunStopped
from openmdao.main.finite_difference import FiniteDifference
from openmdao.main.linearsolver import ScipyGMRES, PETSc_KSP
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly, IImplicitComponent, ISolver
from openmdao.main.vecwrapper import VecWrapper, InputVecWrapper, DataTransfer, idx_merge, petsc_linspace
from openmdao.main.depgraph import break_cycles, get_node_boundary, \
                                   is_driver_node, get_all_deps, gsort, \
                                   collapse_nodes

def call_if_found(obj, fname, *args, **kwargs):
    """If the named function exists in the object, call it
    with the provided args.
    """
    if hasattr(obj, fname):
        return getattr(obj, fname)(*args, **kwargs)

class System(object):
    def __init__(self, scope, nodes, name):
        self.scope = scope
        self.name = str(name)
        self._nodes = nodes

        self.variables = OrderedDict() # dict of all vars owned by this System (outputs)
        self.flat_vars = OrderedDict() # all vars used in vectors, whether they add to vector size or not
        self.vector_vars = OrderedDict() # all vars that contribute to the size of vectors
        self.vector_noadds = OrderedDict() # all vars in vectors that do not contribute to their size
        self.noflat_vars = OrderedDict() # all vars that are not flattenable to float arrays (so are not part of vectors)

        graph = self.scope._reduced_graph

        self._out_nodes = []

        # find our output nodes (outputs from our System and any child Systems)
        for node in nodes:
            self._out_nodes.extend(graph.successors(node))

        # get our input nodes from the depgraph
        self._in_nodes, _ = get_node_boundary(graph, nodes.union(self._out_nodes))

        self._out_nodes = sorted(self._out_nodes)
        self._in_nodes = sorted(self._in_nodes)

        self.mpi = MPI_info()
        self.mpi.requested_cpus = None
        self.vec = {}
        self.app_ordering = None
        self.scatter_full = None
        self.scatter_partial = None

        # Derivatives stuff
        self.mode = None
        self.sol_vec = None
        self.rhs_vec = None
        self.solver = None
        self.fd_solver = None
        self.sol_buf = None
        self.rhs_buf = None

    def find(self, name):
        """A convenience method to allow easy access to descendants."""
        for sub in self.subsystems():
            if name == sub.name:
                return sub
            s = sub.find(name)
            if s is not None:
                return s
        return None

    def initialize_gradient_solver(self):
        """ Initialize the solver that will be used to calculate the
        gradient. """

        if self.solver is None:

            if MPI:
                self.solver = PETSc_KSP(self)
            else:
                self.solver = ScipyGMRES(self)

    def subsystems(self, local=False):
        if local:
            return self.local_subsystems()
        return self.all_subsystems()

    def get_owned_args(self):
        args = []
        for sub in self.simple_subsystems(local=True):
            args.extend([arg for arg in sub._in_nodes
                            if arg in self.variables and
                               (arg not in sub.variables or sub is self)])
        return args

    def get_inputs(self):
        inputs = []
        for system in self.simple_subsystems():
            for tup in system._in_nodes:
                for dest in tup[1]:
                    parts = dest.split('.', 1)
                    if parts[0] in system._nodes:
                        inputs.append(dest)
        return inputs

    def get_outputs(self, local=False):
        outputs = []
        for system in self.simple_subsystems():
            for src, _ in system._out_nodes:
                parts = src.split('.', 1)
                if parts[0] in system._nodes:
                    outputs.append(src)
        return outputs

    def get_size(self, names):
        """Return the total size of the variables
        corresponding to the given names.
        """
        size = 0
        uvec = self.vec['u']
        for name in names:
            # Not sure if derivatives will need local size or global
            # size, but for now, assuming local and using VecWrapper
            #if isinstance(name, tuple):
                #name = name[0]
            #if name in self.variables:
                #size += self.variables[name]['size']
            #else:
                #size += self.all_args[name]['size']

            # Param target support.
            if isinstance(name, tuple):
                name = name[0]

            size += uvec[name].size
        return size

    def set_ordering(self, ordering):
        pass

    def is_active(self):
        return MPI is None or self.mpi.comm != MPI.COMM_NULL

    def local_subsystems(self):
        return ()

    def all_subsystems(self):
        return ()

    def get_req_cpus(self):
        return self.mpi.requested_cpus

    def _get_var_info(self, node):
        vdict = { 'size': 0 }

        # use the name of the src
        name = node[0]

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

    def _flat_only(self, names):
        """Return only those names that are flattenable variables."""
        return [n for n in names if n in self.flat_vars]

    def setup_variables(self):
        #mpiprint("setup_variables: %s" % self.name)

        self.variables = OrderedDict()
        self.all_args = OrderedDict()

        for sub in self.local_subsystems():
            sub.setup_variables()
            self.variables.update(sub.variables)
            self.all_args.update(sub.all_args)

        for vname in self._out_nodes:
            if vname not in self.variables:
                self.variables[vname] = self._get_var_info(vname)

        # gather input info too, but keep separate to avoid adding to
        # u vector
        for vname in self._in_nodes:
            if vname not in self.all_args:
                self.all_args[vname] = self._get_var_info(vname)
                self.all_args[vname[0]] = self.all_args[vname]
                for n in vname[1]:
                    self.all_args[n] = self.all_args[vname]

        # add any driver inputs
        for vname in self._in_nodes:
            src, dests = vname
            if src == dests[0] and vname not in self.variables:
                self.variables[vname] = self._get_var_info(vname)

    def setup_sizes(self):
        """Given a dict of variables, set the sizes for
        those that are local.
        """
        #mpiprint("setup_sizes: %s" % self.name)
        comm = self.mpi.comm

        if not self.is_active():
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

        adds, noadds = self._get_vector_vars_and_subs(self.variables)

        # create an (nproc x numvars) var size vector containing
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((size, len(adds)), int)

        # set up vector so all outputs are contiguous and all inputs are contiguous
        for name in adds:
            self.vector_vars[name] = self.variables[name]

        for name in noadds:
            self.vector_noadds[name] = self.variables[name]

        for name in self._get_flat_vars(self.variables):
            self.flat_vars[name] = self.variables[name]

        for name, info in self.variables.items():
            if name not in self.flat_vars:
                self.noflat_vars[name] = info

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

        #self.input_sizes[rank] = sum([self.scope._system.variables[n]['size']
                                       #for n in self._in_nodes])
        for sub in self.simple_subsystems(local=True):
            for arg in sub._in_nodes:
                if (sub is self and arg in sub.variables) or \
                   (arg not in sub.variables and arg in self.variables):
                    self.input_sizes[rank] += self.scope._system.variables[arg]['size']

        #mpiprint("setup_sizes Allgather (input sizes)")
        if MPI:
            comm.Allgather(self.input_sizes[rank], self.input_sizes)

        #mpiprint("%s input_sizes: %s" % (self.name, self.input_sizes))

    def setup_vectors(self, arrays=None, parent_vec=None):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        #mpiprint("setup_vectors: %s" % self.name)
        if not self.is_active():
            return

        rank = self.mpi.rank
        if arrays is None:  # we're the top level System in our Assembly
            #mpiprint("NO array given. %s must be top level" % self.name)
            arrays = {}
            # create top level vectors
            size = numpy.sum(self.local_var_sizes[rank, :])
            for name in ['u', 'f', 'du', 'df']:
                arrays[name] = numpy.zeros(size)

        #mpiprint("given uvec size is %d for %s" % (arrays['u'].size, self.name))
        for name in ['u', 'f', 'du', 'df']:
            self.vec[name] = VecWrapper(self, arrays[name])

        insize = self.input_sizes[rank]

        for name in ['p', 'dp']:
            self.vec[name] = InputVecWrapper(self, numpy.zeros(insize))

        start, end = 0, 0
        for sub in self.local_subsystems():
            # FIXME: not sure if driver systems with overlapping iteration sets
            # will work at all in MPI, so for now, if MPI is active, assume
            # no systems have any overlapping array views, else raise an exception.
            #if MPI:
            sz = numpy.sum(sub.local_var_sizes[sub.mpi.rank, :])
            end += sz
            if end-start > arrays['u'][start:end].size:
                raise RuntimeError("size mismatch: passing [%d,%d] view of size %d array from %s to %s" %
                            (start,end,arrays['u'][start:end].size,self.name,sub.name))
            ##else:
            # subvecvars = sub.vector_vars.keys()
            # if subvecvars:
            #     start, end = self.vec['u'].bounds(subvecvars)
            # else:
            #     start, end = 0, 0

            # if end-start != numpy.sum(sub.local_var_sizes[sub.mpi.rank, :]):
            #     raise RuntimeError("size mismatch: passing [%d,%d] view of size %d array from %s to %s" %
            #                        (start,end,arrays['u'][start:end].size,self.name,sub.name))


            subarrays = {}
            for n in ('u', 'f', 'du', 'df'):
                subarrays[n] = arrays[n][start:end]

            if parent_vec is None:
                parent_vec = self.vec['u']
            sub.setup_vectors(subarrays, parent_vec)

            #if MPI:
            start += sz

        return self.vec

    def setup_args(self):
        # TODO: find a less hacky way to do this...
        for sub in self.local_subsystems():
            for simple in sub.simple_subsystems():
                for arg in self.vec['p']._info.keys():
                    sub.vec['p'][arg] = self.vec['p'][arg]
                    sub.vec['dp'][arg] = self.vec['dp'][arg]
                    #sub.vec['p0'][arg] = self.vec['p0'][arg]
            sub.setup_args()
            for simple in sub.simple_subsystems():
                for arg in sub._in_nodes:
                #for arg in sub.vec['p']._info.keys():
                    self.vec['p'][arg] = sub.vec['p'][arg]
                    self.vec['dp'][arg] = sub.vec['dp'][arg]
                    #self.vec['p0'][arg] = sub.vec['p0'][arg]

    def scatter(self, srcvecname, destvecname, subsystem=None):
        """ Perform data transfer (partial or full scatter or
        send/receive for data that isn't flattenable to a
        float array.
        """
        if subsystem is None:
            scatter = self.scatter_full
        else:
            scatter = subsystem.scatter_partial

        if scatter is not None:
            srcvec = self.vec[srcvecname]
            destvec = self.vec[destvecname]

            #mpiprint("scatter_conns = %s" % scatter.scatter_conns)
            scatter(self, srcvec, destvec) #, reverse=??)

        return scatter

    def simple_dump(self):
        mpiprint(self.name)
        mpiprint("u:")
        for name, info in self.vec['u']._info.items():
            mpiprint("%s: [%d:%d]" % (name, info[1], info[1]+info[0].size))
        mpiprint("p:")
        for name, info in self.vec['p']._info.items():
            mpiprint("%s: [%d:%d]" % (name, info[1], info[1]+info[0].size))
        for sub in self.subsystems():
            mpiprint("")
            sub.simple_dump()

    def dump(self, nest=0, stream=sys.stdout):
        """Prints out a textual representation of the collapsed
        execution graph (with groups of component nodes collapsed
        into Systems).  It shows which
        components run on the current processor.
        """
        #mpiprint("dump: %s" % self.name)
        if stream is None:
            getval = True
            stream = StringIO()
        else:
            getval = False

        if not self.is_active():
            #mpiprint("returning early for %s" % str(self.name))
            return stream.getvalue() if getval else None

        if MPI is None:
            world_rank = 0
        else:
            world_rank = MPI.COMM_WORLD.rank

        name_map = { 'SerialSystem': 'ser', 'ParallelSystem': 'par',
                     'SimpleSystem': 'simp', 'NonSolverDriverSystem': 'drv',
                     'SolverSystem': 'slv', 'BoundarySystem': 'bnd',
                     'AssemblySystem': 'asm', 'InnerAssemblySystem': 'inner',
                     'ExplicitSystem': 'exp' }
        stream.write(" "*nest)
        stream.write(str(self.name).replace(' ','').replace("'",""))
        stream.write(" [%s](req=%d)(rank=%d)(vsize=%d)(isize=%d)\n" %
                                          (name_map[self.__class__.__name__],
                                           self.get_req_cpus(),
                                           world_rank,
                                           self.vec['u'].array.size,
                                           self.input_sizes[self.mpi.rank]))
        inputs = self.get_owned_args()

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
        if noflats:
            stream.write(' '*(nest+2) + "= noflats =\n")

        for src, dest in noflats:
            stream.write(" "*(nest+2))
            stream.write("%s --> %s\n" % (src, dest))

        nest += 4
        for sub in self.local_subsystems():
            sub.dump(nest, stream)

        return stream.getvalue() if getval else None

    def _get_flat_vars(self, vardict):
        """Return a list of names of vars that represent variables that are
        flattenable to float arrays.
        """
        return [n for n,info in vardict.items() if info.get('flat', True)]

    def _get_vector_vars_and_subs(self, vardict):
        """Return (adds, noadds), where adds are those vars that size the
        vectors, and noadds are vars that are in the vectors but don't
        contribute to the size, e.g. subvars that have a basevar in the vector
        or connected destination vars.
        """
        # FIXME: for now, ignore slicing
        return self._get_flat_vars(vardict), []
        #adds = []
        #noadds = []
        #visited = set()
        #for name in self._get_flat_vars(vardict):
            #src = self._src_map.get(name, name)
            #if src != name:
                #if src not in vardict:
                    #adds.append(name)
                    #continue
                #elif name not in visited:
                    #noadds.append(name)
                    #visited.add(name)
                    #continue
            #name = src
            #if name not in visited:
                #visited.add(name)
                #if '[' in name:
                    #base = name.split('[', 1)[0]
                    #if base in vardict:
                        #noadds.append(name)
                    #else:
                        #adds.append(name)
                #else:
                    #base = name
                    #if '.' in name and base.rsplit('.', 1)[0] in vardict:
                        #noadds.append(name)
                    #else:
                        #adds.append(name)

        #return (adds, noadds)

    def set_options(self, mode, options):
        """ Sets all user-configurable options for this system and all
        subsystems. """

        self.mode = mode
        self.options = options

        if mode == 'forward':
            self.sol_vec = self.vec['du']
            self.rhs_vec = self.vec['df']
        elif mode == 'adjoint':
            self.sol_vec = self.vec['df']
            self.rhs_vec = self.vec['du']

        for subsystem in self.local_subsystems():
            subsystem.set_options(mode, options)

    def linearize(self):
        """ Linearize all subsystems. """

        for subsystem in self.local_subsystems():
            subsystem.linearize()

    def calc_gradient(self, inputs, outputs, mode='auto', options=None,
                      iterbase=''):
        """ Return the gradient for this system. """

        # Mode Precedence
        # -- 1. Direct call argument
        # -- 2. Gradient Options
        # -- 3. Auto determination (when implemented)
        if mode == 'auto':

            # TODO - Support automatic determination of mode
            mode = 'forward'
            #mode = options.derivative_direction

        if options.force_fd is True:
            mode == 'fd'

        self.set_options(mode, options)
        self.initialize_gradient_solver()
        self.linearize()

        self.rhs_vec.array[:] = 0.0
        self.vec['df'].array[:] = 0.0

        if mode == 'fd':
            if self.fd_solver is None:
                self.fd_solver = FiniteDifference(self, inputs, outputs)
            return self.fd_solver.solve(iterbase=iterbase)

        return self.solver.solve(inputs, outputs)

    def applyJ(self):
        """ Apply Jacobian, (dp,du) |-> df [fwd] or df |-> (dp,du) [rev] """
        pass

class SimpleSystem(System):
    """A System for a single Component."""
    def __init__(self, scope, name):
        comp = getattr(scope, name)
        super(SimpleSystem, self).__init__(scope,
                      #depgraph.find_prefixed_nodes(
                        comp.get_full_nodeset(), #),
                      name)
        self._comp = comp
        self.mpi.requested_cpus = self._comp.get_req_cpus()

        self.J = None

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        if self.is_active():
            comp = self._comp
            #mpiprint("running simple system %s" % self.name)

            #mpiprint("%s.run  (system)" % comp.name)
            self.scatter('u','p')
            self.vec['p'].set_to_scope(self.scope)#, self._in_nodes)
            comp.set_itername('%s-%s' % (iterbase, comp.name))
            comp.run(ffd_order=ffd_order, case_uuid=case_uuid)
            self.vec['u'].set_from_scope(self.scope)#, self._out_nodes)

    def stop(self):
        self._comp.stop()

    def simple_subsystems(self, local=False):
        yield self

    # def get_owned_args(self):
    #     return [arg for arg in self._in_nodes if arg in self.variables]

    def setup_communicators(self, comm):
        if comm is not None:
            mpiprint("setup_comms for %s  (%d of %d)" % (self.name, comm.rank, comm.size))
        self.mpi.comm = comm

    def setup_scatters(self):
        if not self.is_active():
            return
        mpiprint("setup_scatters: %s  (%d of %d)" % (self.name,self.mpi.rank,self.mpi.size))
        rank = self.mpi.rank
        start = numpy.sum(self.input_sizes[:rank])
        end = numpy.sum(self.input_sizes[:rank+1])
        dest_idxs = [petsc_linspace(start, end)]
        src_idxs = []
        ukeys = self.vec['u'].keys()
        scatter_conns = []
        other_conns = []
        #srcmap = self.get_src_map()
        #flat_inputs = self._flat_only(self.get_owned_args())
        flat_inputs = self.get_owned_args()
        for dest in flat_inputs:
            ivar = ukeys.index(dest)
            scatter_conns.append((dest,dest))
            # FIXME: currently just using the local var size for input size
            src_idxs.append(numpy.sum(self.local_var_sizes[:, :ivar]) + # ??? args[arg] - user really needs to be able to define size for multi-proc comps
                                  petsc_linspace(0, self.local_var_sizes[rank,ivar]))
        if len(idx_merge(src_idxs)) != len(idx_merge(dest_idxs)):
            raise RuntimeError("ERROR: setting up scatter: (%d != %d) srcs: %s,  dest: %s in %s" %
                                (len(src_idxs), len(dest_idxs), src_idxs, dest_idxs, self.name))

        other_conns = [(n,n) for n in self._in_nodes if n not in flat_inputs]

        if MPI or scatter_conns or other_conns:
            self.scatter_full = DataTransfer(self, src_idxs, dest_idxs,
                                             scatter_conns, other_conns)

    def apply_F(self):
        self.scatter('u', 'p')
        comp = self._comp
        self.vec['p'].set_to_scope(self.scope)
        comp.evaluate()
        self.vec['u'].set_from_scope(self.scope)
        #vec['f'].array[:] = vec['u'].array

    def linearize(self):
        """ Linearize this component. """

        self.J = self._comp.linearize(first=True)


class BoundarySystem(SimpleSystem):
    """A SimpleSystem that has no component to execute. It just
    performs data transfer between a set of boundary variables and the rest
    of the system.
    """
    def __init__(self, scope, name):
        super(SimpleSystem, self).__init__(scope, name, str(name))
        self.mpi.requested_cpus = 1

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        if self.is_active():
            #mpiprint("running boundary system %s" % self.name)
            self.scatter('u', 'p')
            self.vec['p'].set_to_scope(self.scope)

    def stop(self):
        pass

    def apply_F(self):
        self.scatter('u','p')
        self.vec['p'].set_to_scope(self.scope)
        self.vec['u'].set_from_scope(self.scope)


class ExplicitSystem(SimpleSystem):
    """ Simple System with inputs and outputs, but no states or residuals. """

    def apply_F(self):
        """ F_i(p_i,u_i) = u_i - G_i(p_i) = 0 """
        #mpiprint("%s.apply_F" % self.name)
        vec = self.vec
        self.scatter('u', 'p')
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

    def applyJ(self):
        """ df = du - dGdp * dp or du = df and dp = -dGdp^T * df """

        vec = self.vec
        comp = self._comp

        # Forward Mode
        if self.mode == 'forward':

            self.scatter('du', 'dp')

            comp.applyJ(self)
            vec['df'].array[:] *= -1.0

            for var in self.get_outputs():
                vec['df'][var][:] += vec['du'][var][:]

        # Adjoint Mode
        elif self.mode == 'adjoint':

            # Sign on the local Jacobian needs to be -1 before
            # we add in the fake residual. Since we can't modify
            # the 'du' vector at this point without stomping on the
            # previous component's contributions, we can multiply
            # our local 'arg' by -1, and then revert it afterwards.
            vec['df'].array[:] *= -1.0
            comp.applyJT(self)
            vec['df'].array[:] *= -1.0

            for var in self.get_outputs():
                vec['du'][var][:] += vec['df'][var][:]
            self.scatter('du', 'dp')


class AssemblySystem(ExplicitSystem):
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

    def setup_vectors(self, arrays=None, parent_vec=None):
        super(AssemblySystem, self).setup_vectors(arrays, parent_vec)
        # internal Assembly will create new vectors
        self._comp.setup_vectors(arrays, parent_vec)

    def setup_scatters(self):
        super(AssemblySystem, self).setup_scatters()
        self._comp.setup_scatters()


class CompoundSystem(System):
    """A System that has subsystems."""

    def __init__(self, scope, subg, name=None):
        super(CompoundSystem, self).__init__(scope,
                                             get_full_nodeset(scope, subg.nodes()), name)
        self.driver = None
        self.graph = subg
        self._local_subsystems = []  # subsystems in the same process
        self._ordering = ()

    # def get_inputs(self, local=False):
    #     inputs = []
    #     inset = set()
    #     for sub in self.subsystems(local):
    #         for inp in sub.get_inputs(local):
    #             if inp not in inset:
    #                 inputs.append(inp)
    #                 inset.add(inp)

    #     return inputs

    # def get_outputs(self, local=False):
    #     outputs = []
    #     outset = set()
    #     for sub in self.subsystems(local):
    #         for out in sub.get_outputs(local):
    #             if out not in outset:
    #                 outputs.append(out)
    #                 outset.add(out)

    #     return outputs

    def local_subsystems(self):
        if MPI:
            return self._local_subsystems
        else:
            return self.all_subsystems()

    def all_subsystems(self):
        return [data['system'] for node, data in
                     self.graph.nodes_iter(data=True)]

    def simple_subsystems(self, local=False):
        for s in self.subsystems(local=local):
            for sub in s.simple_subsystems(local=local):
                yield sub

    def setup_scatters(self):
        """ Defines a scatter for args at this system's level """
        if not self.is_active():
            return
        mpiprint("setup_scatters: %s  (%d of %d)" % (self.name,self.mpi.rank,self.mpi.size))
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

            if app_idxs:
                app_idxs = numpy.concatenate(app_idxs)

            app_ind_set = PETSc.IS().createGeneral(app_idxs, comm=self.mpi.comm)
            petsc_ind_set = PETSc.IS().createGeneral(petsc_idxs, comm=self.mpi.comm)
            #mpiprint("creating petsc AO for %s" % self.name)
            self.app_ordering = PETSc.AO().createBasic(app_ind_set, petsc_ind_set,
                                                       comm=self.mpi.comm)

        # mpiprint("app indices:   %s\npetsc indices: %s" %
        #           (app_ind_set.getIndices(), petsc_ind_set.getIndices()))
        src_full = []
        dest_full = []
        scatter_conns_full = []
        noflat_conns_full = []
        noflats = set([k for k,v in self.variables.items()
                           if not v.get('flat',True)])

        start = end = numpy.sum(input_sizes[:rank])
        varkeys = self.vector_vars.keys()
        owned_args = set(self.get_owned_args())
        simple_subs = list(self.simple_subsystems())

        for subsystem in self.all_subsystems():
            #mpiprint("setting up scatters from %s to %s" % (self.name, subsystem.name))
            src_partial = []
            dest_partial = []
            scatter_conns = []
            noflat_conns = []  # non-flattenable vars

            if subsystem in simple_subs:
                for node in subsystem._in_nodes:
                    if node in owned_args:
                        if node in noflats:
                            noflat_conns.append(node)
                            noflat_conns_full.append(node)
                        else:
                            isrc = varkeys.index(node)

                            dest_idxs = self.vec['p'].indices(node)
                            src_idxs = numpy.sum(var_sizes[:, :isrc]) + \
                                              petsc_linspace(0, dest_idxs.shape[0]) #args[arg]
                            scatter_conns.append(node)
                            scatter_conns_full.append(node)
                            src_partial.append(src_idxs)
                            dest_partial.append(dest_idxs)

                src_full.extend(src_partial)
                dest_full.extend(dest_partial)

            # mpiprint("PARTIAL scatter setup: %s to %s: %s\n%s" % (self.name, subsystem.name,
            #                                                       src_partial, dest_partial))
            if MPI or scatter_conns or noflat_conns:
                subsystem.scatter_partial = DataTransfer(self, src_partial,
                                                         dest_partial,
                                                         scatter_conns, noflat_conns)

        if MPI or scatter_conns_full or noflat_conns_full:
            self.scatter_full = DataTransfer(self, src_full, dest_full,
                                             scatter_conns_full, noflat_conns_full)

        for sub in self.local_subsystems():
            sub.setup_scatters()

    def apply_F(self):
        """ Delegate to subsystems """
        self.scatter('u', 'p')
        for subsystem in self.local_subsystems():
            subsystem.apply_F()

    def applyJ(self):
        """ Delegate to subsystems """

        if self.mode == 'forward':
            self.scatter('u', 'p')
        for subsystem in self.local_subsystems():
            subsystem.applyJ()
        if self.mode == 'adjoint':
            self.scatter('u', 'p')

    def stop(self):
        for s in self.all_subsystems():
            s.stop()


class SerialSystem(CompoundSystem):

    def __init__(self, scope, subg, name=None):
        super(SerialSystem, self).__init__(scope, subg, name)

    def all_subsystems(self):
        return [self.graph.node[node]['system'] for node in self._ordering]

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
        if self.is_active():
            #mpiprint("running serial system %s: %s" % (self.name, [c.name for c in self.local_subsystems()]))
            self._stop = False
            for sub in self.local_subsystems():
                self.scatter('u', 'p', sub)
                self.vec['p'].set_to_scope(self.scope, sub._in_nodes)
                sub.run(iterbase, ffd_order, case_label, case_uuid)
                x = sub.vec['u'].check(self.vec['u'])
                if self._stop:
                    raise RunStopped('Stop requested')

    def setup_communicators(self, comm):
        self._local_subsystems = []

        self.mpi.comm = get_comm_if_active(self, comm)
        if not self.is_active():
            return

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
        if not self.local_subsystems() or not self.is_active():
            return

        self.scatter('u', 'p')

        for sub in self.local_subsystems():
            sub.run(iterbase, ffd_order, case_label, case_uuid)

    def setup_communicators(self, comm):
        #mpiprint("<Parallel> setup_comms for %s  (%d of %d)" % (self.name, comm.rank, comm.size))
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
        self.variables = OrderedDict()
        if not self.is_active():
            return

        for sub in self.local_subsystems():
            sub.setup_variables()

        if self.local_subsystems():
            sub = self.local_subsystems()[0]
            names = sub.variables.keys()
        else:
            sub = None
            names = []
        #mpiprint("%s before ALLGATHER, varkeys=%s" % (self.name, names))
        #mpiprint("setup_variables Allgather")
        varkeys_list = self.mpi.comm.allgather(names)
        #mpiprint("%s after ALLGATHER, varkeys = %s" % (self.name,varkeys_list))
        for varkeys in varkeys_list:
            for name in varkeys:
                self.variables[name] = self._get_var_info(name)

        for sub in self.local_subsystems():
            for name, var in sub.variables.items():
                self.variables[name] = var


class NonSolverDriverSystem(ExplicitSystem):
    """A System for a Driver component that is not a Solver."""

    def __init__(self, driver):
        driver.setup_systems()
        scope = driver.parent
        super(NonSolverDriverSystem, self).__init__(scope, driver.name)
        driver._system = self

    def setup_communicators(self, comm):
        super(NonSolverDriverSystem, self).setup_communicators(comm)
        self._comp.setup_communicators(self.mpi.comm)

    # FIXME: I'm inconsistent in the way that base methods are handled.  The System
    # base class should call setup methods on subsystems or local_subsystems in
    # order to avoid overriding setup methods like this one in derived classes.
    def setup_scatters(self):
        super(NonSolverDriverSystem, self).setup_scatters()
        self._comp.setup_scatters()

    def local_subsystems(self):
        return self.all_subsystems()

    def all_subsystems(self):
        return (self._comp.workflow._system,)

    def simple_subsystems(self, local=False):
        for sub in self._comp.workflow._system.simple_subsystems(local=local):
            yield sub

class SolverSystem(SimpleSystem):  # Implicit
    """A System for a Solver component."""

    def __init__(self, driver):
        driver.setup_systems()
        scope = driver.parent
        super(SolverSystem, self).__init__(scope, driver.name)
        driver._system = self

    def setup_communicators(self, comm):
        super(SolverSystem, self).setup_communicators(comm)
        self._comp.setup_communicators(self.mpi.comm)

    def setup_scatters(self):
        super(SolverSystem, self).setup_scatters()
        self._comp.setup_scatters()

    def local_subsystems(self):
        return self.all_subsystems()

    def all_subsystems(self):
        return (self._comp.workflow._system,)

    def simple_subsystems(self, local=False):
        for sub in self._comp.workflow._system.simple_subsystems(local=local):
            yield sub


class InnerAssemblySystem(SerialSystem):
    """A system to handle data transfer to an Assembly
    boundary from its inner components. It splits the entire
    graph into three pieces, a boundary input system, a top
    driver system, and a boundary out system.
    """
    def __init__(self, scope):
        drvname = scope._top_driver.name

        conns = scope.list_connections()
        srcset = set([scope._depgraph.base_var(u) for u,v in conns])
        destset = set([scope._depgraph.base_var(v) for u,v in conns])

        bndry_outs = [v for v in scope.list_outputs() if v in destset]
        bndry_ins = [v for v in scope.list_inputs() if v in srcset]

        g = nx.DiGraph()
        g.add_node(drvname)
        g.node[drvname]['system'] = _create_simple_sys(scope, scope._top_driver)

        self.bins = bins = tuple(scope._depgraph.find_prefixed_nodes(bndry_ins))
        self.bouts = bouts = tuple(scope._depgraph.find_prefixed_nodes(bndry_outs))

        ordering = []

        if bins:
            g.add_node(bins)
            g.add_edge(bins, drvname)
            g.node[bins]['system'] = BoundarySystem(scope, bins)
            ordering.append(bins)

        ordering.append(drvname)

        if bouts:
            g.add_node(bouts)
            g.add_edge(drvname, bouts)
            g.node[bouts]['system'] = BoundarySystem(scope, bouts)
            ordering.append(bouts)

        super(InnerAssemblySystem, self).__init__(scope, g, '_inner_asm')
        self.set_ordering(ordering)

    def run(self, iterbase, ffd_order=0, case_label='', case_uuid=None):
        if self.is_active():
            self.vec['u'].set_from_scope(self.scope, self.bins)
            super(InnerAssemblySystem, self).run(iterbase, ffd_order,
                                                 case_label, case_uuid)
            self.vec['u'].set_to_scope(self.scope, self.bouts)

def _create_simple_sys(scope, comp):

    if has_interface(comp, ISolver):
        sub = SolverSystem(comp)
    elif has_interface(comp, IDriver):
        sub = NonSolverDriverSystem(comp)
    elif has_interface(comp, IAssembly):
        sub = AssemblySystem(scope, comp.name)
    elif has_interface(comp, IImplicitComponent):
        sub = SimpleSystem(scope, comp.name)
    else:
        sub = ExplicitSystem(scope, comp.name)
    return sub

def partition_mpi_subsystems(cgraph, scope):
    """Return a nested system graph with metadata for parallel
    and serial subworkflows.  Graph must acyclic. All subdriver
    iterations sets must have already been collapsed.

    """
    if len(cgraph) < 2:
        return cgraph

    gcopy = cgraph.copy()

    to_remove = []

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
                    partition_mpi_subsystems(subg, scope)
                    #mpiprint("%d adding system for %s %s" % (id(g),type(branch),str(branch)))
                    system=SerialSystem(scope, subg, str(branch))
                    update_system_node(cgraph, system, branch)

                    gcopy.remove_nodes_from(branch)
                else: # single comp system
                    gcopy.remove_node(branch)

            #parallel_group = tuple(sorted(parallel_group))
            parallel_group = tuple(parallel_group)
            to_remove.extend(parallel_group)
            subg = cgraph.subgraph(parallel_group)  #_precollapse(scope, g, parallel_group)
            #mpiprint("%d adding system for %s %s" % (id(g),type(parallel_group),str(parallel_group)))
            system=ParallelSystem(scope, subg, str(parallel_group))
            update_system_node(cgraph, system, parallel_group)

        elif len(zero_in_nodes) == 1:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)
        else: # circular - no further splitting
            break

    # Now remove all of the old nodes
    cgraph.remove_nodes_from(to_remove)

    return cgraph

def update_system_node(G, system, name):
    G.add_node(name, system=system)
    collapse_nodes(G, name, name)
    return G

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
    #     name = obj.parent.name
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

def get_full_nodeset(scope, group):
    names = set()
    for name in simple_node_iter(group):
        obj = getattr(scope, name, None)
        if obj is not None and hasattr(obj, 'get_full_nodeset'):
            names.update(obj.get_full_nodeset())
        else:
            names.update(name)
    return names
