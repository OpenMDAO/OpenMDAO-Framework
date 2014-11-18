import sys
from StringIO import StringIO
from collections import OrderedDict
from itertools import chain

import numpy
import networkx as nx
from zope.interface import implements

# pylint: disable-msg=E0611,F0401
from openmdao.main.mpiwrap import MPI, MPI_info, mpiprint, PETSc
from openmdao.main.exceptions import RunStopped
from openmdao.main.finite_difference import FiniteDifference, DirectionalFD
from openmdao.main.linearsolver import ScipyGMRES, PETSc_KSP, LinearGS
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly, IImplicitComponent, \
                                     ISolver, IPseudoComp, IComponent, ISystem
from openmdao.main.vecwrapper import VecWrapper, InputVecWrapper, DataTransfer, idx_merge, petsc_linspace
from openmdao.main.depgraph import break_cycles, get_node_boundary, gsort, \
                                   collapse_nodes, simple_node_iter
from openmdao.main.derivatives import applyJ, applyJT
from openmdao.util.graph import base_var

def call_if_found(obj, fname, *args, **kwargs):
    """If the named function exists in the object, call it
    with the provided args.
    """
    if hasattr(obj, fname):
        return getattr(obj, fname)(*args, **kwargs)

def compound_setup_scatters(self):
    """ Defines a scatter for args at this system's level """
    if not self.is_active():
        return
    #mpiprint("setup_scatters: %s  (%d of %d)" % (self.name,self.mpi.rank,self.mpi.size))
    var_sizes = self.local_var_sizes
    input_sizes = self.input_sizes
    rank = self.mpi.rank

    if MPI:
        self.app_ordering = self.create_app_ordering()

    # mpiprint("app indices:   %s\npetsc indices: %s" %
    #           (app_ind_set.getIndices(), petsc_ind_set.getIndices()))
    src_full = []
    dest_full = []
    scatter_conns_full = set()
    noflat_conns_full = set()
    noflats = set([k for k,v in self.variables.items()
                       if v.get('noflat')])

    start = numpy.sum(input_sizes[:rank])
    varkeys = self.vector_vars.keys()

    visited = {}

    # collect all destinations from p vector
    ret = self.vec['p'].get_dests_by_comp()

    #print "scatters for %s" % self.name
    for subsystem in self.all_subsystems():
        src_partial = []
        dest_partial = []
        scatter_conns = set()
        noflat_conns = set()  # non-flattenable vars
        for sub in subsystem.simple_subsystems():
            #print "sub %s: _in_nodes: %s" % (sub.name, sub._in_nodes)
            for node in self.vector_vars:
                if node in sub._in_nodes:
                    if node not in self._owned_args or node in scatter_conns:
                        continue

                    isrc = varkeys.index(node)
                    src_idxs = numpy.sum(var_sizes[:, :isrc]) + self.arg_idx[node]

                    # FIXME: broadcast var nodes will be scattered
                    #  more than necessary using this scheme
                    if node in visited:
                        dest_idxs = visited[node]
                    else:
                        dest_idxs = start + self.arg_idx[node]
                        start += len(dest_idxs)

                        visited[node] = dest_idxs

                    if node not in scatter_conns:
                        scatter_conns.add(node)
                        src_partial.append(src_idxs)
                        dest_partial.append(dest_idxs)

                    if node not in scatter_conns_full:
                        scatter_conns_full.add(node)
                        src_full.append(src_idxs)
                        dest_full.append(dest_idxs)

            for node in sub._in_nodes:
                if node in noflats:
                    if node not in self._owned_args or node in noflat_conns or node not in subsystem._in_nodes:
                        continue
                    scatter_conns.add(node)
                    scatter_conns_full.add(node)
                    noflat_conns.add(node)
                    noflat_conns_full.add(node)
                else:
                    snames = [c for c in sub._get_comps()]
                    for sname in snames:
                        if sname in ret and node in self.vec['p'] and node in ret[sname]:
                            scatter_conns.add(node)
                            scatter_conns_full.add(node)

        if MPI or scatter_conns or noflat_conns:
            #print "   subsystem %s:\n      %s" % (subsystem.name, str(scatter_conns))
            subsystem.scatter_partial = DataTransfer(self, src_partial,
                                                     dest_partial,
                                                     scatter_conns, noflat_conns)

    if MPI or scatter_conns_full or noflat_conns_full:
        self.scatter_full = DataTransfer(self, src_full, dest_full,
                                         scatter_conns_full, noflat_conns_full)

    for sub in self.local_subsystems():
        sub.setup_scatters()


class System(object):
    implements(ISystem)

    def __init__(self, scope, graph, nodes, name):
        self.name = str(name)
        self.scope = scope
        self._nodes = nodes

        self.variables = OrderedDict() # dict of all vars owned by this System (flat and non-flat)
        self.flat_vars = OrderedDict() # all vars used in vectors, whether they add to vector size or not
        self.noflat_vars = OrderedDict() # all vars that are not flattenable to float arrays (so are not part of vectors)
        self.vector_vars = OrderedDict() # all vars that contribute to the size of vectors

        self._mapped_resids = {}

        self._out_nodes = []

        # find our output nodes (outputs from our System and any child Systems)
        for node in nodes:
            if node in graph:
                for succ in graph.successors(node):
                    if succ not in self._out_nodes:
                        self._out_nodes.append(succ)

        if hasattr(self, '_comp') and \
           IImplicitComponent.providedBy(self._comp):
            states = set(['.'.join((self.name,s))
                                  for s in self._comp.list_states()])
        else:
            states = ()

        pure_outs = [out for out in self._out_nodes if out not in states]

        all_outs = set(nodes)
        all_outs.update(pure_outs)

        # get our input nodes from the depgraph
        ins, _ = get_node_boundary(graph, all_outs)

        # filter out any comps labeled as inputs. this happens when multiple comps
        # output the same variable
        #self._in_nodes = [i for i in ins if 'comp' not in graph.node[i]]

        self._in_nodes = []
        for i in ins:
            if 'comp' not in graph.node[i]:
                self._in_nodes.append(i)
            elif i in self.scope.name2collapsed and self.scope.name2collapsed[i] in graph:
                n = self.scope.name2collapsed[i]
                if i != self.scope.name2collapsed[i] and n not in self._in_nodes:
                    self._in_nodes.append(n)

        self._combined_graph = graph.subgraph(list(all_outs)+list(self._in_nodes))

        # mpiprint("%s in_nodes: %s" % (self.name, self._in_nodes))
        # mpiprint("%s out_nodes: %s" % (self.name, self._out_nodes))

        self._in_nodes = sorted(self._in_nodes)
        self._out_nodes = sorted(self._out_nodes)

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
        self.ln_solver = None
        self.fd_solver = None
        self.dfd_solver = None
        self.sol_buf = None
        self.rhs_buf = None
        self._parent_system = None
        self.complex_step = False

    def __getitem__(self, ident):
        if isinstance(ident, basestring):
            return self.find(ident)
        else:
            return self.subsystems()[ident]

    def is_opaque(self):
        return False

    def find(self, name):
        """A convenience method to allow easy access to descendant
        Systems.
        """
        for sub in self.subsystems():
            if name == sub.name:
                return sub
            s = sub.find(name)
            if s is not None:
                return s
        return None

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        return True

    def pre_run(self):
        """ Runs at assembly execution"""
        pass

    def subsystems(self, local=False):
        if local:
            return self.local_subsystems()
        return self.all_subsystems()

    def list_subsystems(self, local=False):
        """Returns the names of our subsystems."""
        return [s.name for s in self.subsystems(local)]

    def create_app_ordering(self):
        rank = self.mpi.rank

        start = numpy.sum(self.local_var_sizes[:rank])
        end = numpy.sum(self.local_var_sizes[:rank+1])
        petsc_idxs = petsc_linspace(start, end)

        app_idxs = []
        for ivar in xrange(len(self.vector_vars)):
            start = numpy.sum(self.local_var_sizes[:, :ivar]) + \
                        numpy.sum(self.local_var_sizes[:rank, ivar])
            end = start + self.local_var_sizes[rank, ivar]
            app_idxs.append(petsc_linspace(start, end))

        if app_idxs:
            app_idxs = numpy.concatenate(app_idxs)

        #mpiprint("app_idxs: %s, petsc_idxs: %s" % (app_idxs, petsc_idxs))
        app_ind_set = PETSc.IS().createGeneral(app_idxs, comm=self.mpi.comm)
        petsc_ind_set = PETSc.IS().createGeneral(petsc_idxs, comm=self.mpi.comm)

        return PETSc.AO().createBasic(app_ind_set, petsc_ind_set,
                                      comm=self.mpi.comm)

    def flat(self, names):
        """Returns the names from the given list that refer
        to variables that are flattenable to float arrays.
        """
        varmeta = self.scope._var_meta
        return [n for n in names if not varmeta[n].get('noflat')]

    def get_unique_vars(self, vnames):
        """
        Returns a dict of variables and which
        process (lowest rank) is 'responsible' for each
        variable.
        """

        # FIXME: this currently doesn't handle distributed vars,
        #  i.e. variables that have parts located on different
        #  procs.
        comm = self.mpi.comm

        myvars = self.vector_vars.keys()
        collname = self.scope.name2collapsed

        all_keys = comm.allgather(vnames)


        var2proc = {}
        for proc, names in enumerate(all_keys):
            for name in names:
                if name not in var2proc:
                    try:
                        idx = myvars.index(collname[name])
                    except ValueError:
                        mpiprint('valerr')
                        continue

                    if self.local_var_sizes[proc, idx] > 0:
                        var2proc[name] = proc

        return var2proc

    def get_combined_J(self, J):
        """
        Take a J dict that's distributed, i.e., has different values
        across different MPI processes, and return a dict that
        contains all of the values from all of the processes.  If
        values are duplicated, use the value from the lowest rank
        process.  Note that J has a nested dict structure.
        """

        comm = self.mpi.comm
        myrank = comm.rank

        tups = []

        # gather a list of tuples for J
        for param, dct in J.items():
            for output, value in dct.items():
                tups.append((param, output))

        dist_tups = comm.gather(tups, root=0)

        tupdict = {}
        if myrank == 0:
            for rank, tups in enumerate(dist_tups):
                for tup in tups:
                    if not tup in tupdict:
                        tupdict[tup] = rank

            #get rid of tups from the root proc before bcast
            for tup, rank in tupdict.items():
                if rank == 0:
                    del tupdict[tup]

        tupdict = comm.bcast(tupdict, root=0)

        if myrank == 0:
            for (param, output), rank in tupdict.items():
                J[param][output] = comm.recv(source=rank, tag=0)
        else:
            for (param, output), rank in tupdict.items():
                if rank == myrank:
                    comm.send(J[param][output], dest=0, tag=0)


        # FIXME: rework some of this using knowledge of local_var_sizes in order
        # to avoid any unnecessary data passing

        # get the combined dict
        J = comm.bcast(J, root=0)

        return J

    def _get_owned_args(self):
        args = set()
        for sub in self.simple_subsystems():
            for arg in sub._in_nodes:
                if arg in self.variables and \
                        (arg not in sub.variables or sub is self):
                    args.add(arg)

        # ensure that args are in same order that they appear in
        # variables
        return [a for a in self.variables.keys() if a in args]

    def get(self, name):
        return self.vec['u'][name]

    def clear_dp(self):
        """ Recusively sets the dp vector to zero."""
        self.vec['dp'].array[:] = 0.0
        for system in self.local_subsystems():
            system.clear_dp()

    def _get_comps(self):
        """Return a set of comps for this system and all subsystems."""
        comps = set()
        for s in self.local_subsystems():
            comps.update(simple_node_iter(s._get_comps()))
        return comps

    def list_inputs(self):
        """Returns names of input variables from this System and all of its
        children.
        """
        inputs = set()
        bases = set()
        for system in self.simple_subsystems():
            comps = self._get_comps()
            for tup in system._in_nodes:
                # need this to prevent paramgroup inputs on same comp to be
                # counted more than once
                seen = set()
                for dest in tup[1]:
                    comp = dest.split('.', 1)[0]
                    if comp in comps and comp not in seen:
                        inputs.add(dest)
                        base = base_var(self.scope._depgraph, dest)
                        if base == dest:
                            bases.add(base)
                        # Since Opaque systems do finite difference on the
                        # full param groups, we should only include one input
                        # from each group.
                        if isinstance(self, OpaqueSystem):
                            seen.add(comp)

        # filter out subvars of included basevars
        inputs = [n for n in inputs if n in bases or base_var(self.scope._depgraph, n) not in bases]

        return _filter(self.scope, inputs)

    def list_states(self):
        """Returns names of states (not collapsed edges) from this System and
        all of its children.
        """
        states = set()
        for system in self.simple_subsystems():
            try:
                if system._comp.eval_only is False:
                    states.update(['.'.join((system.name,s))
                                      for s in system._comp.list_states()])
            except AttributeError:
                pass

        top = self.scope
        states = [i for i in states if top.name2collapsed[i] in top._system.vector_vars
                    and not top._system.vector_vars[top.name2collapsed[i]].get('deriv_ignore')]

        #print "%s states: %s" % (self.name, states)
        return states

    def list_outputs(self):
        """Returns names of output variables (not collapsed edges)
        from this System and all of its children.  This only lists
        outputs that are relevant to derivatives calculations.
        """
        outputs = []
        for system in self.simple_subsystems():
            states = set()
            try:
                states.update(['.'.join((system.name,s))
                                  for s in system._comp.list_states()])
            except AttributeError:
                pass
            out_nodes = [node for node in system._out_nodes \
                         if node not in self._mapped_resids]
            comps = self._get_comps()
            for src, _ in out_nodes:
                parts = src.split('.', 1)
                if parts[0] in comps and src not in states:
                    outputs.append(src)

        return _filter(self.scope, outputs)

    def list_residuals(self):
        """Returns names of all residuals.
        """
        outputs = []
        for system in self.simple_subsystems():
            try:
                outputs.extend(['.'.join((system.name, s))
                                  for s in system._comp.list_residuals()
                                  if system._comp.eval_only is False])
            except AttributeError:
                pass

        outputs.extend([n for n, m in self._mapped_resids.keys()])
        #print "%s residuals: %s" % (self.name, outputs)
        return outputs

    def get_size(self, names):
        """Return the combined size of the variables
        corresponding to the given names.  If a given
        variable does not exist locally, the size will
        be taken from the lowest rank process that does
        contain that variable.
        """
        if isinstance(names, basestring):
            names = [names]
        uvec = self.scope._system.vec['u']
        varmeta = self.scope._var_meta
        var_sizes = self.scope._system.local_var_sizes
        varkeys = self.scope._system.vector_vars.keys()
        collnames = self.scope.name2collapsed
        procs = range(self.mpi.size)
        size = 0
        for name in names:
            if isinstance(name, tuple):
                name = name[0]

            if name in uvec:
                size += uvec[name].size
            #elif name.split('[',1)[0] in uvec:
                #bval = self.scope.get(name.split('[',1)[0])
                #_, idx = get_val_and_index(self.scope, name)
                #idxs = get_flattened_index(idx, get_shape(bval))
                #size += bval[idxs].size
            elif collnames[name] in varkeys:
                idx = varkeys.index(collnames[name])
                for proc in procs:
                    if var_sizes[proc, idx] > 0:
                        size += var_sizes[proc, idx]
                        break
            else:
                size += varmeta[name]['size']

        return size

    def set_ordering(self, ordering, opaque_map):
        pass

    def is_active(self):
        return MPI is None or self.mpi.comm != MPI.COMM_NULL

    def local_subsystems(self):
        return ()

    def all_subsystems(self):
        return ()

    def get_req_cpus(self):
        return self.mpi.requested_cpus

    def setup_variables(self, resid_state_map=None):
        self.variables = OrderedDict()
        if resid_state_map is None:
            resid_state_map = {}

        variables = {}
        for sub in self.local_subsystems():
            if not isinstance(sub, ParamSystem):
                sub.setup_variables(resid_state_map)
                variables.update(sub.variables)

        for sub in self.local_subsystems():
            if isinstance(sub, ParamSystem):
                sub.setup_variables(variables, resid_state_map)

        # now loop through a final time to keep order of all subsystems the same
        # as local_subsystems()
        for sub in self.local_subsystems():
            self.variables.update(sub.variables)

        self._create_var_dicts(resid_state_map)

    def _create_var_dicts(self, resid_state_map):
        # now figure out all of the inputs we 'own'
        self._owned_args = self._get_owned_args()

        # split up vars into 3 categories:
        #  1) flattenable vars that add to the size of the vectors
        #  2) flattenable vars that don't add to the size of the vectors because they
        #     are slices of other vars already in the vectors
        #  3) non-flattenable vars

        # first, get all flattenable variables
        for name in self._get_flat_vars(self.variables):
            self.flat_vars[name] = self.variables[name]

        # now get all flattenable vars that add to vector size
        self.vector_vars = self._get_vector_vars(self.flat_vars)

        for name, info in self.variables.items():
            if name not in self.flat_vars:
                self.noflat_vars[name] = info

    def setup_sizes(self):
        """Given a dict of variables, set the sizes for
        those that are local.
        """
        varmeta = self.scope._var_meta
        comm = self.mpi.comm

        if not self.is_active():
            self.local_var_sizes = numpy.zeros((0,0), int)
            self.input_sizes = numpy.zeros(0, int)
            return

        size = self.mpi.size
        rank = self.mpi.rank

        # pass the call down to any subdrivers/subsystems
        # and subassemblies.
        for sub in self.local_subsystems():
            sub.setup_sizes()

        # create an (nproc x numvars) var size vector containing
        # local sizes across all processes in our comm
        self.local_var_sizes = numpy.zeros((size, len(self.vector_vars)), int)

        for i, (name, var) in enumerate(self.vector_vars.items()):
            self.local_var_sizes[rank, i] = var['size']

        # collect local var sizes from all of the processes in our comm
        # these sizes will be the same in all processes except in cases
        # where a variable belongs to a multiprocessor component.  In that
        # case, the part of the component that runs in a given process will
        # only have a slice of each of the component's variables.
        if MPI:
            comm.Allgather(self.local_var_sizes[rank,:],
                           self.local_var_sizes)

        # create a (1 x nproc) vector for the sizes of all of our
        # local inputs
        self.input_sizes = numpy.zeros(size, int)

        for arg in self.flat(self._owned_args):
            self.input_sizes[rank] += varmeta[arg]['size']

        if MPI:
            comm.Allgather(self.input_sizes[rank], self.input_sizes)

        # create an arg_idx dict to keep track of indices of
        # inputs
        # TODO: determine how we want the user to specify indices
        #       for distributed inputs...
        self.arg_idx = OrderedDict()
        for name in self.flat(self._owned_args):
            # FIXME: this needs to use the actual indices for this
            #        process' version of the arg once we have distributed
            #        components...
            #flat_idx = varmeta[name].get('flat_idx')
            #if flat_idx and varmeta[name]['basevar'] in varmeta:  # var is an array index into a basevar
            #    self.arg_idx[name] = to_indices(flat_idx, self.scope.get(varmeta[name]['basevar']))
            #else:
            self.arg_idx[name] = numpy.array(range(varmeta[name]['size']), 'i')

    def setup_vectors(self, arrays=None, state_resid_map=None):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        if not self.is_active():
            return

        rank = self.mpi.rank
        if arrays is None:  # we're the top level System in our Assembly
            arrays = {}
            # create top level vectors
            size = numpy.sum(self.local_var_sizes[rank, :])
            for name in ['u', 'f', 'du', 'df']:
                arrays[name] = numpy.zeros(size)

        for name in ['u', 'f', 'du', 'df']:
            self.vec[name] = VecWrapper(self, arrays[name],
                                        name='.'.join((self.name, name)))

        insize = self.input_sizes[rank]

        for name in ['p', 'dp']:
            self.vec[name] = InputVecWrapper(self, numpy.zeros(insize),
                                             name='.'.join((self.name, name)))

        start, end = 0, 0
        for sub in self.local_subsystems():
            sz = numpy.sum(sub.local_var_sizes[sub.mpi.rank, :])
            end += sz
            if end-start > arrays['u'][start:end].size:
                msg = "size mismatch: passing [%d,%d] view of size %d array from %s to %s" % \
                            (start,end,arrays['u'][start:end].size,self.name,sub.name)
                raise RuntimeError(msg)

            subarrays = {}
            for n in ('u', 'f', 'du', 'df'):
                subarrays[n] = arrays[n][start:end]

            sub.setup_vectors(subarrays)

            start += sz

        return self.vec

    def scatter(self, srcvecname, destvecname, subsystem=None):
        """ Perform data transfer (partial or full scatter or
        send/receive for data that isn't flattenable to a
        float array.
        """
        if subsystem is None:
            scatter = self.scatter_full
            #if scatter:
            #    print "%s full scatter" % self.name
        else:
            scatter = subsystem.scatter_partial
            #if scatter:
            #    print "%s scatter to %s" % (self.name, subsystem.name)

        if scatter is not None:
            srcvec = self.vec[srcvecname]
            destvec = self.vec[destvecname]

            scatter(self, srcvec, destvec)

            if destvecname == 'p':

                if self.complex_step is True:
                    scatter(self, self.vec['du'], self.vec['dp'],
                            complex_step = True)

                if scatter is self.scatter_full:
                    self.vec['p'].set_to_scope(self.scope)
                    if self.complex_step is True:
                        self.vec['dp'].set_to_scope_complex(self.scope)
                else:
                    if subsystem._in_nodes:
                        self.vec['p'].set_to_scope(self.scope, subsystem._in_nodes)
                        if self.complex_step is True:
                            self.vec['dp'].set_to_scope_complex(self.scope,
                                                                subsystem._in_nodes)

        return scatter

    def dump(self, nest=0, stream=sys.stdout, verbose=False):
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
                     'SimpleSystem': 'simp', 'FiniteDiffDriverSystem': 'drv',
                     'TransparentDriverSystem': 'tdrv', 'OpaqueSystem': 'opaq',
                     'InVarSystem': 'invar', 'OutVarSystem': 'outvar',
                     'SolverSystem': 'slv',  'ParamSystem': 'param',
                     'AssemblySystem': 'asm', }

        stream.write(" "*nest)
        stream.write(str(self.name).replace(' ','').replace("'",""))
        klass = self.__class__.__name__
        stream.write(" [%s](req=%d)(rank=%d)(vsize=%d)(isize=%d)\n" %
                                          (name_map.get(klass, klass.lower()[:3]),
                                           self.get_req_cpus(),
                                           world_rank,
                                           self.vec['u'].array.size,
                                           self.input_sizes[self.mpi.rank]))

        for v, (arr, start) in self.vec['u']._info.items():
            if verbose or v not in self.vec['u']._subviews:
                stream.write(" "*(nest+2))
                if v in self.vec['p']:
                    stream.write("u (%s)  p (%s): %s\n" %
                                     (list(self.vec['u'].bounds([v])),
                                      list(self.vec['p'].bounds([v])), v))
                else:
                    stream.write("u (%s): %s\n" % (list(self.vec['u'].bounds([v])), v))

        for v, (arr, start) in self.vec['p']._info.items():
            if v not in self.vec['u'] and (verbose or v not in self.vec['p']._subviews):
                stream.write(" "*(nest+2))
                stream.write("           p (%s): %s\n" %
                                 (list(self.vec['p'].bounds([v])), v))

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

        stream.write(" "*(nest+2))
        stream.write("_in_nodes: %s\n" % self._in_nodes)
        stream.write(" "*(nest+2))
        stream.write("_out_nodes: %s\n" % self._out_nodes)
        stream.write(" "*(nest+2))
        stream.write("list_inputs(): %s\n" % self.list_inputs())
        stream.write(" "*(nest+2))
        stream.write("list_outputs(): %s\n" % self.list_outputs())
        stream.write(" "*(nest+2))
        stream.write("list_states(): %s\n" % self.list_states())
        stream.write(" "*(nest+2))
        stream.write("list_residuals(): %s\n" % self.list_residuals())

        nest += 4
        if isinstance(self, OpaqueSystem):
            self._inner_system.dump(nest, stream)
        elif isinstance(self, AssemblySystem):
            self._comp._system.dump(nest, stream)
        else:
            for sub in self.local_subsystems():
                sub.dump(nest, stream)

        return stream.getvalue() if getval else None

    def _get_flat_vars(self, vardict):
        """Return a list of names of vars that represent variables that are
        flattenable to float arrays.
        """
        return [n for n,info in vardict.items() if not info.get('noflat')]

    def _get_vector_vars(self, vardict):
        """Return vector_vars, which are vars that actually add to the
        size of the vectors (as opposed to subvars of vars that are in
        the vector, which don't add anything to the vector but just
        use a subview of the view corresponding to their base var)
        """
        vector_vars = OrderedDict()
        all_srcs = set([n[0] for n in vardict])

        # all bases that actually are found in the vardict
        bases = set([s for s in all_srcs if '[' not in s])

        # use these to find any subs that don't have a full base in the
        # vector. we have to make sure these don't overlap with other
        # baseless subs
        sub_bases = set([s.split('[',1)[0]
                          for s in all_srcs
                             if '[' in s and s.split('[',1)[0] not in bases])

        for name in vardict:
            src = name[0]

            if src in bases:
                vector_vars[name] = vardict[name]
            else:
                base = src.split('[', 1)[0]
                if base in sub_bases: # this sub's base is not in vardict
                    # overlapping will be checked later when we create VecWrappers
                    vector_vars[name] = vardict[name]

        return vector_vars

    def set_options(self, mode, options):
        """ Sets all user-configurable options for this system and all
        subsystems. """

        self.mode = mode
        self.options = options

        if mode in ('forward', 'fd'):
            self.sol_vec = self.vec['du']
            self.rhs_vec = self.vec['df']
        elif mode == 'adjoint':
            self.sol_vec = self.vec['df']
            self.rhs_vec = self.vec['du']
        else:
            raise RuntimeError("invalid mode. must be 'forward' or 'adjoint' but value is '%s'" % mode)

        for subsystem in self.local_subsystems():
            subsystem.set_options(mode, options)


    # ------- derivative stuff -----------

    def initialize_gradient_solver(self):
        """ Initialize the solver that will be used to calculate the
        gradient. """

        if self.ln_solver is None:

            solver_choice = self.options.lin_solver

            # scipy_gmres not supported in MPI, so swap with
            # petsc KSP.
            if MPI and solver_choice=='scipy_gmres':
                solver_choice = 'petsc_ksp'
                msg = "scipy_gmres optimizer not supported in MPI. " + \
                      "Using petsc_ksp instead."
                self.options.parent._logger.warning(msg)

            if solver_choice == 'scipy_gmres':
                self.ln_solver = ScipyGMRES(self)
            elif solver_choice == 'petsc_ksp':
                self.ln_solver = PETSc_KSP(self)
            elif solver_choice == 'linear_gs':
                self.ln_solver = LinearGS(self)

    def linearize(self):
        """ Linearize all subsystems. """

        for subsystem in self.local_subsystems():
            subsystem.linearize()

    def set_complex_step(self, complex_step=False):
        """ Toggles complex_step plumbing for this system and all
        subsystems. """

        self.complex_step = complex_step
        for subsystem in self.local_subsystems():
            subsystem.set_complex_step(complex_step)

    def calc_gradient(self, inputs, outputs, mode='auto', options=None,
                      iterbase='', return_format='array'):
        """ Return the gradient for this system. """

        # Mode Precedence
        # -- 1. Direct call argument
        # -- 2. Gradient Options
        # -- 3. Auto determination (when implemented)
        if mode == 'auto':

            # Automatic determination of mode
            if options.derivative_direction == 'auto':
                num_input = self.get_size(inputs)
                num_output = self.get_size(outputs)
                if num_input > num_output:
                    mode = 'adjoint'
                else:
                    mode = 'forward'
            else:
                mode = options.derivative_direction

        if options.force_fd is True:
            mode = 'fd'

        self.set_options(mode, options)
        self.initialize_gradient_solver()

        if mode == 'fd':
            self.vec['df'].array[:] = 0.0
            self.vec['du'].array[:] = 0.0
            self.clear_dp()
            return self.solve_fd(inputs, outputs, iterbase, return_format)

        self.linearize()

        # Clean out all arrays.
        self.vec['df'].array[:] = 0.0
        self.vec['du'].array[:] = 0.0
        self.clear_dp()

        J = self.ln_solver.calc_gradient(inputs, outputs, return_format)
        self.sol_vec.array[:] = 0.0
        return J

    def solve_fd(self, inputs, outputs, iterbase='', return_format='array'):
        if self.fd_solver is None:
            self.fd_solver = FiniteDifference(self, inputs, outputs,
                                              return_format)
        return self.fd_solver.solve(iterbase=iterbase)

    def calc_newton_direction(self, options=None, iterbase=''):
        """ Solves for the new state in Newton's method and leaves it in the
        df vector."""

        self.set_options('forward', options)

        self.vec['du'].array[:] = 0.0
        self.vec['df'].array[:] = 0.0
        self.vec['dp'].array[:] = 0.0

        self.initialize_gradient_solver()
        self.linearize()

        #print 'Newton Direction', self.vec['f'].array[:]
        self.vec['df'].array[:] = -self.ln_solver.solve(self.vec['f'].array)
        #print 'Newton Solution', self.vec['df'].array[:]

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        if numpy.linalg.norm(self.rhs_vec.array) < 1e-15:
            self.sol_vec.array[:] = 0.0
            return self.sol_vec.array

        if options is not None:
            self.set_options(self.mode, options)
        self.initialize_gradient_solver()

        """ Solve Jacobian, df |-> du [fwd] or du |-> df [rev] """
        self.rhs_buf[:] = self.rhs_vec.array[:]
        self.sol_buf[:] = self.sol_vec.array[:]
        self.sol_buf = self.ln_solver.solve(self.rhs_buf)
        self.sol_vec.array[:] = self.sol_buf[:]

    def applyJ(self, variables):
        """ Defined in derived classes."""
        pass

    def _compute_derivatives(self, vname, ind):
        """ Solves derivatives of system (direct/adjoint).
        ind must be a global petsc index.
        """
        self.rhs_vec.array[:] = 0.0
        self.sol_vec.array[:] = 0.0
        self.vec['dp'].array[:] = 0.0

        varkeys = self.vector_vars.keys()
        ivar = varkeys.index(vname)

        if self.local_var_sizes[self.mpi.rank, ivar] > 0:
            ind += numpy.sum(self.local_var_sizes[:, :ivar])
            ind += numpy.sum(self.local_var_sizes[:self.mpi.rank, ivar])

            ind_set = PETSc.IS().createGeneral([ind], comm=self.mpi.comm)
            if self.app_ordering is not None:
                ind_set = self.app_ordering.app2petsc(ind_set)
            ind = ind_set.indices[0]
            #mpiprint("setting 1.0 into index %d for %s" % (ind, str(vname)))
            self.rhs_vec.petsc_vec.setValue(ind, 1.0, addv=False)
        else:
            # creating an IS is a collective call, so must do it
            # here to avoid hanging, even though we don't need the IS
            ind_set = PETSc.IS().createGeneral([], comm=self.mpi.comm)

        # if self.mpi.rank == rank:
        #     mpiprint("set %d index to 1.0" % ind)
        #     self.rhs_vec.petsc_vec.setValue(ind, 1.0, addv=False)

        self.sol_buf.array[:] = self.sol_vec.array[:]
        self.rhs_buf.array[:] = self.rhs_vec.array[:]

        self.ln_solver.ksp.solve(self.rhs_buf, self.sol_buf)

        self.sol_vec.array[:] = self.sol_buf.array[:]

        #mpiprint('dx', self.sol_vec.array)
        return self.sol_vec

    # def _get_global_indices(self, var, rank):
    #     """Returns an iterator over global indices.
    #     """
    #     mpiprint("getting global indices for %s" % str(var))
    #     var_sizes = self.local_var_sizes
    #     ivar = self.vector_vars.keys().index(var)
    #     start = numpy.sum(self.local_var_sizes[:, :ivar])

    #     #end = start + numpy.sum(var_sizes[self.mpi.rank, ivar])
    #     end = start + var_sizes[rank, ivar]
    #     #end = start + numpy.sum(var_sizes[:, ivar])

    #     idxs = xrange(start, end)
    #     ind_set = PETSc.IS().createGeneral(idxs, comm=self.mpi.comm)
    #     if self.app_ordering is not None:
    #         ind_set = self.app_ordering.app2petsc(ind_set)

    #     mpiprint("global indices: %s" % ind_set.indices)

    #     return ind_set.indices


class SimpleSystem(System):
    """A System for a single Component. This component can have Inputs,
    Outputs, States, and Residuals."""

    def __init__(self, scope, graph, name):
        comp = None
        nodes = set([name])
        cpus = 1
        try:
            comp = getattr(scope, name)
        except (AttributeError, TypeError):
            pass
        else:
            if has_interface(comp, IComponent):
                self._comp = comp
                nodes = comp.get_full_nodeset()
                cpus = comp.get_req_cpus()
            else:
                comp = None

        super(SimpleSystem, self).__init__(scope, graph, nodes, name)

        self.mpi.requested_cpus = cpus
        self._comp = comp
        self.J = None
        self._mapped_resids = {}

    def inner(self):
        return self._comp

    def stop(self):
        self._comp.stop()
        self._stop = True

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        if self._comp is not None:
            return self._comp.is_differentiable()

        return True

    def _get_comps(self):
        return self._nodes

    def simple_subsystems(self):
        yield self

    def setup_communicators(self, comm):
        # if comm is not None:
        #     mpiprint("setup_comms for %s  (%d of %d)" % (self.name, comm.rank, comm.size))
        self.mpi.comm = comm

    def _create_var_dicts(self, resid_state_map):
        varmeta = self.scope._var_meta

        if IImplicitComponent.providedBy(self._comp):
            states = set(['.'.join((self._comp.name, s))
                             for s in self._comp.list_states()])
        else:
            states = ()

        # group outputs into states and non-states
        # comps no longer own their own states (unless they also
        # own the corresponding residual)
        mystates = [v for v in self._out_nodes if v[1][0] in states]
        mynonstates = [v for v in self._out_nodes if v[1][0] not in states
                         and v not in resid_state_map]

        topsys = self
        while topsys._parent_system:
            topsys = topsys._parent_system

        for vname in chain(mystates, mynonstates):
            if vname not in self.variables:
                base = base_var(self.scope._depgraph, vname[0])
                if base != vname[0] and base in topsys._combined_graph: #self.scope._reduced_graph:
                    continue
                self.variables[vname] = varmeta[vname].copy()

        mapped_states = resid_state_map.values()

        # for simple systems, if we're given a mapping of our outputs to
        # states, we need to 'own' the state and later in run we need
        # to copy the residual part of our f vector to the corresponding
        # state.

        if resid_state_map:
            to_remove = set()
            for out in self._out_nodes:
                state = resid_state_map.get(out)
                if state and state not in self.variables:
                    self.variables[state] = varmeta[state].copy()
                    self._mapped_resids[out] = state
                    if out in self.variables:
                        to_remove.add(out)
                if out in mapped_states and state not in self.variables:
                    to_remove.add(out)

            if not isinstance(self, (SolverSystem, FiniteDiffDriverSystem)):
                for name in to_remove:
                    del self.variables[name]

        super(SimpleSystem, self)._create_var_dicts(resid_state_map)

    def setup_scatters(self):
        if not self.is_active():
            return
        #mpiprint("setup_scatters: %s  (%d of %d)" % (self.name,self.mpi.rank,self.mpi.size))
        rank = self.mpi.rank
        start = numpy.sum(self.input_sizes[:rank])
        end = numpy.sum(self.input_sizes[:rank+1])
        dest_idxs = [petsc_linspace(start, end)]
        src_idxs = []
        ukeys = self.vec['u'].keys()
        scatter_conns = []
        other_conns = []

        flat_args = self.flat(self._owned_args)

        for dest in flat_args:
            ivar = ukeys.index(dest)
            scatter_conns.append(dest)
            # FIXME: currently just using the local var size for input size
            src_idxs.append(numpy.sum(self.local_var_sizes[:, :ivar]) + self.arg_idx[dest])# - user really needs to be able to define size for multi-proc comps
        if len(idx_merge(src_idxs)) != len(idx_merge(dest_idxs)):
            raise RuntimeError("ERROR: setting up scatter: (%d != %d) srcs: %s,  dest: %s in %s" %
                                (len(src_idxs), len(dest_idxs), src_idxs, dest_idxs, self.name))

        other_conns = [n for n in self._owned_args if n not in flat_args]

        if MPI or scatter_conns or other_conns:
            self.scatter_full = DataTransfer(self, src_idxs, dest_idxs,
                                             scatter_conns, other_conns)

    def run(self, iterbase, case_label='', case_uuid=None):
        if self.is_active():
            graph = self.scope._reduced_graph

            self.scatter('u', 'p')

            self._comp.set_itername('%s-%s' % (iterbase, self.name))
            self._comp.run(case_uuid=case_uuid)

            # put component outputs in u vector
            vnames = [n for n in graph.successors(self.name)
                                   if n in self.vector_vars]
            self.vec['u'].set_from_scope(self.scope, vnames)

            if self.complex_step is True:
                self.vec['du'].set_from_scope_complex(self.scope, vnames)

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        if self.is_active():
            graph = self.scope._reduced_graph

            vec = self.vec
            vec['f'].array[:] = vec['u'].array[:]
            self.scatter('u', 'p')

            #if IImplicitComponent.providedBy(self._comp) and self._comp.eval_only==False:
            #    self._comp.evaluate()
            #else:
            self._comp.set_itername('%s-%s' % (iterbase, self.name))
            self._comp.run(case_uuid=case_uuid)

            # put component outputs in u vector
            self.vec['u'].set_from_scope(self.scope,
                             [n for n in graph.successors(self.name)
                                   if n in self.vector_vars])

            vec['f'].array[:] -= vec['u'].array[:]
            vec['u'].array[:] += vec['f'].array[:]

    def clear_dp(self):
        """ Sets the dp vector to zero."""
        if 'dp' in self.vec:
            self.vec['dp'].array[:] = 0.0

    def linearize(self):
        """ Linearize this component. """
        self.J = self._comp.linearize(first=True)

    def applyJ(self, variables):
        """ df = du - dGdp * dp or du = df and dp = -dGdp^T * df """

        vec = self.vec

        # Forward Mode
        if self.mode == 'forward':

            self.scatter('du', 'dp')

            self._comp.applyJ(self, variables)
            vec['df'].array[:] *= -1.0

            for var in self.list_outputs():
                vec['df'][var][:] += vec['du'][var][:]


        # Adjoint Mode
        elif self.mode == 'adjoint':

            # Sign on the local Jacobian needs to be -1 before
            # we add in the fake residual. Since we can't modify
            # the 'du' vector at this point without stomping on the
            # previous component's contributions, we can multiply
            # our local 'arg' by -1, and then revert it afterwards.
            vec['df'].array[:] *= -1.0
            self._comp.applyJT(self, variables)
            vec['df'].array[:] *= -1.0

            for var in self.list_outputs():

                collapsed = self.scope.name2collapsed.get(var)
                if collapsed not in variables:
                    continue

                vec['du'][var][:] += vec['df'][var][:]

            self.scatter('du', 'dp')

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        self.sol_vec.array[:] = self.rhs_vec.array[:]


class VarSystem(SimpleSystem):
    """Base class for a System that contains a single variable."""

    def run(self, iterbase, case_label='', case_uuid=None):
        pass

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        pass

    def applyJ(self, variables):
        pass

    def stop(self):
        pass

    def linearize(self):
        pass


class ParamSystem(VarSystem):
    """System wrapper for Assembly input variables (internal perspective)."""

    def setup_variables(self, parent_vars, resid_state_map=None):
        super(ParamSystem, self).setup_variables(resid_state_map)
        to_remove = [v for v in self.variables if v in parent_vars]
        if to_remove: # this param appears in some subdriver, so we don't own it
            self._dup_in_subdriver = True
            del self.variables[to_remove[0]]
            del self.vector_vars[to_remove[0]]
            del self.flat_vars[to_remove[0]]
        else:
            self._dup_in_subdriver = False

    def applyJ(self, variables):
        """ Set to zero """
        system = None
        if self.vector_vars or self._dup_in_subdriver:
            system = self._get_sys()

        if system:
            system.rhs_vec[self.name] += system.sol_vec[self.name]

    def pre_run(self):
        """ Load param value into u vector. """
        self._get_sys().vec['u'].set_from_scope(self.scope)#, [self.name])

    #def run(self, iterbase, case_label='', case_uuid=None):
    #    if self.is_active():
    #        self._get_sys().vec['u'].set_to_scope(self.scope, [self.name])

    def _get_sys(self):
        if self._dup_in_subdriver:
            return self._parent_system
        else:
            return self


class InVarSystem(VarSystem):
    """System wrapper for Assembly input variables (internal perspective)."""

    def run(self, iterbase, case_label='', case_uuid=None):
        if self.is_active():
            self.vec['u'].set_from_scope(self.scope, self._nodes)

            if self.complex_step is True:
                self.vec['du'].set_from_scope_complex(self.scope, self._nodes)

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        self.run(iterbase, case_label=case_label, case_uuid=case_uuid)

    def applyJ(self, variables):
        """ Set to zero """
        # don't do anything if we don't own our output
        # don't do anything if we are not requesting this invar
        if self.variables and \
           self.scope.name2collapsed.get(self.name) in variables:
            #mpiprint("invar sys %s: adding %s to %s" %
                            #(self.name, self.sol_vec[self.name],
                                #self.rhs_vec[self.name]))
            self.rhs_vec[self.name] += self.sol_vec[self.name]

    def pre_run(self):
        """ Load param value into u vector. """
        self.vec['u'].set_from_scope(self.scope, [self.name])


class OutVarSystem(VarSystem):
    pass


class EqConstraintSystem(SimpleSystem):
    """A special system to handle mapping of states and
    residuals.
    """
    def setup_variables(self, resid_state_map=None):
        super(EqConstraintSystem, self).setup_variables(resid_state_map)

        nodemap = self.scope.name2collapsed
        src = self._comp._exprobj.lhs.text
        srcnode = nodemap.get(src, src)
        dest = self._comp._exprobj.rhs.text
        destnode = nodemap.get(dest, dest)

        for _, state_node in resid_state_map.items():
            if state_node == srcnode:
                self._comp._negate = True
                break
            elif state_node == destnode:
                break

    def run(self, iterbase, case_label='', case_uuid=None):
        if self.is_active():
            super(EqConstraintSystem, self).run(iterbase,
                                                case_label=case_label,
                                                case_uuid=case_uuid)
            state = self._mapped_resids.get(self.scope.name2collapsed[self.name+'.out0'])

            # Propagate residuals.
            if state:
                self.vec['f'][state][:] = \
                    -self._comp.get_flattened_value('out0').real

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        self.run(iterbase, case_label=case_label, case_uuid=case_uuid)


class AssemblySystem(SimpleSystem):
    """A System to handle an Assembly."""

    def __init__(self, scope, graph, name):
        super(AssemblySystem, self).__init__(scope, graph, name)
        self._provideJ_bounds = None

    def is_opaque(self):
        return True

    def setup_communicators(self, comm):
        super(AssemblySystem, self).setup_communicators(comm)
        self._comp.setup_communicators(comm)

    def setup_variables(self, resid_state_map=None):
        super(AssemblySystem, self).setup_variables(resid_state_map)
        self._comp.setup_variables()

    def setup_sizes(self):
        super(AssemblySystem, self).setup_sizes()
        self._comp.setup_sizes()

    def setup_vectors(self, arrays=None, state_resid_map=None):
        super(AssemblySystem, self).setup_vectors(arrays)
        # internal Assembly will create new vectors
        self._comp.setup_vectors(arrays)

    def setup_scatters(self):
        super(AssemblySystem, self).setup_scatters()
        self._comp.setup_scatters()

    def set_options(self, mode, options):
        """ Assembly inner system determines its own mode, but we use parents
        mode at this level. Options will be passed when the gradient is
        calculated."""
        self.options = options
        self.mode = mode

        if mode in ('forward', 'fd'):
            self.sol_vec = self.vec['du']
            self.rhs_vec = self.vec['df']
        elif mode == 'adjoint':
            self.sol_vec = self.vec['df']
            self.rhs_vec = self.vec['du']
        else:
            raise RuntimeError("invalid mode. must be 'forward' or 'adjoint' but value is '%s'" % mode)

        # Note, this mode is not important, but the options are needed for
        # linearize.
        self._comp._system.set_options(mode, options)

    def clear_dp(self):
        """ Recusively sets the dp vector to zero."""
        self.vec['dp'].array[:] = 0.0
        for system in self.local_subsystems():
            system.clear_dp()

    def linearize(self):
        """ Calculates and saves the Jacobian for this subassy. """

        inner_system = self._comp._system

        # Calculate and save Jacobian for this assy
        inputs = [item.partition('.')[-1] for item in self.list_inputs()]
        outputs = [item.partition('.')[-1] for item in self.list_outputs()]
        self.J = inner_system.calc_gradient(inputs=inputs, outputs=outputs,
                                            options=self.options)

    def set_complex_step(self, complex_step=False):
        """ Toggles complex_step plumbing for this system and all
        subsystems. Assemblies must prepare their inner system."""

        self.complex_step = complex_step
        self._comp._system.set_complex_step(complex_step)

        #print inputs, outputs, self.J

    #def applyJ(self, variables):
        #""" Call into our assembly's top ApplyJ to get the matrix vector
        #product across the boundary variables.
        #"""

        #inner_system = self._comp._system
        #if self.mode == 'forward':
            #arg = 'du'
            #res = 'df'
        #elif self.mode == 'adjoint':
            #arg = 'df'
            #res = 'du'

        #nonzero = False
        ##needed_vars = flatten_list_of_iters([item[1] for item in variables])
        ##needed_vars.extend([item[0] for item in variables])

        #for item in self.list_inputs() + self.list_states() + \
                    #self.list_outputs() + self.list_residuals():

            #var = self.scope._system.vec[arg][item]
            #if any(var != 0):
                #nonzero = True
            #sub_name = item.partition('.')[2:][0]
            #inner_system.vec[arg][sub_name] = var

            #var = self.scope._system.vec[res][item]
            #sub_name = item.partition('.')[2:][0]
            #inner_system.vec[res][sub_name] = var

        ## Speedhack, don't call component's derivatives if incoming vector is zero.
        #if nonzero is False:
            #return

        #variables = inner_system.variables.keys()
        #inner_system.vec['dp'].array[:] = 0.0
        #inner_system.applyJ(variables)

        #for item in self.list_inputs() + self.list_states()  + \
                    #self.list_outputs() + self.list_residuals():

            #sub_name = item.partition('.')[2:][0]
            #self.scope._system.vec[res][item] = inner_system.vec[res][sub_name]

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        driver = self._comp.driver
        return ISolver.providedBy(self._comp.driver) or \
               driver.__class__.__name__ == 'Driver'

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        # Apply into our assembly.
        for sub in self.local_subsystems():
            sub.solve_linear()


class CompoundSystem(System):
    """A System that has subsystems."""

    def __init__(self, scope, graph, subg, name=None):
        super(CompoundSystem, self).__init__(scope,
                                             graph,
                                             subg.nodes(), #get_full_nodeset(scope, subg.nodes()),
                                             name)
        self.driver = None
        self.graph = subg
        self._local_subsystems = []  # subsystems in the same process
        self._ordering = ()

    def local_subsystems(self):
        if MPI:
            return self._local_subsystems
        else:
            return self.all_subsystems()

    def all_subsystems(self):
        return self._local_subsystems + [data['system'] for node, data in
                                         self.graph.nodes_iter(data=True) if
                                          data['system'] not in self._local_subsystems]

    def simple_subsystems(self):
        for s in self.all_subsystems():
            for sub in s.simple_subsystems():
                yield sub

    def pre_run(self):
        for s in self.local_subsystems():
            s.pre_run()

    def setup_scatters(self):
        """ Defines a scatter for args at this system's level """
        if not self.is_active():
            return
        compound_setup_scatters(self)

    def applyJ(self, variables):
        """ Delegate to subsystems """

        if self.is_active():
            if self.mode == 'forward':
                self.scatter('du', 'dp')
            for subsystem in self.local_subsystems():
                subsystem.applyJ(variables)
            if self.mode == 'adjoint':
                #mpiprint('pre scatter df, du, dp', self.vec['df'].array, self.vec['du'].array, self.vec['dp'].array)
                self.scatter('du', 'dp')
                #mpiprint('post scatter df, du, dp', self.vec['df'].array, self.vec['du'].array, self.vec['dp'].array)
                #mpiprint(self.vec['du'].keys())

    def stop(self):
        self._stop = True
        for s in self.all_subsystems():
            s.stop()

    def _apply_deriv(self, arg, result):
        """Support for directional derivatives, where this function is used
        to perform a matrix vector product.
        """
        self.dfd_solver.calculate(arg, result)

def _get_counts(names):
    """Return a dict with each name keyed to a number indicating the
    number of times it occurs in the list.
    """
    counts = dict([(n,0) for n in names])
    for name in names:
        counts[name] += 1

    return counts

class SerialSystem(CompoundSystem):

    def all_subsystems(self):
        return [self.graph.node[node]['system'] for node in self._ordering]

    def set_ordering(self, ordering, opaque_map):
        """Return the execution order of our subsystems."""
        counts = _get_counts(ordering)
        self._ordering = []

        mapcount = dict([(v, 0) for v in opaque_map.values()])
        for name in ordering:
            if name in self.graph:
                self._ordering.append(name)
                continue

            opaque_name = opaque_map.get(name, name)
            if opaque_name in mapcount:
                mapcount[opaque_name] += 1
            if opaque_name in self.graph:
                count = counts[name]
                if opaque_name in mapcount and mapcount[opaque_name] > count:
                    continue
                self._ordering.append(opaque_name)

        if nx.is_directed_acyclic_graph(self.graph):
            g = self.graph
        else:
            # don't modify real graph
            g = self.graph.subgraph(self.graph.nodes())
            break_cycles(g)

        for node in self.graph.nodes_iter():
            if node not in self._ordering:
                edges = list(nx.dfs_edges(g, node))
                succ = [v for u,v in edges]
                pred = [u for u,v in edges]
                i = 0
                for i,n in enumerate(self._ordering):
                    if n in succ:
                        break
                    elif n in pred:
                        i += 1
                        break
                if i < len(self._ordering):
                    self._ordering.insert(i, node)
                else:
                    self._ordering.append(node)

        self._ordering = gsort(g, self._ordering)

        for s in self.all_subsystems():
            s.set_ordering(ordering, opaque_map)

    def get_req_cpus(self):
        cpus = []
        for sub in self.all_subsystems():
            cpus.append(sub.get_req_cpus())
        self.mpi.requested_cpus = max(cpus+[1])
        return self.mpi.requested_cpus

    def run(self, iterbase, case_label='', case_uuid=None):
        if self.is_active():
            self._stop = False

            for sub in self.local_subsystems():
                self.scatter('u', 'p', sub)

                sub.run(iterbase, case_label=case_label, case_uuid=case_uuid)
                if self._stop:
                    raise RunStopped('Stop requested')

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        if self.is_active():
            self._stop = False

            for sub in self.local_subsystems():
                self.scatter('u', 'p', sub)

                sub.evaluate(iterbase, case_label, case_uuid)
                if self._stop:
                    raise RunStopped('Stop requested')

    def setup_communicators(self, comm):
        self._local_subsystems = []

        self.mpi.comm = get_comm_if_active(self, comm)
        if not self.is_active():
            return

        for sub in self.all_subsystems():
            sub.setup_communicators(self.mpi.comm)
            sub._parent_system = self
            if sub.is_active():
                self._local_subsystems.append(sub)

class ParallelSystem(CompoundSystem):

    def get_req_cpus(self):
        cpus = 0
        # in a parallel system, the required cpus is the sum of
        # the required cpus of the members
        for node, data in self.graph.nodes_iter(data=True):
            cpus += data['system'].get_req_cpus()
        self.mpi.requested_cpus = cpus
        return cpus

    def run(self, iterbase, case_label='', case_uuid=None):
        #mpiprint("running parallel system %s: %s" % (self.name, [c.name for c in self.local_subsystems()]))
        # don't scatter unless we contain something that's actually
        # going to run
        if not self.local_subsystems() or not self.is_active():
            return

        self.scatter('u', 'p')

        for sub in self.local_subsystems():
            sub.run(iterbase, case_label=case_label, case_uuid=case_uuid)

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        self.run(iterbase, case_label=case_label, case_uuid=case_uuid)

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
        sub_comm = comm.Split(rank_color)

        if sub_comm == MPI.COMM_NULL:
            return

        for i,sub in enumerate(subsystems):
            if i == rank_color:
                self._local_subsystems.append(sub)
            elif requested_procs[i] == 0:  # sub is duplicated everywhere
                self._local_subsystems.append(sub)

        for sub in self.local_subsystems():
            sub._parent_system = self
            sub.setup_communicators(sub_comm)

    def setup_variables(self, resid_state_map=None):
        """ Determine variables from local subsystems """
        varmeta = self.scope._var_meta
        self.variables = OrderedDict()
        if not self.is_active():
            return

        for sub in self.local_subsystems():
            sub.setup_variables(resid_state_map)

        if self.local_subsystems():
            sub = self.local_subsystems()[0]
            names = sub.variables.keys()
        else:
            sub = None
            names = []

        varkeys_list = self.mpi.comm.allgather(names)

        for varkeys in varkeys_list:
            for name in varkeys:
                self.variables[name] = varmeta[name].copy()
                self.variables[name]['size'] = 0

        if sub:
            for name, var in sub.variables.items():
                self.variables[name] = var

        self._create_var_dicts(resid_state_map)

    def simple_subsystems(self):
        lsys = self.local_subsystems()
        if lsys:
            return lsys[0].simple_subsystems()
        else:
            return []


class OpaqueSystem(SimpleSystem):
    """A system with an external interface like that
    of a simple system, but encapsulating a compound
    system.
    """
    def __init__(self, scope, graph, subg, name):
        nodes = set(subg.nodes())

        # take the graph we're given, collapse our nodes into a single
        # node, and create a simple system for that
        ograph = graph.subgraph(graph.nodes_iter())
        int_nodes = ograph.internal_nodes(nodes, shared=False)
        shared_int_nodes = ograph.internal_nodes(nodes, shared=True)
        ograph.add_node(tuple(nodes), comp='opaque')
        collapse_nodes(ograph, tuple(nodes), int_nodes)

        super(OpaqueSystem, self).__init__(scope, ograph, tuple(nodes))

        graph = graph.subgraph(shared_int_nodes)

        dests = set()
        nodeset = set()
        internal_comps = set()
        for n in nodes:
            obj = getattr(scope, n, None)
            if obj is not None:
                if has_interface(obj, IDriver):
                    internal_comps.update([c.name for c in obj.iteration_set()])
                else:
                    internal_comps.add(n)

        for node in self._in_nodes:
            for d in node[1]:
                cname, _, vname = d.partition('.')
                if vname and cname in internal_comps and node not in nodeset:
                    dests.add((d, node))
                    nodeset.add(node)

        # sort so that base vars will be before subvars
        dests = sorted(dests)

        # need to create invar nodes here else inputs won't exist in
        # internal vectors
        for dest, node in dests:
            if not graph.in_degree(node):
                base = base_var(graph, dest)
                if base in graph:
                    graph.add_edge(base, node)
                else:
                    graph.add_node(dest, comp='dumbvar')
                    graph.add_edge(dest, node)

            if dest in graph:
                graph.node[dest]['system'] = _create_simple_sys(scope, graph, dest)

        self._inner_system = SerialSystem(scope, graph,
                                          graph.component_graph(),
                                          name = "FD_" + str(name))

        self._inner_system._provideJ_bounds = None
        self._comp = None

    def is_opaque(self):
        return True

    def inner(self):
        return self._inner_system

    def _get_comps(self):
        return self._inner_system._get_comps()

    def setup_communicators(self, comm):
        self.mpi.comm = comm
        self._inner_system.setup_communicators(comm)

    def setup_variables(self, resid_state_map=None):
        super(OpaqueSystem, self).setup_variables(resid_state_map)
        self._inner_system.setup_variables()

    def setup_sizes(self):
        super(OpaqueSystem, self).setup_sizes()
        self._inner_system.setup_sizes()

    def setup_vectors(self, arrays=None, state_resid_map=None):
        super(OpaqueSystem, self).setup_vectors(arrays)
        # internal system will create new vectors
        self._inner_system.setup_vectors(None)

        # Preload all inputs and outputs along to our inner system.
        # This was needed for the case where you regenerate the system
        # hierarchy on the first calc_gradient call.
        inner_u = self._inner_system.vec['u']
        inner_u.set_from_scope(self.scope)#,
                               #self._inner_system.list_inputs() + \
                               #self._inner_system.list_states() +\
                               #self._inner_system.list_outputs() +
                               #self._inner_system.list_residuals())

    def setup_scatters(self):
        super(OpaqueSystem, self).setup_scatters()
        self._inner_system.setup_scatters()

    def set_options(self, mode, options):
        """ Sets all user-configurable options for the inner_system and its
        children. """
        super(OpaqueSystem, self).set_options(mode, options)
        self._inner_system.set_options(mode, options)

    def pre_run(self):
        self._inner_system.pre_run()

    def run(self, iterbase, case_label='', case_uuid=None):
        self_u = self.vec['u']
        inner_u = self._inner_system.vec['u']

        vnames = self._inner_system.list_inputs() + \
                 self._inner_system.list_states()
        inner_u.set_from_scope(self.scope, vnames)
        if self.complex_step is True:
            self._inner_system.vec['du'].set_from_scope_complex(self.scope, vnames)

        self._inner_system.run(iterbase, case_label=case_label, case_uuid=case_uuid)

        #for item in self.list_outputs():
            #self_u[item][:] = inner_u[item][:]
        for name, val in inner_u.items():
            if name in self_u:
                self_u[name][:] = val
                if self.complex_step is True:
                    self.vec['du'][name][:] = self._inner_system.vec['du'][name]

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        self.run(iterbase, case_label=case_label, case_uuid=case_uuid)

    def linearize(self):
        """Do a finite difference on the inner system to calculate a
        Jacobian.
        """

        inner_system = self._inner_system
        inputs = self.list_inputs() + self.list_states()
        outputs = self.list_outputs()

        if self.options.directional_fd is True:
            if self.mode == 'adjoint':
                msg = "Directional derivatives can only be used with forward mode."
                raise RuntimeError(msg)
            self.J = None
            inner_system.dfd_solver = DirectionalFD(inner_system, inputs,
                                                    outputs)
            inner_system.apply_deriv = inner_system._apply_deriv
        else:
            self.J = inner_system.solve_fd(inputs, outputs)

        #print self.J, inputs, outputs

    def set_complex_step(self, complex_step=False):
        """ Toggles complex_step plumbing for this system and all
        subsystems. The Opaque System must call its inner system."""

        self.complex_step = complex_step
        self._inner_system.set_complex_step(complex_step)

    def applyJ(self, variables):
        vec = self.vec
        dfvec = vec['df']

        # Forward Mode
        if self.mode == 'forward':

            self.scatter('du', 'dp')

            applyJ(self, variables)
            dfvec.array[:] *= -1.0

            for var in self.list_outputs():
                if var in dfvec:
                    dfvec[var][:] += vec['du'][var][:]

        # Adjoint Mode
        elif self.mode == 'adjoint':

            # Sign on the local Jacobian needs to be -1 before
            # we add in the fake residual. Since we can't modify
            # the 'du' vector at this point without stomping on the
            # previous component's contributions, we can multiply
            # our local 'arg' by -1, and then revert it afterwards.
            dfvec.array[:] *= -1.0
            applyJT(self, variables)
            dfvec.array[:] *= -1.0

            for var in self.list_outputs():
                if var in dfvec:
                    vec['du'][var][:] += dfvec[var][:]

            self.scatter('du', 'dp')

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        self.sol_vec.array[:] = self.rhs_vec.array[:]

    def simple_subsystems(self):
        yield self

    def set_ordering(self, ordering, opaque_map):
        self._inner_system.set_ordering(ordering, opaque_map)

    def get_req_cpus(self):
        return self._inner_system.get_req_cpus()

class FiniteDiffDriverSystem(SimpleSystem):
    """A System for a Driver component that is not a Solver."""

    def __init__(self, graph, driver):
        scope = driver.parent
        super(FiniteDiffDriverSystem, self).__init__(scope, graph, driver.name)
        driver._system = self

    def setup_communicators(self, comm):
        super(FiniteDiffDriverSystem, self).setup_communicators(comm)
        self._comp.setup_communicators(self.mpi.comm)

    def setup_scatters(self):
        #super(FiniteDiffDriverSystem, self).setup_scatters()
        compound_setup_scatters(self)
        self._comp.setup_scatters()

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        return False

    def local_subsystems(self):
        return [s for s in self.all_subsystems() if s.is_active()]

    def all_subsystems(self):
        return (self._comp.workflow._system,)

    def simple_subsystems(self):
        yield self
        for sub in self._comp.workflow._system.simple_subsystems():
            yield sub

    def pre_run(self):
        for s in self.local_subsystems():
            s.pre_run()


class TransparentDriverSystem(SimpleSystem):
    """A system for an driver that allows derivative calculation across its
    boundary."""

    def __init__(self, graph, driver):
        scope = driver.parent
        super(TransparentDriverSystem, self).__init__(scope, graph, driver.name)
        driver._system = self

    def _get_resid_state_map(self):
        """ Essentially, this system behaves like a solver system, except it
        has no states or residuals.
        """
        return dict()

    def setup_communicators(self, comm):
        super(TransparentDriverSystem, self).setup_communicators(comm)
        self._comp.setup_communicators(self.mpi.comm)

    def setup_variables(self, resid_state_map=None):
        # pass our resid_state_map to our children
        super(TransparentDriverSystem, self).setup_variables(self._get_resid_state_map())

    def setup_scatters(self):
        #super(TransparentDriverSystem, self).setup_scatters()
        compound_setup_scatters(self)
        self._comp.setup_scatters()

    def local_subsystems(self):
        return [s for s in self.all_subsystems() if s.is_active()]

    def all_subsystems(self):
        return (self._comp.workflow._system,)

    def simple_subsystems(self):
        yield self
        for sub in self._comp.workflow._system.simple_subsystems():
            yield sub

    def pre_run(self):
        for s in self.local_subsystems():
            s.pre_run()

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        self.run(iterbase, case_label=case_label, case_uuid=case_uuid)

    def clear_dp(self):
        """ Recusively sets the dp vector to zero."""
        self.vec['dp'].array[:] = 0.0
        for system in self.local_subsystems():
            system.clear_dp()

    def applyJ(self, variables):
        """ Delegate to subsystems """

        # Need to clean out the dp vector because the parent systems can't
        # see into this subsystem.
        self.clear_dp()

        if self.mode == 'forward':
            self.scatter('du', 'dp')
        for subsystem in self.local_subsystems():
            subsystem.applyJ(variables)
        if self.mode == 'adjoint':
            self.scatter('du', 'dp')

    def linearize(self):
        """ Solvers must Linearize all of their subsystems. """

        for subsystem in self.local_subsystems():
            subsystem.linearize()


class SolverSystem(TransparentDriverSystem):  # Implicit
    """A System for a Solver component. While it inherits from a SimpleSystem,
    much of the behavior is like a CompoundSystem, particularly variable
    propagation."""

    def _get_resid_state_map(self):

        varmeta = self.scope._var_meta

        # map of individual var names to collapsed names
        nodemap = self.scope.name2collapsed

        # set up our own resid_state_map
        pairs = self._comp._get_param_constraint_pairs()
        resid_state_map = dict([(nodemap[c], nodemap[p]) for p, c in pairs])
        states = resid_state_map.values()

        pgroups = self._comp.list_param_group_targets()
        resids = self._comp.list_eq_constraint_targets()

        szdict = {}
        for params in pgroups:
            skip = False
            params = tuple(params)
            for p in params:
                if nodemap[p] in states:
                    skip = True
                    break
            if not skip:  # add to the size dict so we can match on size
                node = nodemap[params[0]]
                szdict.setdefault(varmeta[node]['size'], []).append(node)

        # get rid of any residuals we already mapped
        resids = [r for r in resids if nodemap[r] not in resid_state_map]

        # match remaining residuals and states by size
        for resid in resids:
            resnode = nodemap[resid]
            sz = varmeta[resnode]['size']
            try:
                pnode = szdict[sz].pop()
            except:
                raise RuntimeError("unable to find a state of size %d to match residual '%s'" %
                                    (sz, resid))
            resid_state_map[resnode] = pnode

        # all states must have a corresponding residual
        for sz, pnodes in szdict.items():
            if pnodes:
                raise RuntimeError("param node %s of size %d has no matching residual" %
                                    (pnodes, sz))

        return resid_state_map

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        # Apply to inner driver system only. No need to pass options since it
        # has its own.
        for sub in self.local_subsystems():
            sub.solve_linear()


def _create_simple_sys(scope, graph, name):
    """Given a Component, create the appropriate type
    of simple System.
    """
    comp = getattr(scope, name, None)

    if has_interface(comp, ISolver):
        sub = SolverSystem(graph, comp)
    elif has_interface(comp, IDriver):
        from openmdao.main.driver import Driver
        if comp.__class__ == Driver:
            sub = TransparentDriverSystem(graph, comp)
        else:
            sub = FiniteDiffDriverSystem(graph, comp)
    elif has_interface(comp, IAssembly):
        sub = AssemblySystem(scope, graph, name)
    elif has_interface(comp, IPseudoComp) and comp._pseudo_type=='constraint' \
               and comp._subtype == 'equality':
        sub = EqConstraintSystem(scope, graph, name)
    elif has_interface(comp, IComponent):
        sub = SimpleSystem(scope, graph, name)
    elif graph.node[name].get('comp') == 'param':
        sub = ParamSystem(scope, graph, name)
    elif graph.node[name].get('comp') == 'invar':
        sub = InVarSystem(scope, graph, name)
    elif graph.node[name].get('comp') == 'outvar':
        sub = OutVarSystem(scope, graph, name)
    elif graph.node[name].get('comp') == 'dumbvar':
        sub = VarSystem(scope, graph, name)
    else:
        raise RuntimeError("don't know how to create a System for '%s'" % name)

    return sub

def partition_subsystems(scope, graph, cgraph):
    """Return a nested system graph with metadata for parallel
    and serial subworkflows.  Graph must acyclic. All subdriver
    iterations sets must have already been collapsed.

    """
    if len(cgraph) < 2:
        return cgraph

    gcopy = cgraph.subgraph(cgraph.nodes_iter())

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
                    parallel_group.append(tuple(brnodes))
                else:
                    parallel_group.append(brnodes[0])

            for branch in parallel_group:
                if isinstance(branch, tuple):
                    to_remove.extend(branch)
                    subg = cgraph.subgraph(branch)
                    partition_subsystems(scope, graph, subg)
                    system=SerialSystem(scope, graph, subg, str(branch))
                    collapse_to_system_node(cgraph, system, branch)

                    gcopy.remove_nodes_from(branch)
                else: # single comp system
                    gcopy.remove_node(branch)

            #parallel_group = tuple(sorted(parallel_group))
            parallel_group = tuple(parallel_group)
            to_remove.extend(parallel_group)
            subg = cgraph.subgraph(parallel_group)
            system=ParallelSystem(scope, graph, subg, str(parallel_group))
            collapse_to_system_node(cgraph, system, parallel_group)

        elif len(zero_in_nodes) == 1:  # serial
            gcopy.remove_nodes_from(zero_in_nodes)
        else: # circular - no further splitting
            break

    # Now remove all of the old nodes
    cgraph.remove_nodes_from(to_remove)

    return cgraph

def collapse_to_system_node(G, system, name):
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
    if comm.rank+1 > req:
        color = MPI.UNDEFINED
    else:
        color = 1

    return comm.Split(color)

def get_full_nodeset(scope, group):
    names = set()
    for name in simple_node_iter(group):
        obj = getattr(scope, name, None)
        if obj is not None and hasattr(obj, 'get_full_nodeset'):
            names.update(obj.get_full_nodeset())
        else:
            names.add(name)
    return names

def _filter(scope, lst):
    filtered = _filter_subs(lst)
    filtered = _filter_flat(scope, filtered)
    return _filter_ignored(scope, filtered)

def _filter_subs(lst):
    """Return a copy of the list with any subvars of basevars in the list
    removed.
    """
    bases = [n.split('[',1)[0] for n in lst]

    return [n for i,n in enumerate(lst)
               if not (bases[i] in lst and n != bases[i])]

def _filter_ignored(scope, lst):
    # Remove any vars that the user designates as 'deriv_ignore'
    unignored = []
    topvars = scope._system.vector_vars

    for name in lst:
        collapsed_name = scope.name2collapsed[name]
        if collapsed_name in topvars and topvars[collapsed_name].get('deriv_ignore'):
            continue

        # The user sets 'deriv_ignore' in the basevar, so we have to check that for
        # subvars.
        base = base_var(scope._depgraph, name)
        if base != name:
            collname = scope.name2collapsed.get(base)
            if collname and collname in topvars and \
               topvars[collname].get('deriv_ignore'):
                continue

        unignored.append(name)

    return unignored

def _filter_flat(scope, lst):
    keep = []

    for name in lst:
        collapsed_name = scope.name2collapsed[name]

        # ignore non-float-flattenable vars
        if not scope._var_meta[collapsed_name].get('noflat'):
            keep.append(name)

    return keep
