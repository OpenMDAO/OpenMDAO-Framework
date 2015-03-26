import sys
from StringIO import StringIO
from collections import OrderedDict
from itertools import chain

import numpy
import networkx as nx
from zope.interface import implements

# pylint: disable-msg=E0611,F0401
from openmdao.main.mpiwrap import MPI, MPI_info, PETSc, get_norm, make_idx_array,\
                                  to_idx_array, idx_arr_type
from openmdao.main.exceptions import RunStopped
from openmdao.main.finite_difference import FiniteDifference, DirectionalFD
from openmdao.main.linearsolver import ScipyGMRES, PETSc_KSP, LinearGS
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly, IImplicitComponent, \
                                     ISolver, IPseudoComp, IComponent, ISystem
from openmdao.main.vecwrapper import VecWrapper, InputVecWrapper, DataTransfer, \
                                     idx_merge, _filter, _filter_subs, \
                                     _filter_flat, _filter_ignored, \
                                     dedup
from openmdao.main.depgraph import break_cycles, get_node_boundary, gsort, \
                                   collapse_nodes, simple_node_iter
from openmdao.main.derivatives import applyJ, applyJT
from openmdao.util.graph import base_var
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.variable import Variable


class System(object):
    implements(ISystem)

    def __init__(self, scope, graph, nodes, name):
        self.name = str(name)
        self.node = name
        self.scope = scope
        self._nodes = nodes

        self.variables = OrderedDict() # dict of all vars owned by this System (flat and non-flat)
        self.flat_vars = OrderedDict() # all vars used in vectors, whether they add to vector size or not
        self.noflat_vars = OrderedDict() # all vars that are not flattenable to float arrays (so are not part of vectors)
        self.vector_vars = OrderedDict() # all vars that contribute to the size of vectors

        self._inputs = None
        self._outputs = None
        self._states = None
        self._residuals = None

        self._reduced_graph = graph.full_subgraph(nodes)
        self._reduced_graph.fix_dangling_vars(scope)

        self._mapped_resids = {}

        self._out_nodes = []

        # find our output nodes (outputs from our System and any child Systems)
        for node in nodes:
            if node in graph:
                for succ in graph.successors(node):
                    if succ not in self._out_nodes:
                        self._out_nodes.append(succ)

        all_outs = set(nodes)
        all_outs.update(self._out_nodes)

        # get our input nodes from the depgraph
        ins, _ = get_node_boundary(graph, all_outs)

        #print "%s: ins = %s" % (self.name, ins)
        self._in_nodes = []
        for i in ins:
            if 'comp' not in graph.node[i]:
                self._in_nodes.append(i)
            elif i in self.scope.name2collapsed and self.scope.name2collapsed[i] in graph:
                n = self.scope.name2collapsed[i]
                if i != n and n not in self._in_nodes:
                    self._in_nodes.append(n)

        self._in_nodes = sorted(self._in_nodes)
        #print "%s (%s): _in_nodes = %s" % (str(self.name), type(self), self._in_nodes)
        self._out_nodes = sorted(self._out_nodes)

        self.mpi = MPI_info()
        self.mpi.requested_cpus = None
        self.vec = {}
        self.app_ordering = None
        self.scatter_full = None
        self.scatter_rev_full = None
        self.scatter_partial = None
        self.scatter_partial_rev = None

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

        #print "%s: nodes: %s" % (self.name, nodes)
        #print "%s: edges: %s" % (self.name, graph.edges())
        #print "%s: in_nodes: %s" % (self.name, self._in_nodes)
        #print "%s: out_nodes: %s" % (self.name, self._out_nodes)

    def __getitem__(self, key):
        """A convenience method to allow easy access to descendant
        Systems, either by name or by index.
        """
        for i, sub in enumerate(self.subsystems()):
            if key == i or key == sub.name:
                return sub

        if isinstance(key, basestring):
            for sub in self.subsystems():
                s = sub[key]
                if s:
                    return s

        return None

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        return True

    def is_distrib_var(self, node):
        """Return True if the given node contains a var that
        is a distributed input or output to this System.
        """
        for sub in self.simple_subsystems():
            if sub.is_distrib_var(node):
                return True

        return False

    def pre_run(self):
        """ Runs at assembly execution"""
        pass

    def subsystems(self, local=False):
        if local:
            return self.local_subsystems()
        return self.all_subsystems()

    def local_subsystems(self):
        return ()

    def all_subsystems(self):
        return ()

    def list_subsystems(self, local=False):
        """Returns the names of our subsystems."""
        return [s.name for s in self.subsystems(local)]

    def create_app_ordering(self):
        """Creates a PETSc application ordering."""
        rank = self.mpi.rank

        start = numpy.sum(self.local_var_sizes[:rank])
        end = numpy.sum(self.local_var_sizes[:rank+1])
        to_idx_array = make_idx_array(start, end)

        app_idxs = []
        for ivar, v in enumerate(self.vector_vars):
            start = numpy.sum(self.local_var_sizes[:, :ivar]) + \
                        numpy.sum(self.local_var_sizes[:rank, ivar])
            end = start + self.local_var_sizes[rank, ivar]
            app_idxs.append(make_idx_array(start, end))

        if app_idxs:
            app_idxs = numpy.concatenate(app_idxs)

        app_ind_set = PETSc.IS().createGeneral(app_idxs, comm=self.mpi.comm)
        petsc_ind_set = PETSc.IS().createGeneral(to_idx_array, comm=self.mpi.comm)

        return PETSc.AO().createBasic(app_ind_set, petsc_ind_set,
                                      comm=self.mpi.comm)

    def get_combined_J(self, J):
        """
        Take a J dict that's distributed, i.e., has different values
        across different MPI processes, and return a dict that
        contains all of the values from all of the processes.  If
        values are duplicated, use the value from the lowest rank
        process.  Note that J has a nested dict structure.
        """

        comm = self.mpi.comm
        if not self.is_active():
            return

        myrank = comm.rank

        tups = []

        # Gather a list of local tuples for J.
        for output, dct in J.items():
            for param, value in dct.items():

                # Params are already only on this process. We need to add
                # only outputs of components that are on this process.
                sys = self.find_system(output.partition('.')[0])
                if sys.is_active():
                    tups.append((output, param))

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

        # return the combined dict
        return comm.bcast(J, root=0)

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

    def _all_comp_nodes(self, local=False):
        """Return a set of comps for this system and all subsystems."""
        comps = set()
        for s in self.subsystems(local=local):
            comps.update(s._all_comp_nodes(local=local))
        return comps

    def list_inputs(self):
        """Returns names of input variables from this System and all of its
        children.
        """
        if self._inputs is None:
            inputs = set()
            is_opaque = isinstance(self, OpaqueSystem)

            for system in self.simple_subsystems():
                comps = self._all_comp_nodes()
                for tup in system._in_nodes:
                    # need this to prevent paramgroup inputs on same comp to be
                    # counted more than once
                    for dest in tup[1]:
                        comp = dest.split('.', 1)[0]
                        if comp in comps:
                            inputs.add(dest)
                            # Since Opaque systems do finite difference on the
                            # full param groups, we should only include one input
                            # from each group.
                            if is_opaque:
                                break

            self._inputs = _filter(self.scope, inputs)

        return self._inputs

    def list_states(self):
        """Returns names of states (not collapsed edges) from this System and
        all of its children.
        """
        if self._states is None:
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
            self._states = states

        return self._states

    def list_outputs(self):
        """Returns names of output variables (not collapsed edges)
        from this System and all of its children.  This only lists
        outputs that are relevant to derivatives calculations.
        """
        if self._outputs is None:
            outputs = []
            for system in self.simple_subsystems():
                states = set()
                try:
                    states.update(['.'.join((system.name,s))
                                      for s in system._comp.list_states()])
                except AttributeError:
                    pass
                out_nodes = [node for node in system._out_nodes
                             if node not in self._mapped_resids]
                comps = self._all_comp_nodes()
                for src, _ in out_nodes:
                    cname, _, vname = src.partition('.')
                    if cname in comps and src not in states:
                        outputs.append(src)

            self._outputs = _filter(self.scope, outputs)

        return self._outputs

    def list_residuals(self):
        """Returns names of all residuals.
        """
        if self._residuals is None:
            outputs = []
            for system in self.simple_subsystems():
                try:
                    outputs.extend(['.'.join((system.name, s))
                                      for s in system._comp.list_residuals()
                                      if system._comp.eval_only is False])
                except AttributeError:
                    pass

            outputs.extend([n for n, m in self._mapped_resids.keys()])
            self._residuals = outputs

        return self._residuals

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

        size = 0
        for name in names:
            if isinstance(name, tuple):
                name = name[0]

            if name in uvec:
                size += uvec[name].size
            elif collnames[name] in varkeys:
                idx = varkeys.index(collnames[name])
                for proc in range(self.mpi.size):
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
        #print "%s: my vars = %s" % (self.name, [(n,self.variables[n]['size']) for n in self.variables])

    def _create_var_dicts(self, resid_state_map):
        # now figure out all of the inputs we 'own'
        self._owned_args = self._get_owned_args()

        # split up vars into 3 categories:
        #  1) flattenable vars that add to the size of the vectors
        #  2) flattenable vars that don't add to the size of the vectors because they
        #     are slices of other vars already in the vectors
        #  3) non-flattenable vars

        # first, get all flattenable variables
        for name in _filter_flat(self.scope, self.variables.keys()):
            self.flat_vars[name] = self.variables[name]

        # now get all flattenable vars that add to vector size
        self.vector_vars = self._get_vector_vars(self.flat_vars)

        for name, info in self.variables.items():
            if name not in self.flat_vars:
                self.noflat_vars[name] = info

    def get_src_idxs(self, name):
        """Return the index array corresponding to the given source name."""
        if name in self.vector_vars:
            return make_idx_array(0, self.scope._var_meta[name]['size'])
        elif '[' in name[0]:
            return self.scope._var_meta[name].get('flat_idx')

    def get_distrib_idxs(self, name):
        return self.get_src_idxs(name)

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

        ours = numpy.zeros((1, len(self.vector_vars)), int)
        for i, (name, var) in enumerate(self.vector_vars.items()):
            ours[0, i] = var['size']

        # collect local var sizes from all of the processes in our comm
        # these sizes will be the same in all processes except in cases
        # where a variable belongs to a multiprocessor component.  In that
        # case, the part of the component that runs in a given process will
        # only have a slice of each of the component's variables.
        if MPI:
            comm.Allgather(ours[0,:], self.local_var_sizes)

        self.local_var_sizes[rank, :] = ours[0, :]

        # if we're a distrib comp, get full distrib size
        # so that downstream non-distrib comps can automagically gather
        # the full distributed vec to their input
        if hasattr(self, 'distrib_idxs') and self.distrib_idxs:
           for src_idx,v in enumerate(self.vector_vars.keys()):
               distrib_size = numpy.sum(self.local_var_sizes[:, src_idx])
               self.scope._var_meta[v]['distrib_size'] = distrib_size

        # create a (1 x nproc) vector for the sizes of all of our
        # local inputs
        self.input_sizes = numpy.zeros(size, int)

        insize = numpy.zeros(1, int)
        for arg in _filter_flat(self.scope, self._owned_args):
            idx = self.get_distrib_idxs(arg)
            assert(idx is not None)
            insize[0] += len(idx)

        if MPI:
            comm.Allgather(insize[0], self.input_sizes)

        self.input_sizes[rank] = insize[0]

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
                dups = {}
                for s in self.local_subsystems():
                    for k in s.vector_vars.keys():
                        dups.setdefault(k, set()).add(s.name)

                multis = [(k,list(v)) for k,v in dups.items() if len(v) > 1]
                if multis:
                    msg += " The following var nodes are duplicated in subsystems: "
                    for i, (v,s) in enumerate(multis):
                        msg += "%s duplicated in %s" % (v,s)
                        if i:
                            msg += ", "
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
            if self.mode == 'adjoint' and srcvecname == 'du':
                scatter = self.scatter_rev_full
            else:
                scatter = self.scatter_full
        else:
            if self.mode == 'adjoint' and srcvecname == 'du':
                scatter = subsystem.scatter_partial_rev
            else:
                scatter = subsystem.scatter_partial

        if scatter is not None:
            srcvec = self.vec[srcvecname]
            destvec = self.vec[destvecname]

            #print "SCATTER", srcvecname, destvecname
            #print self.name, self
            #print 'srcvec', srcvec.array, srcvec.keys()
            #print 'destvec', destvec.array, destvec.keys()

            scatter(self, srcvec, destvec)
            #print self.name, scatter is self.scatter_full, subsystem

            if destvecname == 'p':

                if self.complex_step is True:
                    scatter(self, self.vec['du'], self.vec['dp'],
                            complex_step=True)

                if scatter:
                    if subsystem is None:
                        destvec.set_to_scope(self.scope)
                        if self.complex_step is True:
                            self.vec['dp'].set_to_scope_complex(self.scope)
                    elif subsystem._in_nodes:
                        destvec.set_to_scope(self.scope, subsystem._in_nodes)
                        if self.complex_step is True:
                            self.vec['dp'].set_to_scope_complex(self.scope,
                                                                subsystem._in_nodes)

    def dump(self, nest=0, stream=sys.stdout, verbose=False):
        """Prints out a textual representation of the collapsed
        execution graph (with groups of component nodes collapsed
        into Systems).  It shows which
        components run on the current processor.
        """
        if stream is None:
            getval = True
            stream = StringIO()
        else:
            getval = False

        if not self.is_active():
            return stream.getvalue() if getval else None

        if MPI is None:
            world_rank = 0
        else:
            world_rank = MPI.COMM_WORLD.rank

        stream.write(" "*nest)
        stream.write(str(self.name).replace(' ','').replace("'",""))
        klass = self.__class__.__name__
        stream.write(" [%s](req=%d)(rank=%d)(vsize=%d)(isize=%d)\n" %
                                          (klass.lower()[:5],
                                           self.get_req_cpus(),
                                           world_rank,
                                           self.vec['u'].array.size,
                                           self.input_sizes[self.mpi.rank]))

        for v, info in self.vec['u']._info.items():
            if verbose or not info.hide:
                stream.write(" "*(nest+2))
                if v in self.vec['p']:
                    stream.write("u (%s)  p (%s): %s\n" %
                                     (list(self.vec['u'].bounds([v])),
                                      list(self.vec['p'].bounds([v])), v))
                else:
                    stream.write("u (%s): %s\n" % (list(self.vec['u'].bounds([v])), v))

        for v, info in self.vec['p']._info.items():
            if v not in self.vec['u'] and (verbose or not info.hide):
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

        nest += 4
        if isinstance(self, OpaqueSystem):
            self._inner_system.dump(nest, stream)
        elif isinstance(self, AssemblySystem):
            self._comp._system.dump(nest, stream)
        else:
            if self.scatter_full:
                self.scatter_full.dump(self, self.vec['u'], self.vec['p'], nest)
            partial_subs = [s for s in self.local_subsystems() if s.scatter_partial]
            for sub in self.local_subsystems():
                if sub in partial_subs:
                    sub.scatter_partial.dump(self, self.vec['u'], self.vec['p'], nest+4, stream)
                sub.dump(nest, stream)

        return stream.getvalue() if getval else None

    def dump_vars(self, stream=sys.stdout):
        """For debugging.  Dumps variable values from scope, u vector and p vector"""
        vnames = sorted(set(self.vec['u'].keys()+self.vec['p'].keys()))
        for name in vnames:
            stream.write("%s: " % str(name))
            strs = []
            names = []
            if self.scope.contains(name[0]):
                names.append('scope')
                strs.append("%s" % self.scope.get(name[0]))
            if name in self.vec['u']:
                names.append('U')
                strs.append("%s" % self.vec['u'][name])
            if name in self.vec['p']:
                names.append('P')
                strs.append("%s" % self.vec['p'][name])
            stream.write('(%s) = (%s)' % (','.join(names), ', '.join(strs)))
            stream.write('\n')

    def _get_vector_vars(self, vardict):
        """Return vector_vars, which are vars that actually add to the
        size of the vectors (as opposed to subvars of vars that are in
        the vector, which don't add anything to the vector but just
        use a subview of the view corresponding to their base var)
        """
        keep_srcs = set(_filter_subs([n[0] for n in vardict]))

        return OrderedDict([(k,v) for k,v in vardict.items() if k[0] in keep_srcs])

    def set_options(self, mode, options):
        """ Sets all user-configurable options for this system and all
        subsystems. """

        for subsystem in self.subsystems():
            subsystem.set_options(mode, options)

        if not self.is_active():
            return

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

    # ------- derivative stuff -----------

    def initialize_gradient_solver(self):
        """ Initialize the solver that will be used to calculate the
        gradient. """

        if not self.is_active():
            return

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
        """ Linearize local subsystems. """

        for subsystem in self.local_subsystems():
            subsystem.linearize()

    def set_complex_step(self, complex_step=False):
        """ Toggles complex_step plumbing for this system and all
        local subsystems.
        """

        self.complex_step = complex_step
        for subsystem in self.local_subsystems():
            subsystem.set_complex_step(complex_step)

    def calc_gradient(self, inputs, outputs, mode='auto', options=None,
                      iterbase='', return_format='array'):
        """ Return the gradient for this system. """

        if not self.is_active():
            return

        if options.force_fd or mode == 'fd':
            self.set_options('fd', options)

            self.vec['df'].array[:] = 0.0
            self.vec['du'].array[:] = 0.0
            self.clear_dp()
            return self.solve_fd(inputs, outputs, iterbase, return_format)

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

        self.set_options(mode, options)
        self.initialize_gradient_solver()

        self.linearize()

        # Clean out all arrays.
        self.vec['df'].array[:] = 0.0
        self.vec['du'].array[:] = 0.0
        self.clear_dp()

        J = self.ln_solver.calc_gradient(inputs, outputs, return_format)
        self.sol_vec.array[:] = 0.0
        return J

    def solve_fd(self, inputs, outputs, iterbase='', return_format='array'):
        """Finite difference solve."""

        if self.fd_solver is None:
            self.fd_solver = FiniteDifference(self, inputs, outputs,
                                              return_format)
        return self.fd_solver.solve(iterbase=iterbase)

    def calc_newton_direction(self, options=None, iterbase=''):
        """ Solves for the new state in Newton's method and leaves it in the
        df vector.
        """

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
        in the RHS vector.
        """
        if get_norm(self.rhs_vec) < 1e-15:
            self.sol_vec.array[:] = 0.0
            return self.sol_vec.array

        #print "solving linear sys", self.name

        if options is not None:
            self.set_options(self.mode, options)
        self.initialize_gradient_solver()

        """ Solve Jacobian, df |-> du [fwd] or du |-> df [rev] """
        self.rhs_buf[:] = self.rhs_vec.array[:]
        self.sol_buf[:] = self.sol_vec.array[:]
        self.sol_buf[:] = self.ln_solver.solve(self.rhs_buf)
        self.sol_vec.array[:] = self.sol_buf[:]

    def _compute_derivatives(self, vname, ind):
        """ Solves derivatives of system (direct/adjoint).
        ind must be a global petsc index.
        """
        self.rhs_vec.array[:] = 0.0
        self.sol_vec.array[:] = 0.0
        self.vec['dp'].array[:] = 0.0

        varkeys = self.vector_vars.keys()
        if vname in varkeys:
            ivar = varkeys.index(vname)
        else:
            base = vname[0].split('[',1)[0]
            base = self.scope.name2collapsed[base]
            ivar = varkeys.index(base)
            ind = self.scope._var_meta[vname]['flat_idx'][ind]

        if self.local_var_sizes[self.mpi.rank, ivar] > 0:
            ind += numpy.sum(self.local_var_sizes[:, :ivar])
            ind += numpy.sum(self.local_var_sizes[:self.mpi.rank, ivar])

            ind_set = PETSc.IS().createGeneral([ind], comm=self.mpi.comm)
            if self.app_ordering is not None:
                ind_set = self.app_ordering.app2petsc(ind_set)
            ind = ind_set.indices[0]
            self.rhs_vec.petsc_vec.setValue(ind, 1.0, addv=False)
        else:
            # creating an IS is a collective call, so must do it
            # here to avoid hanging, even though we don't need the IS
            ind_set = PETSc.IS().createGeneral([], comm=self.mpi.comm)

        self.sol_buf[:] = self.sol_vec.array[:]
        self.rhs_buf[:] = self.rhs_vec.array[:]

        self.ln_solver.ksp.solve(self.rhs_buf_petsc, self.sol_buf_petsc)

        self.sol_vec.array[:] = self.sol_buf[:]

        #print 'dx', self.sol_vec.array
        return self.sol_vec

    def is_var_in_parallel_system(self, varname):
        """ Returns True if a full varnode name is an input under a parallel
        system."""

        for system in self.all_subsystems():
            if system.is_var_in_parallel_system(varname):
                return True

        return False


class SimpleSystem(System):
    """A System for a single Component. This component can have Inputs,
    Outputs, States, and Residuals."""

    def __init__(self, scope, graph, name):
        comp = None
        nodes = set([name])
        cpus = 1
        self._grouped_nodes = name
        if not isinstance(name, tuple):
            self._grouped_nodes = tuple([name])
        try:
            comp = getattr(scope, name)
        except (AttributeError, TypeError):
            pass
        else:
            if has_interface(comp, IComponent):
                self._comp = comp
                if isinstance(comp, PseudoComponent):
                    comp._system = self
                nodes = comp.get_full_nodeset()
                cpus = comp.get_req_cpus()
            else:
                comp = None

        super(SimpleSystem, self).__init__(scope, graph, nodes, name)

        self.mpi.requested_cpus = cpus
        self._comp = comp
        self.J = None
        self._mapped_resids = {}
        self.distrib_idxs = {}

    def setup_sizes(self):
        super(SimpleSystem, self).setup_sizes()
        if self.is_active():
            for var, metadata in self.vector_vars.iteritems():
                if len(self.scope.get_flattened_value(var[0])) == 0:
                    msg = "{} was not initialized. OpenMDAO does not support uninitialized variables."
                    msg = msg.format(var[0])
                    self.scope.raise_exception(msg, ValueError)

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

    def is_distrib_var(self, node):
        """Return True if the given node contains a var that
        is a distributed input or output to this System.
        """
        if self._comp and self.distrib_idxs:
            for name in simple_node_iter(node):
                parts = name.split('.', 1)
                if parts[0] == self._comp.name and len(parts) > 1:
                    if parts[1].split('[')[0] in self.distrib_idxs:
                        return True

        return False

    def _all_comp_nodes(self, local=False):
        return simple_node_iter(self._nodes)

    def simple_subsystems(self):
        yield self

    def setup_communicators(self, comm):
        if self._comp:
            cpus = self._comp.get_req_cpus()
            if cpus == 1 or IAssembly.providedBy(self._comp) or IDriver.providedBy(self._comp):
                pass
            else:
                color = [0] * cpus
                if comm.size > cpus:
                    color.extend([MPI.UNDEFINED]*(comm.size-cpus))

                comm = comm.Split(color[comm.rank])

            self._comp.setup_communicators(comm)

            if hasattr(self._comp, 'get_distrib_idxs'):
                self.distrib_idxs = self._comp.get_distrib_idxs()

        self.mpi.comm = comm

    def get_distrib_idxs(self, name):
        """These indices are actually the indices in the *source*
        vector that get scattered to a particular input.
        """
        me = self.name.strip('@')
        for n in name[1]:
            destbase = n.split('[')[0]
            if n.startswith(me+'.') or destbase == me:
                break
        else:
            return None

        if self._comp is None:
            if n == me:
                return make_idx_array(0, self.scope._var_meta[name]['size'])
            return super(SimpleSystem, self).get_distrib_idxs(name)
        else:
            distrib_size = self.scope._var_meta[name].get('distrib_size')
            if hasattr(self._comp, 'get_distrib_idxs'):
                # get input indices for full variable
                full_idxs = self.distrib_idxs.get(
                                     n.split('[', 1)[0].split('.',1)[1])
                if full_idxs is None:
                    # if full_idxs is None, that means that this particular variable is not
                    # distributed even though the component does have other distributed vars
                    if self._comp and distrib_size is not None:
                        full_idxs = make_idx_array(0, distrib_size)
                    else:
                        full_idxs = self.get_src_idxs(name)
                else:
                    full_idxs = to_idx_array(full_idxs)

                # if input is a subvar, we have to take a subset of the
                # input indices
                if '[' in n:
                    src_idxs = self.get_src_idxs(name)
                    info = self.scope._get_var_info(n)
                    sub_idxs = info['flat_idx']
                    assert(len(src_idxs) == len(sub_idxs))
                    return numpy.array([i for i,j in zip(src_idxs, sub_idxs)
                                           if j in full_idxs], dtype=idx_arr_type)
                return full_idxs
            else:
                if self._comp and distrib_size is not None:
                    sz = distrib_size
                else:
                    sz = self.scope._var_meta[name]['size']
                return make_idx_array(0, sz)

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

            if not isinstance(self, DriverSystem):
                for name in to_remove:
                    del self.variables[name]

        super(SimpleSystem, self)._create_var_dicts(resid_state_map)

    def setup_scatters(self):
        pass

    def run(self, iterbase, case_label='', case_uuid=None):

        if self.is_active():
            #print "    runsys", str(self.name)
            graph = self.scope._reduced_graph

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

            self._comp.set_itername('%s-%s' % (iterbase, self.name))
            self._comp.run(case_uuid=case_uuid)

            # put component outputs in u vector
            vnames = [n for n in graph.successors(self.name)
                                   if n in self.vector_vars]
            self.vec['u'].set_from_scope(self.scope, vnames)

            if self.complex_step is True:
                self.vec['du'].set_from_scope_complex(self.scope, vnames)

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

            if self._comp is None:
                applyJ(self, variables)
            else:
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
            if self._comp is None:
                applyJT(self, variables)
            else:
                self._comp.applyJT(self, variables)
            vec['df'].array[:] *= -1.0

            for var in self.list_outputs():

                #collapsed = self.scope.name2collapsed.get(var)
                #if collapsed not in variables:
                #    continue

                vec['du'][var][:] += vec['df'][var][:]

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        self.sol_vec.array[:] = self.rhs_vec.array[:]

    def find_system(self, name, recurse_subassy=True):
        """ Return system with given name. """
        if self.name == name:
            return self

        return None


class VarSystem(SimpleSystem):
    """Base class for a System that contains a single variable."""

    def run(self, iterbase, case_label='', case_uuid=None):
        #print "    runsys", str(self.name)
        pass

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        pass

    def applyJ(self, variables):
        pass

    def stop(self):
        pass

    def linearize(self):
        pass

    def get_distrib_idxs(self, name):
        return None


class OutVarSystem(VarSystem):
    def get_distrib_idxs(self, name):
        return SimpleSystem.get_distrib_idxs(self, name)


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

        #print "PARAM SYS",str(self.name),self.vector_vars.keys()

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
        #print "PRE_RUN: %s set to %s" % (self.name, self._get_sys().vec['u'].array)

    def _get_sys(self):
        if self._dup_in_subdriver:
            return self._parent_system
        else:
            return self


class InVarSystem(VarSystem):
    """System wrapper for Assembly input variables (internal perspective)."""

    def run(self, iterbase, case_label='', case_uuid=None):
        if self.is_active():# and self.name in self.vector_vars:
            #print "    runsys", str(self.name)
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
            self.rhs_vec[self.name] += self.sol_vec[self.name]

    def pre_run(self):
        """ Load param value into u vector. """
        #if self.name in self.vector_vars:
        self.vec['u'].set_from_scope(self.scope, [self.name])
        #print "PRE_RUN (invar): %s set to %s" % (self.name, self.vec['u'].array)


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
            collapsed_name = self.scope.name2collapsed[self.name+'.out0']
            state = self._mapped_resids.get(collapsed_name)

            # Propagate residuals.
            if state:
                #print "PROPAGATING RESIDS for %s:  state = %s" % (self.name, state)
                #print "outval = %s" % self._comp.get_flattened_value('out0').real
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
        self._nest_lin_solve = False

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
        self._comp.setup_scatters()

    def set_options(self, mode, options):
        """ Assembly inner system determines its own mode, but we use parents
        mode at this level. Options will be passed when the gradient is
        calculated."""
        self.options = options
        self.mode = mode

        if not self.is_active():
            return

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
        options = self._comp.driver.gradient_options

        # If we can, use the new Jacobian-free method
        if self.options.lin_solver == 'petsc_ksp':
            self._nest_lin_solve = True
            #inner_system.ln_solver = None
            inner_system.set_options(self.mode, options)
            inner_system.initialize_gradient_solver()
            inner_system.linearize()
            return

        # Calculate and save Jacobian for this assy
        inputs = [item.partition('.')[-1] for item in self.list_inputs()]
        outputs = [item.partition('.')[-1] for item in self.list_outputs()]
        self.J = inner_system.calc_gradient(inputs=inputs, outputs=outputs,
                                            options=options)

    def applyJ(self, variables):
        """ df = du - dGdp * dp or du = df and dp = -dGdp^T * df """

        # If we are using scipy-gmres, we are stuck with the old method that
        # calculates a full Jacobian for this assembly.
        if self._nest_lin_solve is False:
            super(AssemblySystem, self).applyJ(variables)
            return

        inner = self._comp._system
        fvec = self.vec['df']
        mode = self.mode
        requested_ins = [item for item in self.list_inputs() \
                         if self.scope.name2collapsed.get(item) in variables]
        requested_outs = [item for item in self.list_outputs() \
                          if self.scope.name2collapsed.get(item) in variables]

        # Clean up any old solves
        inner.rhs_vec.array[:] = 0.0
        inner.sol_vec.array[:] = 0.0

        if mode == 'forward':

            # Copy arg to inner scope
            for name in requested_ins:

                key = name
                parent = self

                while True:
                    parent = parent._parent_system
                    if name in parent.vec['dp']:
                        val = parent.vec['dp'][name]
                        break

                _, _, inner_name = name.partition('.')
                inner.rhs_vec[inner_name] = val

            # Solve inner linear system
            if len(requested_ins) > 0 and len(requested_outs) > 0:
                inner.solve_linear()

            # Copy result to outer scope
            for name in self.list_outputs():

                _, _, inner_name = name.partition('.')
                fvec[name] = inner.sol_vec[inner_name]
                fvec[name][:] -= self.vec['du'][name][:]

        else:

            for name in self.list_outputs():
                val = self.sol_vec[name]
                _, _, inner_name = name.partition('.')
                inner.rhs_vec[inner_name] = -val

            # Solve inner linear system
            if len(requested_ins) > 0:
                inner.solve_linear()

            # Copy result to outer scope
            for name in self.list_inputs():

                key = name
                parent = self

                while True:
                    parent = parent._parent_system
                    if name in parent.vec['dp']:
                        var = parent.vec['dp']
                        break

                _, _, inner_name = name.partition('.')
                var[name] = inner.sol_vec[inner_name]

            for var in self.list_outputs():
                self.vec['du'][var][:] += fvec[var][:]

    def set_complex_step(self, complex_step=False):
        """ Toggles complex_step plumbing for this system and all
        subsystems. Assemblies must prepare their inner system."""

        self.complex_step = complex_step
        self._comp._system.set_complex_step(complex_step)

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        driver = self._comp.driver
        return ISolver.providedBy(self._comp.driver) or \
               driver.__class__.__name__ == 'Driver'

    def find_system(self, name, recurse_subassy=True):
        """ Return system with given name. """

        if self.name == name:
            return self

        if not recurse_subassy:
            return None

        return self._comp._system.find_system(name, recurse_subassy=recurse_subassy)


class CompoundSystem(System):
    """A System that has subsystems."""

    def __init__(self, scope, graph, subg, name=None):
        super(CompoundSystem, self).__init__(scope, graph, subg.nodes(), name)
        self.driver = None
        self.graph = subg
        self._local_subsystems = []  # subsystems in the same process
        self._ordering = ()
        self._grouped_nodes = subg.nodes()

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

    def get_distrib_idxs(self, name):
        all_idxs = []
        for s in self.simple_subsystems():
            idxs = s.get_distrib_idxs(name)
            if idxs is not None:
                all_idxs.extend(idxs)
        return to_idx_array(dedup(all_idxs))

    def _get_scatter_idxs(self, node, noflats, arg_idxs, dest_start, destsys):
        varkeys = self.vector_vars.keys()
        varmeta = self.scope._var_meta

        if node in noflats:
            return (None, None, node)

        elif node in self.vector_vars: # basevar or non-duped subvar
            if node not in self.vec['p'] or self.vec['p'][node].size == 0:
                return (None, None, None)

            sizes = self.local_var_sizes
            isrc = varkeys.index(node)
            offset = numpy.sum(sizes[:, :isrc])
            src_idxs = offset + arg_idxs

            dest_idxs = dest_start + self.vec['p']._info[node].start + \
                        make_idx_array(0, len(arg_idxs))

            if sizes[self.mpi.rank, isrc]:
                dist_dest = destsys.is_distrib_var(node)
                if dist_dest:
                    # don't offset because we already have distrib arg_idxs
                    pass
                elif 'distrib_size' in varmeta[node] and not dist_dest:
                    # don't add offset because we want the whole distrib value.
                    pass
                else:
                    src_idxs += numpy.sum(sizes[:self.mpi.rank, isrc])
                sidxs = [src_idxs]
                didxs = [dest_idxs]
                if self.is_var_in_parallel_system(node):
                     # for reverse scatter, need to scatter to srcs from other ranks
                    for i in range(self.mpi.size):
                        if i == self.mpi.rank:
                            continue
                        # src is duplicated in rank i, so add it
                        # to the reverse scatter
                        if sizes[i, isrc]:
                            sidxs.append(offset + numpy.sum(sizes[:i, isrc]) +
                                         arg_idxs)
                            didxs.append(dest_idxs)


                src_idxs = numpy.concatenate(sidxs)
                dest_idxs = numpy.concatenate(didxs)

            return (src_idxs, dest_idxs, None)

        elif node in self.flat_vars:  # duped subvar
            if node not in self.vec['p']:
                return (None, None, None)
            base = self.scope.name2collapsed[node[0].split('[', 1)[0]]
            if base in self.vec['p']:
                basedests = base[1]
                for comp in destsys._all_comp_nodes():
                    if comp in basedests:
                        return (None, None, None)
            isrc = varkeys.index(base)
            src_idxs = numpy.sum(self.local_var_sizes[:, :isrc]) + \
                          self.scope._var_meta[node]['flat_idx']

            dest_idxs = dest_start + self.vec['p']._info[node].start + \
                          make_idx_array(0, len(self.scope._var_meta[node]['flat_idx']))
            return (src_idxs, dest_idxs, None)

        return (None, None, None)

    def setup_scatters(self):
        """ Defines scatters for args at this system's level """
        if not self.is_active():
            return
        input_sizes = self.input_sizes
        rank = self.mpi.rank
        varmeta = self.scope._var_meta

        if MPI:
            self.app_ordering = self.create_app_ordering()

        src_full = []
        dest_full = []
        src_rev_full = []
        dest_rev_full = []
        dest_rev = []
        src_rev = []
        scatter_conns_rev_full = set()
        scatter_conns_full = set()
        noflat_conns_full = set()
        noflats = set([k for k,v in self.variables.items()
                           if v.get('noflat')])
        noflats.update([v for v in self._in_nodes if varmeta[v].get('noflat')])

        dest_start = numpy.sum(input_sizes[:rank])

        #print "SCATTERS for %s" % str(self.name)
        #print "vars:",self.vector_vars.keys()
        for subsystem in self.all_subsystems():
            src_partial = []
            dest_partial = []
            src_rev_partial = []
            dest_rev_partial = []
            scatter_conns = set()
            scatter_conns_rev = set()
            noflat_conns = set()  # non-flattenable vars
            for sub in subsystem.simple_subsystems():
                for node in self.variables:
                    if node not in sub._in_nodes or node in scatter_conns:
                        continue
                    arg_idxs = sub.get_distrib_idxs(node)
                    src_idxs, dest_idxs, nflat = self._get_scatter_idxs(node, noflats,
                                                                        arg_idxs, dest_start,
                                                                        destsys=subsystem)
                    if (arg_idxs is None) and (src_idxs is None) and (dest_idxs is None) and (nflat is None):
                        continue

                    if nflat:
                        if node in noflat_conns or node not in subsystem._in_nodes or node not in self._owned_args:
                            continue
                        noflat_conns.add(node)
                    else:
                        if src_idxs is None:
                            src_idxs = make_idx_array(0, 0)
                        if dest_idxs is None:
                            dest_idxs = make_idx_array(0, 0)

                        src_partial.append(src_idxs)
                        dest_partial.append(dest_idxs)

                        if node in self.vec['u'] or \
                           (node in self.vec['p'] and self.is_var_in_parallel_system(node)):
                            sidxs = src_idxs
                            didxs = dest_idxs
                        else:
                            sidxs = make_idx_array(0, 0)
                            didxs = make_idx_array(0, 0)

                        src_rev_partial.append(sidxs)
                        dest_rev_partial.append(didxs)
                        scatter_conns_rev.add(node)

                        #print "U node size for %s: %s" % (str(node), self.variables[node]['size'])
                        if node not in scatter_conns_rev_full:
                            src_rev_full.append(sidxs)
                            dest_rev_full.append(didxs)
                            scatter_conns_rev_full.add(node)

                        if node not in scatter_conns_full:
                            src_full.append(src_idxs)
                            dest_full.append(dest_idxs)

                    scatter_conns.add(node)
                    scatter_conns_full.add(node)

            if MPI or scatter_conns or noflat_conns:
                #print "PARTIAL %s --> %s: %s --> %s" % (self.name, subsystem.name, idx_merge(src_partial),
                #                                        idx_merge(dest_partial)); sys.stdout.flush()
                subsystem.scatter_partial = DataTransfer(self, src_partial,
                                                         dest_partial,
                                                         scatter_conns, noflat_conns)

            # special partial reverse scatter for adjoint linearGS
            if scatter_conns_rev:
                #print "PARTIAL rev %s --> %s: %s --> %s" % (self.name, subsystem.name, idx_merge(src_rev_partial),
                #                                        idx_merge(dest_rev_partial)); sys.stdout.flush()
                subsystem.scatter_partial_rev = DataTransfer(self, src_rev_partial,
                                                             dest_rev_partial,
                                                             scatter_conns_rev, set())

        if MPI or scatter_conns_full or noflat_conns_full:
            self.scatter_full = DataTransfer(self, src_full, dest_full,
                                             scatter_conns_full, noflat_conns_full)

        if scatter_conns_rev_full:
            self.scatter_rev_full = DataTransfer(self, src_rev_full, dest_rev_full,
                                                 scatter_conns_rev_full, [])

        for sub in self.local_subsystems():
            sub.setup_scatters()

    def applyJ(self, variables):
        """ Delegate to subsystems """

        if self.is_active():
            if self.mode == 'forward':
                self.scatter('du', 'dp')
            for subsystem in self.local_subsystems():
                subsystem.applyJ(variables)
            if self.mode == 'adjoint':
                self.scatter('du', 'dp')

    def stop(self):
        self._stop = True
        for s in self.all_subsystems():
            s.stop()

    def _apply_deriv(self, arg, result):
        """Support for directional derivatives, where this function is used
        to perform a matrix vector product.
        """
        self.dfd_solver.calculate(arg, result)

    def is_variable_local(self, name):
        """Returns True if the variable in name is local to this process,
        otherwise it returns False. If name can't be found, then an exception
        is raised."""

        # Regular paths, get the compname
        cname = name.split('.')[0]

        # If name is a Variable Tree, then it belongs to our containing
        # assembly, which must be local.
        if cname in self.scope.list_vars():
            return True

        system = self.scope._system.find_system(cname, recurse_subassy=False)

        if system:
            return system.is_active()

        msg = 'Cannot find a system that contains varpath %s' % name
        raise RuntimeError(msg)

    def find_system(self, name, recurse_subassy=True):
        """ Return system with given name.

        name: string (OpenMDAO varpath)
            Name of system you want to find.

        recurse_subassy: Bool
            Set to True to search beyond subassembly boundaries. Default
            is True.
        """

        if self.name == name:
            return self

        for sub in self.subsystems():
            found = sub.find_system(name, recurse_subassy=recurse_subassy)
            if found:
                return found

        return None

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

        self._ordering.extend([n for n in self.graph.nodes_iter()
                                 if n not in self._ordering])

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
            #print "    runsys", str(self.name)
            self._stop = False

            for sub in self.local_subsystems():
                #print "PRE - scatter for %s" % self.name
                #self.dump_vars()
                self.scatter('u', 'p', sub)
                #print "POST - scatter for %s" % self.name
                #self.dump_vars()

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

        if isinstance(self._parent_system, TransparentDriverSystem) and \
           self._parent_system._comp.name != 'driver':
            self.mpi.comm = comm
        else:
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
        # don't scatter unless we contain something that's actually
        # going to run
        if not self.local_subsystems() or not self.is_active():
            return

        #print "    runsys", str(self.name)

        #print "PRE - scatter for %s" % self.name
        #self.dump_vars()
        self.scatter('u', 'p')
        #print "POST - scatter for %s" % self.name
        #self.dump_vars()

        for sub in self.local_subsystems():
            sub.run(iterbase, case_label=case_label, case_uuid=case_uuid)

    def evaluate(self, iterbase, case_label='', case_uuid=None):
        """ Evalutes a component's residuals without invoking its
        internal solve (for implicit comps.)
        """
        if not self.local_subsystems() or not self.is_active():
            return

        self.scatter('u', 'p')

        for sub in self.local_subsystems():
            sub.evaluate(iterbase, case_label=case_label, case_uuid=case_uuid)

    def setup_communicators(self, comm):
        self.mpi.comm = comm
        size = comm.size
        rank = comm.rank

        subsystems = []
        requested_procs = []
        for system in self.all_subsystems():
            subsystems.append(system)
            cpus = system.get_req_cpus()
            if cpus < 1:
                cpus = 1
            requested_procs.append(cpus)

        assigned_procs = [0]*len(requested_procs)

        assigned = 0

        requested = sum(requested_procs)

        limit = min(size, requested)

        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        if requested:
            while assigned < limit:
                for i, system in enumerate(subsystems):
                    if assigned_procs[i] < requested_procs[i]:
                        assigned_procs[i] += 1
                        assigned += 1
                        if assigned == limit:
                            break

        self._local_subsystems = []

        for i,sub in enumerate(subsystems):
            if requested_procs[i] > 0 and assigned_procs[i] == 0:
                raise RuntimeError("subsystem group %s requested %d processors but got 0" %
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

        for sub in self.local_subsystems():
            sub._parent_system = self
            sub.setup_communicators(sub_comm)

    def setup_variables(self, resid_state_map=None):
        """ Determine variables from local subsystems """
        varmeta = self.scope._var_meta
        self.variables = OrderedDict()
        if not self.is_active():
            return

        variables = {}
        for sub in self.local_subsystems():
            if not isinstance(sub, ParamSystem):
                sub.setup_variables(resid_state_map)
                variables.update(sub.variables)

        for sub in self.local_subsystems():
            if isinstance(sub, ParamSystem):
                sub.setup_variables(variables, resid_state_map)

        if self.local_subsystems():
            sub = self.local_subsystems()[0]
            names = sub.variables.keys()
        else:
            sub = None
            names = []

        varkeys_list = self.mpi.comm.allgather(names)

        for rank, varkeys in enumerate(varkeys_list):
            for name in varkeys:
                self.variables[name] = varmeta[name].copy()
                if rank != self.mpi.rank:
                    self.variables[name]['size'] = 0

        self._create_var_dicts(resid_state_map)

    def set_ordering(self, ordering, opaque_map):
        """Return the execution order of our subsystems."""
        for s in self.all_subsystems():
            s.set_ordering(ordering, opaque_map)

    def is_var_in_parallel_system(self, varname):
        """ Returns True if a full varnode name is an input under a parallel
        system. """

        if varname in self._in_nodes:
            return True

        for system in self.all_subsystems():
            if system.is_var_in_parallel_system(varname):
                return True

        return False

class OpaqueSystem(SimpleSystem):
    """A system with an external interface like that
    of a simple system, but encapsulating a compound
    system.
    """
    def __init__(self, scope, pgraph, subg, name):
        nodes = sorted(subg.nodes())

        # take the graph we're given, collapse our nodes into a single
        # node, and create a simple system for that
        ograph = pgraph.subgraph(pgraph.nodes_iter())
        full = get_full_nodeset(scope, nodes)
        int_nodes = ograph.internal_nodes(full, shared=False)
        shared_int_nodes = ograph.internal_nodes(full, shared=True)
        #ograph.add_node(tuple(nodes), comp='opaque')
        collapse_nodes(ograph, tuple(nodes), int_nodes)

        super(OpaqueSystem, self).__init__(scope, ograph, tuple(nodes))

        graph = pgraph.subgraph(shared_int_nodes)

        dests = set()
        nodeset = set()
        internal_comps = set()
        subdrivers = []
        for n in nodes:
            obj = getattr(scope, n, None)
            if obj is not None:
                if has_interface(obj, IDriver):
                    internal_comps.update([c.name for c in obj.iteration_set()])
                    subdrivers.append(obj)
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

        graph.collapse_subdrivers([], subdrivers)

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

                cname, _, vname = dest.partition('.')
                if vname and cname in graph and 'comp' in graph.node[cname] and not graph.has_edge(node, cname):
                    graph.add_edge(node, cname)

            if dest in graph:
                graph.node[dest]['system'] = _create_simple_sys(scope, graph, dest)

        self._inner_system = SerialSystem(scope, graph,
                                          graph.component_graph(),
                                          name="FD_" + str(name))

        self._inner_system._provideJ_bounds = None
        self._comp = None

    def inner(self):
        return self._inner_system

    def _all_comp_nodes(self, local=False):
        return self._inner_system._all_comp_nodes(local=local)

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

        if self._inner_system.is_active():
            # Preload all inputs and outputs along to our inner system.
            # This was needed for the case where you regenerate the system
            # hierarchy on the first calc_gradient call.
            inner_u = self._inner_system.vec['u']
            inner_u.set_from_scope(self.scope)

    def get_distrib_idxs(self, name):
        """These indices are actually the indices in the *source*
        vector that get scattered to a particular input.
        """
        return self._inner_system.get_distrib_idxs(name)

    def setup_scatters(self):
        self._inner_system.setup_scatters()

    def set_options(self, mode, options):
        """ Sets all user-configurable options for the inner_system and its
        children.
        """
        super(OpaqueSystem, self).set_options(mode, options)
        self._inner_system.set_options(mode, options)

    def pre_run(self):
        self._inner_system.pre_run()

    def run(self, iterbase, case_label='', case_uuid=None):
        if not self.is_active() or not self._inner_system.is_active():
            return

        #print "    runsys", str(self.name)
        self_u = self.vec['u']
        self_du = self.vec['du']
        inner_u = self._inner_system.vec['u']
        inner_du = self._inner_system.vec['du']

        # Make sure the inner_vector is initialized with values from the
        # scope. Only need to do the inputs that span the outer and inner
        # vectors.
        bnames = self.list_inputs() + \
                 self.list_states()
        inner_u.set_from_scope(self.scope, bnames)
        if self.complex_step is True:
            inner_du.set_from_scope_complex(self.scope, bnames)

        self._inner_system.run(iterbase, case_label=case_label, case_uuid=case_uuid)

        for name, val in inner_u.items():
            if name in self_u:
                self_u[name][:] = val
                if self.complex_step is True:
                    self_du[name][:] = inner_du[name]

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

    def set_ordering(self, ordering, opaque_map):
        self._inner_system.set_ordering(ordering, {})

    def get_req_cpus(self):
        return self._inner_system.get_req_cpus()

    def find_system(self, name, recurse_subassy=True):
        """ Return system with given name. """

        if self.name == name:
            return self
        return self._inner_system.find_system(name, recurse_subassy=recurse_subassy)


class DriverSystem(SimpleSystem):
    """Base System class for all Drivers."""

    def __init__(self, graph, driver):
        scope = driver.parent
        super(DriverSystem, self).__init__(scope, graph, driver.name)
        driver._system = self

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

    def setup_variables(self, resid_state_map=None):
        super(DriverSystem, self).setup_variables(resid_state_map)
        # calculate relevant vars for GMRES mult
        varmeta = self.scope._var_meta
        vnames = set(self.flat_vars.keys())
        g = self._comp.get_reduced_graph()
        vnames.update([n for n,data in g.nodes_iter(data=True)
                           if 'comp' not in data and not varmeta[n].get('noflat')])
        self._relevant_vars = vnames

        # if a state is 'stolen' by the system that owns
        # the corresponding residual, we can get a lag when we have parallel
        # systems depending on execution order (of the parent SerialSystem), so
        # to ensure the U vector is up-to-date, we force a full scatter
        # just before the workflow runs.
        if MPI and resid_state_map:
            # TODO: make this check smarter to avoid doing the prescatter
            #       unless we really have to.
            self._comp.workflow._need_prescatter = True

    def get_distrib_idxs(self, name):
        """These indices are actually the indices in the *source*
        vector that get scattered to a particular input.
        """
        idx = super(DriverSystem, self).get_distrib_idxs(name)
        if idx is None:
            idx = self._comp.workflow._system.get_distrib_idxs(name)
        return idx

    def setup_scatters(self):
        self._comp.setup_scatters()

    def find_system(self, name, recurse_subassy=True):
        """ Return system with given name. """

        if self.name == name:
            return self

        for sub in self.all_subsystems():
            found = sub.find_system(name, recurse_subassy=recurse_subassy)
            if found:
                return found

        return None

    def get_req_cpus(self):
        return self._comp.workflow._system.get_req_cpus()


class FiniteDiffDriverSystem(DriverSystem):
    """A System for a Driver component that is not a Solver."""

    def is_differentiable(self):
        """Return True if analytical derivatives can be
        computed for this System.
        """
        return False


class TransparentDriverSystem(DriverSystem):
    """A system for an driver that allows derivative calculation across its
    boundary. At present this system will only appear for the Driver base class."""

    def _get_resid_state_map(self):
        """ Essentially, this system behaves like a solver system, except it
        has no states or residuals.
        """
        return {}

    def setup_variables(self, resid_state_map=None):
        # pass our resid_state_map to our children
        local_resid_map = self._get_resid_state_map()
        if local_resid_map is None or resid_state_map is None:
            resid_state_map = local_resid_map
        else:
            for key, value in local_resid_map.iteritems():
                resid_state_map[key] = value
        super(TransparentDriverSystem, self).setup_variables(resid_state_map)

    def setup_communicators(self, comm):
        """ Special case if Driver is not base driver """

        if self._comp.name == 'driver':
            super(TransparentDriverSystem, self).setup_communicators(comm)
        else:
            self.mpi.comm = comm
            self._comp.workflow._system.setup_communicators(comm)
            self._comp.workflow.mpi.comm = comm

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

        if self.is_active():
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

    def solve_linear(self, options=None):
        """ Single linear solve solution applied to whatever input is sitting
        in the RHS vector."""

        # Apply to inner driver system only. No need to pass options since it
        # has its own.
        for sub in self.local_subsystems():
            sub.solve_linear()


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

        sub_options = self._comp.gradient_options
        for sub in self.subsystems():
            sub.solve_linear(sub_options)


def _create_simple_sys(scope, graph, name):
    """Given a Component or Variable node, create the
    appropriate type of simple System.
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

    No nested parallel systems will result from this algorithm.

    """
    if len(cgraph) < 2:
        return cgraph

    gcopy = cgraph.subgraph(cgraph.nodes_iter())

    parnodes = []

    while len(gcopy) > 1:
        # find all nodes with in degree 0. If we find
        # more than one, we can execute them in parallel
        zero_in_nodes = [n for n in gcopy.nodes_iter()
                            if not gcopy.in_degree(n)]

        if len(zero_in_nodes) > 1: # start of parallel chunk
            for z in zero_in_nodes:
                if isinstance(z, basestring) and ('.' in z or isinstance(scope.get(z), PseudoComponent)):
                    gcopy.remove_node(z)

            zero_in_nodes = [z for z in zero_in_nodes
                                if z in gcopy]

            if len(zero_in_nodes) <= 1:
                gcopy.remove_nodes_from(zero_in_nodes)
                continue

            parnodes.append(zero_in_nodes)

        elif len(zero_in_nodes) == 0:  # circular - no further splitting
            break

        gcopy.remove_nodes_from(zero_in_nodes)

    for nodes in parnodes:
        subg = cgraph.subgraph(nodes)
        psys = ParallelSystem(scope, graph, subg, tuple(sorted(subg.nodes())))
        collapse_to_system_node(cgraph, psys, tuple(sorted(subg.nodes())))#tuple(psys._nodes))

    return cgraph

# def partition_subsystems(scope, graph, cgraph):
#     """Return a nested system graph with metadata for parallel
#     and serial subworkflows.  Graph must acyclic. All subdriver
#     iterations sets must have already been collapsed.
#
#     This algorithm frequently results in nested parallel systems.
#
#     """
#     if len(cgraph) < 2:
#         return cgraph
#
#     gcopy = cgraph.subgraph(cgraph.nodes_iter())
#
#     to_remove = []
#
#     while len(gcopy) > 1:
#         # find all nodes with in degree 0. If we find
#         # more than one, we can execute them in parallel
#         zero_in_nodes = [n for n in gcopy.nodes_iter()
#                             if not gcopy.in_degree(n)]
#
#         if len(zero_in_nodes) > 1: # start of parallel chunk
##
#             parallel_group = []
#             for node in zero_in_nodes:
#                 brnodes = sorted(get_branch(gcopy, node))
#                 if len(brnodes) > 1:
#                     parallel_group.append(tuple(brnodes))
#                 else:
#                     parallel_group.append(brnodes[0])
#
#             for branch in parallel_group:
#                 if 'system' in gcopy.node.get(branch,()):
#                     gcopy.remove_node(branch)
#                 else: # multiple nodes
#                     branch = tuple(branch)
#                     to_remove.extend(branch)
#                     subg = cgraph.subgraph(branch)
#                     partition_subsystems(scope, graph, subg)
#                     system=SerialSystem(scope, graph, subg, tuple(sorted(subg.nodes())))
#                     collapse_to_system_node(cgraph, system, branch)
#
#                     gcopy.remove_nodes_from(branch)
#
#             parallel_group = tuple(sorted(parallel_group))
#             to_remove.extend(parallel_group)
#             subg = cgraph.subgraph(parallel_group)
#             system=ParallelSystem(scope, graph, subg, tuple(sorted(subg.nodes())))
#             collapse_to_system_node(cgraph, system, parallel_group)
#
#         elif len(zero_in_nodes) == 1:  # serial
#             gcopy.remove_nodes_from(zero_in_nodes)
#         else: # circular - no further splitting
#             break
#
#     # Now remove all of the old nodes
#     cgraph.remove_nodes_from(to_remove)
#
#     return cgraph

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
    if req == comm.size:
        return comm

    if comm.rank+1 > req:
        color = MPI.UNDEFINED
    else:
        color = 1

    return comm.Split(color)

def get_full_nodeset(scope, group):
    names = set()
    for name in simple_node_iter(group):
        obj = getattr(scope, name, None)
        if hasattr(obj, 'get_full_nodeset'):
            names.update(obj.get_full_nodeset())
        else:
            names.add(name)
    return names
