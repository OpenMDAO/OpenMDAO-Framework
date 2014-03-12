
import sys

from openmdao.main.assembly import Assembly
from openmdao.main.rbac import rbac

class DistribAssembly(Assembly):
    """An Assembly with distributed components. Data passing uses
    MPI and PETSc.
    """

    def __init__(self):
        super(DistribAssembly, self).__init__()

    def _connect(self, src, dest):
        super(DistribAssembly, self)._connect(src, dest)

    def disconnect(self, varpath, varpath2=None):
        super(DistribAssembly, self).disconnect(varpath, varpath2)

    def config_changed(self, update_parent=True):
        super(DistribAssembly, self).config_changed(update_parent)

    # def execute(self):
    #     """Runs driver and updates our boundary variables."""
    #     self.driver.run(ffd_order=self.ffd_order,
    #                     case_id=self._case_id)
    #     self._depgraph.update_boundary_outputs(self)

    # @rbac(('owner', 'user'))
    # def update_inputs(self, compname):
    #     """Transfer input data to connected input variables on 
    #     the specified component.
    #     """
    #     invalid_ins = self._depgraph.list_inputs(compname,
    #                                              invalid=True)
    #     if invalid_ins:
    #         self._update_invalid_dests(compname, invalid_ins)

    # def update_outputs(self, outnames):
    #     """Execute any necessary internal or predecessor
    #     components in order to make the specified output
    #     variables valid.
    #     """
    #     data = self._depgraph.node
    #     invalid_dests = [n for n in outnames
    #                        if data[n]['valid'] is False]
    #     if invalid_dests:
    #         self._update_invalid_dests(None, invalid_dests)

    def _update_invalid_dests(self, startcomp, invalid_dests):
        graph = self._depgraph
        invalids = set()
        for inv_dest in invalid_dests:
            invalids.update([s for s in graph.get_sources(inv_dest)
                                if not graph.node[s]['valid']])

        # if source vars are invalid, request an update
        if invalids:
            loops = graph.get_loops()

            for cname, vnames in partition_names_by_comp(invalids).items():
                if cname is None or not is_comp_node(graph, cname): # boundary var
                    if self.parent:
                        self.parent.update_inputs(self.name)

                # If our start component is in a loop with us, don't
                # run it. Otherwise you have infinite recursion. It is
                # the responsibility of the solver to properly execute
                # the comps in its loop.
                elif loops:
                    for loop in loops:
                        if startcomp in loop and cname in loop:
                            break
                    else:
                        getattr(self, cname).update_outputs(vnames)
                else:
                    getattr(self, cname).update_outputs(vnames)

        try:
            for inv_dest in invalid_dests:
                self._depgraph.update_destvar(self, inv_dest)
        except Exception as err:
            self.raise_exception(str(err), type(err))

    def _input_updated(self, name, fullpath=None):
        outs = self.invalidate_deps([name])
        if self.parent:
            outs.add(name)
            self.parent.child_invalidated(self.name, outs)

    ## Distributed computing methods ##

    def setup_communicators(self):
        """Allocate communicators from here down to all of our
        child Components.
        """
        comm = self.mpi.comm
        if comm == MPI.COMM_NULL:
            return

        size = comm.size
        child_comps = self.get_comps()
        
        cpus = [c.get_cpu_range() for c in child_comps]
        assigned_procs = [c[0] for c in cpus]
        max_procs = [c[1] for c in cpus]

        # if get_max_cpus() returns None, it means that comp can use
        # as many cpus as we can give it
        if None in max_procs:
            max_usable = size
        else:
            max_usable = sum(max_procs)

        assigned = sum(assigned_procs)
        unassigned = size - assigned
        if unassigned < 0:
            raise RuntimeError("Allocated CPUs is short by %d" % -unassigned)

        limit = min(size, max_usable)

        # for now, just use simple round robin assignment of extra CPUs
        # until everybody is at their max or we run out of available CPUs
        while assigned < limit:
            for i, comp in enumerate(child_comps):
                if assigned_procs[i] == 0: # skip and deal with these later
                    continue
                if max_procs[i] is None or assigned_procs[i] != max_procs[i]:
                    assigned_procs[i] += 1
                    assigned += 1
                    if assigned == limit:
                        break

        color = []
        for i, assigned in enumerate([a for a in assigned_procs if a != 0]):
            color.extend([i]*assigned)

        if max_usable < size:
            color.extend([MPI.UNDEFINED]*(size-max_usable))

        rank = self.mpi.comm.rank
        sub_comm = comm.Split(color[rank])

        if sub_comm == MPI.COMM_NULL:
            pass #print "null comm in rank %d" % self.mpi.comm.rank
        else:
            #print "comm size = %d in rank %d" % (sub_comm.size, self.mpi.comm.rank)

            rank_color = color[rank]
            for i,c in enumerate(child_comps):
                if i == rank_color:
                    c.mpi.comm = sub_comm
                    if hasattr(c, 'setup_communicators'):
                        c.setup_communicators()
                elif assigned_procs[i] == 0:
                    c.mpi.comm = sub_comm

        # # now set up synchronization comms for all Drivers and Assemblies
        # # so that the iteration order matches in all processes
        # for comp in child_comps:
        #     if has_interface(comp, IDriver) or has_interface(comp, IAssembly):
        #         comp.mpi.copy_comm = comm.Dup()

    def calc_var_sizes(self, nameset=None):
        """Returns a sorted vector of tuples of the form:
        (full_var_pathname, flattened_size).  nameset is the set
        of names that sizes are required for.
        """
        names = super(Assembly, self).calc_var_sizes(nameset)

        # get a list of all vars referenced by parameters,
        # objectives, and constraints
        nset, destset = self.driver.get_expr_var_depends(recurse=True)
        nset.update(destset)

        # now add all vars that are connected
        conns = self._depgraph.list_connections()
        nset.update([u.split('[',1)[0] for u,v in conns])
        nset.update([v.split('[',1)[0] for u,v in conns])
        
        for cname, vnames in partition_names_by_comp(nset).items():
            if cname is None:
                continue
            comp = getattr(self, cname)
            names.extend(comp.calc_var_sizes(set(vnames)))
    
        return names

        

def dump_iteration_tree(obj, f=sys.stdout, full=True, tabsize=4, derivs=False):
    """Returns a text version of the iteration tree
    of an OpenMDAO object.  The tree shows which are being 
    iterated over by which drivers.

    If full is True, show pseudocomponents as well.
    If derivs is True, include derivative input/output 
    information.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        tab = ' ' * tablevel
        if is_instance(obj, Driver):
            f.write("%s%s\n" % (tab, obj.name))
            if derivs:
                try:
                    dgraph = obj.workflow.derivative_graph()
                except Exception as err:
                    f.write("%s*ERR in deriv graph: %s\n" % (' '*(tablevel+tabsize+2), str(err)))
                else:
                    inputs = dgraph.graph.get('mapped_inputs', dgraph.graph.get('inputs', []))
                    outputs = dgraph.graph.get('mapped_outputs', dgraph.graph.get('outputs', []))
                    f.write("%s*deriv inputs: %s\n" %(' '*(tablevel+tabsize+2), inputs))
                    f.write("%s*deriv outputs: %s\n" %(' '*(tablevel+tabsize+2), outputs))
            names = set(obj.workflow.get_names())
            for comp in obj.workflow:
                if not full and comp.name not in names:
                    continue
                if is_instance(comp, Driver) or is_instance(comp, Assembly):
                    _dump_iteration_tree(comp, f, tablevel + tabsize)
                elif is_instance(comp, PseudoComponent):
                    f.write("%s%s  (%s)\n" %
                        (' ' * (tablevel+tabsize), comp.name, comp._orig_expr))
                else:
                    f.write("%s%s\n" % (' ' * (tablevel+tabsize), comp.name))
        elif is_instance(obj, Assembly):
            f.write("%s%s\n" % (tab, obj.name))
            _dump_iteration_tree(obj.driver, f, tablevel + tabsize)

    _dump_iteration_tree(obj, f, 0)
