""" Base class for all workflows. """

from openmdao.main.exceptions import RunStopped
from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.pseudocomp import PseudoComponent

class JacobiWorkflow(SequentialWorkflow):
    """
    A Workflow where all components are intended to be executed in
    parallel.
    """
    def __init__(self, parent=None, scope=None, members=None):
        super(JacobiWorkflow, self).__init__(parent, scope, members)
        self.local_comps = []

    def run(self, ffd_order=0, case_id=''):
        """ Run the Components in this Workflow in parallel. """

        self._stop = False
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)

        for comp in self.local_comps:
            if isinstance(comp, PseudoComponent):
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                self._comp_count += 1
                comp.set_itername('%s-%d' % (iterbase, self._comp_count))
                comp.run(ffd_order=ffd_order, case_id=case_id)
            if self._stop:
                raise RunStopped('Stop requested')

    def setup_communicators(self):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.local_comps = []

        comm = self.mpi.comm

        size = comm.size
        child_comps = [c for c in self]
        
        cpus = [c.get_cpu_range() for c in child_comps]
        requested_procs = [c[0] for c in cpus]
        assigned_procs = [0 for c in cpus]
        max_procs = [c[1] for c in cpus]

        # if get_max_cpus() returns None, it means that comp can use
        # as many cpus as we can give it
        if None in max_procs:
            max_usable = size
        else:
            max_usable = sum(max_procs)

        assigned = 0 #sum(assigned_procs)
        #unassigned = size - assigned
        # if unassigned < 0:
        #     raise RuntimeError("Allocated CPUs is short by %d" % -unassigned)

        requested = sum(requested_procs)
        limit = min(size, requested)

        # first, just use simple round robin assignment of requested CPUs
        # until everybody has what they asked for or we run out
        while assigned < limit:
            for i, comp in enumerate(child_comps):
                if requested_procs[i] == 0: # skip and deal with these later
                    continue
                if assigned_procs[i] < requested_procs[i]:
                    assigned_procs[i] += 1
                    assigned += 1
                    if assigned == limit:
                        break

        # now, if we have any procs left after assigning all the requested 
        # ones, allocate any remaining ones to any comp that can use them
        limit = min(size, max_usable)
        while assigned < limit:
            for i, comp in enumerate(child_comps):
                if requested_procs[i] == 0: # skip and deal with these later
                    continue
                if max_procs[i] is None or assigned_procs[i] < max_procs[i]:
                    assigned_procs[i] += 1
                    assigned += 1
                    if assigned == limit:
                        break

        color = []
        for i, assigned in enumerate([p for p in assigned_procs if p != 0]):
            color.extend([i]*assigned)

        # if max_usable < size:
        #     color.extend([MPI.UNDEFINED]*(size-max_usable))

        rank = self.mpi.comm.rank
        sub_comm = comm.Split(color[rank])

        # if sub_comm == MPI.COMM_NULL:
        #     pass #print "null comm in rank %d" % self.mpi.comm.rank
        # else:
        #     #print "comm size = %d in rank %d" % (sub_comm.size, self.mpi.comm.rank)

        rank_color = color[rank]
        for i,c in enumerate(child_comps):
            if i == rank_color:
                c.mpi.comm = sub_comm
                c.mpi.cpus = assigned_procs[i]
                self.local_comps.append(c)
            elif assigned_procs[i] == 0:
                c.mpi.comm = sub_comm  # TODO: make sure this is the right comm
                self.local_comps.append(c)

        for comp in self.local_comps:
            if hasattr(c, 'setup_communicators'):
                c.setup_communicators()

        # # now set up synchronization comms for all Drivers and Assemblies
        # # so that the iteration order matches in all processes
        # for comp in child_comps:
        #     if has_interface(comp, IDriver) or has_interface(comp, IAssembly):
        #         comp.mpi.copy_comm = comm.Dup()

