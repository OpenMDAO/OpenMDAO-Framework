import os
import sys

from openmdao.main.interfaces import obj_has_interface, IAssembly

def under_mpirun():
    """Return True if we're being executed under mpirun."""
    # TODO: this is a bit of a hack and there appears to be
    # no consistent set of environment vars between MPI implementations
    for name in os.environ.keys():
        if name.startswith('OMPI_COMM') or name.startswith('MPICH_'):
            from mpi4py import MPI
            if MPI.COMM_WORLD.rank == 0:
                print "running under mpirun"
            return True
    print "NOT running under mpirun"
    return False

if under_mpirun():
    from mpi4py import MPI
    from petsc4py import PETSc
    import traceback

    COMM_NULL = MPI.COMM_NULL

    def MPI_run(top):
        """Run a parallel version of the top object. comp_map
        is a dict that maps component names (full pathname) to processes.
        """
        try:
            _setup_mpi(top)
            return top.run()
        except Exception:
            mpiprint(traceback.format_exc())

    def get_petsc_vec(comm, arr):
        return PETSc.Vec().createWithArray(arr, comm=comm) 

    def mpiprint(msg, rank=-1):
        if rank < 0 or rank == MPI.COMM_WORLD.rank:
            for part in str(msg).split('\n'):
                print "{%d} %s" % (MPI.COMM_WORLD.rank, part)
            sys.stdout.flush()
else:
    MPI = None
    PETSc = None
    COMM_NULL = None
    
    def MPI_run(top):
        return top.run()

    def get_petsc_vec(comm, arr):
        return None

    def mpiprint(msg, rank=-1):
        print msg
        
class MPI_info(object):
    def __init__(self):
        self.requested_cpus = 0  # requested number of processors.
                                 # 0 means it's duplicated across 
                                 # all processors
        self.cpus = 0  # actual number of CPUs assigned. 

        # the MPI communicator used by this comp and its children
        self.comm = COMM_NULL
      
def _setup_mpi(obj):
    """This is called on the top Assembly in the hierarchy."""

    if not obj_has_interface(obj, IAssembly):
        raise RuntimeError("object passed to setup_mpi does not have "
                           "the IAssembly interface.")

    obj.setup_communicators(MPI.COMM_WORLD)
    obj.setup_sizes()
    obj.setup_vectors()
    #obj.setup_scatters()
