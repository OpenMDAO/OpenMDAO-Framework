import os
import sys

from openmdao.main.interfaces import IAssembly

# if this has a rank value, then all mpiprints, unless explicitly
# specified in the call, will print results from that rank only
MPI_PRINT_RANK = None

def set_print_rank(rank):
    global MPI_PRINT_RANK
    MPI_PRINT_RANK = rank

def under_mpirun():
    """Return True if we're being executed under mpirun."""
    # TODO: this is a bit of a hack and there appears to be
    # no consistent set of environment vars between MPI implementations
    for name in os.environ.keys():
        if name.startswith('OMPI_COMM') or name.startswith('MPICH_'):
            return True
    return False

if under_mpirun():
    from mpi4py import MPI
    from petsc4py import PETSc
    import traceback

    COMM_NULL = MPI.COMM_NULL

    def MPI_run(top):
        """Run a parallel version of the top object.
        """
        try:
            _setup_mpi(top)
            return top.run()
        except Exception:
            mpiprint(traceback.format_exc())

    def create_petsc_vec(comm, arr):
        return PETSc.Vec().createWithArray(arr, comm=comm) 

    def mpiprint(msg, rank=-1):
        if rank < 0:
            if MPI_PRINT_RANK is not None and MPI_PRINT_RANK != MPI.COMM_WORLD.rank:
                return
        elif rank != MPI.COMM_WORLD.rank:
            return

        for part in str(msg).split('\n'):
            print "{%d} %s" % (MPI.COMM_WORLD.rank, part)
        sys.stdout.flush()
else:
    MPI = None
    PETSc = None
    COMM_NULL = None
    
    def MPI_run(top):
        return top.run()

    def create_petsc_vec(comm, arr):
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
      
def setup_mpi(obj):
    """This is called on the top Assembly in the hierarchy."""

    try:
        return _setup_mpi(obj)
    except Exception:
        mpiprint(traceback.format_exc())

def _setup_mpi(obj):
    if not IAssembly.providedBy(obj):
        raise RuntimeError("object passed to setup_mpi does not have "
                           "the IAssembly interface.")

    MPI.COMM_WORLD.Set_errhandler(MPI.ERRORS_ARE_FATAL)

    obj.setup_communicators(MPI.COMM_WORLD)
    obj.setup_variables()
    obj.setup_sizes()
    obj.setup_vectors()
    obj.setup_scatters()
