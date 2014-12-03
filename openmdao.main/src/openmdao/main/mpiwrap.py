import os
import sys

# if this has a rank value, then all mpiprints, unless explicitly
# specified in the call, will print results from that rank only
MPI_PRINT_RANK = None

MPI_STREAM = sys.stdout

def use_proc_files():
    global MPI_STREAM
    if MPI is None:
        rank = 'non_mpi'
    else:
        rank = MPI.COMM_WORLD.rank
    sname = "%s.out" % rank
    MPI_STREAM = open(sname, 'w')


def set_print_rank(rank):
    global MPI_PRINT_RANK
    MPI_PRINT_RANK = rank

def _under_mpirun():
    """Return True if we're being executed under mpirun."""
    # TODO: this is a bit of a hack and there appears to be
    # no consistent set of environment vars between MPI 
    # implementations.
    for name in os.environ.keys():
        if name.startswith('OMPI_COMM') or name.startswith('MPICH_'):
            return True
    return False


if _under_mpirun():
    from mpi4py import MPI
    from petsc4py import PETSc

    COMM_NULL = MPI.COMM_NULL

    def create_petsc_vec(comm, arr):
        return PETSc.Vec().createWithArray(arr, comm=comm)

    def mpiprint(*args, **kwargs):
        rank = kwargs.get('rank', -1)
        stream = kwargs.get('stream', MPI_STREAM)
        if rank < 0:
            if MPI_PRINT_RANK is not None and MPI_PRINT_RANK != MPI.COMM_WORLD.rank:
                return
        elif rank != MPI.COMM_WORLD.rank:
            return

        # allow for usage like normal print statement
        if len(args) > 1:
            stream.write("{%d} " % MPI.COMM_WORLD.rank)
            for arg in args:
                if isinstance(arg, tuple):
                    arg = str(arg)
                stream.write("%s " % arg)
            stream.write("\n")
            stream.flush()
        elif len(args) > 0:
            for part in str(args[0]).split('\n'):
                stream.write("{%d} %s\n" % (MPI.COMM_WORLD.rank, part))
                stream.flush()
else:
    MPI = None
    COMM_NULL = None

    try:
        from petsc4py import PETSc
        
        def create_petsc_vec(comm, arr):
            return PETSc.Vec().createWithArray(arr, comm=comm)
        
    except ImportError:
        PETSc = None
        
        def create_petsc_vec(comm, arr):
            return None

    def mpiprint(*args, **kwargs):
        for arg in args:
            if isinstance(arg, tuple):
                arg = str(arg)
            MPI_STREAM.write("%s " % arg)
        MPI_STREAM.write('\n')

class MPI_info(object):
    def __init__(self):
        self.requested_cpus = 1

        # the MPI communicator used by this comp and its children
        self.comm = COMM_NULL

    @property
    def size(self):
        if MPI:
            return self.comm.size
        return 1

    @property
    def rank(self):
        if MPI:
            return self.comm.rank
        return 0

#use_proc_files()
