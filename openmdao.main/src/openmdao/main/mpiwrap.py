import os
import sys

def use_proc_files():
    if MPI is not None:
        rank = MPI.COMM_WORLD.rank
        sname = "%s.out" % rank
        sys.stdout = sys.stderr = open(sname, 'w')

def under_mpirun():
    """Return True if we're being executed under mpirun."""
    # TODO: this is a bit of a hack and there appears to be
    # no consistent set of environment vars between MPI 
    # implementations.
    for name in os.environ.keys():
        if name.startswith('OMPI_COMM') or name.startswith('MPICH_'):
            return True
    return False

class PETSc(object):
    def __init__(self):
        self.needs_ksp = False # PETSc won't actually be imported unless this is True
        self._PETSc = None
        
    @property
    def installed(self):
        try:
            if self._PETSc is None:
                from petsc4py import PETSc
                del sys.modules['petsc4py']
                self._PETSc = PETSc
            return True
        except ImportError:
            self._PETSc = None
            return False
            
    def __getattr__(self, name):
        if self.installed:
            return getattr(self._PETSc, name)
        raise AttributeError(name)
    
def create_petsc_vec(comm, arr):
    if under_mpirun() or PETSc.needs_ksp:
        if PETSc.installed:
            return PETSc.Vec().createWithArray(arr, comm=comm)

    return None

if under_mpirun():
    from mpi4py import MPI
    from petsc4py import PETSc
    PETSc.installed = True

    COMM_NULL = MPI.COMM_NULL
    
else:
    MPI = None
    COMM_NULL = None
    PETSc = PETSc()


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

if os.environ.get('USE_PROC_FILES'):
    use_proc_files()
