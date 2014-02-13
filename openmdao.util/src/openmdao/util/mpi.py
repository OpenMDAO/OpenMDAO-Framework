
from mpi4py import MPI

def mpi_setup(asm):
    comm = MPI.COMM_WORLD
    rank = comm.rank
    size = comm.size

    if rank == 0:  # master process
        # calculate the graph, map comps to procs
        ...
        comm.send(mapping, dest=???)  # actually this should be a bcast
    else:
        # wait to receive the graph/mapping
        mapping = comm.recv(source=???)
        