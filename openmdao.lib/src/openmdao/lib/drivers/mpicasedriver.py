""" A driver that runs input cases in parallel via MPI."""

from openmdao.main.api import Driver
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.hasparameters import HasVarTreeParameters
from openmdao.main.hasresponses import HasVarTreeResponses
from openmdao.main.interfaces import IHasResponses, IHasParameters, implements
from openmdao.main.variable import is_legal_name, make_legal_path
from openmdao.main.array_helpers import flattened_value
from openmdao.main.mpiwrap import MPI, evenly_distrib_idxs, MPIContext

from openmdao.util.decorators import add_delegate


@add_delegate(HasVarTreeParameters, HasVarTreeResponses)
class MPICaseDriver(Driver):
    """
    A Driver that runs each parameter set concurrently in the specified number
    of processes.
    """
    implements(IHasParameters, IHasResponses)

    def get_req_cpus(self):
        # None means there is no max procs. It will use as many as it's given
        req = self.workflow.get_req_cpus()
        return (req[0], None)

    def execute(self):
        """ Run each parameter set. """

        color = self._color[self.mpi.rank]

        if color == MPI.UNDEFINED or self.mpi.comm == MPI.COMM_NULL:
            return

        # Prepare parameters and responses.
        case_paths = {}
        inputs = []
        values = []

        for path in self.get_parameters():
            if isinstance(path, tuple):
                for target in path:
                    inputs.append(target)
                path = path[0]
            else:
                inputs.append(path)

            val = self.case_inputs.get(make_legal_path(path))

            values.append(val)

        if not inputs:
            return

        length = len(values[0])

        for path in self.get_responses():
            case_paths[path] = make_legal_path(path)

        sizes, offsets = evenly_distrib_idxs(self._num_parallel_subs,
                                             length)
        start = offsets[color]
        end = start + sizes[color]

        self.init_responses(length)

        # Run each parameter set.
        for i in range(start, end):

            # Set inputs.
            for j, path in enumerate(inputs):
                self.set_parameter_by_name(path, values[j][i])

            # Run workflow.
            with MPIContext():
                self.run_iteration()

            # Get outputs.
            for path in self.get_responses():
                cpath = case_paths[path]
                self.case_outputs.get(cpath)[i] = self.parent.get(path)

        if self._num_parallel_subs > 1:
            # Now, collect the results back from all parallel processes
            for path in self.get_responses():
                path = case_paths[path]
                vals = self.case_outputs.get(path)
                if self._resp_comm != MPI.COMM_NULL:
                    allvals = self._resp_comm.gather(vals, root=0)

                    if self._resp_comm.rank == 0:
                        for i in range(self._num_parallel_subs):
                            vals[offsets[i]:offsets[i]+sizes[i]] = allvals[i][offsets[i]:offsets[i]+sizes[i]]
                        junk = self.mpi.comm.bcast(vals, root=0)
                    else:
                        vals = self.mpi.comm.bcast(None, root=0)
                else:
                    vals = self.mpi.comm.bcast(vals, root=0)

                self.case_outputs.set(path, vals)

    def setup_communicators(self, comm):
        self.mpi.comm = comm
        size = comm.size
        rank = comm.rank

        mincpu, maxcpu = self.workflow.get_req_cpus()
        self._num_parallel_subs = size / mincpu
        leftover = size % mincpu

        color = []
        resp_color = []
        undefs = [MPI.UNDEFINED] * mincpu
        for i in range(self._num_parallel_subs):
            color.extend([i] * mincpu)
            resp_color.extend([0]+undefs[1:])

        # TODO: give leftover procs to subsystems if they can utilize them
        if leftover:
            color.extend([MPI.UNDEFINED] * leftover)
            resp_color.extend([MPI.UNDEFINED] * leftover)

        sub_comm = comm.Split(color[rank])
        self._color = color

        # if we weren't given enough procs to run parallel workflows,
        # just run serial
        if self._num_parallel_subs == 1:
            self.workflow.setup_communicators(comm)
            self._resp_comm = MPI.COMM_NULL
            return

        if mincpu > comm.size:
            raise RuntimeError("subsystem %s requested %d processors but got %s" %
                               (self.name, mincpu, comm.size))

        self.workflow.setup_communicators(sub_comm)

        # Now set up a special comm for just the MPICaseDrivers that have 0 rank
        # sub_comm.  The responses are duplicated in each proc of the sub_comm,
        # so we just want the first one in order to avoid unnecessary data
        # passing. Later we'll broadcast the fully assembled case_outputs
        # vartree to all procs.
        self._resp_comm = comm.Split(resp_color[rank])
