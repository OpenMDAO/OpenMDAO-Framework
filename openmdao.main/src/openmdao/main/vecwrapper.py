from collections import OrderedDict
import numpy

from openmdao.main.mpiwrap import mpiprint, create_petsc_vec, PETSc
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start
from openmdao.util.typegroups import int_types


class VecWrapper(object):
    """A wrapper object for a local vector, a distributed PETSc vector,
    and info about what var maps to what range within the distributed
    vector.
    """
    def __init__(self, system, array, inputs=None):
        self.array = array

        allvars = system.all_variables
        variables = system.variables
        
        self._info = OrderedDict() # dict of (start_idx, view)
        self._subvars = set()  # set of all names representing subviews of other views

        if inputs is None:
            vset = allvars
        else:
            vset = inputs
            #mpiprint("SCATTER_INPUTS: %s" % inputs)
            notfound = [n for n in inputs if n not in variables]
            if notfound:
                mpiprint("SCATTER for %s: inputs %s\nnot in variables %s" %
                        (system.name, notfound, variables.keys()))

        # first, add views for vars whose sizes are added to the total,
        # i.e., their basevars are not included in the vector.
        start, end = 0, 0
        for i, (name, var) in enumerate(variables.items()):
            if name not in vset:
                continue
            sz = var['size']
            if sz != system.local_var_sizes[system.mpi.comm.rank,i]:
                raise RuntimeError("ERROR: sz (%d) != local_var_sizes (%d) in %s" % 
                          (sz,system.local_var_sizes[system.mpi.comm.rank,i],system.name))
            if sz > 0:
                end += sz
                if self.array.size < (end-start):
                    raise RuntimeError("size mismatch: in system %s, can't create a view of [%d,%d] from a %d size array" %
                                         (system.name,start,end,self.array.size))
                self._info[name] = (self.array[start:end], start)
                if end-start > self.array[start:end].size:
                    raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" % 
                                 (system.name,name, [start,end],self._info[name][0].size))
                start += sz

        # now add views for subvars that are subviews of their
        # basevars
        for name, var in allvars.items():
            if name not in variables and name in vset:
                sz = var['size']
                if sz > 0:
                    idx = var['flat_idx']
                    try:
                        basestart = self.start(var['basevar'])
                    except KeyError:
                        mpiprint("name: %s, base: %s, vars: %s" %
                                 (name, var['basevar'], self._info.keys()))
                        raise
                    sub_idx = offset_flat_index(idx, basestart)
                    substart = get_flat_index_start(sub_idx)
                    self._info[name] = (self.array[sub_idx], substart)
                    self._subvars.add(name)
                    if self.array[sub_idx].size != sz:
                        raise RuntimeError("size mismatch: in system %s, view for %s is %s, idx=%s, size=%d" % 
                                             (system.name, name, 
                                             list(self.bounds(name)),
                                             sub_idx,self.array[sub_idx].size))

        # create the PETSc vector
        self.petsc_vec = create_petsc_vec(system.mpi.comm, self.array)

        #mpiprint("SCATTER VEC: %s" % self._info.keys())

    def __getitem__(self, name):
        return self._info[name][0]

    def __contains__(self, name):
        return name in self._info
        
    def list_views(self):
        """Return a list of names of subviews of our array."""
        return self._info.keys()

    def start(self, name):
        """Return the starting index for the array view belonging
        to the given name. name may contain array indexing.
        """
        return self._info[name][1]

    def bounds(self, name):
        """Return the bounds corresponding to the slice occupied
        by the named variable within the flattened array.
        name may contain array indexing.
        """
        view, start = self._info[name]
        return (start, start + view.size)

    def indices(self, name):
        """Return the set of indices corresponding to name
        as an index array.
        """
        view, start = self._info[name]
        return petsc_linspace(start, start+view.size)

    def set_from_scope(self, scope, vnames=None):
        """Get the named values from the given scope and set flattened
        versions of them in our array.
        """
        if vnames is None:
            vnames = self._info.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None and name not in self._subvars:
                array_val[:] = scope.get_flattened_value(name)
                mpiprint("getting %s (%s) from scope" % (name, array_val))

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self._info.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None and name not in self._subvars:
                mpiprint("setting %s (%s) to scope %s" % (name, array_val,scope.name))
                scope.set_flattened_value(name, array_val)
           
    def dump(self, vecname):
        for name, (array_val, start) in self._info.items():
            mpiprint("%s - %s: (%d,%d) %s" % (vecname,name, start, start+len(array_val),array_val))
        mpiprint("%s - petsc sizes: %s" % (vecname,self.petsc_vec.sizes))

    def items(self):
        lst = []
        for name, (array_val, start) in self._info.items():
            if len(array_val) == 1:
                lst.append((name, array_val[0]))
            else:
                lst.append((name, array_val))
        return lst


class DataTransfer(object):
    """A wrapper object that manages data transfer between
    MPI processes via scatters (and possibly send/receive for 
    non-array values)
    """
    def __init__(self, system, var_idxs, input_idxs, scatter_vars):
        self.scatter_vars = scatter_vars[:]

        # TODO: remove the following attrs (used for debugging only)
        self.var_idxs = var_idxs[:]
        self.input_idxs = input_idxs[:]

        #mpiprint("SCATTER_VARS: %s" % scatter_vars)
        merged_vars = idx_merge(var_idxs)
        merged_inputs = idx_merge(input_idxs)

        var_idx_set = PETSc.IS().createGeneral(merged_vars, 
                                               comm=system.mpi.comm)
        input_idx_set = PETSc.IS().createGeneral(merged_inputs, 
                                                 comm=system.mpi.comm)
        if system.app_ordering is not None:
            var_idx_set = system.app_ordering.app2petsc(var_idx_set)

        if len(merged_vars) != len(merged_inputs):
            raise RuntimeError("ERROR: creating scatter (index size mismatch): (%d != %d) srcs: %s,  dest: %s in %s" % 
                                (len(merged_vars), len(merged_inputs),
                                  merged_vars, merged_inputs, system.name))
        try:
            # note that scatter created here can be reused for other vectors as long
            # as their sizes are the same as 'u' and 'p'
            self.scatter = PETSc.Scatter().create(system.vec['u'].petsc_vec, var_idx_set,
                                                  system.vec['p'].petsc_vec, input_idx_set)
        except Exception as err:
            raise RuntimeError("ERROR in %s (var_idxs=%s, input_idxs=%s, usize=%d, psize=%d): %s" % 
                                  (system.name, var_idxs, input_idxs, system.vec['u'].array.size,
                                   system.vec['p'].array.size, str(err)))
            raise

    def __call__(self, system, srcvec, destvec, reverse=False):

        src_petsc = srcvec.petsc_vec
        dest_petsc = destvec.petsc_vec

        #srcvec.array *= system.vec['u0'].array
        #if system.mode == 'fwd':
        self.scatter.scatter(src_petsc, dest_petsc, addv=False, mode=False)
        #elif system.mode == 'rev':
        #    scatter.scatter(dest_petsc, src_petsc, addv=True, mode=True)
        #else:
        #    raise Exception('mode type not recognized')
        #srcvec.array /= system.vec['u0'].array

def idx_merge(idxs):
    """Combines a mixed iterator of int and iterator indices into an
    array of int indices.
    """
    if len(idxs) > 0:
        idxs = [i for i in idxs if isinstance(i, int_types) or
                           len(i)>0]
        if len(idxs) > 0:
            if isinstance(idxs[0], int_types):
                return idxs
            else:
                return numpy.concatenate(idxs)
    return idxs

def petsc_linspace(start, end):
    """ Return a linspace vector of the right int type for PETSc """
    #return numpy.arange(start, end, dtype=PETSc.IntType)
    return numpy.array(numpy.linspace(start, end-1, end-start), 
                       dtype=PETSc.IntType)

