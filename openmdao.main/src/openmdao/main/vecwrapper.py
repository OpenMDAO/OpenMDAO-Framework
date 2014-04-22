from collections import OrderedDict

import numpy

from openmdao.main.mpiwrap import mpiprint, get_petsc_vec
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start


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

        # first, add views for vars whose sizes are added to the total,
        # i.e., their basevars are not included in the vector.
        start, end = 0, 0
        for i, (name, var) in enumerate(variables.items()):
            if name not in vset:
                continue
            sz = var['size']
            if sz != system.local_var_sizes[system.mpi.comm.rank,i]:
                mpiprint("ERROR: sz (%d) != local_var_sizes (%d) in %s" % 
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
                    basestart = self.start(var['basevar'])
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
        self.petsc_vec = get_petsc_vec(system.mpi.comm, self.array)

    def __getitem__(self, name):
        return self._info[name][0]
        
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

    def set_from_scope(self, scope):
        """Get the named values from the given scope and set flattened
        versions of them in our array.
        """
        for name, (array_val, start) in self._info.items():
            if name not in self._subvars:
                array_val[:] = scope.get_flattened_value(name)

    def set_to_scope(self, scope):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        for name, (array_val, start) in self._info.items():
            if name not in self._subvars:
                scope.set_flattened_value(name, array_val)
           
    def dump(self, vname):
        for name, (array_val, start) in self._info.items():
            mpiprint("%s - %s: (%d,%d) %s" % (vname,name, start, start+len(array_val),array_val))
        mpiprint("%s - petsc sizes: %s" % (vname,self.petsc_vec.sizes))