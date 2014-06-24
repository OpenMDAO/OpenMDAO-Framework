from collections import OrderedDict
import numpy

from openmdao.main.mpiwrap import MPI, mpiprint, create_petsc_vec, PETSc
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
        vector_vars = system.vector_vars
        
        self._info = OrderedDict() # dict of (start_idx, view)
        self._subvars = set()  # set of all names representing subviews of other views

        if inputs is None:
            vset = allvars
        else:
            vset = set(inputs)

        ## top conserve memory, connected sources and dests will occupy the same entries in the
        ## vector (assuming both are present)
        #self._dups = dict([(v,u) for u,v in system.in_edges if u != v and u in vector_vars])

        # first, add views for vars whose sizes are added to the total,
        # i.e., either they are basevars or their basevars are not included 
        # in the vector.
        start, end = 0, 0
        for i, (name, var) in enumerate(vector_vars.items()):
            if name not in vset: # or name in self._dups:
                continue
            sz = var['size']
            assert(sz == system.local_var_sizes[system.mpi.rank,i])
            if sz > 0:
                end += sz
                if self.array.size < (end-start):
                    raise RuntimeError("size mismatch: in system %s, can't create a view of [%d,%d] from a %d size array" %
                                         (system.name,start,end,self.array.size))
                self._info[name] = (self.array[start:end], start)
                if end-start > self.array[start:end].size:
                    raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" % 
                                 (system.name,name, [start,end],self[name].size))
                start += sz

        # now add views for subvars that are subviews of their
        # basevars
        for name, var in allvars.items():
            if name not in vector_vars and name in vset:
                sz = var['size']
                if sz > 0 and var.get('flat', True):
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

        ## now add entries for any connected destinations
        #for dest, src in self._dups.items():
            #if dest in self._info:
                #continue
            #if src in self._info:
                #self._info[dest] = self._info[src]

        # create the PETSc vector
        self.petsc_vec = create_petsc_vec(system.mpi.comm, self.array)

        #mpiprint("SCATTER VEC: %s" % self._info.keys())

    def __getitem__(self, name):
        return self._info[name][0]

    def __setitem__(self, name, value):
        self._info[name][0][:] = value.flat

    def __contains__(self, name):
        return name in self._info

    def keys(self):
        return self._info.keys()

    def items(self):
        lst = []
        for name, (view, start) in self._info.items():
            if len(view) == 1:
                lst.append((name, view[0]))
            else:
                lst.append((name, view))
        return lst
        
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
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None and name not in self._subvars:
                array_val[:] = scope.get_flattened_value(name)
                #mpiprint("getting %s (%s) from scope" % (name, array_val))

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None and name not in self._subvars:
                mpiprint("setting %s (%s) to scope %s" % (name, array_val,scope.name))
                scope.set_flattened_value(name, array_val)
           
    def set_to_vec(self, vec, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given array.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None and name not in self._subvars:
                mpiprint("setting %s (%s) to vector" % (name, array_val))
                vec[name][:] = array_val
           
    def dump(self, vecname=''):
        for name, (array_val, start) in self._info.items():
            mpiprint("%s - %s: (%d,%d) %s" % (vecname,name, start, start+len(array_val),array_val))
        if self.petsc_vec is not None:
            mpiprint("%s - petsc sizes: %s" % (vecname,self.petsc_vec.sizes))


class DataTransfer(object):
    """A wrapper object that manages data transfer between
    systems via scatters (and possibly send/receive for 
    non-array values)
    """
    def __init__(self, system, var_idxs, input_idxs, scatter_conns, noflat_vars):
        self.scatter = None
        self.scatter_conns = scatter_conns[:]
        self.noflat_vars = noflat_vars[:]

        #print "noflat: %s" % noflat_vars

        # TODO: remove the following attrs (used for debugging only)
        self.var_idxs = idx_merge(var_idxs)
        self.input_idxs = idx_merge(input_idxs)
        
        if not (scatter_conns or noflat_vars):
            return  # no data to xfer

        #mpiprint("%s scatter_conns: %s" % (system.name, scatter_conns))

        if len(self.var_idxs) != len(self.input_idxs):
            raise RuntimeError("ERROR: creating scatter (index size mismatch): (%d != %d) srcs: %s,  dest: %s in %s" % 
                                (len(self.var_idxs), len(self.input_idxs),
                                  self.var_idxs, self.input_idxs, system.name))

        if MPI:
            var_idx_set = PETSc.IS().createGeneral(self.var_idxs, 
                                                   comm=system.mpi.comm)
            input_idx_set = PETSc.IS().createGeneral(self.input_idxs, 
                                                     comm=system.mpi.comm)
            if system.app_ordering is not None:
                var_idx_set = system.app_ordering.app2petsc(var_idx_set)
    
            try:
                # note that scatter created here can be reused for other vectors as long
                # as their sizes are the same as 'u' and 'p'
                self.scatter = PETSc.Scatter().create(system.vec['u'].petsc_vec, var_idx_set,
                                                      system.vec['p'].petsc_vec, input_idx_set)
            except Exception as err:
                raise RuntimeError("ERROR in %s (var_idxs=%s, input_idxs=%s, usize=%d, psize=%d): %s" % 
                                      (system.name, var_idxs, input_idxs, system.vec['u'].array.size,
                                       system.vec['p'].array.size, str(err)))
        else:  # serial execution
            if len(self.var_idxs) and len(self.input_idxs):
                self.scatter = SerialScatter(system.vec['u'], self.var_idxs,
                                             system.vec['p'], self.input_idxs)

    def __call__(self, system, srcvec, destvec, reverse=False):

        if self.scatter is None and not self.noflat_vars:
            mpiprint("dataxfer is a noop for system %s" % system.name)
            return
        
        if MPI:
            src = srcvec.petsc_vec
            dest = destvec.petsc_vec
        else:
            src = srcvec.array
            dest = destvec.array

        #srcvec.array *= system.vec['u0'].array
        #if system.mode == 'fwd':
        if self.scatter:
            #mpiprint("%s scattering %s" % (system.name, self.scatter_conns))
            self.scatter.scatter(src, dest, addv=False, mode=False)

        if self.noflat_vars:
            if MPI:
                raise NotImplementedError("passing of non-flat vars %s has not been implemented yet" %
                                          self.noflat_vars) # FIXME
            else:
                for src, dest in self.noflat_vars:
                    system.scope.set(dest, system.scope.get_attr(src))
        #elif system.mode == 'rev':
        #    scatter.scatter(dest_petsc, src_petsc, addv=True, mode=True)
        #else:
        #    raise Exception('mode type not recognized')
        #srcvec.array /= system.vec['u0'].array

        # # copy dest vector values back into local src vector after
        # # scatter since other systems share parts of the src
        # # vector (via shared views) with this system.
        # # FIXME: make sure we're not duplicating copy operation because
        # #        in some cases we copy vector values back and forth from
        # #        scoping Assembly...
        # subvars = destvec._subvars
        # for name, (array, start) in destvec._info.items():
        #     if name not in subvars:
        #         #mpiprint("copying %s (%s) back into vector %s" % (name, array, srcvecname))
        #         srcvec[name][:] = array

        for _, dvar in self.scatter_conns:
            srcvec[dvar][:] = destvec[dvar][:]

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
    if MPI:
        dtype = PETSc.IntType
    else:
        dtype = 'i'
    return numpy.array(numpy.linspace(start, end-1, end-start), 
                       dtype=dtype)


class SerialScatter(object):
    def __init__(self, srcvec, src_idxs, destvec, dest_idxs):
        self.src_idxs = src_idxs
        self.dest_idxs = dest_idxs
        self.svec = srcvec
        self.dvec = destvec

    def scatter(self, srcvec, destvec, addv, mode):
        if mode:  # reverse?
            raise RuntimeError("reverse mode not supported yet")
        else:   # fwd
            ## the following debug stuff only works if all vars are size=1
            #sk = self.svec.keys()
            #dk = self.dvec.keys()
            #for s,d in zip(self.src_idxs, self.dest_idxs):
                #print "%s (%s) -> %s (%s)" % (sk[s], srcvec[s], dk[d], destvec[d])
            destvec[self.dest_idxs] = srcvec[self.src_idxs]
            #print "post-scatter, dest = %s" % destvec
