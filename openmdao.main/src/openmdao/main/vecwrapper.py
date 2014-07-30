from collections import OrderedDict
import numpy

from openmdao.main.mpiwrap import MPI, mpiprint, create_petsc_vec, PETSc
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start
from openmdao.util.typegroups import int_types


class VecWrapperBase(object):
    """A wrapper object for a local vector, a distributed PETSc vector,
    and info about what var maps to what range within the distributed
    vector.
    """
    def __init__(self, system, array):
        self.array = array
        self._info = OrderedDict() # dict of (start_idx, view)
        self._subviews = set()  # set of all names representing subviews of other views

        # create the PETSc vector
        self.petsc_vec = create_petsc_vec(system.mpi.comm, self.array)

        self._initialize(system)

        self._add_tuple_members(self._info.keys())

        self._add_resid(system)


    def _add_resid(self, system):
        pass

    def _add_tuple_members(self, tups):
        # now add all srcs and dests from var tuples so that views for particular openmdao variables
        # can be accessed.
        for tup in tups:
            info = self._info[tup]
            names = set([tup[0]])  # src
            names.update(tup[1]) # adding dests
            for name in names:
                self._info[name] = info
                self._subviews.add(name)

    def __getitem__(self, name):
        return self._info[name][0]

    def __setitem__(self, name, value):
        if name in self._info:
            self._info[name][0][:] = value.flat
        else:
            # FIXME: this makes me nervous...  certain uses will be broken for this new item
            self._info[name] = (value, 0)
            self._subviews.add(name)
            self._add_tuple_members([name])

    def __contains__(self, name):
        return name in self._info

    def keys(self, subviews=False):
        """Return list of names of views. If subviews is
        True, include names of views that are subviews of other views.
        """
        if subviews:
            return self._info.keys()
        else:
            return [k for k in self._info.keys() if k not in self._subviews]

    def items(self, subviews=False):
        """Return list of (name, view) for each view. If subviews is
        True, include views that are subviews of other views.
        """
        lst = []
        for name, (view, start) in self._info.items():
            if subviews or name not in self._subviews:
                lst.append((name, view))
        return lst

    def start(self, name):
        """Return the starting index for the array view belonging
        to the given name. name may contain array indexing.
        """
        return self._info[name][1]

    def bounds(self, names):
        """Return the bounds corresponding to the slice occupied
        by the named variable(s) within the flattened array.
        name may contain array indexing.  Note that if all of the
        names given are not contiguous, the bounds will encompass
        the named variables AND any variables between them.
        """
        if isinstance(names, basestring):
            view, start = self._info[names]
            return (start, start + view.size)

        infos = [self._info[n] for n in names]
        bnds = [(start, start+view.size) for view,start in infos]
        return (min([u for u,v in bnds]), max([v for u,v in bnds]))

    def multi_indices(self, names):
        """Returns an index array that corresponds to names.
        """
        return idx_merge([self.indices(n) for n in names])

    def indices(self, name):
        """Return the index array corresponding to a single name."""
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
            if start is not None:
                if isinstance(name, tuple):
                    array_val[:] = scope.get_flattened_value(name[0])
                else:
                    array_val[:] = scope.get_flattened_value(name)

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None:
                if isinstance(name, tuple):
                    scope.set_flattened_value(name[0], array_val)
                    for dest in name[1]:
                        scope.set_flattened_value(dest, array_val)
                else:
                    scope.set_flattened_value(name, array_val)

    def set_to_vec(self, vec, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given array.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None:
                #mpiprint("setting %s (%s) to vector" % (name, array_val))
                vec[name][:] = array_val

    def dump(self, vecname='', verbose=False):
        for name, (array_val, start) in self._info.items():
            if verbose or name not in self._subviews:
                if start is None:
                    start = 0
                    mpiprint("bad start idx")
                mpiprint("%s - %s: (%d,%d) %s" %
                           (vecname,name, start, start+len(array_val),array_val))
        if self.petsc_vec is not None:
            mpiprint("%s - petsc sizes: %s" % (vecname,self.petsc_vec.sizes))

    def check(self, parent_vec):
        """Debugging method to check consistency of indexing between a child
        vecwrapper and its parent.
        """
        retval = True
        for name in self._info.keys():
            if not all(self[name] == parent_vec[name]):
                mpiprint("%s not the same in parent (%s) and child (%s)" % (name, parent_vec[name], self[name]))
                retval = False
        return retval

class VecWrapper(VecWrapperBase):
    def _initialize(self, system):
        allvars = system.variables
        vector_vars = system.vector_vars

        # first, add views for vars whose sizes are added to the total,
        # i.e., either they are basevars or their basevars are not included
        # in the vector.
        start, end = 0, 0
        for i, (name, var) in enumerate(vector_vars.items()):
            sz = var['size']
            if sz > 0:
                end += sz
                self._info[name] = (self.array[start:end], start)
                if end-start > self.array[start:end].size:
                    raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" %
                                 (system.name,name, [start,end],self[name].size))
                start += sz

        # now add views for subvars that are subviews of their
        # basevars
        for name, var in allvars.items():
            if name not in vector_vars:
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
                    self._subviews.add(name)
                    if self.array[sub_idx].size != sz:
                        raise RuntimeError("size mismatch: in system %s, view for %s is %s, idx=%s, size=%d" %
                                             (system.name, name,
                                             list(self.bounds(name)),
                                             sub_idx,self.array[sub_idx].size))

    def _add_resid(self, system):
        try:
            cname = system._comp.name
            states = ['.'.join((cname, s)) for s in system._comp.list_states()]
            resids = ['.'.join((cname, s)) for s in system._comp.list_residuals()]
        except AttributeError:
            return

        start, end = self.bounds(states)

        # verify contiguous states
        size = sum([self._info[n][0].size for n in states])

        view = self.array[start:end]

        assert(size == view.size)
        assert(len(resids) == 1)

        self._info[resids[0]] = (view, start)
        self._subviews.add(resids[0])


class InputVecWrapper(VecWrapperBase):
    def _initialize(self, system):

        flat_ins = system.flat(system._get_sized_inputs())
        start, end = 0, 0
        for i, name in enumerate(flat_ins): #_flat_owned_args):
            sz = system._var_meta[name]['size']
            if sz > 0:
                end += sz
                self._info[name] = (self.array[start:end], start)
                if end-start > self.array[start:end].size:
                    raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" %
                                 (system.name,name, [start,end],self[name].size))
                start += sz

        #for sub in system.simple_subsystems(local=True):
            #for i, name in enumerate(sub._owned_args):
                #if (sub is system and name in sub.variables) or (name in system.variables and name not in sub.variables):
                    #var = system.variables[name]
                    #sz = var['size']
                    #assert(sz == system.local_var_sizes[system.mpi.rank,i])
                    #if sz > 0:
                        #end += sz
                        #self._info[name] = (self.array[start:end], start)
                        #if end-start > self.array[start:end].size:
                            #raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" %
                                         #(system.name,name, [start,end],self[name].size))
                        #start += sz

        # # now add views for subvars that are subviews of their
        # # basevars
        # for name, var in allvars.items():
        #     if name not in vector_vars and name in vset:
        #         sz = var['size']
        #         if sz > 0 and var.get('flat', True):
        #             idx = var['flat_idx']
        #             try:
        #                 basestart = self.start(var['basevar'])
        #             except KeyError:
        #                 mpiprint("name: %s, base: %s, vars: %s" %
        #                          (name, var['basevar'], self._info.keys()))
        #                 raise
        #             sub_idx = offset_flat_index(idx, basestart)
        #             substart = get_flat_index_start(sub_idx)
        #             self._info[name] = (self.array[sub_idx], substart)
        #             self._subviews.add(name)
        #             if self.array[sub_idx].size != sz:
        #                 raise RuntimeError("size mismatch: in system %s, view for %s is %s, idx=%s, size=%d" %
        #                                      (system.name, name,
        #                                      list(self.bounds(name)),
        #                                      sub_idx,self.array[sub_idx].size))

class DataTransfer(object):
    """A wrapper object that manages data transfer between
    systems via scatters (and possibly send/receive for
    non-array values)
    """
    def __init__(self, system, var_idxs, input_idxs,
                 scatter_conns, noflat_vars):
        self.scatter = None
        self.scatter_conns = scatter_conns
        self.noflat_vars = noflat_vars

        #print "noflat: %s" % noflat_vars

        # TODO: remove the following attrs (used for debugging only)
        self.var_idxs = idx_merge(var_idxs)
        self.input_idxs = idx_merge(input_idxs)

        if not (MPI or scatter_conns or noflat_vars):
            #mpiprint("RETURNING (no xfer) for %s" % system.name)
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
            #mpiprint("dataxfer is a noop for system %s" % system.name)
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
                for src, dests in self.noflat_vars:
                    for dest in dests:
                        if src != dest:
                            try:
                                system.scope.set(dest, system.scope.get_attr(src))
                            except Exception:
                                system.scope.reraise_exception("cannot set '%s' from '%s'" % (dest, src))
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
            #self.svec.dump()
            #self.dvec.dump()
            #print "post-scatter, dest = %s" % destvec
