
from collections import OrderedDict
import numpy

from openmdao.main.mpiwrap import MPI, MPI_STREAM, mpiprint, create_petsc_vec, PETSc
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start, get_val_and_index, get_shape, \
                                        get_flattened_index, to_slice
from openmdao.main.interfaces import IImplicitComponent
from openmdao.util.typegroups import int_types
from openmdao.util.graph import base_var


class VecWrapperBase(object):
    """A wrapper object for a local vector, a distributed PETSc vector,
    and info about what var maps to what range within the distributed
    vector.
    """
    def __init__(self, system, array, name=''):
        self.array = array
        self.name = name
        self._info = OrderedDict() # dict of (start_idx, view)
        self._subviews = set()  # set of all names representing subviews of other views

        # create the PETSc vector
        self.petsc_vec = create_petsc_vec(system.mpi.comm,
                                          self.array)

        self._initialize(system)

        self._map_resids_to_states(system)

        self._add_tuple_members(system, self._info.keys())
        self._add_resid(system)

    def _add_resid(self, system):
        pass

    def _add_tuple_members(self, system, tups):
        # now add all srcs and dests from var tuples so that views for particular openmdao variables
        # can be accessed.
        for tup in tups:
            info = self._info[tup]
            names = set([tup[0]])  # src
            names.update(tup[1])   # adding dests
            for name in names:
                self._info[name] = info
                self._subviews.add(name)
                # also add 1 item tuple form, since that's used for derivative inputs/outputs
                self._info[(name,)] = info
                self._subviews.add((name,))

    def _add_subview(self, scope, name):
        var = scope._var_meta[name]
        name2collapsed = scope.name2collapsed

        sz = var['size']
        if sz > 0 and not var.get('noflat'):
            idx = var['flat_idx']
            try:
                basestart = self.start(name2collapsed[var['basevar']])
            except KeyError:
                mpiprint("name: %s, base: %s, vars: %s" %
                         (name, var['basevar'], self._info.keys()))
                raise
            sub_idx = offset_flat_index(idx, basestart)
            substart = get_flat_index_start(sub_idx)
            self._info[name] = (self.array[sub_idx], substart)
            self._subviews.add(name)

    def __getitem__(self, name):
        return self._info[name][0]

    def __setitem__(self, name, value):
        self._info[name][0][:] = value.flat

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
        bnds = [(strt, strt+v.size) for v,strt in infos]
        return (min([u for u,v in bnds]), max([v for u,v in bnds]))

    def multi_indices(self, names):
        """Returns an index array that corresponds to names.
        """
        return idx_merge([self.indices(n) for n in names])

    def indices(self, scope, name):
        """Return the index array corresponding to a single name."""
        if name not in self._info:
            if base_var(scope._depgraph, name) in self._info:
                self._add_subview(scope, name)
        view, start = self._info[name]
        return petsc_linspace(start, start+view.size)

    def set_to_vecwrapper(self, vec, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given vecwrapper.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None:
                vec[name][:] = array_val

    def set_to_array(self, arr, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given array.
        """
        if vnames is None:
            vnames = self.keys()

        asize = arr.size

        start = end = 0
        for name in vnames:
            array_val = self[name]
            end += array_val.size
            if end > asize:
                raise ValueError("end index %d exceeds size of target array" % (end-1))
            arr[start:end] = array_val
            start += array_val.size

    def set_from_array(self, arr, vnames):
        """Pull values for the given set of names out of the array
        and set them into our array.
        """
        asize = self.array.size

        start = end = 0
        for name in vnames:
            array_val = self[name]
            end += array_val.size
            if end > asize:
                raise ValueError("end index %d exceeds size of target array" % (end-1))
            array_val[:] = arr[start:end]
            start += array_val.size

    def dump(self, verbose=False, stream=MPI_STREAM):
        for name, (array_val, start) in self._info.items():
            if verbose or name not in self._subviews:
                if start is None:
                    start = 0
                    mpiprint("bad start idx", stream=stream)
                mpiprint("%s - %s: (%d,%d) %s" %
                           (self.name, array_val, start, start+len(array_val), name),
                           stream=stream)
        if self.petsc_vec is not None:
            mpiprint("%s - petsc sizes: %s" % (self.name, self.petsc_vec.sizes),
                     stream=stream)

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
        scope = system.scope
        name2collapsed = scope.name2collapsed
        allvars = system.variables
        vector_vars = system.vector_vars
        self.app_ordering = system.app_ordering
        rank = system.mpi.rank

        vec_srcs = set([n[0] for n in vector_vars])

        # to detect overlapping index sets, we need all of the subvar bases
        # that are NOT included in the vectors.  If a base IS included in the vectors,
        # then all of its subvars are just subviews of the base view, so no chance
        # of overlapping causing redundant data in the vectors
        bases = dict([(n.split('[',1)[0], []) for n in vec_srcs
                                    if n.split('[',1)[0] not in vec_srcs])

        # first, add views for vars whose sizes are added to the total,
        # i.e., either they are basevars or their basevars are not included
        # in the vector.
        start, end = 0, 0
        for ivar, (name, var) in enumerate(vector_vars.items()):
            sz = system.local_var_sizes[rank, ivar]
            if sz > 0:
                end += sz
                # store the view, local start idx, and distributed start idx
                self._info[name] = (self.array[start:end], start)#, dist_start)

                base = name[0].split('[',1)[0]

                if base in bases and base not in vec_srcs:
                    bases[base].append(name[0])

                    if len(bases[base]) > 1:
                        # check for overlaping subvars
                        idxset = set()
                        bval = scope.get(base)

                        for subname in bases[base]:
                            _, idx = get_val_and_index(scope, subname)
                            idxs = get_flattened_index(idx, get_shape(bval), cvt_to_slice=False)
                            if idxset.intersection(set(idxs)):
                                raise RuntimeError("Subvars %s share overlapping indices. Try reformulating the problem to prevent this." %
                                                   [n for n in bases[base]])
                            idxset.update(idxs)

                if end-start > self.array[start:end].size:
                    raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" %
                                 (system.name,name, [start,end],self[name].size))
                start += sz


        # now add views for subvars that are subviews of their
        # basevars
        if vector_vars:
            for name, var in allvars.items():
                if name not in vector_vars:
                    sz = var['size']
                    if sz > 0 and not var.get('noflat'):
                        idx = var['flat_idx']
                        try:
                            basestart = self.start(name2collapsed[var['basevar']])
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

        # TODO: handle cases where we have overlapping subvars but no basevar


    def _add_resid(self, system):
        nodemap = system.scope.name2collapsed
        if hasattr(system, '_comp') and IImplicitComponent.providedBy(system._comp):
            cname = system._comp.name
            states = ['.'.join((cname, s)) for s in system._comp.list_states()]
            resids = ['.'.join((cname, s)) for s in system._comp.list_residuals()]
        else:
            states = []
            resids = []

        states = [s for s in states if nodemap[s] in system.variables]
        if states:
            start, end = self.bounds(states)

            # verify contiguous states
            size = sum([self._info[n][0].size for n in states])

            view = self.array[start:end]

            assert(size == view.size)
            assert(len(resids) == 1)

            self._info[resids[0]] = (view, start)
            self._subviews.add(resids[0])

    def _map_resids_to_states(self, system):
        # add any mappings of residuals to states
        for resid, state in system._mapped_resids.items():
            if resid not in self._info:
                self._info[resid] = self._info[state]
                self._subviews.add(resid)

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
                    array_val[:] = scope.get_flattened_value(name[0]).real
                else:
                    array_val[:] = scope.get_flattened_value(name).real
                #mpiprint("setting %s from scope (%s)" % (name, array_val))

    def set_from_scope_complex(self, scope, vnames=None):
        """Get the named values from the given scope and set flattened
        versions of just the complex portion into our array.
        """
        if vnames is None:
            vnames = self.keys()

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None:
                if isinstance(name, tuple):
                    array_val[:] = scope.get_flattened_value(name[0]).imag
                else:
                    array_val[:] = scope.get_flattened_value(name).imag
                #mpiprint("getting %s from scope (%s)" % (name, array_val))

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            array_val, start = self._info.get(name,(None,None))
            if start is not None:
                if isinstance(name, tuple):
                    scope.set_flattened_value(name[0], array_val)
                    for dest in name[1]:
                        if dest != name[0]:
                            scope.set_flattened_value(dest, array_val)
                else:
                    scope.set_flattened_value(name, array_val)


class InputVecWrapper(VecWrapperBase):
    def _initialize(self, system):

        varmeta = system.scope._var_meta
        name2collapsed = system.scope.name2collapsed
        flat_ins = _filter_flat(system.scope, system._owned_args)
        start, end = 0, 0
        arg_idx = system.arg_idx

        for sub in system.simple_subsystems():
            for name in [n for n in system.vector_vars if n in sub._in_nodes]:
                if name in flat_ins and name not in self._info:
                    sz = len(arg_idx[name])
                    end += sz
                    self._info[name] = (self.array[start:end], start)
                    if end-start > self.array[start:end].size:
                        raise RuntimeError("size mismatch: in system %s view for %s is %s, size=%d" %
                                     (system.name,name, [start,end],self[name].size))
                    start += sz

        all_ins = set()
        for sub in system.simple_subsystems():
            for arg in sub._in_nodes:
                all_ins.add(arg)

        # now add views for subvars that are subviews of their
        # basevars
        for name in all_ins:
            var = varmeta[name]

            if name in system.vector_vars or name2collapsed.get(var.get('basevar')) not in self:
                continue

            sz = var['size']
            if sz > 0 and not var.get('noflat'):
                idx = var['flat_idx']
                try:
                    basestart = self.start(name2collapsed[var['basevar']])
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

    def _map_resids_to_states(self, system):
        pass

    def get_dests_by_comp(self):
        """Return a dict of comp name keyed to a list of input nodes, with
        any subvars removed that have basevars in the vector.
        """
        ret = OrderedDict()
        for node, (start, view) in self._info.items():
            if isinstance(node, tuple) and len(node) > 1:
                src, dests = node
                for d in dests:
                    cname, _, vname = d.partition('.')
                    ret.setdefault(cname, []).append(node)

        for cname, in_nodes in ret.items():
            bases = [n[0] for n in in_nodes if n[0].split('[',1)[0]==n[0]]
            ret[cname] = [n for n in in_nodes if n[0] in bases or n[0].split('[',1)[0] not in bases]

        return ret

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            array_val, start = self._info.get(name,(None, None))
            if start is not None:
                if isinstance(name, tuple):
                    for dest in name[1]:
                        scope.set_flattened_value(dest, array_val)
                else:
                    scope.set_flattened_value(name, array_val)

    def set_to_scope_complex(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope as the complex part of the value.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            step, start = self._info.get(name,(None, None))

            if start is not None:
                if isinstance(name, tuple):
                    for dest in name[1]:
                        array_val = scope.get_flattened_value(dest)
                        scope.set_flattened_value(dest, array_val + step*1j)
                else:
                    array_val = scope.get_flattened_value(name)
                    scope.set_flattened_value(name, array_val + step*1j)


class DataTransfer(object):
    """A wrapper object that manages data transfer between
    systems via scatters (and possibly send/receive for
    non-array values)
    """
    def __init__(self, system, var_idxs, input_idxs,
                 scatter_conns, noflat_vars):
        self.scatter = None
        self.scatter_conns = scatter_conns
        self.noflat_vars = list(noflat_vars)

        if not (MPI or scatter_conns or noflat_vars):
            return  # no data to xfer

        var_idxs = idx_merge(var_idxs)
        input_idxs = idx_merge(input_idxs)

        if len(var_idxs) != len(input_idxs):
            raise RuntimeError("ERROR: creating scatter (index size mismatch): (%d != %d) srcs: %s,  dest: %s in %s" %
                                (len(var_idxs), len(input_idxs),
                                  var_idxs, input_idxs, system.name))

        if MPI:
            var_idx_set = PETSc.IS().createGeneral(var_idxs,
                                                   comm=system.mpi.comm)
            input_idx_set = PETSc.IS().createGeneral(input_idxs,
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
            if len(var_idxs) and len(input_idxs):
                self.scatter = SerialScatter(system.vec['u'], var_idxs,
                                             system.vec['p'], input_idxs)

    def __call__(self, system, srcvec, destvec, complex_step=False):

        if self.scatter is None and not self.noflat_vars:
            #mpiprint("dataxfer is a noop for system %s" % system.name)
            return

        if MPI:
            src = srcvec.petsc_vec
            dest = destvec.petsc_vec
        else:
            src = srcvec.array
            dest = destvec.array

        addv = mode = False
        if not complex_step and system.mode == 'adjoint' and srcvec.name.endswith('du'):
            addv = mode = True
            destvec, srcvec = srcvec, destvec
            dest, src = src, dest

        if self.scatter:
            self.scatter.scatter(src, dest, addv=addv, mode=mode)

        if destvec.name.endswith('.p') and self.noflat_vars:
            if MPI:
                raise NotImplementedError("passing of non-flat vars %s has not been implemented yet" %
                                          self.noflat_vars) # FIXME
            else:
                for src, dests in self.noflat_vars:
                    for dest in dests:
                        if src != dest:
                            try:
                                system.scope.set(dest, system.scope.get_attr_w_copy(src))
                            except Exception:
                                system.scope.reraise_exception("cannot set '%s' from '%s'" % (dest, src))


class SerialScatter(object):
    def __init__(self, srcvec, src_idxs, destvec, dest_idxs):
        self.src_idxs = to_slice(src_idxs)
        self.dest_idxs = to_slice(dest_idxs)
        self.svec = srcvec
        self.dvec = destvec

    def scatter(self, srcvec, destvec, addv, mode):
        if addv is True:
            destvec[self.src_idxs] += srcvec[self.dest_idxs]
        else:
            destvec[self.dest_idxs] = srcvec[self.src_idxs]


def idx_merge(idxs):
    """Combines a mixed iterator of int and iterator indices into an
    array of int indices.
    """
    # TODO: (for serial at least) convert the collection of indices into
    #       a slice object (if possible) to avoid any unnecessary copying.  Not sure if petsc
    #       will allow use of slice objects, but even if it's serial only it may still be worth it.
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
    if MPI:
        return numpy.arange(start, end, dtype=PETSc.IntType)
    else:
        return numpy.arange(start, end, dtype='i')

def _filter(scope, lst):
    filtered = _filter_subs(lst)
    filtered = _filter_flat(scope, filtered)
    return _filter_ignored(scope, filtered)

def _filter_subs(lst):
    """Return a copy of the list with any subvars of basevars in the list
    removed.
    """
    bases = [n.split('[',1)[0] for n in lst]

    return [n for i,n in enumerate(lst)
               if not (bases[i] in lst and n != bases[i])]

def _filter_ignored(scope, lst):
    # Remove any vars that the user designates as 'deriv_ignore'
    unignored = []
    topvars = scope._system.vector_vars

    for name in lst:
        collapsed_name = scope.name2collapsed[name]
        if collapsed_name in topvars and topvars[collapsed_name].get('deriv_ignore'):
            continue

        # The user sets 'deriv_ignore' in the basevar, so we have to check that for
        # subvars.
        base = base_var(scope._depgraph, name)
        if base != name:
            collname = scope.name2collapsed.get(base)
            if collname and collname in topvars and \
               topvars[collname].get('deriv_ignore'):
                continue

        unignored.append(name)

    return unignored

def _filter_flat(scope, lst):
    return [n for n in lst if not scope._var_meta[n].get('noflat')]
