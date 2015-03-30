import sys
from collections import OrderedDict, namedtuple
import numpy
from numpy import ndarray, zeros

from openmdao.main.mpiwrap import MPI, create_petsc_vec, PETSc, make_idx_array, to_idx_array
from openmdao.main.array_helpers import offset_flat_index, \
                                        get_flat_index_start, get_val_and_index, get_shape, \
                                        get_flattened_index, to_slice, to_indices
from openmdao.main.interfaces import IImplicitComponent
from openmdao.util.typegroups import int_types
from openmdao.util.graph import base_var

ViewInfo = namedtuple('ViewInfo', 'view, start, idxs, size, hide')


class VecWrapperBase(object):
    """A wrapper object for a local vector, a distributed PETSc vector,
    and info about what var maps to what range within the distributed
    vector.
    """
    def __init__(self, system, array, name=''):
        self.array = array
        self.name = name
        self._info = OrderedDict() # dict of ViewInfos

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
        """Add all srcs and dests from var tuples so that views for
        particular openmdao variables can be accessed.
        """
        for tup in tups:
            names = set([tup[0]]+list(tup[1])) # src + dests
            for name in names:
                self._add_aliasview(name, tup)

    def _add_aliasview(self, name, base):
        """Add a view that just points to an existing view using a different
        name.
        """
        info = self._info[base]
        newinfo = ViewInfo(info.view, info.start, info.idxs, info.size, True)
        self._info[name] = newinfo

    def _add_subview(self, scope, name):
        var = scope._var_meta[name]
        name2collapsed = scope.name2collapsed

        sz = var['size']
        if sz > 0 and not var.get('noflat'):
            idx = var['flat_idx']
            base = self._info[name2collapsed[var['basevar']]]
            sub_idx = offset_flat_index(idx, base.start)
            substart = get_flat_index_start(sub_idx)
            self._info[name] = ViewInfo(base.view, substart, to_slice(idx),
                                        len(to_indices(idx, base.view)), True)

            if self.array[sub_idx].size != sz:
                raise RuntimeError("size mismatch: in system %s, view for %s is %s, idx=%s, size=%d" %
                                     (system.name, name,
                                     list(self.bounds(name)),
                                     sub_idx,self.array[sub_idx].size))

    def __getitem__(self, name):
        view, _, idxs, _, _ = self._info[name]
        return view[idxs]

    def __setitem__(self, name, value):
        view, _, idxs, _, _ = self._info[name]
        if isinstance(value, ndarray):
            try:
                view[idxs] = value.flat
            except Exception as err:
                if value.shape != view[idxs].shape:
                    msg = "Array size mis-match in '%s'. " % name[0]
                    msg += "Initial shape was %s " % str(view[idxs].shape)
                    msg += "but found size %s at runtime" % str(value.shape)
                else:
                    msg = "cannot set array %s to value:\n %s\n %s" % \
                                    (name, str(value), str(err))
                raise RuntimeError(msg)
        else:
            view[idxs] = value

    def __contains__(self, name):
        return name in self._info

    def keys(self):
        """Return list of names of views."""
        return [k for k,v in self._info.items() if not v.hide]

    def items(self):
        """Return list of (name, view) for each view."""
        return [(k, v.view) for k,v in self._info.items() if not v.hide]

    def start(self, name):
        """Return the starting index for the array view belonging
        to the given name. name may contain array indexing.
        """
        return self._info[name].start

    def bounds(self, names):
        """Return the bounds corresponding to the slice occupied
        by the named variable(s) within the flattened array.
        name may contain array indexing.  Note that if all of the
        names given are not contiguous, the bounds will encompass
        the named variables AND any variables between them.
        """
        if isinstance(names, basestring):
            info = self._info[names]
            return (info.start, info.start + info.size)

        infos = [self._info[n] for n in names]
        bnds = [(i.start, i.start+i.size) for i in infos]
        return (min([u for u,v in bnds]), max([v for u,v in bnds]))

    def indices(self, system, name):
        """Return the index array corresponding to a single name."""
        if name not in self._info:
            scope = system.scope
            if isinstance(name, basestring):
                name = scope.name2collapsed[name]
            if name[0].split('[',1)[0] in self._info:
                self._add_subview(scope, name)
                self._add_tuple_members(system, [name])
        _, start, _, size, _ = self._info[name]
        return make_idx_array(start, start+size)

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
            size = self[name].size
            end += size
            if end > asize:
                raise ValueError("end index %d exceeds size of target array" % (end-1))
            self[name] = arr[start:end]
            start += size

    def _is_var_idx(self, info, idx):
        if isinstance(info.idxs, slice):
            if info.idxs.step == 1 or info.idxs.step is None:
                return idx >= info.idxs.start and idx < info.idxs.stop
            return idx in to_indices(info.idxs, info.view)
        return idx in info.idxs

    def find_var(self, idx):
        for name, info in self._info.items():
            if not info.hide and self._is_var_idx(info, idx):
                return name
        for name, info in self._info.items():
            if info.hide and self._is_var_idx(info, idx):
                return name

    def dump(self, verbose=False, stream=sys.stdout):
        for name, info in self._info.items():
            if verbose or not info.hide:
                stream.write("%s - %s: (%d,%d) %s\n" %
                           (self.name, info.view[info.idxs], info.start,
                            info.start+len(info.view[info.idxs]), name))
        if self.petsc_vec is not None:
            stream.write("%s - petsc sizes: %s\n" % (self.name, self.petsc_vec.sizes))


class VecWrapper(VecWrapperBase):
    def _initialize(self, system):
        scope = system.scope
        name2collapsed = scope.name2collapsed
        flat_vars = system.flat_vars
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
                self._info[name] = ViewInfo(self.array[start:end], start, slice(None),
                                            end-start, False)

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
            for name, var in flat_vars.items():
                if name not in vector_vars:
                    self._add_subview(scope, name)

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

            self._info[resids[0]] = ViewInfo(view, start, slice(None),
                                             end-start, True)

    def _map_resids_to_states(self, system):
        # add any mappings of residuals to states
        for resid, state in system._mapped_resids.items():
            if resid not in self._info:
                self._add_aliasview(resid, state)

    def set_from_scope(self, scope, vnames=None):
        """Get the named values from the given scope and set flattened
        versions of them in our array.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            if isinstance(name, tuple):
                self[name] = scope.get_flattened_value(name[0]).real
            else:
                self[name] = scope.get_flattened_value(name).real

    def set_from_scope_complex(self, scope, vnames=None):
        """Get the named values from the given scope and set flattened
        versions of just the complex portion into our array.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            if isinstance(name, tuple):
                self[name] = scope.get_flattened_value(name[0]).imag
            else:
                self[name] = scope.get_flattened_value(name).imag

    def set_to_scope(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            if isinstance(name, tuple):
                array_val = self[name]
                scope.set_flattened_value(name[0], array_val)
                for dest in name[1]:
                    if dest != name[0]:
                        scope.set_flattened_value(dest, array_val)
                        #print "scope set", dest, array_val
            else:
                scope.set_flattened_value(name, self[name])
                #print "scope set", name, self[name]


class InputVecWrapper(VecWrapperBase):
    def _initialize(self, system):
        scope = system.scope
        varmeta = scope._var_meta
        name2collapsed = scope.name2collapsed
        flat_ins = _filter_flat(scope, system._owned_args)
        start, end = 0, 0

        #print "%s: %s: %s" % (system.name, type(system), flat_ins)
        for sub in system.simple_subsystems():
            #print "SUB %s: %s  _in_nodes = %s" % (sub.name, type(sub),sub._in_nodes)
            for name in [n for n in system.vector_vars if n in sub._in_nodes]:
                if name in flat_ins and name not in self._info:
                    arg_idx = sub.get_distrib_idxs(name)
                    if arg_idx is None:
                        continue
                    sz = len(arg_idx) #arg_idx[name])
                    end += sz
                    self._info[name] = ViewInfo(self.array[start:end], start,
                                                slice(None), end-start, False)
                    if end-start != self.array[start:end].size:
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

            self._add_subview(scope, name)

    def _map_resids_to_states(self, system):
        pass

    def get_dests_by_comp(self):
        """Return a dict of comp name keyed to a list of input nodes, with
        any subvars removed that have basevars in the vector.
        """
        ret = OrderedDict()
        for node in self._info.keys():
            if isinstance(node, tuple) and len(node) > 1:
                src, dests = node
                for d in dests:
                    cname, _, vname = d.partition('.')
                    ret.setdefault(cname, []).append(node)

        for cname, in_nodes in ret.items():
            bases = [n[0] for n in in_nodes if n[0].split('[',1)[0]==n[0]]
            ret[cname] = [n for n in in_nodes
                             if n[0] in bases or n[0].split('[',1)[0] not in bases]

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
            array_val = self[name]
            if isinstance(name, tuple):
                for dest in name[1]:
                    scope.set_flattened_value(dest, array_val)
                    #print "scope set", dest, array_val
            else:
                scope.set_flattened_value(name, array_val)
                #print "scope set", name, array_val

    def set_to_scope_complex(self, scope, vnames=None):
        """Pull values for the given set of names out of our array
        and set them into the given scope as the complex part of the value.
        """
        if vnames is None:
            vnames = self.keys()
        else:
            vnames = [n for n in vnames if n in self]

        for name in vnames:
            step = self[name]

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

        try:
            var_idxs, input_idxs = merge_idxs(var_idxs, input_idxs)
        except Exception as err:
            raise RuntimeError("ERROR creating scatter for system %s in scope %s: %s" %
                                (system.name, str(system.scope), str(err)))

        self.var_idxs = to_slice(var_idxs)
        self.input_idxs = to_slice(input_idxs)

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
                # print "PETSC srcs: %s" % var_idx_set.indices
                # print "PETSC dests: %s" % input_idx_set.indices
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
            #print "%s for %s\n%s <-- %s" % (destvec.name.rsplit('.', 1)[1],
            #                                destvec.name.rsplit('.',1)[0],
            #                                list(self.scatter_conns),
            #                                src[self.input_idxs if addv else self.var_idxs]); sys.stdout.flush()
            #print destvec.name
            #print self.scatter_conns
            #print srcvec.keys(), '-->'
            #print '-->', destvec.keys()
            #print "Before", srcvec.array, destvec.array
            #print "Before: %s" % str(system.name)
            #srcvec.dump()
            #print ''
            #destvec.dump()
            self.scatter.scatter(src, dest, addv=addv, mode=mode)
            #print "After", srcvec.array, destvec.array
            #print "After:"
            #destvec.dump()
            #print '.'


        if destvec.name.endswith('.p') and self.noflat_vars:
            if MPI:
                raise NotImplementedError("passing of non-flat vars %s has not been implemented yet" %
                                          self.noflat_vars) # FIXME
            else:
                for src, dests in self.noflat_vars:
                    for dest in dests:
                        if src != dest:
                            try:
                                system.scope.set(dest,
                                                 system.scope.get_attr_w_copy(src))
                            except Exception:
                                system.scope.reraise_exception("cannot set '%s' from '%s'" %
                                                               (dest, src), sys.exc_info())

    def dump(self, system, srcvec, destvec, nest=0, stream=sys.stdout):
        if not self.scatter_conns:
            return
        stream.write(" "*nest)
        stream.write("Scatters for %s:\n" % system.name)
        stream.write(" "*nest)
        stream.write("scatter vars: %s\n" % sorted(self.scatter_conns))
        stream.write(" "*nest)
        stream.write("%s --> %s\n" % (self.var_idxs, self.input_idxs))
        stream.write(" "*nest)
        stream.write("local array = %s\n" % srcvec.array)
        var_idxs = to_indices(self.var_idxs, zeros(numpy.sum(system.local_var_sizes)))
        input_idxs = self.input_idxs
        stream.write(" "*nest)
        stream.write("%s --> %s\n" % (var_idxs, input_idxs))

        if MPI and system.app_ordering:
            var_idx_set = system.app_ordering.app2petsc(var_idxs)
            stream.write(" "*nest)
            stream.write("(petsc): %s --> %s\n" % (var_idx_set, input_idxs))
        if self.noflat_vars:
            stream.write(" "*nest)
            stream.write("no-flats: %s\n" % self.noflat_vars)


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

def merge_idxs(src_idxs, dest_idxs):
    """Return source and destination index arrays, built up from
    smaller index arrays and combined in order of ascending source
    index (to allow us to convert src indices to a slice in some cases).
    """
    assert(len(src_idxs) == len(dest_idxs))

    # filter out any zero length idx array entries
    src_idxs = [i for i in src_idxs if len(i)]
    dest_idxs = [i for i in dest_idxs if len(i)]

    if len(src_idxs) == 0:
        return make_idx_array(0, 0), make_idx_array(0,0)

    src_tups = list(enumerate(src_idxs))

    src_sorted = sorted(src_tups, key=lambda x: x[1].min())

    new_src = [idxs for i, idxs in src_sorted]
    new_dest = [dest_idxs[i] for i,_ in src_sorted]

    return idx_merge(new_src), idx_merge(new_dest)

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

def dedup(lst):
    """Remove duplicates from the list while maintaining original order"""
    seen = set()
    return [x for x in lst if x not in seen and not seen.add(x)]
