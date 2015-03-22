""" Some functions for dealing with array bookkeeping. Mostly flatteners. """

import itertools


from openmdao.main.interfaces import IVariableTree
from openmdao.main.exceptions import NoFlatError

from traits.trait_handlers import TraitListObject

from openmdao.util.typegroups import real_types, int_types

from numpy import ndarray, ravel_multi_index, prod, arange, array, zeros


class IndexGetter(object):
    """A simple class the returns the slice object or index used
    to call its __getitem__ method.
    """
    def __getitem__(self, idx):
        return idx

_flat_idx_cache = {}
_idx_cache = {}
_eval_globals = {'_idx_getter': IndexGetter() }

__missing = object()

def get_index(name):
    """Return the index (int or slice or tuple combination)
    associated with the given string, e.g. x[1:3, 5] would return
    a (slice(1,3),5) tuple.  This value can be passed into
    an object's __getitem__ method, e.g., myval[idx], in order
    to retrieve a particular slice from that object without
    having to parse the index more than once.

    If name contains an index containing any non-literals, an
    exception will be raised.
    """

    try:
        idxstr = name[name.index('['):]
    except ValueError:
        return None

    idx = _idx_cache.get(idxstr)
    if idx is None:
        newstr = idxstr.replace('][',',')
        # _eval_globals dict contains nothing but _idx_getter, so
        # eval will fail if index contains any non-literals. This
        # is intentional since we don't want to allow any indices
        # that aren't constant.
        _idx_cache[idxstr] = idx = eval('_idx_getter'+newstr, _eval_globals)
    return idx

def get_val_and_index(scope, name):
    """Return a tuple of (value, index) for the given name,
    which may contain array element access.
    """
    if '[' in name:
        return (scope.get(name), get_index(name))
    else:
        return (scope.get(name), None)

def idx_size(idxs, size=None):
    """Return the number of entries corresponding to the given
    indices.  idxs can be a slice, an index array, or a simple index.
    slices with negative values for start, stop, or stride are not
    supported unless the 'size' arg is provided. slices with stop
    values of None are also not supported without 'size'.
    """
    if isinstance(idxs, slice):
        if size is not None:
            start, stop, step = idxs.indices(size)
        else:
            start = 0 if idxs.start is None else idxs.start
            stop = idxs.stop
            step = 1 if idxs.step is None else idxs.step
            if stop is None:
                raise RuntimeError("can't get size of slice with stop of None")
            elif start < 0 or stop < 0:
                raise RuntimeError("negative start or stop not allowed for slice unless size is provided")

        sz = 0
        i = start
        if step > 0:
            while i < stop:
                sz += 1
                i += step
        elif step < 0:
            while i > stop:
                sz += 1
                i += step
        return sz

    elif isinstance(idxs, ndarray):
        return len(idxs)
    elif isinstance(idxs, int_types):
        return 1
    else:
        raise RuntimeError("can't get size for indices of type '%s'" %
                            str(type(idxs)))

def to_slice(idxs):
    """Convert an index array or list to a slice if possible. Otherwise,
    return the index array or list.
    """
    if isinstance(idxs, slice):
        return idxs
    elif isinstance(idxs, ndarray) or isinstance(idxs, list):
        if len(idxs) == 1:
            return slice(idxs[0], idxs[0]+1)
        elif len(idxs) == 0:
            return slice(0,0)

        if isinstance(idxs, ndarray):
            imin = idxs.min()
            imax = idxs.max()
        else:
            imin = min(idxs)
            imax = max(idxs)

        stride = idxs[1]-idxs[0]

        if stride == 0:
            return idxs

        for i in xrange(len(idxs)):
            if i and idxs[i] - idxs[i-1] != stride:
                return idxs

        if stride < 0:
            ## negative strides cause some failures, so just do positive for now
            #return slice(imax+1, imin, stride)
            return idxs
        else:
            return slice(imin, imax+1, stride)
    elif isinstance(idxs, int_types):
        return slice(idxs, idxs+1)
    else:
        raise RuntimeError("can't convert indices of type '%s' to a slice" %
                            str(type(idxs)))

def to_indices(idxs, val=None):
    """Convert an slice or simple index into an index array.
    index arrays are just returned unchanged.
    """
    if isinstance(idxs, slice):
        start, stop, step = idxs.indices(len(val))
        iarr = zeros(stop-start, dtype='i')
        count = 0
        i = start
        if step > 0:
            while i < stop:
                iarr[count] = i
                count += 1
                i += step
        elif step < 0:
            while i > stop:
                iarr[count] = i
                count += 1
                i += step
        else:
            raise ValueError("slice step cannot be zero")

        return iarr[:count+1]

    elif isinstance(idxs, ndarray):
        return idxs

    elif isinstance(idxs, int_types):
        return array([idxs], 'i')

    elif isinstance(idxs, tuple):
        return get_flattened_index(idxs, val.shape, cvt_to_slice=False)

    else:
        raise RuntimeError("can't convert indices of type '%s' to an index array" %
                            str(type(idxs)))

def get_flattened_index(index, shape, cvt_to_slice=True):
    """Given an index (int, slice, or tuple of ints and slices), into
    an array, return the equivalent index into a flattened version
    of the array.

    """
    global _flat_idx_cache

    sindex = str(index)
    fidx = _flat_idx_cache.get((sindex, shape, cvt_to_slice))
    if fidx is not None:
        return fidx

    if not isinstance(index, (tuple, list, ndarray)):
        index = (index,)

    if len(index) < len(shape):
        index = list(index)
        for i in range(len(shape)-len(index)):
            index.append(slice(None))

    indices = []
    for idx, size in zip(index, shape):
        i = arange(size)[idx]
        if not isinstance(i, ndarray):
            i = array([i])
        indices.append(i)

    if len(indices) > 1:
        indices = zip(*itertools.product(*indices))

    idxs = ravel_multi_index(indices, dims=shape)

    # see if we can convert the discrete list of indices
    # into a single slice object
    if cvt_to_slice:
        idxs = to_slice(idxs)

    if isinstance(idxs, slice):
        _flat_idx_cache[(sindex, shape, cvt_to_slice)] = idxs
    else:
        # if all else fails, return a discrete list of indices into
        # the flat array.
        _flat_idx_cache[(sindex, shape, cvt_to_slice)] = idxs.copy()

    return idxs

def offset_flat_index(idx, offset):
    """Return an index into a flat array with the
    given offset applied.  All indices are assumed
    to have been converted to explicit form, so no
    negative indices, slices with ':', or tuples
    are allowed.
    """
    if isinstance(idx, slice):
        return slice(idx.start+offset, idx.stop+offset, idx.step)
    else:  # simple index or index array
        return idx + offset

def get_flat_index_start(idx):
    """Return the starting simple index for the given
    simple index, slice, or array idx.  All indices are
    assumed to have been converted to explicit form, so no
    negative indices, slices with ':', or tuples
    are allowed.
    """
    if isinstance(idx, slice):
        return idx.start
    elif isinstance(idx, ndarray):  # index array
        return idx.min()
    else:  # simple index
        return idx

def get_var_shape(name, scope):
    val = scope.get(name)
    if isinstance(val, ndarray):
        if val.shape == ():
            return (1,)
        return val.shape
    if isinstance(val, real_types):
        return (1,)

    if IVariableTree.providedBy(val):
        raise NotImplementedError("get_var_shape not supported for vartrees")

    sz = flattened_size(name, val, scope)
    if sz:
        return (sz,)

    return None

def is_differentiable_var(name, scope):
    return is_differentiable_val(scope.get(name))

def is_differentiable_val(val):
    if isinstance(val, int_types):
        return False
    elif isinstance(val, real_types):
        return True
    elif isinstance(val, ndarray) and (str(val.dtype).startswith('float') or \
                                       str(val.dtype).startswith('complex')):
        return True
    return False

def flattened_size(name, val, scope=None):
    """ Return size of `val` flattened to a 1D float array. Raises
    a NoFlatError if the value is not convertible to a 1D float array.
    """

    # have to check int_types before real_types because apparently
    # int_types are considered also to be real types
    if isinstance(val, int_types):
        pass # fall through to exception

    # Floats
    elif isinstance(val, real_types):
        return 1

    # Numpy arrays
    elif isinstance(val, ndarray) and (str(val.dtype).startswith('float') or \
                                       str(val.dtype).startswith('complex')):
        return val.size

    elif isinstance(val, TraitListObject):
        sz = len(val)
        if sz != 0 and not isinstance(val[0], int_types) and isinstance(val[0], real_types):
            return len(val)
        # else fall through and exception
    else:
        getsize = getattr(val, 'get_flattened_size', None)
        if getsize is not None:
            return getsize()

    raise NoFlatError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float (or complex) array. A NoFlatError
    will be raised if val is not completely flattenable to a float
    array.  A VariableTree is not considered completely
    flattenable unless all of its leaf nodes are flattenable.
    """
    # have to check int_types before real_types because apparently
    # int_types are considered also to be real types
    if isinstance(val, int_types):
        pass  # fall through to exception
    elif isinstance(val, real_types):
        return array([val])
    elif isinstance(val, complex):
        return array([val])
    elif isinstance(val, ndarray):
        return val.flatten()
    elif isinstance(val, TraitListObject): #FIXME: list must contain floats
        # HACK: just check first value
        if len(val) > 0 and (isinstance(val[0], int_types) or \
                        not isinstance(val[0], real_types)):
            pass # fall through to exception
        else:
            return array(val)
    elif hasattr(val, 'get_flattened_value'):
        return val.get_flattened_value()

    raise NoFlatError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def get_shape(val):
    """Return a shape tuple for the given value. This will work
    for nested lists and tuples, but only if all subs at every
    level have the same length.
    """
    if isinstance(val, ndarray):
        return val.shape
    if isinstance(val, (list, tuple)) and len(val) > 0:
        entry = val
        shape = []
        while isinstance(entry, (list, tuple)):
            shape.append(len(entry))
            if shape[-1] > 0:
                entry = entry[0]
        return tuple(shape)
    return ()

def flatten_slice(index, shape, name='flat_index', offset=0):
    """ Return a string index that flattens an arbitrary slice denoted by
    'index' into an matrix of shape 'shape'.

    index: string
        OpenMDAO string index

    shape: tuple
        Numpy style shape tuple

    name: string
        Name for the returned var in the string, default is 'ix'

    offset: int
        Starting index for target flat slice
    """

    # Handle complicated slices that may have : or -1 in them
    # We just use numpy index math to convert unravelable indices into
    # index arrays so that we can ravel them to find the set of indices
    # that we need to grab from J.
    idx = index.replace(' ', '').replace('][', ',').strip(']').strip('[')
    idx = idx.strip('(').strip(')')
    if '-' in idx or ':' in idx:

        idx_list = idx.split(',')
        indices = []
        for index, size in zip(idx_list, shape):
            temp = eval('arange(size)[%s]' % index)
            if not isinstance(temp, ndarray):
                temp = array([temp])
            indices.append(temp)

        if len(indices) > 1:
            indices = zip(*itertools.product(*indices))
        flat_index = ravel_multi_index(indices, dims=shape) + offset
        istring = name

    # Multi-integer index into a multi-D array
    elif ',' in idx:
        idx = list(eval(idx))
        flat_index = ravel_multi_index(idx, shape) + offset
        istring = '%s:%s+1' % (name, name)

    # Single integer index into a 1D array
    else:
        flat_index = int(idx) + offset
        istring = '%s:%s+1' % (name, name)

    return istring, flat_index
