""" Some functions for dealing with array bookkeeping. Mostly flatteners. """

import itertools

from openmdao.main.vartree import VariableTree, get_val_and_index
from openmdao.util.typegroups import real_types, int_types

try:
    from numpy import ndarray, ravel_multi_index, prod, arange, array

except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, arange, array



_flat_idx_cache = {}

def get_flattened_index(index, shape):
    """Given an index (int, slice, or tuple of ints and slices), into
    an array, return the equivalent index into a flattened version 
    of the array.

    """
    global _flat_idx_cache

    sindex = str(index)
    fidx = _flat_idx_cache.get((sindex, shape))
    if fidx:
        return fidx

    if not isinstance(index, (tuple, list)):
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
    if len(idxs) == 1:
        return idxs[0]

    # see if we can convert the discrete list of indices 
    # into a single slice object
    imin = min(idxs)
    imax = max(idxs)
    stride = idxs[1]-idxs[0]
    if all(arange(imin, imax+1, stride) == list(idxs)):
        _flat_idx_cache[(sindex, shape)] = slice(imin, imax+1, stride)
        return slice(imin, imax+1, stride)
        
    # if all else fails, return a discrete list of indices into
    # the flat array.
    _flat_idx_cache[(sindex, shape)] = idxs.copy()
    return idxs

def offset_flat_index(idx, offset):
    """Return an index into a flat array with the
    given offset applied.  All indices are assumed
    to have been converted to explicit form, so no
    negative indices, slices with ':', etc. are
    allowed.
    """
    if isinstance(idx, tuple):
        pass
    elif isinstance(idx, slice):
        pass
    elif isinstance(idx, ndarray):  # index array
        pass
    else:  # simple index
        return idx + offset

def get_flat_index_start(parent_start, idx):
    if isinstance(idx, tuple):
        pass
    elif isinstance(idx, slice):
        pass
    elif isinstance(idx, ndarray):  # index array
        pass
    else:  # simple index
        return parent_start + idx

def get_var_shape(name, scope):
    meta = scope.get_metadata(name, 'data_shape')
    if meta:
        return meta
    val = getattr(scope, name)
    if isinstance(val, ndarray):
        return val.shape
    if isinstance(val, real_types):
        return (1,)

    if isinstance(val, VariableTree):
        raise NotImplementedError("get_var_shape not supported for vartrees")
        sz = flattened_size(name, val, scope)
        if sz:
            return (sz,)

    return None

def flattened_size_info(name, scope):
    """Return the local flattened size of the variable with
    the given name along with its flattened index into
    its basevar and its basevar name (if it has one). If it
    doesn't have a basevar, then index and basevar name are
    None.
    """
    # TODO: add checking of local_size metadata...
    parts = name.split('.')
    if len(parts) > 1:  
        vt = getattr(scope, parts[0]) # vartree reference
        obj = vt
        for part in parts[1:-1]:
            obj = getattr(obj, part)
        val, idx = get_val_and_index(obj, parts[-1])
    else:
        vt = None
        val, idx = get_val_and_index(scope, name)

    if vt is not None:  # name is a vartree subvar
        base = vt.name
        if '[' in name:  # array ref inside of a vartree
            raise NotImplementedError("no support yet for array element access within vartrees")
        else:
            flat_idx = vt.get_flattened_index(name[len(base)+1:])
    elif '[' in name:  # array index into basevar
        base = name.split('[',1)[0]
        flat_idx = get_flattened_index(idx, get_var_shape(base, scope))
    else:
        base = None
        flat_idx = None
        
    return (flattened_size(name, val, scope=scope), flat_idx,  base)

def is_differentiable_var(name, scope):
    meta = scope.get_metadata(name, 'data_shape')
    if meta:
        return True

    if is_differentiable_val(scope.get(name)):
        return True
    return False

def is_differentiable_val(val):
    if isinstance(val, int_types):
        return False
    elif isinstance(val, real_types):
        return True
    elif isinstance(val, ndarray) and str(val.dtype).startswith('float'):
        return True
    elif isinstance(val, VariableTree):
        return all([is_differentiable_val(getattr(val,k)) for k in val.list_vars()])
    return False

def flattened_size(name, val, scope=None):
    """ Return size of `val` flattened to a 1D float array. """

    # have to check int_types before real_types because apparently
    # int_types are considered also to be real types
    if isinstance(val, int_types):
        pass # fall through to exception

    # Floats
    elif isinstance(val, real_types):
        return 1

    # Numpy arrays
    elif isinstance(val, ndarray) and str(val.dtype).startswith('float'):
        return val.size

    # Variable Trees
    elif isinstance(val, VariableTree):
        size = 0
        for key in sorted(val.list_vars()):  # Force repeatable order.
            size += flattened_size('.'.join((name, key)), 
                                   getattr(val, key))
        return size

    elif scope is not None:
        dshape = scope.get_metadata(name,'data_shape')

        # Custom data objects with data_shape in the metadata
        if dshape:
            return prod(dshape)

    raise TypeError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float array. An exception will be
    raised if val is not completely flattenable to a float
    array.  A VariableTree is not considered completely 
    flattenable unless all of its leaf nodes are flattenable.
    """
    # have to check int_types before real_types because apparently
    # int_types are considered also to be real types
    if isinstance(val, int_types): 
        pass  # fall through to exception
    if isinstance(val, real_types):
        return array([val])
    elif isinstance(val, ndarray):
        return val.flatten()
    elif isinstance(val, VariableTree):
        vals = []
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            vals.extend(flattened_value('.'.join((name, key)), value))
        return array(vals)

    raise TypeError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

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

