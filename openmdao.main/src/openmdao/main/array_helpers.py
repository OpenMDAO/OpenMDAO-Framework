""" Some functions for dealing with array bookkeeping. Mostly flatteners. """

import itertools


from openmdao.main.interfaces import IVariableTree
from traits.trait_handlers import TraitListObject

from openmdao.util.typegroups import real_types, int_types

from numpy import ndarray, ravel_multi_index, prod, arange, array


class IndexGetter(object):
    """A simple class the returns the slice object or index used
    to call its __getitem__ method.
    """
    def __getitem__(self, idx):
        return idx

_flat_idx_cache = {}
_idx_cache = {}
_eval_globals = {'_idx_getter': IndexGetter() }


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
        # is intentional since we want to avoid caching any indices
        # that aren't constant.
        _idx_cache[idxstr] = idx = eval('_idx_getter'+newstr, _eval_globals)
    return idx
    
def get_val_and_index(scope, name):
    """Return a tuple of (value, index) for the given name, 
    which may contain array element access.
    """
    if '[' in name:
        val = getattr(scope, name.split('[',1)[0])
        idx = get_index(name)
        # for objects that are not numpy arrays, an index tuple
        #  really means [idx0][idx1]...[idx_n]
        if isinstance(idx, tuple) and not isinstance(val, ndarray):
            for i in idx:
                val = val[i]
        else:
            val = val[idx]
        return (val, idx)
    else:
        return (getattr(scope, name), None)

def to_slice(idxs):
    """Convert an index array to a slice if possible. Otherwise,
    return the index array.
    """
    if isinstance(idxs, slice):
        return idxs
    elif isinstance(idxs, ndarray):
        if len(idxs) == 1:
            return slice(idxs[0], idxs[0]+1)
        elif len(idxs) == 0:
            return slice(0,0)

        imin = idxs.min()
        imax = idxs.max()
        stride = idxs[1]-idxs[0]

        for i in xrange(idxs):
            if i and idxs[i] - idxs[i-1] != stride:
                return idxs
            return slice(imin, imax+1, stride)
    elif isinstance(idxs, int_types):
        return slice(idxs, idxs+1)
    else:
        raise RuntimeError("can't convert indices of type '%s' to a slice" %
                            str(type(idxs)))
        
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
    if len(idxs) == 1:
        # _flat_idx_cache[(sindex, shape)] = slice(idxs[0], idxs[0]+1)
        # return slice(idxs[0], idxs[0]+1)
        _flat_idx_cache[(sindex, shape)] = idxs[0]
        return idxs[0]

    # see if we can convert the discrete list of indices 
    # into a single slice object
    idxs = to_slice(idxs)
        
    if isinstance(idxs, slice):
        _flat_idx_cache[(sindex, shape)] = idxs
    else:
        # if all else fails, return a discrete list of indices into
        # the flat array.
        _flat_idx_cache[(sindex, shape)] = idxs.copy()
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
        return idx[0]
    else:  # simple index
        return idx

def get_var_shape(name, scope):
    meta = scope.get_metadata(name, 'data_shape')
    if meta:
        return meta
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

    return is_differentiable_val(scope.get(name))

def is_differentiable_val(val):
    if isinstance(val, int_types):
        return False
    elif isinstance(val, real_types):
        return True
    elif isinstance(val, ndarray) and str(val.dtype).startswith('float'):
        return True
    elif IVariableTree.providedBy(val):
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

    elif isinstance(val, TraitListObject):
        sz = len(val)
        if sz != 0 and not isinstance(val[0], int_types) and isinstance(val[0], real_types):
            return len(val)
        # else fall through and exception
    else:
        getsize = getattr(val, 'get_flattened_size', None)
        if getsize is not None:
            return getsize()
        
        elif scope is not None:
            dshape = scope.get_metadata(name.split('[')[0]).get('data_shape')

            # Custom data objects with data_shape in the metadata
            if dshape:
                return prod(dshape)

    raise TypeError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float (or complex) array. An exception 
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
    elif IVariableTree.providedBy(val):
        vals = []
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            vals.extend(flattened_value('.'.join((name, key)), value))
        return array(vals)
    elif isinstance(val, TraitListObject): #FIXME: list must contain floats
        return array(val)

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

