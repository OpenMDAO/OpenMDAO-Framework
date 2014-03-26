""" Some functions for dealing with array bookkeeping. Mostly flatteners. """

import itertools

from openmdao.main.vartree import VariableTree
from openmdao.util.typegroups import real_types, int_types

try:
    from numpy import ndarray, ravel_multi_index, prod, arange, array

except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, arange, array


class IndexGetter(object):
    """A simple class the returns the slice object used
    to call its __getitem__ method.
    """
    def __getitem__(self, idx):
        return idx

_idx_cache = {}
_idx_getter = IndexGetter()


def get_index(name):
    """Return the index (int or slice or tuple combination) 
    associated with the given string, e.g. x[1:3, 5] would return 
    a (slice(1,3),5) tuple.  This value can be passed into
    an object's __getitem__ method, e.g., myval[idx], in order
    to retrieve a particular slice from that object without 
    having to parse the index more than once.
    """
    global _idx_getter, _idx_cache
    name = name.replace('][',',')
    brack = name.index('[')
    if brack < 0:
        return None
    idxstr = name[brack:]
    idx = _idx_cache.get(idxstr)
    if idx is None:
        _idx_cache[idxstr] = idx = eval('_idx_getter'+idxstr)
    return idx
    
def get_val(scope, name):
    """Return the value of the attr with the given name.
    name may contain element access, e.g. x[2:8]
    """
    if '[' in name:
        val = getattr(scope, name.split('[',1)[0])
        idx = get_index(name)
        # for object that are not numpy arrays, an index tuple
        #  really means [idx0][idx1]...[idx_n]
        if isinstance(idx, tuple) and not isinstance(val, ndarray):
            for i in idx:
                val = val[i]
        else:
            val = val[idx]
        return val
    else:
        return getattr(scope, name)

def is_differentiable_var(name, scope):
    meta = scope.get_metadata(name)
    if 'data_shape' in meta and meta['data_shape']:
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

def group_flattened_indices(names, scope):
    """Return a list of slices corresponding to each name's value, 
    as well as the total flattened size of the vector
    of flattened values. Names may be subvars.
    """
    pass

def group_flattened_vec(names, vec=None):
    """Will fill vec with the flattened representation
    of the variables specified in names.  If vec is not
    provided, a new vector will be allocated and returned.
    """
    pass

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
        dshape = scope.get_metadata(name).get('data_shape')

        # Custom data objects with data_shape in the metadata
        if dshape:
            return prod(dshape)

    raise TypeError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float array. """
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

