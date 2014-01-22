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

def is_differentiable_var(name, scope):
    meta = scope.get_metadata(name)
    if 'data_shape' in meta and meta['data_shape']:
        return True
    if is_differentiable_val(getattr(scope, name)):
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

    # Floats
    if isinstance(val, float):
        return 1

    # Numpy arrays
    elif isinstance(val, ndarray): # FIXME: should check dtype
        return val.size

    # Variable Trees
    elif isinstance(val, VariableTree):
        size = 0
        for key in sorted(val.list_vars()):  # Force repeatable order.
            size += flattened_size('.'.join((name, key)), getattr(val, key))
        return size

    else:
        dshape = scope.get_metadata(name.split('[')[0]).get('data_shape')

        # Custom data objects with data_shape in the metadata
        if dshape:
            return prod(dshape)

    raise TypeError('Variable %s is of type %s which is not convertable'
                    ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float array. """
    if isinstance(val, float):
        return array([val])
    elif isinstance(val, ndarray):
        return val.flatten()
    elif isinstance(val, VariableTree):
        vals = []
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            vals.extend(flattened_value('.'.join((name, key)), value))
        return array(vals)
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))


def flattened_names(name, val, names=None):
    """ Return list of names for values in `val`. """
    if names is None:
        names = []
    if isinstance(val, float):
        names.append(name)
    elif isinstance(val, ndarray):
        for i in range(len(val)):
            value = val[i]
            flattened_names('%s[%s]' % (name, i), value, names)
    elif isinstance(val, VariableTree):
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            flattened_names('.'.join((name, key)), value, names)
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))
    return names


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




