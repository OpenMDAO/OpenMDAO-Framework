import numbers
import array

real_types = [numbers.Real]
int_types = [numbers.Integral]
complex_types = [numbers.Complex]
iterable_types = [set, list, tuple, array.array]

try:
    import numpy
except ImportError:
    pass
else:
    real_types.extend([numpy.float32, numpy.float64])
    int_types.extend([numpy.int32, numpy.int64])
    complex_types.extend([numpy.complex])
    iterable_types.append(numpy.ndarray)
    
# use these with isinstance to test for various types that include builtins
# and numpy types (if numpy is available)
    
complex_or_real_types = tuple(real_types+complex_types)
real_types = tuple(real_types)
int_types = tuple(int_types)
complex_types = tuple(complex_types)
iterable_types = tuple(iterable_types)
