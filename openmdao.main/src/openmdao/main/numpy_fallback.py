"""
This is intended to be a simple drop-in replacement for numpy in those cases where 
numpy fails to import and only very basic (1-D) numpy functionality is being used.
"""

try:
    #import numpy
    #from numpy import *
    raise ImportError('blah')
except ImportError:   
    
    from copy import deepcopy
    
    _dtype_dct = {
        'd': float,
        'f': float,
        'i': int,
        'l': long,
        float: float,
        int: int,
        long: long,
        }
    
    class _FakeNumpyArray(list):
        """A list (possible recursive) that mimics some of the behavior of
        a numpy array.
        """
        def __eq__(self, other):
            if not isinstance(other, type(self)):
                return False
            if len(self) != len(other):
                return False
            return self.__class__([x==y for x,y in zip(self, other)])
        
        def _get_val(self, args):
            val = self
            for arg in args:
                val = val.__getitem__(arg)
            return val
            
        def __getitem__(self, *args):
            if isinstance(args[0], tuple):
                return self._get_val(args[0]) 
            return super(_FakeNumpyArray, self).__getitem__(args[0])
        
        def __call__(self, *args):
            return self._get_val(args)
    
    # this ndarray doesn't have the same __init__ signature as the real one, but
    # people aren't supposed to construct them directly anyway (should use zeros, ones, etc. instead)
    ndarray = _FakeNumpyArray
    
    def _array_factory(shape, init_val):
        if isinstance(shape, (tuple, list)):
            if len(shape) == 1:
                return _array_factory(shape[0], init_val)
            else:
                arr = _FakeNumpyArray([_array_factory(shape[1:], init_val) for i in range(shape[0])])
        else:
            if isinstance(init_val, (float, int, long, str)):
                arr = _FakeNumpyArray([init_val]*shape)
            else:
                arr = _FakeNumpyArray([deepcopy(init_val) for v in range(shape)])
        return arr
        
    def zeros(shape, dtype=float):
        return _array_factory(shape, _dtype_dct[dtype](0))
    
    def ones(shape, dtype=float):
        return _array_factory(shape, _dtype_dct[dtype](1))
    
    def _guess_shape(arr):
        """This is only correct for arrays with uniform length subarrays"""
        shape = []
        val = arr
        while True:
            try:
                sz = len(val)
            except TypeError:
                break
            shape.append(sz)
            if sz > 0:
                val = val[0]
            else:
                shape.append(0)
                break
        if len(shape) == 0:
            raise ValueError("can't guess the shape of an object that has no length")
        return tuple(shape)         
            
    def array(obj, dtype=float):
        if len(obj)>0:
            dtype = _dtype_dct[dtype]
            typ = type(obj)
            typ0 = type(obj[0])
            lst = list(obj)
            if typ == typ0:
                for i,v in enumerate(lst):
                    lst[i] = array(v, dtype)
                return _FakeNumpyArray(lst)
            else:
                return _FakeNumpyArray([dtype(v) for v in lst])
        else:
            return _FakeNumpyArray([])           
        
    
    
if __name__ == '__main__':
    x = array([[1,2,3],[4,5,6]])
    y = array([[1,2,3],[4,5,6]])
    a = x[1][1]
    b = x[1,1]
    c = x(1,1)
    assert(a == b)
    assert(b == c)
    assert(all(x==y))
    
    
    