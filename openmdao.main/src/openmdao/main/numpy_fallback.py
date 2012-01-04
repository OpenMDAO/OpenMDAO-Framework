"""
This is intended to be a simple drop-in replacement for numpy in those cases where 
numpy fails to import and only very basic numpy functionality is being used.
"""

try:
    # pylint: disable-msg=E0611,F0401
    import numpy
    from numpy import *
except ImportError as err:   
    
    import logging
    logging.warn("In %s: %r" % (__file__, err))
    
    import operator
    from copy import deepcopy
    
    _dtype_dct = {
        'd': float,
        'f': float,
        'i': int,
        'l': long,
        'c': complex,
        'S': str,
        'a': str,
        'U': unicode,
        'b': bool,
        float: float,
        int: int,
        long: long,
        str: str,
        complex: complex,
        bool: bool,
        }
    
    class _FakeNumpyArray(list):
        """A list (possibly recursive) that mimics some of the behavior of
        a numpy array. This is a work in progress...
        """
        def __init__(self, lst, dtype=None):
            super(_FakeNumpyArray, self).__init__(lst)
            if dtype is None:
                dtype = _guess_dtype(lst)
            self.dtype = dtype

        def __eq__(self, other):
            try:
                if len(self) != len(other):
                    return False
            except:
                return False
            return self.__class__([x==y for x,y in zip(self, other)])
        
        def __getitem__(self, *args):
            if isinstance(args[0], tuple):
                val = self
                for arg in args[0]:
                    val = val.__getitem__(arg)
                return val
            elif isinstance(args[0], slice):
                return _FakeNumpyArray(super(_FakeNumpyArray, self).__getitem__(args[0]))
            return super(_FakeNumpyArray, self).__getitem__(args[0])
        
        def __setitem__(self, *args):
            if isinstance(args[0], tuple):
                val = self
                for arg in args[0][:-1]:
                    val = val.__getitem__(arg)
                val.__setitem__(args[0][-1], args[1])
            else:
                super(_FakeNumpyArray, self).__setitem__(args[0], args[1])
        
        def __add__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([v+other for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([v+o for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
                
        __radd__ = __add__
        
        def __sub__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([v-other for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([v-o for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
                
        def __rsub__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([other-v for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([o-v for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
                
        def __mul__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([v*other for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([v*o for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
                
        __rmul__ = __mul__
        
        def __div__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([v/other for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([v/o for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
        
        def __rdiv__(self, other):
            if isinstance(other, (int, float, long, complex)):
                return _FakeNumpyArray([other/v for v in self], self.dtype)
            elif len(self) == len(other):
                return _FakeNumpyArray([o/v for v,o in zip(self, other)],
                                       self.dtype)
            else:
                raise ValueError("arrays have different shapes")
        
        __truediv__ = __div__
        
        def flatten(self):
            """Return a copy of the array collapsed into one dimension"""
            if len(self) == 0:
                return _FakeNumpyArray([], self.dtype)
            elif not isinstance(self[0], _FakeNumpyArray):
                return _FakeNumpyArray(self, self.dtype)
            else:
                ret = []
                for val in self:
                    ret.extend(val.flatten())
                return _FakeNumpyArray(ret, self.dtype)
    
    # this ndarray doesn't have the same __init__ signature as the real one, but
    # people aren't supposed to construct them directly anyway 
    # (should use zeros, ones, etc. instead)
    ndarray = _FakeNumpyArray
    
    def _array_factory(shape, init_val, dtype=None):
        if isinstance(shape, (tuple, list)):
            if len(shape) == 1:
                return _array_factory(shape[0], init_val, dtype)
            else:
                return _FakeNumpyArray([_array_factory(shape[1:], init_val, dtype) 
                                        for i in range(shape[0])], dtype)
        elif isinstance(init_val, (float, int, long, str)):
            return _FakeNumpyArray([init_val]*shape, dtype)
        return _FakeNumpyArray([deepcopy(init_val) for v in range(shape)],
                               dtype)
        
    def zeros(shape, dtype=float):
        dtype = _dtype_dct[dtype]
        return _array_factory(shape, dtype(0), dtype)
    
    def ones(shape, dtype=float):
        dtype = _dtype_dct[dtype]
        return _array_factory(shape, dtype(1), dtype)
    
    def append(arr1, arr2):
        """Add values to the end of an array. Returns a flattened copy."""
        if not isinstance(arr1, _FakeNumpyArray):
            arr1 = _FakeNumpyArray(arr1)
        if not isinstance(arr2, _FakeNumpyArray):
            arr2 = _FakeNumpyArray(arr2)
        return arr1.flatten() + arr2.flatten()
        
    def _guess_dtype(obj):
        val = obj
        while True:
            if isinstance(val, basestring):
                return str
            try:
                it = iter(val)
            except TypeError:
                return _dtype_dct.get(type(val))
            try:
                val = it.next()
            except StopIteration:
                return None
        
    def array(obj, dtype=None):
        if len(obj)>0:
            if dtype is None:
                dtype = _guess_dtype(obj)
            dtype = _dtype_dct[dtype]
            typ = type(obj)
            typ0 = type(obj[0])
            lst = list(obj)
            if typ == typ0 and not isinstance(obj, basestring):
                for i,v in enumerate(lst):
                    lst[i] = array(v, dtype)
                return _FakeNumpyArray(lst, dtype)
            else:
                return _FakeNumpyArray([dtype(v) for v in lst], dtype)
        else:
            return _FakeNumpyArray([], dtype)
        
    # this is adapted from the numpy version
    def linspace(start, stop, num=50, endpoint=True, retstep=False):
        """
        Return evenly spaced numbers over a specified interval.
        """
        num = int(num)
        if num <= 0:
            return array([], float)
        if endpoint:
            if num == 1:
                return array([float(start)])
            step = (stop-start)/float((num-1))
            y = array(range(0, num), dtype=float) * step + start
            y[-1] = stop
        else:
            step = (stop-start)/float(num)
            y = array(range(0, num), dtype=float) * step + start
        if retstep:
            return y, step
        else:
            return y
    
    
if __name__ == '__main__':
    x = array([[1,2,3],[4,5,6]])
    y = array([[1,2,3],[4,5,6]])
    a = x[1][1]
    b = x[1,1]
    assert(a == b)
    assert(b == a)
    assert(all(x==y))
    
    z = array([1,2,3,4,5,6])
    assert(z[::-1] == array([6,5,4,3,2,1]))
    
    x[1,1] = 99
    assert(x[0] == array([1, 2, 3]))
    assert(x[1] == array([4, 99, 6]))
    
    z[2:4] = [44, 55, 66]
    assert(all(z == array([1,2,44,55,66,5,6])))
    
    
    