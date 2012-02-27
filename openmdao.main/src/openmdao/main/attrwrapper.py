
__all__ = ['AttrWrapper']

from openmdao.units import PhysicalQuantity

class AttrWrapper(object):
    """A class that encapsulates a value and any metadata necessary
    for validation of that value.  For example, an AttrWrapper for
    a Float object would include 'units' metadata to allow for unit
    compatability checking and conversion.
    """
    def __init__(self, value=None, **metadata):
        self.value = value
        self.metadata = metadata

def _get_PQ(obj):
    if isinstance(obj, UnitsAttrWrapper):
        return PhysicalQuantity(obj.value, obj.metadata['units'])
    return obj

class UnitsAttrWrapper(AttrWrapper):
    """A class that allows us to check for units metadata specifically and
    to determine the units of an expression (sometimes).
    
    Note that this class has a number of 'numeric' functions defined to support checking
    of units, but value information is not carried through, i.e., if you have an
    expression that multiplies two Floats together, the result, if UnitsAttrWrappers are
    used in place of the floating point values, will be a UnitsAttrWrapper object having the
    correct units for the expression, but not the correct resulting numerical value.
    """
    
    def __add__(self, other):
        pq = _get_PQ(self) + _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, pq.unit)
  
    __radd__ = __add__
    
    # we only care here about the units, not the value, so addition/subtraction are the same
    __sub__ = __add__
    __rsub__ = __sub__
  
    def __mul__(self, other):
        pq = _get_PQ(self) * _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, pq.unit)

    __rmul__ = __mul__

    def __div__(self, other):
        opq = _get_PQ(other)
        opq.value = 1.0  # avoid possible div by zero
        pq = _get_PQ(self) / opq
        return UnitsAttrWrapper(pq.value, pq.unit)

    def __rdiv__(self, other):
        spq = _get_PQ(self)
        spq.value = 1.0  # avoid possible div by zero
        pq = _get_PQ(other) / spq
        return UnitsAttrWrapper(pq.value, pq.unit)

    def __pow__(self, other):
        pq = _get_PQ(self) ** _get_PQ(other)
        return UnitsAttrWrapper(pq.value, pq.unit)

    def __rpow__(self, other):
        raise TypeError('Exponents must be dimensionless')
  
    def __abs__(self):
        return self
  
    __pos__ = __abs__
    __neg__ = __neg__

    