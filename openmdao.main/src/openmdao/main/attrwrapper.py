
__all__ = ['AttrWrapper']

import operator

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
        return obj.pq
    return obj

class UnitsAttrWrapper(AttrWrapper):
    """A class that allows us to check for units metadata specifically and
    to determine the units of an expression (sometimes).
    """
    def __init__(self, value=None, **metadata):
        super(UnitsAttrWrapper, self).__init__(value, **metadata)
        self.pq = PhysicalQuantity(value, metadata['units'])
        
    def __add__(self, other):
        pq = self.pq + _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())
  
    __radd__ = __add__
    
    def __sub__(self, other):
        pq = self.pq - _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())
    
    def __rsub__(self, other):
        pq = _get_PQ(other) - self.pq
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())
    
    def __mul__(self, other):
        pq = self.pq * _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())

    __rmul__ = __mul__

    def __div__(self, other):
        pq = self.pq / _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())

    def __rdiv__(self, other):
        pq = _get_PQ(other) / self.pq
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())

    def __pow__(self, other):
        pq = self.pq / _get_PQ(other) 
        return UnitsAttrWrapper(pq.value, units=pq.get_unit_name())

    def __rpow__(self, other):
        raise TypeError('Exponents must be dimensionless but this one has units of %s' % self.pq.get_unit_name())
  
    def __abs__(self):
        return UnitsAttrWrapper(abs(self.value), units=self.pq.get_unit_name())
  
    def __pos__(self):
        return self
    
    def __neg__(self):
        return UnitsAttrWrapper(-self.value, units=self.pq.get_unit_name())

    def convert_from(self, wrapper):
        if isinstance(wrapper, UnitsAttrWrapper):
            return wrapper.pq.convert_value(self.pq.unit)
        raise ValueError("incompatible AttrWrapper objects")
        