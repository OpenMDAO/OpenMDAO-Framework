
#public symbols
__all__ = ["UnitsFloat", "convert_units"]



from enthought.traits.api import TraitType, Float, Range, TraitError

from units import PhysicalQuantity

from openmdao.main.tvalwrapper import TraitValMetaWrapper


def convert_units(value, units, convunits):
    """Return the given value (given in units) converted 
    to convunits.
    """
    pq = PhysicalQuantity(value, units)
    pq.convertToUnit(convunits)
    return pq.value
    
class UnitsFloat(TraitType):
    """A Variable wrapper for floats with units and an allowed range of
    values.
    """
    
    def __init__(self, default_value = None, low=None, high=None,
                 exclude_low=False, exclude_high=False, **metadata):
        if default_value is None:
            if low is None and high is None:
                default_value = 0.0
            elif low is None:
                default_value = high
            else:
                default_value = low
            
        if low is None and high is None:
            self._validator = Float(default_value, **metadata)
        else:
            if low is None:
                low = -1.e99
            else:
                self.low = low
            if high is None:
                high = 1.e99
            else:
                self.high = high
            self._validator = Range(low=low, high=high, value=default_value,
                                          exclude_low=exclude_low,
                                          exclude_high=exclude_high,
                                          **metadata)
        if 'units' not in metadata:
            raise TraitError('UnitsFloat must have units defined')
        else:
            try:
                pq = PhysicalQuantity(0., metadata['units'])
            except:
                raise TraitError("Units of '%s' are invalid" %
                                 metadata['units'])
        super(UnitsFloat, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        if isinstance(value, TraitValMetaWrapper):
            return self.validate_with_metadata(object, name, 
                                               value.value, 
                                               value.metadata)
        else:    
            try:
                return self._validator.validate(object, name, value)
            except TraitError:
                self.error(object, name, value)

    def error(self, object, name, value):
        """Returns a string describing the type handled by UnitsFloat."""
        if self.low is None and self.high is None:
            info = "a float having units compatible with '%s'" % self.units
        elif self.low is not None and self.high is not None:
            right = ']'
            left = '['
            if self.exclude_high:
                right = ')'
            if self.exclude_low:
                left = '('
            info = "a float in the range %s%s, %s%s"%\
                   (left,self.low,self.high,right)
        elif self.low is not None:
            info = "a float with a value > %s"% self.low
        else: # self.high is not None
            info = "a float with a value < %s"% self.high
        object.raise_exception("Trait '%s' must be %s but attempted value is %s" %
                               (name, info, value), TraitError)

    def get_val_meta_wrapper(self):
        """Return a TraitValMetaWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        return TraitValMetaWrapper(units=self.units)
            
    def validate_with_metadata(self, object, name, value, srcmeta):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        src_units = srcmeta['units']
        dst_units = object.trait(name).units
        if src_units == dst_units:
            try:
                return self._validator.validate(object, name, value)
            except TraitError:
                self.error(object, name, value)

        try:
            pq = PhysicalQuantity(value, src_units)
        except KeyError:
            raise TraitError("while setting value of %s: no 'units' metadata found."%
                             name)
        except NameError:
            raise TraitError("while setting value of %s: undefined unit '%s'" %
                             (src_units, name))
        try:
            pq.convertToUnit(dst_units)
        except NameError:
            raise TraitError("undefined unit '%s' for attribute '%s'" %
                             (dst_units, name))
        except TypeError, err:
            raise TraitError("%s: units '%s' are incompatible with assigning units of '%s'" %
                             (name, pq.getUnitName(), dst_units))
        try:
            return self._validator.validate(object, name, pq.value)
        except TraitError:
            self.error(object, name, pq.value)
