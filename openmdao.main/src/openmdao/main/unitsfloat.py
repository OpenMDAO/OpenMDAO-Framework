
#public symbols
__all__ = ["UnitsFloat"]

__version__ = "0.1"

from enthought.traits.api import BaseFloat, Range, TraitError

from Scientific.Physics.PhysicalQuantities import PhysicalQuantity
    
class UnitsFloat(BaseFloat):
    """A Variable wrapper for floats with units and an allowed range of
    values.
    """
    
    def __init__(self, default_value = None, low=None, high=None,
                 exclude_low=False, exclude_high=False, **metadata):
        if default_value is None:
            default_value = 0.0
            
        if low is None and high is None:
            self._validator = None
        else:
            if low is None:
                low = -1.e99
            if high is None:
                high = 1.e99
            value = default_value
            self._validator = Range(low=low, high=high, value=value,
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
        metadata['validate_with_trait'] = self._validate_with_trait
        super(UnitsFloat, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        if self._validator:
            return self._validator.validate(object, name, value)
        else:
            return super(UnitsFloat, self).validate(object, name, value)

    def _validate_with_trait(self, object, name, value, trait):
        """Perform validation and unit conversion using the source
        trait units value.
        """
        try:
            pq = PhysicalQuantity(value, trait.units)
        except NameError:
            raise TraitError("undefined unit '%s'" % trait.units)

        try:
            pq.convertToUnit(object.trait(name).units)
        except NameError:
            raise TraitError("undefined unit '%s'" % trait.units)
        except TypeError, err:
            raise TraitError("%s: units '%s' are incompatible with assigning units of '%s'" % 
                             (name, pq.getUnitName(),
                              object.trait(name).units))
            
        object.debug('%s (%s) converted to %s (%s)' % 
                     (value,trait.units,pq.value,object.trait(name).units))
        return self.validate(object, name, pq.value)
        