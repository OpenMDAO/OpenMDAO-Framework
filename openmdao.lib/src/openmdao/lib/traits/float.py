"""
Trait for floating point variables, with optional min, max, and units
"""

#public symbols
__all__ = ["Float", "convert_units"]


from sys import float_info

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import TraitType, Range, TraitError
from enthought.traits.api import Float as TraitFloat
from openmdao.units import PhysicalQuantity

from openmdao.main.tvalwrapper import TraitValMetaWrapper

def convert_units(value, units, convunits):
    """Return the given value (given in units) converted 
    to convunits.
    """
    pq = PhysicalQuantity(value, units)
    pq.convert_to_unit(convunits)
    return pq.value
    
class Float(TraitType):
    """A Public Variable wrapper for floating point number valid within a
    specified range of values.
    """
    
    def __init__(self, default_value=None, iotype=None, desc=None, \
                 low=None, high=None, exclude_low=False, exclude_high=False, \
                 units=None, **metadata):
        
        # Determine defalt_value if unspecified
        if default_value is None:
            if low is None and high is None:
                default_value = 0.0
            elif low is None:
                default_value = high
            else:
                default_value = low
                
        # excludes must be saved locally because we override error()
        self.exclude_low = exclude_low
        self.exclude_high = exclude_high
        
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        # Put units in the metadata dictionary
        if units is not None:
            metadata['units'] = units
            
        # The Range trait must be used if High or Low is set
        if low is None and high is None:
            self._validator = TraitFloat(default_value, **metadata)
        else:
            if low is None:
                low = -float_info.max
            else:
                self.low = low
            if high is None:
                high = float_info.max
            else:
                self.high = high
            self._validator = Range(low=low, high=high, value=default_value,
                                          exclude_low=exclude_low,
                                        exclude_high=exclude_high,
                                          **metadata)
            
        # If there are units, test them by creating a physical quantity
        if 'units' in metadata:
            try:
                pq = PhysicalQuantity(0., metadata['units'])
            except:
                raise TraitError("Units of '%s' are invalid" %
                                 metadata['units'])
            
            
        super(Float, self).__init__(default_value=default_value,
                                         **metadata)

    def validate(self, object, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """
        
        # If both source and target have units, we need to process differently
        if isinstance(value, TraitValMetaWrapper):
            if self.units:
                return self._validate_with_metadata(object, name, 
                                                    value.value, 
                                                    value.metadata)
            
            value = value.value
            
        try:
            return self._validator.validate(object, name, value)
        except TraitError:
            self.error(object, name, value)

    def error(self, object, name, value):
        """Returns a string describing the type handled by Float."""
        
        if self.low is None and self.high is None:
            if self.units:
                info = "a float having units compatible with '%s'" % self.units
            else:
                info = "a float"
        elif self.low is not None and self.high is not None:
            right = ']'
            left = '['
            if self.exclude_high is True:
                right = ')'
            if self.exclude_low is True:
                left = '('
            info = "a float in the range %s%s, %s%s"% \
                   (left,self.low,self.high,right)
        elif self.low is not None:
            info = "a float with a value > %s"% self.low
        else: # self.high is not None
            info = "a float with a value < %s"% self.high

        msg = "Trait '%s' must be %s but attempted value is %s" % \
                               (name, info, value)
        object.raise_exception(msg, TraitError)

    def get_val_meta_wrapper(self):
        """Return a TraitValMetaWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        return TraitValMetaWrapper(units=self.units)
            
    def _validate_with_metadata(self, object, name, value, srcmeta):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        
        dst_units = self.units
        try:
            src_units = srcmeta['units']
        except KeyError:
            msg = "while setting value of %s: no 'units' metadata found."% \
                             name
            raise TraitError(msg)

        # Note: benchmarking showed that this check does speed things up -- KTM
        if src_units == dst_units:
            try:
                return self._validator.validate(object, name, value)
            except TraitError:
                self.error(object, name, value)

        try:
            pq = PhysicalQuantity(value, src_units)
        except NameError:
            raise TraitError("while setting value of %s: undefined unit '%s'" %
                             (src_units, name))
        
        try:
            pq.convert_to_unit(dst_units)
        except NameError:
            raise TraitError("undefined unit '%s' for attribute '%s'" %
                             (dst_units, name))
        except TypeError, err:
            msg = "%s: units '%s' are incompatible " % (name, src_units) + \
                   "with assigning units of '%s'" % (dst_units)
            raise TraitError(msg)
        
        try:
            return self._validator.validate(object, name, pq.value)
        except TraitError:
            self.error(object, name, pq.value)
