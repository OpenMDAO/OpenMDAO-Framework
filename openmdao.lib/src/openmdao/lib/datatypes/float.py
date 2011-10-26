"""
Trait for floating point variables, with optional min, max, and units.
"""

#public symbols
__all__ = ["Float"]


from sys import float_info

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Range
from enthought.traits.api import Float as TraitFloat
from openmdao.units import PhysicalQuantity

from openmdao.main.variable import Variable
from openmdao.main.attrwrapper import AttrWrapper

from openmdao.main.uncertain_distributions import UncertainDistribution

class Float(Variable):
    """A Variable wrapper for floating point number valid within a
    specified range of values.
    """
    
    def __init__(self, default_value=None, iotype=None, desc=None, 
                 low=None, high=None, exclude_low=False, exclude_high=False, 
                 units=None, **metadata):

        # Determine defalt_value if unspecified
        if default_value is None:
            if low is None and high is None:
                default_value = 0.0
            elif low is None:
                default_value = high
            else:
                default_value = low
        else:
            if not isinstance(default_value, float):
                if isinstance(default_value, int):
                    default_value = float(default_value)
                else:
                    raise ValueError("Default value should be a float.")
              
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
                low = float(low)
                
            if high is None:
                high = float_info.max
            else:
                high = float(high)

            if low > high:
                raise ValueError("Lower bound is greater than upper bound.")
        
            if default_value > high or default_value < low:
                raise ValueError("Default value is outside of bounds [%s, %s]." %
                                 (str(low), str(high)))
                     
            # Range can be float or int, so we need to force these to be float.
            default_value = float(default_value)
                
            self._validator = Range(low=low, high=high, value=default_value,
                                          exclude_low=exclude_low,
                                          exclude_high=exclude_high,
                                          **metadata)
            
        # If there are units, test them by creating a physical quantity
        if 'units' in metadata:
            try:
                pq = PhysicalQuantity(0., metadata['units'])
            except:
                raise ValueError("Units of '%s' are invalid" %
                                 metadata['units'])
            
        # Add low and high to the trait's dictionary so they can be accessed
        metadata['low'] = low
        metadata['high'] = high
        metadata['validation_trait'] = self
        super(Float, self).__init__(default_value=default_value,
                                    **metadata)

    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """
        
        # pylint: disable-msg=E1101
        # If both source and target have units, we need to process differently
        if isinstance(value, AttrWrapper) and 'units' in value.metadata:
            if self.units and value.metadata['units']:
                return self._validate_with_metadata(obj, name, 
                                                    value.value, 
                                                    value.metadata)
            
            value = value.value
        elif isinstance(value, UncertainDistribution):
            value = value.getvalue()
        try:
            return self._validator.validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)

    def error(self, obj, name, value):
        """Returns a descriptive error string."""
        
        # pylint: disable-msg=E1101
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

        vtype = type( value )
        msg = "Variable '%s' must be %s, but a value of %s %s was specified." % \
                               (name, info, value, vtype)
        try:
            obj.raise_exception(msg, ValueError)
        except AttributeError:
            raise ValueError(msg)

    def get_val_wrapper(self, value):
        """Return an AttrWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        # pylint: disable-msg=E1101
        return AttrWrapper(value, units=self.units)
            
    def _validate_with_metadata(self, obj, name, value, srcmeta):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        
        # pylint: disable-msg=E1101
        dst_units = self.units
        src_units = srcmeta['units']
        
        if isinstance(value, UncertainDistribution):
            value = value.getvalue()
            
        # FIXME: The try blocks testing whether the unit is bogus or undefined
        # are generally redundant because that test is done at creation. HOWEVER
        # you might have a case where it wasn't tested because it's technically
        # not a float. NPSS wrapper may be such a case. A test needs to be 
        # constructed to test these lines.

        # Note: benchmarking showed that this check does speed things up -- KTM
        if src_units == dst_units:
            try:
                return self._validator.validate(obj, name, value)
            except Exception:
                self.error(obj, name, value)

        try:
            pq = PhysicalQuantity(value, src_units)
        except NameError:
            raise NameError("while setting value of %s: undefined unit '%s'" %
                             (src_units, name))
        
        try:
            pq.convert_to_unit(dst_units)
        except NameError:
            raise NameError("undefined unit '%s' for variable '%s'" %
                             (dst_units, name))
        except TypeError:
            msg = "%s: units '%s' are incompatible " % (name, src_units) + \
                   "with assigning units of '%s'" % (dst_units)
            raise TypeError(msg)
        
        try:
            return self._validator.validate(obj, name, pq.value)
        except Exception:
            self.error(obj, name, pq.value)

