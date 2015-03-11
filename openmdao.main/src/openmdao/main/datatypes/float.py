"""
Trait for floating point variables, with optional min, max, and units.
"""

#public symbols
__all__ = ["Float"]


from sys import float_info

# pylint: disable=E0611,F0401
from traits.api import Range
from traits.api import Complex as TraitComplex
from traits.api import Float as TraitFloat

from openmdao.main.uncertain_distributions import UncertainDistribution
from openmdao.main.variable import Variable
from openmdao.units import PhysicalQuantity


class Float(Variable):
    """A Variable wrapper for floating point number valid within a
    specified range of values.
    """

    def __init__(self, default_value=None, iotype=None, desc=None,
                 low=None, high=None, exclude_low=False, exclude_high=False,
                 units=None, **metadata):

        _default_set = False

        # Determine defalt_value if unspecified
        if default_value is None:
            if low is None and high is None:
                default_value = 0.0
            elif low is None:
                default_value = high
            else:
                default_value = low
        else:
            _default_set = True
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
                raise ValueError("Default value is outside of bounds [%s, %s]."
                                 % (str(low), str(high)))

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

        if 'assumed_default' in metadata:
            del metadata['assumed_default']

        if not _default_set and metadata.get('required') is True:
            super(Float, self).__init__(**metadata)

        if not _default_set:
            super(Float, self).__init__(default_value=default_value,
                                        assumed_default=True, **metadata)
        else:
            super(Float, self).__init__(default_value=default_value,
                                        assumed_default=False, **metadata)

    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """

        # pylint: disable=E1101
        if isinstance(value, UncertainDistribution):
            value = value.getvalue()

        # Support for complex step method. We can step this trait in the
        # complex direction while keeping the Range validator.
        is_complex_step = False
        if isinstance(value, complex):
            value_imag = value.imag
            value = value.real
            if value_imag != 0:
                is_complex_step = True

        try:
            new_val = self._validator.validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)

        if is_complex_step:
            new_val += value_imag*1j

        #print name, new_val
        return new_val

    def error(self, obj, name, value):
        """Returns a descriptive error string."""

        # pylint: disable=E1101
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
            info = "a float in the range %s%s, %s%s" % \
                   (left, self.low, self.high, right)
        elif self.low is not None:
            info = "a float with a value > %s" % self.low
        else:  # self.high is not None
            info = "a float with a value < %s" % self.high

        vtype = type(value)
        msg = "Variable '%s' must be %s, but a value of %s %s was specified." \
              % (name, info, value, vtype)
        try:
            obj.raise_exception(msg, ValueError)
        except AttributeError:
            raise ValueError(msg)

    def _validate_with_metadata(self, obj, name, value, src_units):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """

        # pylint: disable=E1101
        dst_units = self.units

        if isinstance(value, UncertainDistribution):
            value = value.getvalue()

        # FIXME: The try blocks testing whether the unit is bogus or undefined
        # are generally redundant because that test is done at creation. HOWEVER
        # you might have a case where it wasn't tested because it's technically
        # not a float. NPSS wrapper may be such a case. A test needs to be
        # constructed to test these lines.

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

        return pq.value

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. The basic functionality that
        most variables need is provided here; you can overload this for
        special cases, like lists and dictionaries, or custom datatypes.

        name: str
          Name of variable

        value: object
          The value of the variable

        trait: CTrait
          The variable's trait

        meta: dict
          Dictionary of metadata for this variable
        """
        attr, other = super(Float, self).get_attribute(name, value, trait, meta)
        # Fix type 'synonym'.
        if attr['type'] == 'float64':
            attr['type'] = 'float'
            attr['value'] = float(value)
        return attr, other
