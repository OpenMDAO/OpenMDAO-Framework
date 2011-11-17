"""
Trait for numpy array variables, with optional units.
"""

#public symbols
__all__ = ["Array"]

import logging

# pylint: disable-msg=E0611,F0401
try:
    from numpy import array, ndarray, zeros
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))

from enthought.traits.api import Array as TraitArray
from openmdao.units import PhysicalQuantity

from openmdao.main.attrwrapper import AttrWrapper
from openmdao.util.decorators import stub_if_missing_deps

@stub_if_missing_deps('numpy')
class Array(TraitArray):
    """A variable wrapper for a numpy array with optional units.
    The unit applies to the entire array."""
    
    def __init__(self, default_value=None, dtype = None, shape = None,
                 iotype=None, desc=None, units=None, **metadata):
        
        # Determine default_value if unspecified
        if default_value is None:
            if shape is None or len(shape) == 1:
                default_value = array([])
            elif len(shape) == 2:
                default_value = array([[]])
            elif len(shape) == 3:
                default_value = array([[[]]])
                    
        elif isinstance(default_value, ndarray):
            pass
        elif isinstance(default_value, list):
            default_value = array(default_value)
        else:
            raise TypeError("Default value should be a numpy array, "
                             "not a %s." % type(default_value))
        
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        # Put units in the metadata dictionary
        if units is not None:
            metadata['units'] = units
            
            # Since there are units, test them by creating a physical quantity
            try:
                pq = PhysicalQuantity(0., metadata['units'])
            except:
                raise ValueError("Units of '%s' are invalid" %
                                 metadata['units'])
            
        # Put shape in the metadata dictionary
        if shape is not None:
            metadata['shape'] = shape
            
            # Make sure default matches the shape.
            if len(shape) != len(default_value.shape):
                raise ValueError("Shape of the default value does not match "
                                 "the shape attribute.")
            for i, sh in enumerate(shape):
                if sh is not None and sh != default_value.shape[i]:
                    raise ValueError("Shape of the default value does not match "
                                     "the shape attribute.")
            
        super(Array, self).__init__(dtype=dtype, value=default_value,
                                    **metadata)


    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """
        
        # pylint: disable-msg=E1101
        # If both source and target have units, we need to process differently
        if isinstance(value, AttrWrapper):
            valunits = value.metadata.get('units')
            if self.units and valunits and self.units != valunits:
                return self._validate_with_metadata(obj, name, 
                                                    value.value, 
                                                    valunits)
            
            value = value.value
            
        try:
            return super(Array, self).validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)

    def error(self, obj, name, value):
        """Returns an informative and descriptive error string."""
        
        wtype = "value"
        wvalue = value
        info = "a numpy array"
        
        # pylint: disable-msg=E1101
        if self.shape and value.shape:
            if self.shape != value.shape:
                info += " of shape %s" % str(self.shape)
                wtype = "shape"
                wvalue = str(value.shape)

        vtype = type( value )
        msg = "Variable '%s' must be %s, but a %s of %s (%s) was specified." % \
                               (name, info, wtype, wvalue, vtype)
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
            
    def _validate_with_metadata(self, obj, name, value, src_units):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        
        # pylint: disable-msg=E1101
        dst_units = self.units

        try:
            pq = PhysicalQuantity(1.0, src_units)
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
            value *= pq.value
            return super(Array, self).validate(obj, name, value)
        except Exception:
            self.error(obj, name, value)
