"""
Trait for numpy array variables, with optional units.
"""

#public symbols
__all__ = ["Array"]


# pylint: disable-msg=E0611,F0401
from numpy import array, ndarray

from enthought.traits.api import TraitError
from enthought.traits.api import Array as TraitArray
from openmdao.units import PhysicalQuantity

from openmdao.main.tvalwrapper import TraitValMetaWrapper

class Array(TraitArray):
    """A variable wrapper for a numpy array with optional units.
    The unit applies to the entire array."""
    
    def __init__(self, default_value=None, dtype = None, shape = None,
                 iotype=None, desc=None, units=None, **metadata):
        
        # Determine default_value if unspecified
        if default_value is None:
            default_value = array([])
        elif isinstance(default_value, ndarray):
            pass
        elif isinstance(default_value, list):
            default_value = array(default_value)
        else:
            raise TraitError("Default value should be a numpy array, "
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
                raise TraitError("Units of '%s' are invalid" %
                                 metadata['units'])
            
        # Put shape in the metadata dictionary
        if shape is not None:
            metadata['shape'] = shape
            
            # Make sure default matches the shape.
            if default_value.shape != shape:
                msg = "Shape of the default value does not match the " \
                      "shape attribute."
                raise TraitError(msg)
            
        super(Array, self).__init__(dtype=dtype, value=default_value, \
                                    **metadata)


    def validate(self, obj, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """
        
        # pylint: disable-msg=E1101
        # If both source and target have units, we need to process differently
        if isinstance(value, TraitValMetaWrapper):
            valunits = value.metadata.get('units')
            if self.units and valunits and self.units != valunits:
                return self._validate_with_metadata(obj, name, 
                                                    value.value, 
                                                    valunits)
            
            value = value.value
            
        try:
            return super(Array, self).validate(obj, name, value)
        except TraitError:
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
        msg = "Trait '%s' must be %s, but a %s of %s (%s) was specified." % \
                               (name, info, wtype, wvalue, vtype)
        obj.raise_exception(msg, TraitError)

    def get_val_meta_wrapper(self):
        """Return a TraitValMetaWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        # pylint: disable-msg=E1101
        return TraitValMetaWrapper(units=self.units)
            
    def _validate_with_metadata(self, obj, name, value, src_units):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        
        # pylint: disable-msg=E1101
        dst_units = self.units

        try:
            pq = PhysicalQuantity(1.0, src_units)
        except NameError:
            raise TraitError("while setting value of %s: undefined unit '%s'" %
                             (src_units, name))
        
        try:
            pq.convert_to_unit(dst_units)
        except NameError:
            raise TraitError("undefined unit '%s' for variable '%s'" %
                             (dst_units, name))
        except TypeError:
            msg = "%s: units '%s' are incompatible " % (name, src_units) + \
                   "with assigning units of '%s'" % (dst_units)
            raise TraitError(msg)
        
        try:
            value *= pq.value
            return super(Array, self).validate(obj, name, value)
        except TraitError:
            self.error(obj, name, value)
