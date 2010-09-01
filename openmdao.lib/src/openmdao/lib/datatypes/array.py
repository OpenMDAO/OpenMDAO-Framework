"""
Trait for numpy array variables, with optional units.
"""

#public symbols
__all__ = ["Array"]


import numpy

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import TraitType, Range, TraitError
from enthought.traits.api import Array as TraitArray
from openmdao.units import PhysicalQuantity

from openmdao.main.tvalwrapper import TraitValMetaWrapper

class Array(TraitArray):
    """A Public Variable wrapper for a numpy array with optional units."""
    
    def __init__(self, default_value=None, dtype = None, shape = None,
                 iotype=None, desc=None, units=None,  **metadata):
        
        # Determine defalt_value if unspecified
        if default_value is None:
            default_value = numpy.array([])
        elif isinstance(default_value, numpy.ndarray):
            pass
        elif isinstance(default_value, list):
            default_value = numpy.array(default_value)
        else:
            raise TraitError("Default value should be a numpy array, not a %s." %
                             type(default_value))
        
        # Put iotype in the metadata dictionary
        if iotype is not None:
            metadata['iotype'] = iotype
            
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        # Put units in the metadata dictionary
        if units is not None:
            metadata['units'] = units
            
        # If there are units, test them by creating a physical quantity
        if 'units' in metadata:
            try:
                pq = PhysicalQuantity(0., metadata['units'])
            except:
                raise TraitError("Units of '%s' are invalid" %
                                 metadata['units'])
            
        super(Array, self).__init__(dtype=dtype, shape=shape, value=default_value,
                                    **metadata )


    def validate(self, object, name, value):
        """ Validates that a specified value is valid for this trait.
        Units are converted as needed.
        """
        
        # pylint: disable-msg=E1101
        # If both source and target have units, we need to process differently
        if isinstance(value, TraitValMetaWrapper):
            valunits = value.metadata.get('units')
            if self.units and valunits and self.units!=valunits:
                return self._validate_with_metadata(object, name, 
                                                    value.value, 
                                                    valunits)
            
            value = value.value
            
        try:
            return super(Array,self).validate(object, name, value)
        except TraitError:
            self.error(object, name, value)

    def error(self, object, name, value):
        """Returns a string describing the type handled by Array."""
        
        # pylint: disable-msg=E1101
        if self.units:
            info = "a numpy array having units compatible with '%s'" % self.units
        else:
            info = "a numpy array."

        vtype = type( value )
        msg = "Trait '%s' must be %s, but a value of %s %s was specified." % \
                               (name, info, value, vtype)
        object.raise_exception(msg, TraitError)

    def get_val_meta_wrapper(self):
        """Return a TraitValMetaWrapper object.  Its value attribute
        will be filled in by the caller.
        """
        # pylint: disable-msg=E1101
        return TraitValMetaWrapper(units=self.units)
            
    def _validate_with_metadata(self, object, name, value, src_units):
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
            raise TraitError("undefined unit '%s' for attribute '%s'" %
                             (dst_units, name))
        except TypeError:
            msg = "%s: units '%s' are incompatible " % (name, src_units) + \
                   "with assigning units of '%s'" % (dst_units)
            raise TraitError(msg)
        
        try:
            value *= pq.value
            return super(Array, self).validate(object, name, value)
        except TraitError:
            self.error(object, name, value)
