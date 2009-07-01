#public symbols
__all__ = ['FileVariable']
__version__ = "0.1"

from enthought.traits.api import Str
from enthought.traits.trait_handlers import NoDefaultSpecified

class FileVariable(Str):
    """ A Variable wrapper for a file path attribute. Metadata for the file
    is available in the 'metadata' attribute of the variable.  Standard
    metadata includes:
        'content_type': string
            default ''
        'content_encoding': string
            default ''
        'binary': bool
            default False
        'big_endian': bool
            default False
        'single_precision': bool
            default False
        'unformatted': bool
            default False
        'recordmark_8': bool
            default False
    """
    
    def __init__(self, default_value=NoDefaultSpecified, **metadata):
        meta = {
            'content_type' : '',
            'content_encoding' : '',
            'binary' : False,
            'big_endian' : False,
            'single_precision' : False,
            'unformatted' : False,
            'recordmark_8' : False }
        
        meta.update(metadata)
        super(FileVariable, self).__init__(default_value, **meta)

    def _validation_metadata(self):
        """Returns a dict containing names and values of any metadata 
        deemed necessary for validation by a destination trait. For
        example, UnitFloat includes 'units' in its metadata so that
        unit conversions can be performed at the destination.
        """
        return {
            'content_type' : self.content_type,
            'content_encoding' : self.content_encoding,
            'binary' : self.binary,
            'big_endian' : self.big_endian,
            'single_precision' : self.single_precision,
            'unformatted' : self.unformatted,
            'recordmark_8' : self.recordmark_8,
            'validate_with_metadata' : self._validate_with_metadata,
            'validation_metadata': self._validation_metadata }

    def _validate_with_metadata(self, object, name, value, srcmeta):
        """Perform validation and unit conversion using metadata from
        the source trait.
        """
        # TODO: use metadata here to determine compatibility between
        # files. 
        # TODO: instead of storing a simple string, we may want a
        # 'shadow' value that contains more information and could
        # provide different forms of access to the file data, e.g.,
        # a file object vs. a string with full contents maybe? This
        # is also where we could automatically create adapters 
        # to convert some file formats. This may not be useful that
        # often though, so it may not be worth the trouble.
        return self.validate(object, name, value)
        
    