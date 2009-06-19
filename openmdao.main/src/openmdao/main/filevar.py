#public symbols
__all__ = ['FileVariable']
__version__ = "0.1"

from enthought.traits.api import Str, Missing
from enthought.traits.trait_handlers import NoDefaultSpecified

class FileValue(str):
    def __new__(cls, text, parent):
        s = super(FileValue, cls).__new__(FileValue, text)
        s.parent = weakref.ref(parent)

        s.metadata = {
            'content_type' : '',
            'content_encoding' : '',
            'binary' : False,
            'big_endian' : False,
            'single_precision' : False,
            'unformatted' : False,
            'recordmark_8' : False, }
        return s
    
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
        super(FileVariable, self).__init__(default_value, **metadata)
        self.iostatus = metadata.get('iostatus', 'in')

    def validate(self, object, name, value):
        s = super(FileVariable, self).validate(object, name, value) # normal string validation
        return FileValue(s, object)
    