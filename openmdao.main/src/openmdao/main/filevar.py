#public symbols
__all__ = ['FileTrait']
__version__ = "0.1"

import copy

from enthought.traits.api import TraitType

_filemeta = { 'filename': '',
              'content_type' : '',
              'content_encoding' : '',
              'binary' : False,
              'big_endian' : False,
              'single_precision' : False,
              'unformatted' : False,
              'recordmark_8' : False }

_meta_exclude = set(['iostatus'])

class FileValue(object):
    """A string indicating a file path, with a number of additional
    attributes indicating content type.
    """
    def __init__(self, **metadata):
        super(FileValue, self).__init__()
        self.__dict__.update(_filemeta)
        self.__dict__.update(metadata)
        
    def __str__(self):
        return str(self.__dict__)
    
    
class FileTrait(TraitType):
    """ A trait wrapper for a FileValue attribute."""
    
    def __init__(self, default_value=None, **metadata):
        if default_value is None:
            meta = metadata.copy()
            for name in _meta_exclude:
                del meta[name]
            default_value = FileValue(**meta)
        super(FileTrait, self).__init__(default_value, **metadata)


    def validate(self, object, name, value):
        if isinstance(value, FileValue):
            value = copy.copy(value)
            value.filename = getattr(object, name).filename
            return value
        else:
            self.error(object, name, value)
    