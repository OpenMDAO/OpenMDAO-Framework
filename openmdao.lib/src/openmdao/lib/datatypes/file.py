"""
Support for files, either as :class:`File` or external files.
"""

#Public Symbols
__all__ = ['File']


import os.path

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import TraitError
from openmdao.main.filevar import FileRef, _get_valid_owner

from openmdao.main.variable import Variable

class File(Variable):
    """
    A trait wrapper for a :class:`FileRef` object. For input files
    :attr:`legal_types` may be set to a list of expected 'content_type' strings.
    Then upon assignment the actual 'content_type' must match one of the
    :attr:`legal_types` strings.  Also for input files, if :attr:`local_path`
    is set, then upon assignent the associated file will be copied to that path.
    """
    
    def __init__(self, default_value=None, iotype=None, desc=None, **metadata):
        
        if default_value is not None:
            if not isinstance(default_value, FileRef):
                raise TraitError('File default value must be a FileRef.')
            
        if iotype is None:
            raise TraitError("File must have 'iotype' defined.")
        metadata['iotype'] = iotype
        
        # Put desc in the metadata dictionary
        if desc is not None:
            metadata['desc'] = desc
            
        if iotype == 'out':
            if default_value is None:
                if 'path' not in metadata:
                    raise TraitError("Output File must have 'path' defined.")
                if 'legal_types' in metadata:
                    raise TraitError("'legal_types' invalid for output File.")
                if 'local_path' in metadata:
                    raise TraitError("'local_path' invalid for output File.")
                meta = metadata.copy()
                path = metadata['path']
                for name in ('path', 'legal_types', 'local_path', 'iotype'):
                    if name in meta:
                        del meta[name]
                default_value = FileRef(path, **meta)
        else:
            if 'path' in metadata:
                raise TraitError("'path' invalid for input File.")
            
        super(File, self).__init__(default_value, **metadata)

# It appears this scheme won't pickle, requiring a hack in Container...
#    def get_default_value(self):
#        """ Return (default_value_type, default_value). """
#        return (8, self.make_default)
#
#    def make_default(self, obj):
#        """ Make a default value for obj. """
#        iotype = self._metadata['iotype']
#        if iotype == 'out':
#            default = self.default_value.copy(obj)
#        else:
#            default = None
#        return default

    def validate(self, obj, name, value):
        """ Verify that `value` is a FileRef of a legal type. """
        if value is None:
            return value
        elif isinstance(value, FileRef):
            legal_types = self._metadata.get('legal_types', None)
            if legal_types:
                if value.content_type not in legal_types:
                    raise TraitError("Content type '%s' not one of %s"
                                     % (value.content_type, legal_types))
            return value
        else:
            self.error(obj, name, value)

    def post_setattr(self, obj, name, value):
        """
        If 'local_path' is set on an input, then copy the source FileRef's
        file to that path.
        """
        if value is None:
            return
        iotype = self._metadata.get('iotype')
        if iotype != 'in':
            return
        path = self._metadata.get('local_path', None)
        if not path:
            return

        owner = _get_valid_owner(obj)
        if os.path.isabs(path):
            if owner is None:
                raise ValueError("Path '%s' is absolute and no path checker"
                                 " is available." % path)
            owner.check_path(path)
        else:
            if owner is None:
                raise ValueError("Path '%s' is relative and no absolute"
                                 " directory is available." % path)
            directory = owner.get_abs_directory()
            path = os.path.join(directory, path)

        mode = 'wb' if value.binary else 'w'
        chunk = 1 << 20  # 1MB
        src = value.open()
        dst = open(path, mode)
        data = src.read(chunk)
        while data:
            dst.write(data)
            data = src.read(chunk)
        src.close()
        dst.close()

