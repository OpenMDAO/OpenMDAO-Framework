"""
Support for files, either as FileTraits or external files.
"""
import os.path

from enthought.traits.api import TraitType, TraitError

__all__ = ('FileMetadata', 'FileRef', 'FileTrait')

# Standard metadata and default values.
_FILEMETA = {
    'path': '',
    'desc': '',
    'content_type': '',
    'binary': False,
    'big_endian': False,
    'single_precision': False,
    'unformatted': False,
    'recordmark_8': False,
}

# Metadata from Trait that must be excluded before creating FileRef.
_META_EXCLUDE = ('path', 'iostatus')


class FileMetadata(object):
    """
    Metadata related to a file. By default, the metadata includes:

    - 'path', a string, no default value. It may be a glob-style pattern in \
      the case of an external file description. Non-absolute paths are \
      relative to their owning component's directory.
    - 'desc', a string, default null.
    - 'content_type', a string, default null.
    - 'binary', boolean, default False.
    - 'big_endian', boolean, default False. Only meaningful if binary.
    - 'single_precision', boolean, default False. Only meaningful if binary.
    - 'unformatted', boolean, default False. Only meaningful if binary.
    - 'recordmark_8', boolean, default False. Only meaningful if binary.

    In addition, external files have defined behavior for:

    - 'input', boolean, default False. If True, the file(s) should exist \
      before execution.
    - 'output', boolean, default False. If True, the file(s) should exist \
      after execution.
    - 'constant', boolean, default False. If True, the file(s) may be safely \
      symlinked.
    """

    def __init__(self, path, **metadata):
        super(FileMetadata, self).__init__()
        assert isinstance(path, basestring) and path
        self.__dict__.update(_FILEMETA)
        self.__dict__.update(metadata)
        self.path = path

    def __str__(self):
        return str(self.__dict__)


class FileRef(FileMetadata):
    """
    A reference to a file. As well as containing metadata information,
    it supports operations to read and copy the file's contents. If 'path'
    is relative, then the 'owner' attribute must be set to an object supporting
    'get_abs_directory()'. If the 'server' attribute is not None, then
    it must be set to an object supporting 'open(path, mode)'.
    """

    def __init__(self, path, **metadata):
        super(FileRef, self).__init__(path, **metadata)
        self.owner = None
        self.server = None

    def open(self):
        """ Open file for reading. """
        path = self.path
        if not os.path.isabs(path):
            if hasattr(self.owner, 'get_abs_directory'):
                directory = self.owner.get_abs_directory()
            else:
                raise ValueError("Path '%s' is relative and no absolute"
                                 " directory is available." % path)
            path = os.path.join(directory, path)
        mode = 'rb' if self.binary else 'rU'
        if self.server is None:
            return open(path, mode)
        else:
            return self.server.open(path, mode)

    def copy(self, dst_path):
        """ Copy file to `dst_path` on the local machine. """
        assert isinstance(dst_path, basestring) and dst_path
        src = self.open()
        mode = 'wb' if self.binary else 'w'
        try:
            dst = open(dst_path, mode)
            if self.server is None:
                chunk = 1 << 20  # 1MB
            else:
                chunk = 1 << 17  # 128KB
            try:
                bytes = src.read(chunk)
                while bytes:
                    dst.write(bytes)
                    bytes = src.read(chunk)
            finally:
                dst.close()
        finally:
            src.close()


class FileTrait(TraitType):
    """
    A trait wrapper for a FileRef attribute. If desired, the 'legal_types'
    attribute may be set to a list of expected 'content_type' strings.
    Then upon assignment the actual 'content_type' must match one of the
    'legal_types' strings.
    """
    
    def __init__(self, default_value=None, legal_types=None, **metadata):
        if 'path' not in metadata:
            raise TraitError('FileTrait must have path defined.')
        self.legal_types = legal_types or []
        if default_value is None:
            meta = metadata.copy()
            path = metadata['path']
            for name in _META_EXCLUDE:
                del meta[name]
            default_value = FileRef(path, **meta)
        super(FileTrait, self).__init__(default_value, **metadata)

    def validate(self, object, name, value):
        """ Verify that `value` is a FileRef of a legal type. """
        if isinstance(value, FileRef):
            if self.legal_types:
                if value.content_type not in self.legal_types:
                    raise TraitError("Content type '%s' not one of %s"
                                     % (value.content_type, self.legal_types))
            return value
        else:
            self.error(object, name, value)

