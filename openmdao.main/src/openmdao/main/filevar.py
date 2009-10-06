"""
Support for files, either as FileTraits or external files.
"""
import copy
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
    A reference to a file on disk. As well as containing metadata information,
    it supports 'open()' to read the file's contents. Before open() is
    called, 'owner' must be set to an object supporting 'check_path()' and
    'get_abs_directory()'.
    """

    def __init__(self, path, owner=None, **metadata):
        super(FileRef, self).__init__(path, **metadata)
        self.owner = owner

    def copy(self, owner):
        """ Return a copy of ourselves, owned by `owner`. """
        ref = copy.copy(self)
        ref.owner = owner
        return ref

    def open(self):
        """ Open file for reading. """
        path = self.path
        if os.path.isabs(path):
            owner = self._get_valid_owner()
            if owner is None:
                raise ValueError("Path '%s' is absolute and no path checker"
                                 " is available." % path)
            owner.check_path(path)
        else:
            owner = self._get_valid_owner()
            if owner is None:
                raise ValueError("Path '%s' is relative and no absolute"
                                 " directory is available." % path)
            directory = owner.get_abs_directory()
            path = os.path.join(directory, path)
        mode = 'rb' if self.binary else 'rU'
        return open(path, mode)

    def _get_valid_owner(self):
        """ Try to find an owner that supports the required functionality. """
        owner = self.owner
        while owner is not None:
            if hasattr(owner, 'check_path') and \
               hasattr(owner, 'get_abs_directory'):
                return owner
            if hasattr(owner, 'parent'):
                owner = owner.parent
            else:
                return None
        return None


class FileTrait(TraitType):
    """
    A trait wrapper for a FileRef attribute. If desired, the 'legal_types'
    attribute may be set to a list of expected 'content_type' strings.
    Then upon assignment the actual 'content_type' must match one of the
    'legal_types' strings.
    """
    
    def __init__(self, default_value=None, **metadata):
        if default_value is not None:
            if not isinstance(default_value, FileRef):
                raise TraitError('FileTrait default value must be a FileRef.')
        if 'iostatus' not in metadata:
            raise TraitError("FileTrait must have 'iostatus' defined.")
        iostatus = metadata['iostatus']
        if iostatus == 'out':
            if 'path' not in metadata:
                raise TraitError("Output FileTrait must have 'path' defined.")
            if 'legal_types' in metadata:
                raise TraitError("'legal_types' invalid for output FileTrait.")
            if default_value is None:
                meta = metadata.copy()
                path = metadata['path']
                for name in ('path', 'iostatus'):
                    if name in meta:
                        del meta[name]
                default_value = FileRef(path, **meta)
        super(FileTrait, self).__init__(default_value, **metadata)

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

