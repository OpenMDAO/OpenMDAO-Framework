"""
Support for files, either as :class:`File` or external files.
"""
import copy
import os.path
import pprint

__all__ = ('FileMetadata', 'FileRef', '_get_valid_owner')

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
    Metadata related to a file, specified by keyword arguments (except for
    'path').  By default, the metadata includes:

    - 'path', a string, no default value. It may be a :mod:`glob`-style \
      pattern in the case of an external file description. Non-absolute paths \
      are relative to their owning component's directory.
    - 'desc', a string, default null.
    - 'content_type', a string, default null.
    - 'binary', boolean, default False.
    - 'big_endian', boolean, default False. Only meaningful if binary.
    - 'single_precision', boolean, default False. Only meaningful if binary.
    - 'unformatted', boolean, default False. Only meaningful if binary.
    - 'recordmark_8', boolean, default False. Only meaningful if unformatted.

    In addition, external files have defined behavior for:

    - 'input', boolean, default False. If True, the file(s) should exist \
      before execution.
    - 'output', boolean, default False. If True, the file(s) should exist \
      after execution.
    - 'constant', boolean, default False. If True, the file(s) may be safely \
      symlinked.

    Arbitrary additional metadata may be assigned.
    """

    def __init__(self, path, **metadata):
        super(FileMetadata, self).__init__()
        assert isinstance(path, basestring) and path
        self.__dict__.update(_FILEMETA)
        self.__dict__.update(metadata)
        self.path = path

    def __str__(self):
        """ Return sorted, possibly pruned, dictionary data. """
        data = self.__dict__.copy()
        if 'owner' in data:
            del data['owner']
        return pprint.pformat(data).replace('\n', '')

    def get(self, attr, default):
        """ Return `attr` value, or default if `attr` has not been defined. """
        try:
            return getattr(self, attr)
        except AttributeError:
            return default

        
class FileRef(FileMetadata):
    """
    A reference to a file on disk. As well as containing metadata information,
    it supports :meth:`open` to read the file's contents. Before :meth:`open` \
    is called, 'owner' must be set to an object supporting :meth:`check_path` \
    and :meth:`get_abs_directory` (typically a :class:`Component` or one of \
    its child :class:`Container` objects).
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
            try:
                self.owner.check_path(path)
            except AttributeError:
                owner = _get_valid_owner(self.owner)
                if owner is None:
                    raise ValueError("Path '%s' is absolute and no path checker"
                                     " is available." % path)
                self.owner = owner
                self.owner.check_path(path)
        else:
            try:
                directory = self.owner.get_abs_directory()
            except AttributeError:
                owner = _get_valid_owner(self.owner)
                if owner is None:
                    raise ValueError("Path '%s' is relative and no absolute"
                                     " directory is available." % path)
                self.owner = owner
                directory = self.owner.get_abs_directory()
            path = os.path.join(directory, path)
        mode = 'rb' if self.binary else 'rU'
        return open(path, mode)

def _get_valid_owner(owner):
    """ Try to find an owner that supports the required functionality. """
    while owner is not None:
        if hasattr(owner, 'check_path') and \
           hasattr(owner, 'get_abs_directory'):
            return owner
        if hasattr(owner, 'parent'):
            owner = owner.parent
        else:
            return None
    return None

    
