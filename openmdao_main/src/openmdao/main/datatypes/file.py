"""
Support for file variables.
"""

import copy
import os.path

# pylint: disable-msg=E0611,F0401
from openmdao.main.file_supp import FileMetadata, RemoteFile
from openmdao.main.rbac import rbac
from openmdao.main.variable import Variable

#Public Symbols
__all__ = ['File', 'FileRef']


class File(Variable):
    """
    A trait wrapper for a :class:`FileRef` object.

    If `default_value` is a string, then a :class:`FileRef` will be created
    using that for the path.

    If `default_value` is a file object, then a :class:`FileRef` will be
    created for the file's name, if the named file exists.

    For input files :attr:`legal_types` may be set to a list of expected
    'content_type' strings. Then upon assignment the actual 'content_type'
    must match one of the :attr:`legal_types` strings.  Also, if
    :attr:`local_path` is set, then upon assignent the associated file will
    be copied to that path.
    """
    
    def __init__(self, default_value=None, iotype=None, **metadata):
        
        if default_value is not None:
            if isinstance(default_value, FileRef):
                pass
            elif isinstance(default_value, basestring):
                default_value = FileRef(default_value)
            elif isinstance(default_value, file) and \
                 hasattr(default_value, 'name') and \
                 os.path.exists(default_value.name):
                default_value = FileRef(default_value.name)
            else:
                raise TypeError('File default value must be a FileRef.')
            
        if iotype is not None:
            metadata['iotype'] = iotype
            if iotype == 'out':
                if 'legal_types' in metadata:
                    raise ValueError("'legal_types' invalid for output File.")
                if 'local_path' in metadata:
                    raise ValueError("'local_path' invalid for output File.")

        # iotype of None => we can't check anything.
            
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

        if isinstance(value, basestring):
            value = FileRef(value)
        elif isinstance(value, file) and hasattr(value, 'name') and \
             os.path.exists(value.name):
            value = FileRef(value.name)

        if isinstance(value, FileRef):
            legal_types = self._metadata.get('legal_types', None)
            if legal_types:
                if value.content_type not in legal_types:
                    raise ValueError("Content type '%s' not one of %s"
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
        if iotype == 'out':
            if value.owner is None:
                value.owner = obj
            return

        path = self._metadata.get('local_path', None)
        if not path:
            return

        owner = _get_valid_owner(obj)
        if os.path.isabs(path):
            if owner is None:
                raise ValueError('local_path %s is absolute and no path checker'
                                 ' is available.' % path)
            owner.check_path(path)
        else:
            if owner is None:
                raise ValueError('local_path %s is relative and no absolute'
                                 ' directory is available.' % path)
            directory = owner.get_abs_directory()
            path = os.path.join(directory, path)

        # If accessing same path on same host (i.e. passthrough), skip.
        if isinstance(value, FileRef):
            try:
                src_path = value.abspath()
            except Exception as exc:
                raise RuntimeError("Can't get source path for local copy: %s"
                                   % (str(exc) or repr(exc)))
            else:
                if path == src_path:
                    return

        mode = 'wb' if value.binary else 'w'
        try:
            src = value.open()
        except Exception as exc:
            raise RuntimeError("Can't open source for local copy: %s"
                               % (str(exc) or repr(exc)))
        try:
            dst = open(path, mode)
        except Exception as exc:
            src.close()
            raise RuntimeError("Can't open destination for local copy: %s"
                               % (str(exc) or repr(exc)))

        chunk = 1 << 20  # 1MB
        data = src.read(chunk)
        while data:
            dst.write(data)
            data = src.read(chunk)
        src.close()
        dst.close()


class FileRef(FileMetadata):
    """
    A reference to a file on disk. As well as containing metadata information,
    it supports :meth:`open` to read the file's contents. Before :meth:`open`
    is called, 'owner' must be set to an object supporting :meth:`check_path`
    and :meth:`get_abs_directory` (typically a :class:`Component` or one of
    its child :class:`Container` objects).
    """

    def __init__(self, path, owner=None, **metadata):
        super(FileRef, self).__init__(path, **metadata)
        self.owner = owner

    def copy(self, owner):
        """
        Return a copy of ourselves, owned by `owner`.

        owner: Component
            The component used to determine the root for relative paths
            and checking the legality of absolute paths.
        """
        ref = copy.copy(self)
        ref.owner = owner
        return ref

    def abspath(self):
        """ Return absolute path to file. """
        if self.owner is None:
            raise ValueError("abspath() failed: no owner specified for FileRef")
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
        return path

    @rbac('owner', proxy_types=[RemoteFile])
    def open(self):
        """ Open file for reading. """
        mode = 'rb' if self.binary else 'rU'
        return RemoteFile(open(self.abspath(), mode))


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

