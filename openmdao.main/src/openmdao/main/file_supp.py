"""
Support for files, either as :class:`File` or external files.
"""
import pprint
import sys

from openmdao.main.rbac import rbac, rbac_decorate

__all__ = ('FileMetadata', 'RemoteFile')

# Standard metadata and default values.
_FILEMETA = {
    'path': '',
    'desc': '',
    'content_type': '',
    'platform': sys.platform,
    'binary': False,
    'big_endian': sys.byteorder == 'big',
    'single_precision': False,
    'integer_8': False,
    'unformatted': False,
    'recordmark_8': False,
}


class FileMetadata(object):
    """
    Metadata related to a file, specified by keyword arguments (except for
    'path').  By default, the metadata includes:

    - 'path' -- a string, no default value. It may be a :mod:`glob`-style \
      pattern in the case of an external file description. Non-absolute paths \
      are relative to their owning component's directory.
    - 'desc' -- a string, default null.
    - 'content_type' -- a string, default null.
    - 'platform' -- string, default is the value of :mod:`sys.platform`.
    - 'binary' -- boolean, default False.
    - 'big_endian' -- boolean, default set from :mod:`sys.byteorder`. \
      Only meaningful if binary.
    - 'single_precision' -- boolean, default False. Only meaningful if binary.
    - 'integer_8' -- boolean, default False. Only meaningful if binary.
    - 'unformatted' -- boolean, default False. Only meaningful if binary.
    - 'recordmark_8' -- boolean, default False. Only meaningful if unformatted.

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

    def json_encode(self):
        """ Return sorted, possibly pruned, dictionary data. """
        data = self.__dict__.copy()
        if 'owner' in data:
            del data['owner']
        return data

    def get(self, attr, default):
        """ Return `attr` value, or default if `attr` has not been defined. """
        try:
            return getattr(self, attr)
        except AttributeError:
            return default


class RemoteFile(object):
    """
    Wraps a :class:`file` with remote-access annotations such that only role
    'owner' may access the file.
    """

    def __init__(self, fileobj):
        self.fileobj = fileobj

    @property
    def closed(self):
        """ True if file is not open. """
        return self.fileobj.closed

    # Decorated below since we need to proxy ourselves.
    def __enter__(self):
        """ Enter context. """
        self.fileobj.__enter__()
        return self

    @rbac('owner')
    def __exit__(self, exc_type, exc_value, traceback):
        """ Exit context. """
        return self.fileobj.__exit__(exc_type, exc_value, traceback)

    @rbac('owner')
    def close(self):
        """ Close the file. """
        return self.fileobj.close()

    @rbac('owner')
    def flush(self):
        """ Flush any buffered output. """
        return self.fileobj.flush()

    @rbac('owner')
    def __iter__(self):
        """ Return iterator. """
        self.fileobj.__iter__()
        return self

    @rbac('owner')
    def next(self):
        """ Return next input line or raise StopIteration. """
        return self.fileobj.next()

    @rbac('owner')
    def read(self, size=-1):
        """ Read up to `size` bytes. """
        return self.fileobj.read(size)

    @rbac('owner')
    def readline(self, size=-1):
        """ Read one line. """
        return self.fileobj.readline(size)

    @rbac('owner')
    def readlines(self, sizehint=-1):
        """ Read until EOF. """
        return self.fileobj.readlines(sizehint)

    @rbac('owner')
    def write(self, data):
        """ Write `data` to the file. """
        return self.fileobj.write(data)

rbac_decorate(RemoteFile.__enter__, 'owner', proxy_types=(RemoteFile,))
rbac_decorate(RemoteFile.__iter__,  'owner', proxy_types=(RemoteFile,))

