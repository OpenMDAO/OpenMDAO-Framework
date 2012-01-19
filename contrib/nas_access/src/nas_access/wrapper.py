"""
Wrappers to translate from the DMZ protocol to :class:`ResourceAllocator` and
:class:`ObjServer`.
"""

from __future__ import absolute_import

import logging
import os.path
import tempfile
import threading
import time
import traceback

from openmdao.util.filexfer import filexfer

from .protocol import Connection, RemoteError


class BaseWrapper(object):
    """
    Base class for wrappers using the DMZ protocol.

    delegate: object
        The wrapped object.

    delegate_methods: set(string)
        Names of legal delegate methods.

    local_methods: set(string)
        Names of legal local methods.

    connection: :class:`Connection`
        The connection to serve.
    """

    def __init__(self, delegate, delegate_methods, local_methods, connection):
        self._delegate = delegate
        self._delegate_methods = delegate_methods
        self._local_methods = local_methods
        self._conn = connection
        self._poll_delay = connection.poll_delay
        self._logger = connection.logger
        self.stop = False

    def process_requests(self):
        """ Wait for a request, process it, and send the reply. """
        delay = 1
        while not self.stop:
            time.sleep(delay)
            if not self._conn.poll_request():
                delay = min(delay + 1, self._poll_delay)  # Back-off.
                continue
            delay = 1  # Reset to high rate.

            method, args, kwargs = self._conn.recv_request(wait=False)
            self._logger.debug('request: %r %r %r', method, args, kwargs)
            try:
                if method in self._delegate_methods:
                    function = getattr(self._delegate, method)
                elif method in self._local_methods:
                    function = getattr(self, method)
                else:
                    raise RuntimeError('illegal method %r' % method)
                result = function(*args, **kwargs)
            except Exception as exc:
                tback = traceback.format_exc()
                self._logger.error('Exception: %s', exc)
                self._logger.error(tback)
                self._conn.send_reply(RemoteError(exc, tback))
            else:
                self._conn.send_reply(result)

        self.cleanup()

    def cleanup(self):
        """ Called to clean up any allocated resources. """
        return


class AllocatorWrapper(BaseWrapper):
    """
    Wraps a :class:`ResourceAllocator` in the DMZ protocol.

    allocator: :class:`ResourceAllocator`
        The allocator to wrap.

    client: string
        Name of client.

    connection: :class:`Connection`
        The connection to serve.
    """

    _delegate_methods = set(('max_servers', 'time_estimate'))
    _local_methods = set(('deploy', 'release', 'shutdown'))

    def __init__(self, allocator, client, connection):
        super(AllocatorWrapper, self).__init__(allocator,
                                               self._delegate_methods,
                                               self._local_methods, connection)
        self._client = client
        self._wrappers = {}

    def cleanup(self):
        """ Called to clean up any allocated resources. """
        for root in self._wrappers.keys():
            self.release(root)

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns how to connect to server wrapper.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        fullname = '%s-%s' % (self._client, name)
        server = self._delegate.deploy(fullname, resource_desc, criteria)
        if server is None:
            raise RuntimeError('deploy(%r, %r, %r) failed'
                               % (fullname, resource_desc, criteria))

        path = '%s/%s' % (self._conn.root, name)
        logger = logging.getLogger(fullname)
        connection = Connection(self._conn.dmz_host, path, True,
                                self._poll_delay, logger)
        wrapper = ServerWrapper(server, connection, fullname)

        handler = threading.Thread(name='%s_handler' % fullname,
                                   target=wrapper.process_requests)
        handler.daemon = True
        handler.start()

        self._wrappers[connection.root] = (wrapper, handler, server)
        return (connection.root, server.pid)

    def release(self, root):
        """
        Lookup server associated with `root` and have allocator release that.

        root: string
            Path to communications root directory.
        """
        wrapper, handler, server = self._wrappers.pop(root)
        wrapper.stop = True
        handler.join(self._poll_delay * 3)
        self._delegate.release(server)

    def shutdown(self):
        """ Shutdown this allocator. """
        self.stop = True


class ServerWrapper(BaseWrapper):
    """
    Wraps an :class:`ObjServer` in the DMZ protocol.

    server: :class:`ObjServer`
        The server to wrap.

    connection: :class:`Connection`
        The connection to serve.

    name: string
        Name of server.
    """

    _delegate_methods = set(('echo', 'execute_command',
                             'pack_zipfile', 'unpack_zipfile',
                             'chmod', 'isdir', 'listdir', 'remove'))
    _local_methods = set(('getfile', 'putfile', 'stat', 'load_model'))

    def __init__(self, server, connection, name):
        super(ServerWrapper, self).__init__(server, self._delegate_methods,
                                            self._local_methods, connection)
        self._name = name
        self._models = set()
        self._wrappers = {}

    def cleanup(self):
        """ Called to clean up any allocated resources. """
        for root in self._wrappers.keys():
            self._release(root)

    def getfile(self, filename):
        """
        Copy `filename` from remote file server.

        filename: string
            Name of file to receive.
        """
        fd, path = tempfile.mkstemp()
        try:
            os.close(fd)
            self._conn.recv_file(filename, path)
            filexfer(None, path, self._delegate, filename)
            self._conn.remove_files((filename,))
        finally:
            try:
                os.remove(path)
            except Exception as exc:  # pragma no cover
                self._logger.warning("Can't remove temporary file: %s", exc)

    def putfile(self, filename):
        """
        Copy `filename` to remote file server.

        filename: string
            Name of file to send.
        """
        fd, path = tempfile.mkstemp()
        try:
            os.close(fd)
            filexfer(self._delegate, filename, None, path)
            self._conn.send_file(path, filename)
        finally:
            try:
                os.remove(path)
            except Exception as exc:  # pragma no cover
                self._logger.warning("Can't remove temporary file: %s", exc)

    def stat(self, path):
        """
        Return portable portion of ``os.stat()`` on `path`.

        path: string
            Name of file to interrogate.
        """
        info = self._delegate.stat(path)
        data = []
        for i in range(10):
            data.append(info[i])
        return data

    def load_model(self, egg_filename):
        """
        Load model from egg and return top-level object if this server's
        `allow_shell` attribute is True.

        egg_filename: string
            Filename of egg to be loaded.
        """
        base, dash, rest = egg_filename.partition('-')
        i = 1
        name = base
        while name in self._models:  # Ensure unique name.
            i += 1
            name = '%s_%d' % (base, i)
        self._models.add(name)
        
        tlo = self._delegate.load_model(egg_filename)
        if tlo is None:
            raise RuntimeError('load_model(%r) failed' % egg_filename)

        path = '%s/%s' % (self._conn.root, name)
        fullname = '%s-%s' % (self._name, name)
        logger = logging.getLogger(fullname)
        connection = Connection(self._conn.dmz_host, path, True,
                                self._poll_delay, logger)
        wrapper = ComponentWrapper(tlo, connection)

        handler = threading.Thread(name='%s_handler' % fullname,
                                   target=wrapper.process_requests)
        handler.daemon = True
        handler.start()

        self._wrappers[connection.root] = (wrapper, handler, tlo)
        return connection.root

    def _release(self, root):
        """
        Lookup server associated with `root` and have allocator release that.

        root: string
            Path to communications root directory.
        """
        wrapper, handler, tlo = self._wrappers.pop(root)
        wrapper.stop = True
        handler.join(self._poll_delay * 3)
        tlo.pre_delete()


class ComponentWrapper(BaseWrapper):
    """
    Wraps a :class:`Component` in the DMZ protocol.

    component: :class:`Component`
        The component to wrap.

    connection: :class:`Connection`
        The connection to serve.
    """

    _delegate_methods = set(('get', 'set', 'run'))
    _local_methods = set()

    def __init__(self, component, connection):
        super(ComponentWrapper, self).__init__(component,
                                               self._delegate_methods,
                                               self._local_methods, connection)

