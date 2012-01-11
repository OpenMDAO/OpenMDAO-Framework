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


class AllocatorWrapper(object):
    """
    Wraps a :class:`ResourceAllocator` in the DMZ protocol.

    allocator: :class:`ResourceAllocator`
        The allocator to wrap.

    client: string
        Name of client.

    connection: :class:`Connection`
        The connection to serve.

    poll_delay: int
        Polling delay (seconds).
    """

    _legal_methods = set(('max_servers', 'time_estimate'))
    _local_methods = set(('deploy', 'release', 'shutdown'))

    def __init__(self, allocator, client, connection, poll_delay):
        self._allocator = allocator
        self._client = client
        self._conn = connection
        self._poll_delay = poll_delay
        self._logger = connection.logger
        self._wrappers = {}
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
                if method in self._legal_methods:
                    function = getattr(self._allocator, method)
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
        server = self._allocator.deploy(fullname, resource_desc, criteria)
        if server is None:
            raise RuntimeError('deploy(%r, %r, %r) failed'
                               % (fullname, resource_desc, criteria))

        path = '%s/%s' % (self._conn.root, name)
        logger = logging.getLogger(fullname)
        connection = Connection(self._conn.dmz_host, path, True,
                                self._poll_delay, logger)
        wrapper = ServerWrapper(server, connection, self._poll_delay)

        handler = threading.Thread(name='%s_handler' % fullname,
                                   target=wrapper.process_requests)
        handler.daemon = True
        handler.start()

        self._wrappers[wrapper.conn.root] = (wrapper, handler)
        return (wrapper.conn.root, wrapper.server.pid)

    def release(self, root):
        """
        Lookup server associated with `root` and have allocator release that.

        root: string
            Path to communications root directory.
        """
        wrapper, handler = self._wrappers.pop(root)
        self._allocator.release(wrapper.server)
        wrapper.stop = True
        handler.join(self._poll_delay * 3)

    def shutdown(self):
        """ Shutdown this allocator. """
        self.stop = True


class ServerWrapper(object):
    """ Wraps an :class:`ObjServer` in the DMZ protocol.

    server: :class:`ObjServer`
        The server to wrap.

    connection: :class:`Connection`
        The connection to serve.

    poll_delay: int
        Polling delay (seconds).
    """

    _legal_methods = set(('echo', 'execute_command',
                          'pack_zipfile', 'unpack_zipfile',
                          'chmod', 'isdir', 'listdir', 'remove'))
    _local_methods = set(('getfile', 'putfile', 'stat'))

    def __init__(self, server, connection, poll_delay):
        self._server = server
        self._conn = connection
        self._poll_delay = poll_delay
        self._logger = connection.logger
        self.stop = False

    @property
    def server(self):
        """ Object server. """
        return self._server

    @property
    def conn(self):
        """ DMZ protocol connection. """
        return self._conn

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
                if method in self._legal_methods:
                    function = getattr(self._server, method)
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
            filexfer(None, path, self._server, filename)
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
            filexfer(self._server, filename, None, path)
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
        info = self._server.stat(path)
        data = []
        for i in range(10):
            data.append(info[i])
        return data

