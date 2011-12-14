"""
Wrappers to translate from the file protocol to :class:`ResourceAllocator` and
:class:`ObjServer`.
"""

from __future__ import absolute_import

import logging
import os.path
import shutil
import threading
import time
import traceback

from openmdao.util.filexfer import filexfer

from .protocol import Connection


class AllocatorWrapper(object):
    """
    Wraps a :class:`ResourceAllocator` in the file protocol.

    allocator: :class:`ResourceAllocator`
        The allocator to wrap.

    connection: :class:`Connection`
        The connection to serve.
    """

    _legal_methods = set(('max_servers', 'time_estimate', 'deploy'))
    _local_methods = set(('release', 'shutdown'))

    def __init__(self, allocator, connection):
        print 'AllocatorWrapper %r %r' % (allocator.name, connection)
        self._allocator = allocator
        self._conn = connection
        self._poll_delay = 1
        self._logger = connection._logger
        self._wrappers = {}
        self.stop = False

    def process_requests(self):
        """
        Wait for a request, process it, and send the reply.
        If the request result is a :class:`ObjectServer`, then a
        :class:`ServerWrapper` is created for it and the reply data is
        the associated :class:`_ServerWrapperInfo`.
        """
        while not self.stop:
            print 'AW process_requests: waiting...'
            if not self._conn.poll_request():
                time.sleep(self._poll_delay)
                continue
            method, args, kwargs = self._conn.recv_request(wait=False)
            print 'AW received request: %r %r %r' % (method, args, kwargs)
            self._logger.debug('received request: %r %r %r',
                               method, args, kwargs)
            try:
                if method in self._legal_methods:
                    function = getattr(self._allocator, method)
                elif method in self._local_methods:
                    function = getattr(self, method)
                else:
                    raise RuntimeError('illegal method %r' % method)
                result = function(*args, **kwargs)
            except Exception as exc:
                traceback.print_exc()
                self. logger.error('Exception: %s', exc)
                self._conn.send_exception(exc)
            else:
                if method == 'deploy':
                    # Return how to connect to server thread.
                    path = '%s/%s' % (self._conn.root, result.name)
                    name = '%s_wrapper' % result.name
                    logger = logging.getLogger(name)
                    connection = Connection(self._conn.dmz_host, path, True,
                                            logger)
                    wrapper = ServerWrapper(result, connection)
                    handler = threading.Thread(name=name,
                                               target=wrapper.process_requests)
                    handler.daemon = True
                    handler.start()
                    result = _ServerWrapperInfo(wrapper)
                    self.add_wrapper(wrapper, handler)
                print 'sending reply %r' % (result,)
                self._logger.debug('sending reply')
                self._conn.send_reply(result)

        print 'AW done'
        for root in self._wrappers.keys():
            self.release(root)
        shutil.rmtree(self._conn.root)

    def add_wrapper(self, wrapper, handler):
        """
        Remember server for later release.

        wrapper: :class:`ServerWrapper`
            Wrapper to be added.

        handler: :class:`Thread`
            Thread associated with `wrapper` to be added.
        """
        print 'add_wrapper', wrapper, wrapper.conn.root
        print '    handler', handler
        self._wrappers[wrapper.conn.root] = (wrapper, handler)

    def release(self, root):
        """
        Lookup server associated with `root` and have allocator release that.

        root: string
            Path to communications root directory.
        """
        print 'release', root
        wrapper, handler = self._wrappers[root]
        del self._wrappers[root]
        self._allocator.release(wrapper.server)
        wrapper.stop = True
        handler.join(wrapper.poll_delay*3)
        print 'handler %r joined' % handler

    def shutdown(self):
        """ Shutdown this allocator. """
        self.stop = True


class ServerWrapper(object):
    """ Wraps an :class:`ObjServer` in the file protocol.

    server: :class:`ObjServer`
        The server to wrap.

    connection: :class:`Connection`
        The connection to serve.
    """

    _legal_methods = set(('echo', 'execute_command',
                          'pack_zipfile', 'unpack_zipfile',
                          'chmod', 'isdir', 'listdir', 'remove', 'stat'))
    _local_methods = set(('getfile', 'putfile'))

    def __init__(self, server, connection):
        self._server = server
        self._conn = connection
        self._poll_delay = 1
        self._logger = connection._logger
        self.stop = False

    @property
    def server(self):
        """ Object server. """
        return self._server

    @property
    def conn(self):
        """ DMZ protocol connection. """
        return self._conn

    @property
    def poll_delay(self):
        """ Delay between polling for a new request. """
        return self._poll_delay

    def process_requests(self):
        """ Wait for a request, process it, and send the reply. """
        while not self.stop:
            print 'SW process_requests: waiting...'
            if not self._conn.poll_request():
                time.sleep(self._poll_delay)
                continue
            method, args, kwargs = self._conn.recv_request(wait=False)
            print 'SW received request: %r %r %r' % (method, args, kwargs)
            self._logger.debug('received request: %r %r %r',
                               method, args, kwargs)
            try:
                if method in self._legal_methods:
                    function = getattr(self._server, method)
                elif method in self._local_methods:
                    function = getattr(self, method)
                else:
                    raise RuntimeError('illegal method %r' % method)
                result = function(*args, **kwargs)
            except Exception as exc:
                traceback.print_exc()
                self._logger.error('Exception: %s', exc)
                self._conn.send_exception(exc)
            else:
                self._conn.send_reply(result)

        print 'SW done'
        shutil.rmtree(self._conn.root)

    def getfile(self, filename):
        """
        Copy `filename` from remote file server.

        filename: string
            Name of file to receive.
        """
        self._conn.recv_file(filename)
        local = os.path.join(self._conn.root, filename)
        filexfer(None, local, self._server, filename)
        self._conn.remove_file(filename)
        os.remove(local)

    def putfile(self, filename):
        """
        Copy `filename` to remote file server.

        filename: string
            Name fo file to send.
        """
        # Local copy will be removed when connection is closed.
        filexfer(self._server, filename,
                 None, os.path.join(self._conn.root, filename))
        self._conn.send_file(filename)


class _ServerWrapperInfo(object):
    """
    Information used to connect to `wrapper`.

    wrapper: :class:`ServerWrapper`
        Wrapper to connect to.
    """

    def __init__(self, wrapper):
        self.dmz_host = wrapper.conn.dmz_host
        self.root = wrapper.conn.root
        self.pid = wrapper.server.pid

