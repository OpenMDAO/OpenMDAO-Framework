"""
Wrappers to translate from the file protocol to :class:`ResourceAllocator` and
:class:`ObjServer`.
"""

from __future__ import absolute_import

import threading

from openmdao.main.objserverfactory import ObjServer

from .protocol import Connection


class AllocatorWrapper(object):
    """
    Wraps a :class:`ResourceAllocator` in the file protocol.

    allocator: :class:`ResourceAllocator`
        The allocator to wrap.

    connection: :class:`Connection`
        The connection to serve.
    """

    legal_methods = set(('max_servers', 'time_estimate', 'deploy'))

    def __init__(self, allocator, connection):
        self.allocator = allocator
        self.conn = connection

    def process_requests(self):
        """
        Wait for a request, process it, and send the reply.
        If the request result is a :class:`ObjectServer`, then a
        :class:`ServerWrapper` is created for it and the reply data is
        the associated :class:`ServerWrapperInfo`.
        """
        while True:
            method, args, kwargs = self.conn.recv_request()
            try:
                if method not in self.legal_methods:
                    raise RuntimeError('illegal method %r' % method)
                function = getattr(self.allocator, method)
                result = function(*args, **kwargs)
            except Exception as exc:
                self.conn.send_exception(exc)
            else:
                if isinstance(result, ObjServer):
                    # Return how to connect to server thread.
                    connection = Connection(self.conn.dmz_host,
                                          '%s/%s' % self.conn.root, result.name)
                    wrapper = ServerWrapper(result, connection)
                    handler = threading.Thread(target=wrapper.process_requests)
                    handler.daemon = True
                    handler.start()
                    result = ServerWrapperInfo(wrapper)
                self.conn.send_reply(result)


class AllocatorWrapperInfo(object):
    """ Information used to connect to an :class:`AllocatorWrapper`. """

    def __init__(self, wrapper):
        self.dmz_host = wrapper.dmz_host
        self.root = wrapper.root


class ServerWrapper(object):
    """ Wraps an :class:`ObjServer` in the file protocol.

    server: :class:`ObjServer`
        The server to wrap.

    connection: :class:`Connection`
        The connection to serve.
    """

    legal_methods = set(('echo', 'execute_command',
                         'pack_zipfile', 'unpack_zipfile',
                         'chmod', 'isdir', 'listdir', 'remove', 'stat'))

    def __init__(self, server, connection):
        self.server = server
        self.conn = connection

    def process_requests(self):
        """ Wait for a request, process it, and send the reply. """
        while True:
            method, args, kwargs = self.conn.recv_request()
            try:
                if method not in self.legal_methods:
                    raise RuntimeError('illegal method %r' % method)
                function = getattr(self.server, method)
                result = function(*args, **kwargs)
            except Exception as exc:
                self.conn.send_exception(exc)
            else:
                self.conn.send_reply(result)


class ServerWrapperInfo(object):
    """ Information used to connect to an :class:`AllocatorWrapper`. """

    def __init__(self, wrapper):
        self.dmz_host = wrapper.conn.dmz_host
        self.root = wrapper.conn.root

