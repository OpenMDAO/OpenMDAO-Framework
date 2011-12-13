"""
Wrappers to translate from the file protocol to :class:`ResourceAllocator` and
:class:`ObjServer`.
"""

from __future__ import absolute_import

import logging
import os.path
import threading
import time
import traceback

from openmdao.main.objserverfactory import ObjServer

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

    legal_methods = set(('max_servers', 'time_estimate', 'deploy'))
    local_methods = set(('release', 'shutdown'))

    def __init__(self, allocator, connection):
        print 'AllocatorWrapper %r %r' % (allocator.name, connection)
        self.allocator = allocator
        self.conn = connection
        self.poll_delay = 1
        self.logger = connection.logger
        self.wrappers = {}
        self.stop = False

    def process_requests(self):
        """
        Wait for a request, process it, and send the reply.
        If the request result is a :class:`ObjectServer`, then a
        :class:`ServerWrapper` is created for it and the reply data is
        the associated :class:`ServerWrapperInfo`.
        """
        while not self.stop:
            print 'AW process_requests: waiting...'
            if not self.conn.poll_request():
                time.sleep(self.poll_delay)
                continue
            method, args, kwargs = self.conn.recv_request(wait=False)
            print 'AW received request: %r %r %r' % (method, args, kwargs)
            self.logger.debug('received request: %r %r %r',
                              method, args, kwargs)
            try:
                if method in self.legal_methods:
                    function = getattr(self.allocator, method)
                elif method in self.local_methods:
                    function = getattr(self, method)
                else:
                    raise RuntimeError('illegal method %r' % method)
                result = function(*args, **kwargs)
            except Exception as exc:
                traceback.print_exc()
                self.logger.error('Exception: %s', exc)
                self.conn.send_exception(exc)
            else:
                if method == 'deploy':
                    # Return how to connect to server thread.
                    path = '%s/%s' % (self.conn.root, result.name)
                    name = '%s_wrapper' % result.name
                    logger = logging.getLogger(name)
                    connection = Connection(self.conn.dmz_host, path, True,
                                            logger)
                    wrapper = ServerWrapper(result, connection)
                    handler = threading.Thread(name=name,
                                               target=wrapper.process_requests)
                    handler.daemon = True
                    handler.start()
                    result = ServerWrapperInfo(wrapper)
                    self.add_wrapper(wrapper, handler)
                print 'sending reply %r' % (result,)
                self.logger.debug('sending reply')
                self.conn.send_reply(result)
        print 'AW done'

    def add_wrapper(self, wrapper, handler):
        """ Remember server for later release. """
        print 'add_wrapper', wrapper, wrapper.conn.root
        print '    handler', handler
        self.wrappers[wrapper.conn.root] = (wrapper, handler)

    def release(self, root):
        """ Lookup real server and have allocator release that. """
        print 'release', root
        wrapper, handler = self.wrappers[root]
        self.allocator.release(wrapper.server)
        wrapper.stop = True
        handler.join(wrapper.poll_delay*3)
        print 'handler %r joined' % handler

    def shutdown(self):
        """ Shutdown this allocator. """
        self.stop = True


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
    local_methods = set(('getfile', 'putfile'))

    def __init__(self, server, connection):
        self.server = server
        self.conn = connection
        self.poll_delay = 1
        self.logger = connection.logger
        self.stop = False

    def process_requests(self):
        """ Wait for a request, process it, and send the reply. """
        while not self.stop:
            print 'SW process_requests: waiting...'
            if not self.conn.poll_request():
                time.sleep(self.poll_delay)
                continue
            method, args, kwargs = self.conn.recv_request(wait=False)
            print 'SW received request: %r %r %r' % (method, args, kwargs)
            self.logger.debug('received request: %r %r %r',
                              method, args, kwargs)
            try:
                if method in self.legal_methods:
                    function = getattr(self.server, method)
                elif method in self.local_methods:
                    function = getattr(self, method)
                else:
                    raise RuntimeError('illegal method %r' % method)
                result = function(*args, **kwargs)
            except Exception as exc:
                traceback.print_exc()
                self.logger.error('Exception: %s', exc)
                self.conn.send_exception(exc)
            else:
                self.conn.send_reply(result)
        print 'SW done'

    def getfile(self, filename):
        """ Copy `filename` from remote file server. """
        self.conn.recv_file(filename)
        filexfer(None, os.path.join(self.conn.root, filename),
                 self.server, filename)
        self.conn.remove_file(filename)

    def putfile(self, filename):
        """ Copy `filename` to remote file server. """
        filexfer(self.server, filename,
                 None, os.path.join(self.conn.root, filename))
        self.conn.send_file(filename)


class ServerWrapperInfo(object):
    """ Information used to connect to an :class:`AllocatorWrapper`. """

    def __init__(self, wrapper):
        self.dmz_host = wrapper.conn.dmz_host
        self.root = wrapper.conn.root
        self.pid = wrapper.server.pid

