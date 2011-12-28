"""
Proxy classes for resource allocators and object servers accessed via
the DMZ protocol.
"""

from __future__ import absolute_import

import logging
import os.path
import shutil
import sys

from openmdao.main.rbac import rbac
from openmdao.main.resource import ResourceAllocator

from .protocol import connect


class NAS_Allocator(ResourceAllocator):
    """
    Allocator which uses :class:`NAS_Server` instead of :class:`ObjServer`
    when deploying.

    By adding the allocator to the resource allocation manager, resource
    requests will interrogate the allocator to see if it could be used. This
    would typically be done by :class:`ExternalCode` to execute a
    compute-intensive or parallel application.

    name: string
        Name of allocator, used in log messages, etc.

    dmz_host: string
        Name of intermediary file server.

    server_host: string
        Name of host to communicate with.

    Example resource configuration file entry::

        [Pleiades]
        classname: nas_access.NAS_Allocator
        dmz_host: dmzfs1.nas.nasa.gov
        server_host: pfe1.nas.nasa.gov

    """

    def __init__(self, name='NAS_Allocator', dmz_host=None, server_host=None):
        super(NAS_Allocator, self).__init__(name)
        self._servers = []
        self._dmz_host = dmz_host
        self._server_host = server_host
        self._logger.debug('init')
        if dmz_host and server_host:
            self._conn = connect(dmz_host, server_host, name, self._logger)
            self._logger.debug('connected to %r on %r', server_host, dmz_host)

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying 'dmz_host' and 'server_host'.
        """
        if cfg.has_option(self.name, 'dmz_host'):
            self._dmz_host = cfg.get(self.name, 'dmz_host')
            self._logger.debug('    dmz_host: %s', self._dmz_host)

        if cfg.has_option(self.name, 'server_host'):
            self._server_host = cfg.get(self.name, 'server_host')
            self._logger.debug('    server_host: %s', self._server_host)

        if self._dmz_host and self._server_host:
            self._conn = connect(self._dmz_host, self._server_host, self.name,
                                 self._logger)
            self._logger.debug('connected')

    @rbac('*')
    def max_servers(self, resource_desc):
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.

        resource_desc: dict
            Description of required resources.
        """
        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return 0
        return self._conn.invoke('max_servers', (rdesc,))

    @rbac('*')
    def time_estimate(self, resource_desc):
        """
        Return ``(estimate, criteria)`` indicating how well this resource
        allocator can satisfy the `resource_desc` request.  The estimate will
        be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as hostnames, load averages, unsupported
        resources, etc.

        resource_desc: dict
            Description of required resources.
        """
        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return info
        return self._conn.invoke('time_estimate', (rdesc,))

    def _check_local(self, resource_desc):
        """ Check locally-relevant resources. """
        rdesc = resource_desc.copy()
        for key in ('localhost', 'allocator'):
            if key not in rdesc:
                continue
            value = rdesc[key]
            if key == 'localhost':
                if value:
                    return None, (-2, {'localhost': 'requested local host'})
            if key == 'allocator':
                if value != self.name:
                    return None, (-2, {'allocator': 'wrong allocator'})
            del rdesc[key]
        return (rdesc, None)

    @rbac('*')
    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        r_root, r_pid = \
            self._conn.invoke('deploy', (name, resource_desc, criteria))
        proxy_name = '%s/%s' % (self.name, name)
        server = NAS_Server(proxy_name, self._dmz_host, self._server_host,
                            r_pid, r_root)
        self._servers.append(server)
        return server

    @rbac(('owner', 'user'))
    def release(self, server):
        """
        Release `server`.

        server: :class:`NAS_Server`
            Server to be released.
        """
        if server in self._servers:
            self._servers.remove(server)
            self._conn.invoke('release', (server.conn.root,))
            server.shutdown()
        else:
            raise ValueError('No such server %r' % server)

    def shutdown(self):
        """ Shut-down this allocator. """
        for server in self._servers:
            server.shutdown()
        self._conn.invoke('shutdown')
        self._conn.close()
        shutil.rmtree(os.path.dirname(self._conn.root))


class NAS_Server(object):
    """ Knows about executing a command via DMZ protocol. """

    def __init__(self, name, dmz_host, server_host, pid, path):
        self._name = name
        self._host = server_host
        self._pid = pid
        self._logger = logging.getLogger(name)
        self._conn = connect(dmz_host, server_host, path, self._logger)
        self._close = _Finalizer()

    @property
    def name(self):
        """ Name of this server. """
        return self._name

    @property
    def host(self):
        """ Host this server is running on. """
        return self._host

    @property
    def pid(self):
        """ Process ID of server. """
        return self._pid

    @property
    def conn(self):
        """ DMZ protocol connection. """
        return self._conn

    def shutdown(self):
        """ Shut-down this server. """
        self._logger.debug('shutdown')
        self._conn.close()

    @rbac('*')
    def echo(self, *args):
        """
        Simply return the arguments. This can be useful for latency/thruput
        masurements, connectivity testing, firewall keepalives, etc.
        """
        return self._conn.invoke('echo', args)

    @rbac('owner')
    def execute_command(self, resource_desc):
        """
        Submit command based on `resource_desc`.

        resource_desc: dict
            Description of command and required resources.

        Forwards request via DMZ protocol and waits for reply.
        """
        return self._conn.invoke('execute_command', (resource_desc,))

    @rbac('owner')
    def pack_zipfile(self, patterns, filename):
        """
        Create ZipFile of files matching `patterns` if `filename` is legal.

        patterns: list
            List of :mod:`glob`-style patterns.

        filename: string
            Name of ZipFile to create.
        """
        return self._conn.invoke('pack_zipfile', (patterns, filename))

    @rbac('owner')
    def unpack_zipfile(self, filename, textfiles=None):
        """
        Unpack ZipFile `filename` if `filename` is legal.

        filename: string
            Name of ZipFile to unpack.
        """
        return self._conn.invoke('unpack_zipfile', (filename, textfiles))

    @rbac('owner')
    def chmod(self, path, mode):
        """
        Returns ``os.chmod(path, mode)`` if `path` is legal.

        path: string
            Path to file to modify.

        mode: int
            New mode bits (permissions).
        """
        return self._conn.invoke('chmod', (path, mode))

    @rbac('owner')
    def isdir(self, path):
        """
        Returns ``os.path.isdir(path)`` if `path` is legal.

        path: string
            Path to check.
        """
        return self._conn.invoke('isdir', (path,))

    @rbac('owner')
    def listdir(self, path):
        """
        Returns ``os.listdir(path)`` if `path` is legal.

        path: string
            Path to directory to list.
        """
        return self._conn.invoke('listdir', (path,))

    @rbac('owner')
    def open(self, filename, mode='r', bufsize=-1):
        """
        Returns ``open(filename, mode, bufsize)`` if `filename` is legal.

        filename: string
            Name of file to open.

        mode: string
            Access mode.

        bufsize: int
            Size of buffer to use.
        """
        self._logger.debug('open %r %r %s', filename, mode, bufsize)
        if 'r' in mode:
            # Transfer file here and then return regular file object.
            # Local copy will be removed when connection is closed.
            self._conn.invoke('putfile', (filename,))
            self._conn.recv_file(filename)
            self._conn.remove_file(filename)
            return open(os.path.join(self._conn.root, filename), mode, bufsize)
        else:
            # Write file here and upon closing transfer to remote.
            return _File(filename, mode, bufsize, self._conn)

    @rbac('owner')
    def remove(self, path):
        """
        Remove `path` if `path` is legal.

        path: string
            Path to file to remove.
        """
        return self._conn.invoke('remove', (path,))

    @rbac('owner')
    def stat(self, path):
        """
        Returns ``os.stat(path)`` if `path` is legal.

        path: string
            Path to file to interrogate.
        """
        info = self._conn.invoke('stat', (path,))
        if sys.platform == 'win32':  #pragma no cover
            import nt
            return nt.stat_result(info)
        else:
            import posix
            return posix.stat_result(info)


class _File(object):
    """
    Something that acts like a file, but via DMZ protocol.
    This is only used for files being written.

    filename: string
        Name of file to open.

    mode: string
        Access mode.

    bufsize: int
        Size of buffer to use.
    
    conn: :class:`Connection`
        Connection to remote server.
    """

    def __init__(self, filename, mode, bufsize, conn):
        self._filename = filename
        self._fileobj = open(os.path.join(conn.root, filename), mode, bufsize)
        self._conn = conn

    def close(self):
        """ Close the file and send to remote. """
        self._fileobj.close()
        self._conn.send_file(self._filename)
        self._conn.invoke('getfile', (self._filename,))
        os.remove(os.path.join(self._conn.root, self._filename))

    def flush(self):
        """ Flush buffered data. """
        self._fileobj.flush()

    def write(self, data):
        """
        Write `data`.

        data: string
            Bytes to be written.
        """
        self._fileobj.write(data)

    def writelines(self, data):
        """
        Write `data`.

        data: sequence
            Lines to be written.
        """
        self._fileobj.writelines(data)


class _Finalizer(object):
    """ Fake finalizer to look like 'real' proxy. """

    def cancel(self):
        """ Called during RAM.release(). """
        pass

