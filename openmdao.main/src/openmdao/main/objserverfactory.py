"""
Support for an OpenMDAO 'object service', a factory that can create servers
which support various operations such as creating objects, loading models via
egg files, remote execution, and remote file access.
"""

import logging
import optparse
import os.path
import pkg_resources
import platform
import shutil
import signal
import socket
import sys
import time
import traceback

from multiprocessing import current_process, active_children, util

from openmdao.main.component import SimulationRoot
from openmdao.main.container import Container
from openmdao.main.factory import Factory
from openmdao.main.factorymanager import create, get_available_types
from openmdao.main.mp_support import OpenMDAO_Manager, OpenMDAO_Proxy, \
                                     register, write_server_config, keytype
from openmdao.main.rbac import Credentials, get_credentials, set_credentials, \
                               rbac

from openmdao.util.filexfer import pack_zipfile, unpack_zipfile
from openmdao.util.shellproc import ShellProc, STDOUT

_PROXIES = {}


class ObjServerFactory(Factory):
    """
    An :class:`ObjServerFactory` creates :class:`ObjServers` and objects
    within those servers.
    """

    def __init__(self, name='ObjServerFactory', authkey=None):
        super(ObjServerFactory, self).__init__()
        self._authkey = authkey
        self._managers = {}
        self._logger = logging.getLogger(name)
        self._logger.info('PID: %d, %r', os.getpid(), keytype(self._authkey))

    @rbac('*')
    def echo(self, *args):
        """
        Simply return the arguments. This can be useful for latency/thruput
        masurements, connectivity testing, firewall keepalives, etc.
        """
        return args

# FIXME: ('owner', 'user') can create,
#        whoever created should be the one to release, not just anybody.
#        => record credentials at creation.
    @rbac(('owner', 'user'))
    def release(self, server, remove_dir=True):
        """ Shut-down :class:`ObjServer` `server`. """
        self._logger.debug('release %r', server)
        try:
            manager, root_dir = self._managers[server]
        except KeyError:
            # Not identical to any of our proxies.
            # Could still be a reference to the same remote object.
            try:
                server_host = server.host
                server_pid = server.pid
            except Exception as exc:
                self._logger.error("release: can't identify server %r" % server)
                raise ValueError("can't identify server %r" % server)

            for key in self._managers.keys():
                if key.host == server_host and key.pid == server_pid:
                    manager, root_dir = self._managers[key]
                    server = key
                    break
            else:
                self._logger.error('release: server %r not found' % server)
                for key in self._managers.keys():
                    self._logger.debug('    %r', key)
                raise ValueError('server %r not found' % server)

        manager.shutdown()
        server._close.cancel()
        del self._managers[server]
        keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
        if not keep_dirs and os.path.exists(root_dir):
            shutil.rmtree(root_dir)

    @rbac('owner')
    def cleanup(self):
        """ Shut-down all remaining :class:`ObjServers`. """
        self._logger.debug('cleanup')
        servers = self._managers.keys()
        for server in servers:
            self.release(server)
        self._managers = {}

    @rbac('*')
    def get_available_types(self, groups=None):
        """
        Returns a set of tuples of the form ``(typename, dist_version)``,
        one for each available plugin type in the given entry point groups.
        If groups is *None,* return the set for all openmdao entry point groups.
        """
        self._logger.debug('get_available_types %s', groups)
        types = get_available_types(groups)
        for typname, version in types:
            self._logger.debug('    %s %s', typname, version)
        return types

    @rbac(('owner', 'user'))
    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """
        Create a new `typname` object in `server` or a new
        :class:`ObjectServer`.  Returns a proxy for for the new object.
        Starts servers in a subdirectory of the current directory.

        typname: string
            Type of object to create. If null then a proxy for the new
            :class:`ObjServer` is returned.

        version: string or None
            Version of `typname` to create.

        server: proxy
            :class:`ObjServer` on which to create `typname`.
            If none, then a new server is created.

        res_desc: dict or None
            Required resources. Currently not used.

        ctor_args:
            Other constructor arguments.  If `name` is specified, that
            is used as the name of the :class:`ObjServer`.
        """
        self._logger.info('create typname %r, version %r server %s,'
                          ' res_desc %s, args %s', typname, version, server,
                          res_desc, ctor_args)

        if server is None:
            name = ctor_args.get('name', '')
            if not name:
                name = 'Server_%d' % (len(self._managers) + 1)
            manager = _ServerManager(authkey=self._authkey, name=name)
# Helpful?
            if sys.platform == 'win32':  #pragma no cover
                for handler in logging._handlerList:
                    handler.flush()
                sys.stdout.flush()
                sys.stderr.flush()

            root_dir = name
            count = 1
            while os.path.exists(root_dir):
                count += 1
                root_dir = '%s_%d' % (name, count)
            os.mkdir(root_dir)

            # On Windows, when running the full test suite under Nose,
            # starting the process starts a new Nose test session, which
            # will eventually get here and start a new Nose session, which...
            if sys.platform == 'win32' and \
               sys.modules['__main__'].__file__.endswith('openmdao_test-script.py'):  #pragma no cover
                orig_main = sys.modules['__main__'].__file__
                sys.modules['__main__'].__file__ = \
                    pkg_resources.resource_filename('openmdao.main',
                                                    'objserverfactory.py')
            else:
                orig_main = None
            self._logger.debug('starting server %r in dir %s', name, root_dir)
            try:
                manager.start(cwd=root_dir)
            finally:
                self._logger.debug('   startup %r attempt complete', name)
                if orig_main is not None:  #pragma no cover
                    sys.modules['__main__'].__file__ = orig_main

            self._logger.info('new server %r in dir %s listening on %s',
                              name, root_dir, manager.address)
            server = manager.openmdao_main_objserverfactory_ObjServer(name=name)
            self._managers[server] = (manager, root_dir)

        if typname:
            obj = server.create(typname, version, None, res_desc, **ctor_args)
        else:
            obj = server

        self._logger.debug('create returning %s', obj)
        return obj


class _FactoryManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`ObjServerFactory`.
    """
    pass

register(ObjServerFactory, _FactoryManager, 'openmdao.main.objserverfactory')

    
class RemoteFile(object):
    """
    Wraps a :class:`file` with remote-access annotations such that only role
    'owner' may access the file.
    """

    def __init__(self, fileobj):
        self.fileobj = fileobj

    @rbac('owner')
    def __enter__(self):
        """ Enter context. """
        return self.fileobj.__enter__()

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
    def read(self, size=None):
        """ Read up to `size` bytes. """
        if size is None:
            return self.fileobj.read()
        else:
            return self.fileobj.read(size)

    @rbac('owner')
    def write(self, data):
        """ Write `data` to the file. """
        return self.fileobj.write(data)


class ObjServer(object):
    """
    An object which knows how to create other objects, load a model, etc.
    All remote file accesses must be within the tree rooted in the current
    directory at startup.
    """

    def __init__(self, name=''):
        self.host = platform.node()
        self.pid = os.getpid()
        self.name = name or ('sim-%d' % self.pid)

        self.root_dir = os.getcwd()
        self._logger = logging.getLogger(self.name)
        self._logger.info('PID: %d', os.getpid())
        print 'ObjServer %r PID: %d' % (self.name, os.getpid())
        sys.stdout.flush()

        SimulationRoot.chroot(self.root_dir)
        self.tlo = None

    # We only reset logging on the remote side.
    def _reset_logging(self, filename='server.out'):  #pragma no cover
        """ Reset stdout/stderr and logging after switching destination. """
# Helpful?
        if sys.platform == 'win32':
            for handler in logging._handlerList:
                handler.flush()
            sys.stdout.flush()
            sys.stderr.flush()

        sys.stdout = open(filename, 'w')
        sys.stderr = sys.stdout
        logging.root.handlers = []
        logging.basicConfig(level=logging.NOTSET, datefmt='%b %d %H:%M:%S',
            format='%(asctime)s %(levelname)s %(name)s: %(message)s',
            filename='openmdao_log.txt', filemode='w')

    @rbac('*')
    def echo(self, *args):
        """
        Simply return the arguments. This can be useful for latency/thruput
        masurements, connectivity testing, firewall keepalives, etc.
        """
        return args

    @rbac('owner', proxy_types=[object])
    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """
        Returns an object of type *typname,* using the specified
        package version, server location, and resource description.
        """
        self._logger.info('create typname %r, version %r server %s,'
                          ' res_desc %s, args %s', typname, version, server,
                          res_desc, ctor_args)
        obj = create(typname, version, server, res_desc, **ctor_args)
        self._logger.info('    returning %s', obj)
        return obj

    @rbac('owner')
    def execute_command(self, command, stdin, stdout, stderr, env_vars,
                        poll_delay, timeout):
        """
        Run `command` in a subprocess.

        command: string
            Command line to be executed.

        stdin, stdout, stderr: string
            Filenames for the corresponding stream.

        env_vars: dict
            Environment variables for the command.

        poll_delay: float (seconds)
            Delay between polling subprocess for completion.

        timeout: float (seconds)
            Maximum time to wait for command completion. A value of zero
            implies no timeout.
        """
        self._logger.debug('execute_command %r', command)
        for arg in (stdin, stdout, stderr):
            if isinstance(arg, basestring):
                self._check_path(arg, 'execute_command')
        try:
            process = ShellProc(command, stdin, stdout, stderr, env_vars)
        except Exception as exc:
            self._logger.error('exception creating process: %s', exc)
            raise

        self._logger.debug('    PID = %d', process.pid)
        return_code, error_msg = process.wait(poll_delay, timeout)
        self._logger.debug('    returning %s', (return_code, error_msg))
        return (return_code, error_msg)

    @rbac('owner', proxy_types=[Container])
    def load_model(self, egg_filename):
        """
        Load model from egg and return top-level object.

        egg_filename: string
            Filename of egg to be loaded.
        """
        self._logger.debug('load_model %r', egg_filename)
        self._check_path(egg_filename, 'load_model')
        if self.tlo:
            self.tlo.pre_delete()
        self.tlo = Container.load_from_eggfile(egg_filename)
        return self.tlo

    @rbac('owner')
    def pack_zipfile(self, patterns, filename):
        """
        Create ZipFile of files matching `patterns`.

        patterns: list
            List of :mod:`glob`-style patterns.

        filename: string
            Name of ZipFile to create.
        """
        self._logger.debug('pack_zipfile %r', filename)
        self._check_path(filename, 'pack_zipfile')
        return pack_zipfile(patterns, filename, self._logger)

    @rbac('owner')
    def unpack_zipfile(self, filename):
        """
        Unpack ZipFile `filename`.

        filename: string
            Name of ZipFile to unpack.
        """
        self._logger.debug('unpack_zipfile %r', filename)
        self._check_path(filename, 'unpack_zipfile')
        return unpack_zipfile(filename, self._logger)

    @rbac('owner')
    def chmod(self, path, mode):
        """
        Returns ``os.chmod(path, mode)`` if `path` is legal.

        path: string
            Path to file to modify.

        mode: int
            New mode bits (permissions).
        """
        self._logger.debug('chmod %r %o', path, mode)
        self._check_path(path, 'chmod')
        try:
            return os.chmod(path, mode)
        except Exception as exc:
            self._logger.error('chmod %r %o in %s failed %s',
                               path, mode, os.getcwd(), exc)
            raise

    @rbac('owner', proxy_types=[RemoteFile])
    def open(self, filename, mode='r', bufsize=-1):
        """
        Returns ``open(filename, mode, bufsize)`` if `filename` is legal.

        filename: string
            Name of file to open.

        mode: string
            Accees mode.

        bufsize: int
            Size of buffer to use.
        """
        self._logger.debug('open %r %r %s', filename, mode, bufsize)
        self._check_path(filename, 'open')
        try:
            return RemoteFile(open(filename, mode, bufsize))
        except Exception as exc:
            self._logger.error('open %r %r %s in %s failed %s',
                               filename, mode, bufsize, os.getcwd(), exc)
            raise

    @rbac('owner')
    def remove(self, path):
        """
        Remove `path` if `path` is legal.

        path: string
            Path to file to remove.
        """
        self._logger.debug('remove %r', path)
        self._check_path(path, 'remove')
        try:
            return os.remove(path)
        except Exception as exc:
            self._logger.error('remove %r in %s failed %s',
                               path, os.getcwd(), exc)
            raise

    @rbac('owner')
    def stat(self, path):
        """
        Returns ``os.stat(path)`` if `path` is legal.

        path: string
            Path to file to interrogate.
        """
        self._logger.debug('stat %r', path)
        self._check_path(path, 'stat')
        try:
            return os.stat(path)
        except Exception as exc:
            self._logger.error('stat %r in %s failed %s',
                               path, os.getcwd(), exc)
            raise

    def _check_path(self, path, operation):
        """ Check if path is allowed to be used. """
        abspath = os.path.abspath(path)
        if not abspath.startswith(self.root_dir):
            raise RuntimeError("Can't %s %r, not within root %s"
                               % (operation, path, self.root_dir))


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`ObjServer`.
    """
    pass

register(ObjServer, _ServerManager, 'openmdao.main.objserverfactory')

    
def connect(address, port, authkey='PublicKey', pubkey=None):
    """
    Connects to the :class:`ObjServerFactory` at `address` and `port`
    using `key` and returns a (shared) proxy for it.

    address: string
        IP address for server, or pipe filename.

    port: int
        Server port.  If < 0, `address` is a pipe filename.

    authkey:
        Server authorization key.

    pubkey:
        Server public key, required if `authkey` is 'PublicKey'.
    """
    if port < 0:
        location = address
    else:
        location = (address, port)
    try:
        return _PROXIES[location]
    except KeyError:
        if not OpenMDAO_Proxy.manager_is_alive(location):
            raise RuntimeError("can't connect to %s" % (location,))
        mgr = _FactoryManager(location, authkey, pubkey=pubkey)
        mgr.connect()
        proxy = mgr.openmdao_main_objserverfactory_ObjServerFactory()
        _PROXIES[location] = proxy
        return proxy


def start_server(authkey='PublicKey', port=0, prefix='server', timeout=60):
    """
    Start an :class:`ObjServerFactory` service in a separate process
    in the current directory.

    authkey: string
        Authorization key, must be matched by clients.

    port: int
        Port to use, or zero for next avaiable port.

    prefix: string
        Prefix for server config file and stdout/stderr file.

    timeout: int
        Seconds to wait for server to start. Note that public key generation
        can take a while.

    Returns :class:`ShellProc`.
    """
    server_key = prefix+'.key'
    server_cfg = prefix+'.cfg'
    server_out = prefix+'.out'
    for path in (server_cfg, server_out):
        if os.path.exists(path):
            os.remove(path)
    with open(server_key, 'w') as out:
        out.write('%s\n' % authkey)

    factory_path = pkg_resources.resource_filename('openmdao.main',
                                                   'objserverfactory.py')
    args = ['python', factory_path, '--port', str(port), '--prefix', prefix]
    proc = ShellProc(args, stdout=server_out, stderr=STDOUT)

    try:
        retry = 0
        while not os.path.exists(server_cfg):
            return_code = proc.poll()
            if return_code:
                error_msg = proc.error_message(return_code)
                raise RuntimeError('Server startup failed %s' % error_msg)
            retry += 1
            if retry < 50*timeout:  # ~5 sec.
                time.sleep(.1)
            # Hard to cause a startup timeout.
            else:  #pragma no cover
                proc.terminate(timeout)
                raise RuntimeError('Server startup timeout')
        return proc
    finally:
        if os.path.exists(server_key):
            os.remove(server_key)


# Remote process code.

_LOGGER = logging.getLogger()
_SERVER_CFG = ''

def main():  #pragma no cover
    """
    OpenMDAO factory service process.

    Usage: python objserver.py [--port=number][--prefix=name]

    port: int
        Server port (default of 0 implies next available port).
        Note that ports below 1024 typically require special privileges.
        If port is negative, then a local pipe is used for communication.

    prefix: string
        Prefix for configuration and stdout/stderr files (default 'server').

    If ``prefix.key`` exists, it is read for an authorization key string.
    Otherwise public key authorization and encryption is used.
    Once initialized ``prefix.cfg`` is written with address, port, and
    public key information.
    """
    parser = optparse.OptionParser()
    parser.add_option('--port', action='store', type='int', default=0,
                      help='server port (0 implies next available port)')
    parser.add_option('--prefix', action='store', default='server',
                      help='prefix for config and stdout/stderr files')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    server_key = options.prefix+'.key'
    server_cfg = options.prefix+'.cfg'
    global _SERVER_CFG
    _SERVER_CFG = server_cfg

    authkey = 'PublicKey'
    try:
        with open(server_key, 'r') as inp:
            authkey = inp.readline().strip()
        os.remove(server_key)
    except IOError:
        pass

    _LOGGER.setLevel(logging.DEBUG)
    if options.port >= 0:
        address = (platform.node(), options.port)
    else:
        address = None
    set_credentials(Credentials())
    _LOGGER.info('Starting FactoryManager %s %r', address, keytype(authkey))
    current_process().authkey = authkey
    manager = _FactoryManager(address, authkey, name='Factory')

    server = None
    retries = 0
    while server is None:
        try:
            server = manager.get_server()
        except socket.error as exc:
            if str(exc).find('Address already in use') >= 0:
                if retries < 10:
                    msg = 'Address %s in use, retrying...' % (address,)
                    _LOGGER.debug(msg)
                    print msg
                    time.sleep(5)
                    retries += 1
                else:
                    msg = 'Address %s in use, too many retries.' % (address,)
                    _LOGGER.error(msg)
                    print msg
                    sys.exit(1)
            else:
                raise

    write_server_config(server, _SERVER_CFG)
    msg = 'Serving on %s' % (server.address,)
    _LOGGER.info(msg)
    print msg
    sys.stdout.flush()

    signal.signal(signal.SIGTERM, _sigterm_handler)
    try:
        server.serve_forever()
    finally:
        _cleanup()
    sys.exit(0)


def _sigterm_handler(signum, frame):  #pragma no cover
    """ Try to go down gracefully. """
    _LOGGER.info('sigterm_handler invoked')
    print 'sigterm_handler invoked'
    sys.stdout.flush()
    _cleanup()
    sys.exit(1)


def _cleanup():  #pragma no cover
    """ Cleanup in preparation to shut down. """
    if os.path.exists(_SERVER_CFG):
        os.remove(_SERVER_CFG)


if __name__ == '__main__':  #pragma no cover
    main()

