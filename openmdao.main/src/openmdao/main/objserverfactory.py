"""
Support for an OpenMDAO 'object service', a factory that can create servers
which support various operations such as creating objects, loading models via
egg files, remote execution, and remote file access.
"""

import atexit
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

from multiprocessing import current_process

from openmdao.main.component import SimulationRoot
from openmdao.main.container import Container
from openmdao.main.factory import Factory
from openmdao.main.factorymanager import create, get_available_types, \
                                         get_signature
from openmdao.main.file_supp import RemoteFile
from openmdao.main.mp_support import OpenMDAO_Manager, OpenMDAO_Proxy, register
from openmdao.main.mp_util import keytype, read_allowed_hosts, setup_tunnel, \
                                  read_server_config, write_server_config
from openmdao.main.rbac import get_credentials, set_credentials, \
                               rbac, RoleError
from openmdao.main.releaseinfo import __version__

from openmdao.util.filexfer import pack_zipfile, unpack_zipfile
from openmdao.util.log import install_remote_handler, remove_remote_handlers, \
                              logging_port, LOG_DEBUG2
from openmdao.util.publickey import make_private, read_authorized_keys, \
                                    write_authorized_keys, HAVE_PYWIN32
from openmdao.util.shellproc import ShellProc, STDOUT, DEV_NULL
from openmdao.util.fileutil import onerror

_PROXIES = {}


class ObjServerFactory(Factory):
    """
    An :class:`ObjServerFactory` creates :class:`ObjServers` and objects
    within those servers.

    name: string
        Name of factory; used in log messages, etc.

    authkey: string
        Passed to created :class:`ObjServer` servers.

    allow_shell: bool
        Passed to created :class:`ObjServer` servers.

    allowed_types: list(string)
        Passed to created :class:`ObjServer` servers.

    address: tuple or string
        A :mod:`multiprocessing` address specifying an Internet address or
        a pipe (default).  Created :class:`ObjServer` servers will use the
        same form of address.

    The environment variable ``OPENMDAO_KEEPDIRS`` can be used to avoid
    having server directory trees removed when servers are shut down.
    """

    # These are used to propagate selections from main().
    # There isn't a good way to propagate them through a manager.
    _address = None
    _allow_shell = False
    _allowed_types = None
    _allow_tunneling = False

    def __init__(self, name='ObjServerFactory', authkey=None, allow_shell=False,
                 allowed_types=None, address=None):
        super(ObjServerFactory, self).__init__()
        self._name = name
        self._authkey = authkey
        self._address = address or ObjServerFactory._address
        self._allow_shell = allow_shell or ObjServerFactory._allow_shell
        self._allowed_types = allowed_types or ObjServerFactory._allowed_types
        self._managers = {}
        self._logger = logging.getLogger(name)
        self._logger.setLevel(logging.DEBUG)
        self._logger.info('PID: %d, %r, allow_shell %s', os.getpid(),
                          keytype(self._authkey), allow_shell)
        self.host = platform.node()
        self.pid = os.getpid()
        self.version = __version__
        self.manager_class = _ServerManager
        self.server_classname = 'openmdao_main_objserverfactory_ObjServer'

    @rbac('*', proxy_types=[object])  # ResourceAllocationManager import loop.
    def get_ram(self):
        """
        Returns the :class:`ResourceAllocationManager` instance.
        Used by :meth:`ResourceAllocationManager.add_remotes`.
        """
        self._logger.debug('get_ram')
        from openmdao.main.resource import ResourceAllocationManager
        return ResourceAllocationManager._get_instance()

    @rbac('*')
    def echo(self, *args):
        """
        Simply return the arguments. This can be useful for latency/thruput
        masurements, connectivity testing, firewall keepalives, etc.
        """
        return args

    @rbac(('owner', 'user'))
    def release(self, server):
        """
        Shut-down :class:`ObjServer` `server`.

        server: :class:`ObjServer`
            Server to be shut down.
        """
        try:
            address = server._token.address
        except AttributeError:
            address = 'not-a-proxy'
        self._logger.debug('release %r', server)
        self._logger.debug('        at %r', address)
        try:
            manager, root_dir, owner = self._managers[server]
        except KeyError:
            # Not identical to any of our proxies.
            # Could still be a reference to the same remote object.
            try:
                server_host = server.host
                server_pid = server.pid
            except Exception:
                self._logger.error("release: can't identify server at %r",
                                   address)
                raise ValueError("can't identify server at %r" % (address,))

            for key in self._managers.keys():
                if key.host == server_host and key.pid == server_pid:
                    manager, root_dir, owner = self._managers[key]
                    server = key
                    break
            else:
                self._logger.error('release: server %r not found', server)
                for key in self._managers.keys():
                    self._logger.debug('    %r', key)
                    self._logger.debug('    at %r', key._token.address)
                raise ValueError('server %r not found' % server)

        if get_credentials().user != owner.user:
            raise RoleError('only the owner can release')

        manager.shutdown()
        server._close.cancel()
        del self._managers[server]
        keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
        if not keep_dirs and os.path.exists(root_dir):
            shutil.rmtree(root_dir, onerror=onerror)

    @rbac('owner')
    def cleanup(self):
        """ Shut-down all remaining :class:`ObjServers`. """
        self._logger.debug('cleanup')
        cleanup_creds = get_credentials()
        servers = self._managers.keys()
        for server in servers:
            # Cleanup overrides release() 'owner' protection.
            set_credentials(self._managers[server][2])
            try:
                self.release(server)
            finally:
                set_credentials(cleanup_creds)
        self._managers = {}

    @rbac('*')
    def get_available_types(self, groups=None):
        """
        Returns a set of tuples of the form ``(typename, metadata)``,
        one for each available plugin type in the given entry point groups.
        If groups is *None,* return the set for all openmdao entry point groups.
        """
        self._logger.debug('get_available_types %s', groups)
        types = get_available_types(groups)
        for typname, version in types:
            self._logger.log(LOG_DEBUG2, '    %s %s', typname, version)
        return types

    @rbac(('owner', 'user'))
    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary.

        typname: string
            Type of object to constructor to inspect.

        version: string or None
            Version of `typname` to create.
        """
        self._logger.debug('get_signature typname %s version %s',
                           typname, version)
        signature = get_signature(typname, version)
        self._logger.log(LOG_DEBUG2, '    %s', signature)
        return signature

    @rbac(('owner', 'user'))
    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """
        Create a new `typname` object in `server` or a new
        :class:`ObjectServer`.  Returns a proxy for for the new object.
        Starts servers in a subdirectory of the current directory.

        typname: string
            Type of object to create. If null, then a proxy for the new
            :class:`ObjServer` is returned.

        version: string or None
            Version of `typname` to create.

        server: proxy
            :class:`ObjServer` on which to create `typname`.
            If none, then a new server is created.

        res_desc: dict or None
            Required resources. ``working_directory`` is used to set a
            created server's directory, other keys are ignored.
            If `allow_shell` has been set, then an absolute directory
            reference may be used (including '~' expansion). If not, then
            the reference must be relative and the working directory will be
            relative to the factory's directory. If the directory already
            exists, a new name will be used of the form ``<directory>_N``

        ctor_args: dict
            Other constructor arguments.
            If `name` or `allowed_users` are specified, they are used when
            creating the :class:`ObjServer`. If no `allowed_users` are
            specified, the server is private to the current user.
        """
        self._logger.info('create typname %r, version %r server %s,'
                          ' res_desc %s, args %s', typname, version, server,
                          res_desc, ctor_args)

        if server is None:
            name = ctor_args.get('name', '')
            if not name:
                name = 'Server_%d' % (len(self._managers) + 1)

            allowed_users = ctor_args.get('allowed_users')
            if not allowed_users:
                credentials = get_credentials()
                allowed_users = {credentials.user: credentials.public_key}
            else:
                del ctor_args['allowed_users']

            if self._address is None or \
               isinstance(self._address, basestring) or \
               self._allow_tunneling:
                # Local access only via pipe if factory accessed by pipe
                # or factory is accessed via tunnel.
                address = None
            else:
                # Network access via same IP as factory, system-selected port.
                address = (self._address[0], 0)

            manager = self.manager_class(address, self._authkey, name=name,
                                         allowed_users=allowed_users)

            # Set (unique) working directory of server.
            # Server cleanup removes this directory, so we avoid any
            # existing directory to not delete existing files.
            base = None
            if res_desc is not None:
                base = res_desc.get('working_directory')
                if base:
                    if self._allow_shell:  # Absolute allowed.
                        base = os.path.expanduser(base)
                    elif os.path.isabs(base) or base.startswith('..'):
                        raise ValueError('working_directory %r must be subdirectory'
                                         % base)
                    res_desc = res_desc.copy()
                    del res_desc['working_directory']
            if not base:
                base = name
            count = 1
            root_dir = base
            while os.path.exists(root_dir):
                count += 1
                root_dir = '%s_%d' % (base, count)
            os.mkdir(root_dir)

            # On Windows, when running the full test suite under Nose,
            # starting the process starts a new Nose test session, which
            # will eventually get here and start a new Nose session, which...
            orig_main = None
            if sys.platform == 'win32':  #pragma no cover
                scripts = ('openmdao-script.py', 'openmdao_test-script.py')
                try:
                    main_file = sys.modules['__main__'].__file__
                except AttributeError:
                    pass
                else:
                    if main_file.endswith(scripts):
                        orig_main = main_file
                        sys.modules['__main__'].__file__ = \
                            pkg_resources.resource_filename('openmdao.main',
                                                            'objserverfactory.py')
            owner = get_credentials()
            self._logger.log(LOG_DEBUG2, '%s starting server %r in dir %s',
                             owner, name, root_dir)
            try:
                manager.start(cwd=root_dir,
                              log_level=self._logger.getEffectiveLevel())
            finally:
                if orig_main is not None:  #pragma no cover
                    sys.modules['__main__'].__file__ = orig_main

            self._logger.info('new server %r for %s', name, owner)
            self._logger.info('    in dir %s', root_dir)
            self._logger.info('    listening on %s', manager.address)
            server_class = getattr(manager, self.server_classname)
            server = server_class(name=name, allow_shell=self._allow_shell,
                                  allowed_types=self._allowed_types)
            self._managers[server] = (manager, root_dir, owner)

        if typname:
            obj = server.create(typname, version, None, res_desc, **ctor_args)
        else:
            obj = server

        self._logger.log(LOG_DEBUG2, 'create returning %r at %r',
                         obj, obj._token.address)
        return obj


class _FactoryManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`ObjServerFactory`.
    """
    pass

register(ObjServerFactory, _FactoryManager, 'openmdao.main.objserverfactory')


class ObjServer(object):
    """
    An object which knows how to create other objects, load a model, etc.
    All remote file accesses must be within the tree rooted in the current
    directory at startup.

    name: string
        Name of server; used in log messages, etc.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed.
        Use with caution!

    allowed_types: list(string)
        Names of types which may be created. If None, then allow types listed
        by :meth:`factorymanager.get_available_types`. If empty, no types are
        allowed.
    """

    def __init__(self, name='', allow_shell=False, allowed_types=None):
        self._allow_shell = allow_shell
        if allowed_types is None:
            allowed_types = [typname for typname, version
                                      in get_available_types()]
        self._allowed_types = allowed_types

        self.host = platform.node()
        self.pid = os.getpid()
        self.name = name or ('sim-%d' % self.pid)
        self.version = __version__

        self._root_dir = os.getcwd()
        self._logger = logging.getLogger(self.name)
        self._logger.info('PID: %d, allow_shell %s',
                          os.getpid(), self._allow_shell)
        print '%s %r PID: %d, allow_shell %s' \
              % (self.__class__.__name__, self.name, os.getpid(),
                 self._allow_shell)
        sys.stdout.flush()

        SimulationRoot.chroot(self._root_dir)
        self.tlo = None

        # Ensure Traits Array support is initialized. The code contains
        # globals for numpy symbols that are initialized within
        # AbstractArray.__init__() which won't be executed if we simply
        # load up egg state (or don't do a real fork, like Windows).
        try:
            import numpy
        except ImportError:
            pass
        else:
            from traits.trait_numeric import AbstractArray
            dummy = AbstractArray()

    @rbac(('owner', 'user'))
    def set_log_level(self, level):
        """ Set logging level to `level`. """
        self._logger.info('log_level %s', level)
        self._logger.setLevel(level)
        logging.getLogger().setLevel(level)

    @rbac('owner')
    def config_ram(self, filename):
        """
        Configure the :class:`ResourceAllocationManager` instance from `filename`.
        Used to define resources needed for model execution.
        """
        self._logger.debug('config_ram %r', filename)
        from openmdao.main.resource import ResourceAllocationManager
        ResourceAllocationManager.configure(filename)

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
        If *typname* is an allowed type, returns an object of type *typname,*
        using the specified package version, server location, and resource
        description. All arguments are passed to :meth:`factorymanager.create`.
        """
        self._logger.debug('create typname %r, version %r server %s,'
                           ' res_desc %s, args %s', typname, version, server,
                           res_desc, ctor_args)
        if typname in self._allowed_types:
            obj = create(typname, version, server, res_desc, **ctor_args)
            self._logger.log(LOG_DEBUG2, '    returning %s', obj)
            return obj
        else:
            raise TypeError('%r is not an allowed type' % typname)

    @rbac('owner')
    def execute_command(self, resource_desc):
        """
        Run command described by `resource_desc` in a subprocess if this
        server's `allow_shell` attribute is True.

        resource_desc: dict
            Contains job description.

        The current environment, along with any 'job_environment' specification,
        is in effect while running 'remote_command'.

        If 'input_path' is not specified, ``/dev/null`` or ``nul:`` is used.
        If 'output_path' is not specified, ``<remote_command>.stdout`` is used.
        If neither 'error_path' nor 'join_files' are specified,
        ``<remote_command>.stderr`` is used.

        If specified in the 'resource_limits' dictionary, 'wallclock_time' is
        used as a timeout.

        All other queuing resource keys are ignored.

        The ``HOME_DIRECTORY`` and ``WORKING_DIRECTORY`` placeholders are
        ignored.
        """
        try:
            job_name = resource_desc['job_name']
        except KeyError:
            job_name = ''

        command = resource_desc['remote_command']
        self._check_path(command, 'execute_command')
        base = os.path.basename(command)
        command = [command]
        if 'args' in resource_desc:
            command.extend(resource_desc['args'])

        self._logger.debug('execute_command %s %r', job_name, command)
        if not self._allow_shell:
            self._logger.error('attempt to execute %r by %r', command,
                               get_credentials().user)
            raise RuntimeError('shell access is not allowed by this server')

        env_vars = resource_desc.get('job_environment')

        try:
            stdin = resource_desc['input_path']
            self._check_path(stdin, 'execute_command')
        except KeyError:
            stdin = DEV_NULL

        try:
            stdout = resource_desc['output_path']
            self._check_path(stdout, 'execute_command')
        except KeyError:
            stdout = base+'.stdout'

        try:
            stderr = resource_desc['error_path']
            self._check_path(stderr, 'execute_command')
        except KeyError:
            try:
                join_files = resource_desc['join_files']
            except KeyError:
                stderr = base+'.stderr'
            else:
                stderr = STDOUT if join_files else base+'.stderr'

        limits = resource_desc.get('resource_limits', {})
        timeout = limits.get('wallclock_time', 0)
        poll_delay = 1

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
        Load model from egg and return top-level object if this server's
        `allow_shell` attribute is True.

        egg_filename: string
            Filename of egg to be loaded.
        """
        self._logger.debug('load_model %r', egg_filename)
        if not self._allow_shell:
            self._logger.error('attempt to load %r by %r', egg_filename,
                               get_credentials().user)
            raise RuntimeError('shell access is not allowed by this server')
        self._check_path(egg_filename, 'load_model')
        if self.tlo:
            self.tlo.pre_delete()
        self.tlo = Container.load_from_eggfile(egg_filename, log=self._logger)
        return self.tlo

    @rbac('owner')
    def pack_zipfile(self, patterns, filename):
        """
        Create ZipFile of files matching `patterns` if `filename` is legal.

        patterns: list
            List of :mod:`glob`-style patterns.

        filename: string
            Name of ZipFile to create.
        """
        self._logger.debug('pack_zipfile %r', filename)
        self._check_path(filename, 'pack_zipfile')
        return pack_zipfile(patterns, filename, self._logger)

    @rbac('owner')
    def unpack_zipfile(self, filename, textfiles=None):
        """
        Unpack ZipFile `filename` if `filename` is legal.

        filename: string
            Name of ZipFile to unpack.

        textfiles: list
            List of :mod:`fnmatch` style patterns specifying which unpacked
            files are text files possibly needing newline translation. If not
            supplied, the first 4KB of each is scanned for a zero byte. If none
            is found, then the file is assumed to be a text file.
        """
        self._logger.debug('unpack_zipfile %r', filename)
        self._check_path(filename, 'unpack_zipfile')
        return unpack_zipfile(filename, self._logger, textfiles)

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

    @rbac('owner')
    def isdir(self, path):
        """
        Returns ``os.path.isdir(path)`` if `path` is legal.

        path: string
            Path to check.
        """
        self._logger.debug('isdir %r', path)
        self._check_path(path, 'isdir')
        try:
            return os.path.isdir(path)
        except Exception as exc:
            self._logger.error('isdir %r in %s failed %s',
                               path, os.getcwd(), exc)
            raise

    @rbac('owner')
    def listdir(self, path):
        """
        Returns ``os.listdir(path)`` if `path` is legal.

        path: string
            Path to directory to list.
        """
        self._logger.debug('listdir %r', path)
        self._check_path(path, 'listdir')
        try:
            return os.listdir(path)
        except Exception as exc:
            self._logger.error('listdir %r in %s failed %s',
                               path, os.getcwd(), exc)
            raise

    @rbac('owner', proxy_types=[RemoteFile])
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
        if not abspath.startswith(self._root_dir):
            raise RuntimeError("Can't %s %r, not within root %s"
                               % (operation, path, self._root_dir))


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`ObjServer`.
    """
    pass

register(ObjServer, _ServerManager, 'openmdao.main.objserverfactory')


def connect_to_server(config_filename):
    """
    Connects to the the server specified by `config_filename` and returns a
    (shared) proxy for the associated :class:`ObjServerFactory`.

    config_filename: string:
        Name of server configuration file.
    """
    cfg = read_server_config(config_filename)
    return connect(cfg['address'], cfg['port'], cfg['tunnel'],
                   pubkey=cfg['key'], logfile=cfg['logfile'])


def connect(address, port, tunnel=False, authkey='PublicKey', pubkey=None,
            logfile=None):
    """
    Connects to the server at `address` and `port` using `key` and returns
    a (shared) proxy for the associated :class:`ObjServerFactory`.

    address: string
        IP address for server or pipe filename.

    port: int
        Server port.  If < 0, `address` is a pipe filename.

    tunnel: bool
        Connect via SSH tunnel.

    authkey:
        Server authorization key.

    pubkey:
        Server public key; required if `authkey` is 'PublicKey'.

    logfile:
        Location of server's log file, if known.
    """
    if port < 0:
        key = address
    else:
        key = (address, port)
    try:
        return _PROXIES[key]
    except KeyError:
        # Requires ssh setup.
        if tunnel:  # pragma no cover
            location, cleanup = setup_tunnel(address, port)
            atexit.register(*cleanup)
        else:
            location = key
        via = ' (via tunnel)' if tunnel else ''
        log = ' at %s' % logfile if logfile else ''
        if not OpenMDAO_Proxy.manager_is_alive(location):
            raise RuntimeError("Can't connect to server at %s:%s%s. It appears"
                               " to be offline. Please check the server log%s."
                               % (address, port, via, log))

        mgr = _FactoryManager(location, authkey, pubkey=pubkey)
        try:
            mgr.connect()
        except EOFError:
            raise RuntimeError("Can't connect to server at %s:%s%s. It appears"
                               " to be rejecting the connection. Please check"
                               " the server log%s." % (address, port, via, log))

        proxy = mgr.openmdao_main_objserverfactory_ObjServerFactory()
        if proxy.version != __version__:
            logging.warning('Server version %r different than local version %r',
                            proxy.version, __version__)
        _PROXIES[key] = proxy
        return proxy


def start_server(authkey='PublicKey', address=None, port=0, prefix='server',
                 allowed_hosts=None, allowed_users=None, allow_shell=False,
                 allowed_types=None, timeout=None, tunnel=False,
                 resources=None, log_prefix=None):
    """
    Start an :class:`ObjServerFactory` service in a separate process
    in the current directory.

    authkey: string
        Authorization key; must be matched by clients.

    address: string
        IPv4 address, hostname, or pipe name.
        Default is the host's default IPv4 address.

    port: int
        Server port (default of 0 implies next available port).
        Note that ports below 1024 typically require special privileges.
        If port is negative, then a local pipe is used for communication.

    prefix: string
        Prefix for server config file and stdout/stderr file.

    allowed_hosts: list(string)
        Host address patterns to check against. Required if `port` >= 0.
        Ignored if `allowed_users` is specified.

    allowed_users: dict
        Dictionary of users and corresponding public keys allowed access.
        If None, *any* user may access. If empty, no user may access.
        The host portions of user strings are used for address patterns.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed.
        Use with caution!

    allowed_types: list(string)
        Names of types which may be created. If None, then allow types listed
        by :meth:`get_available_types`. If empty, no types are allowed.

    timeout: int
        Seconds to wait for server to start. Note that public key generation
        can take a while. The default value of None will use an internally
        computed value based on host type (and for Windows, the availability
        of pyWin32).

    tunnel: bool
        If True, report host IP address but listen for connections from a
        local SSH tunnel.

    resources: string
        Filename for resource configuration.

    log_prefix: string
        Name used to identify remote remote logging messages from server.
        Implies that the local process will be receiving the messages.

    Returns ``(server_proc, config_filename)``.
    """
    if timeout is None:
        if sys.platform == 'win32' and not HAVE_PYWIN32:  #pragma no cover
            timeout = 120
        else:
            timeout = 30

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

    if address is not None:
        args.extend(['--address', address])

    if tunnel:
        args.append('--tunnel')

    if resources is not None:
        args.append('--resources')
        args.append(resources)

    if allowed_users is not None:
        write_authorized_keys(allowed_users, 'users.allow', logging.getLogger())
        args.extend(['--users', 'users.allow'])
    else:
        args.append('--allow-public')
        if port >= 0:
            if allowed_hosts is None:
                allowed_hosts = [socket.gethostbyname(socket.gethostname())]
                if allowed_hosts[0].startswith('127.') and \
                   '127.0.0.1' not in allowed_hosts:
                    allowed_hosts.append('127.0.0.1')
            with open('hosts.allow', 'w') as out:
                for pattern in allowed_hosts:
                    out.write('%s\n' % pattern)
            if sys.platform != 'win32' or HAVE_PYWIN32:
                make_private('hosts.allow')
            else:  #pragma no cover
                logging.warning("Can't make hosts.allow private")

    if allow_shell:
        args.append('--allow-shell')

    if allowed_types is not None:
        with open('types.allow', 'w') as out:
            for typname in allowed_types:
                out.write('%s\n' % typname)
        if sys.platform != 'win32' or HAVE_PYWIN32:
            make_private('types.allow')
        else:  #pragma no cover
            logging.warning("Can't make types.allow private")
        args.extend(['--types', 'types.allow'])

    if log_prefix is not None:
        log_host = socket.gethostname()
        log_port = logging_port(log_host, log_host)
        args.extend(['--log-host', log_host, '--log-port', str(log_port)])
        if log_prefix:  # Could be null (for default).
            args.extend(['--log-prefix', log_prefix])

    proc = ShellProc(args, stdout=server_out, stderr=STDOUT)

    try:
        # Wait for valid server_cfg file.
        retry = 0
        while (not os.path.exists(server_cfg)) or \
              (os.path.getsize(server_cfg) == 0):
            return_code = proc.poll()
            if return_code:
                error_msg = proc.error_message(return_code)
                raise RuntimeError('Server startup failed %s' % error_msg)
            retry += 1
            if retry < 10*timeout:
                time.sleep(.1)
            # Hard to cause a startup timeout.
            else:  #pragma no cover
                proc.terminate(timeout)
                raise RuntimeError('Server startup timeout')
        return (proc, server_cfg)
    finally:
        if os.path.exists(server_key):
            os.remove(server_key)


def stop_server(server, config_filename):
    """
    Shutdown :class:`ObjServerFactory` specified by `config_filename` and
    terminate its process `server`.

    server: :class:`ShellProc`
        Server process returned by :meth:`start_server`.

    config_filename: string:
        Name of server configuration file.
    """
    factory = connect_to_server(config_filename)
    factory.cleanup()
    server.terminate(timeout=10)


# Remote process code.

_SERVER_CFG = ''

def main():  #pragma no cover
    """
    OpenMDAO factory service process.

    Usage: python objserverfactory.py [--allow-public][--allow-shell][--hosts=filename][--types=filename][--users=filename][--address=address][--port=number][--prefix=name][--tunnel][--resources=filename][--log-host=hostname][--log-port=number][--log-prefix=string]

    --allow-public:
        Allows access by anyone from any allowed host. Use with care!

    --allow-shell:
        Allows access to :meth:`execute_command` and :meth:`load_model`.
        Use with care!

    --hosts: string
        Filename for allowed hosts specification. Default ``hosts.allow``.
        Ignored if '--users' is specified.
        The file should contain IPv4 host addresses, IPv4 domain addresses,
        or hostnames, one per line. Blank lines are ignored, and '#' marks the
        start of a comment which continues to the end of the line.
        For security reasons this file must be accessible only by the user
        running this server.

    --types: string
        Filename for allowed types specification.
        If not specified then allow types listed by
        :meth:`factorymanager.get_available_types`.
        The file should contain one type name per line.

    --users: string
        Filename for allowed users specification.
        Ignored if '--allow-public' is specified.
        Default is ``~/.ssh/authorized_keys``, other files should be of the
        same format: each line has ``key-type public-key-data user@host``,
        where `user` is the username on `host`. `host` will be translated to an
        IPv4 address and included in the allowed hosts list.
        Note that this ``user@host`` form is not necessarily enforced by
        programs which generate keys.
        For security reasons this file must be accessible only by the user
        running this server.

    --address: string
        IPv4 address, hostname, or pipe name.
        Default is the host's default IPv4 address.

    --port: int
        Server port (default of 0 implies next available port).
        Note that ports below 1024 typically require special privileges.
        If port is negative, then a local pipe is used for communication.

    --prefix: string
        Prefix for configuration and stdout/stderr files (default ``server``).

    --tunnel:
        Report host IP address but listen for connections from a local
        SSH tunnel.

    --resources: string
        Filename for resource configuration. If not specified then the
        default of ``~/.openmdao/resources.cfg`` will be used.

    --log-host: string
        Hostname to send remote log messages to.

    --log-port: int
        Port on `log-host` to send remote log messages to.

    --log-prefix: string
        Prefix to apply to remote log messages. Default is ``pid@host``.

    If ``prefix.key`` exists, it is read for an authorization key string.
    Otherwise, public key authorization and encryption is used.

    Allowed hosts *must* be specified if `port` is >= 0. Only allowed hosts
    may connect to the server.

    Once initialized ``prefix.cfg`` is written with address, port, and
    public key information.
    """
    parser = optparse.OptionParser()
    parser.add_option('--address', action='store', type='str',
                      help='Network address to serve.')
    parser.add_option('--allow-public', action='store_true', default=False,
                      help='Allows access by any user, use with care!')
    parser.add_option('--allow-shell', action='store_true', default=False,
                      help='Allows potential shell access, use with care!')
    parser.add_option('--hosts', action='store', type='str',
                      default='hosts.allow', help='Filename for allowed hosts')
    parser.add_option('--types', action='store', type='str',
                      help='Filename for allowed types')
    parser.add_option('--users', action='store', type='str',
                      default='~/.ssh/authorized_keys',
                      help='Filename for allowed users')
    parser.add_option('--port', action='store', type='int', default=0,
                      help='Server port (0 implies next available port)')
    parser.add_option('--prefix', action='store', default='server',
                      help='Prefix for config and stdout/stderr files')
    parser.add_option('--tunnel', action='store_true', default=False,
                      help='Report host IP address but listen for connections'
                           ' from a local SSH tunnel')
    parser.add_option('--resources', action='store', type='str',
                      default=None, help='Filename for resource configuration')
    parser.add_option('--log-host', action='store', type='str',
                      default=None, help='hostname for remote log messages')
    parser.add_option('--log-port', action='store', type='int',
                      default=None, help='port for remote log messages')
    parser.add_option('--log-prefix', action='store', type='str',
                      default=None, help='prefix for remote log messages')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)
    if options.log_host and options.log_port:
        install_remote_handler(options.log_host, int(options.log_port),
                               options.log_prefix)

    server_key = options.prefix+'.key'
    server_cfg = options.prefix+'.cfg'
    global _SERVER_CFG
    _SERVER_CFG = server_cfg

    # Get authkey.
    authkey = 'PublicKey'
    try:
        with open(server_key, 'r') as inp:
            authkey = inp.readline().strip()
        os.remove(server_key)
    except IOError:
        pass

    if options.allow_shell:
        msg = 'Shell access is ALLOWED'
        logger.warning(msg)
        print msg

    allowed_users = None
    allowed_hosts = None

    # Get allowed_users.
    if options.allow_public:
        allowed_users = None
        msg = 'Public access is ALLOWED'
        logger.warning(msg)
        print msg

        if options.port >= 0:
            # Get allowed_hosts.
            if os.path.exists(options.hosts):
                try:
                    allowed_hosts = read_allowed_hosts(options.hosts)
                except Exception as exc:
                    msg = "Can't read allowed hosts file %r: %s" \
                          % (options.hosts, exc)
                    logger.error(msg)
                    print msg
                    sys.exit(1)
            else:
                msg = 'Allowed hosts file %r does not exist.' % options.hosts
                logger.error(msg)
                print msg
                sys.exit(1)

            if not allowed_hosts:
                msg = 'No hosts in allowed hosts file %r.' % options.hosts
                logger.error(msg)
                print msg
                sys.exit(1)
    else:
        if os.path.exists(options.users):
            try:
                allowed_users = read_authorized_keys(options.users, logger)
            except Exception as exc:
                msg = "Can't read allowed users file %r: %s" \
                      % (options.users, exc)
                logger.error(msg)
                print msg
                sys.exit(1)
        else:
            msg = 'Allowed users file %r does not exist.' % options.users
            logger.error(msg)
            print msg
            sys.exit(1)

        if not allowed_users:
            msg = 'No users in allowed users file %r.' % options.users
            logger.error(msg)
            print msg
            sys.exit(1)

    # Get allowed_types.
    allowed_types = None
    if options.types:
        if os.path.exists(options.types):
            allowed_types = []
            with open(options.types, 'r') as inp:
                line = inp.readline()
                while line:
                    line = line.strip()
                    if line:
                        allowed_types.append(line)
                    line = inp.readline()
        else:
            msg = 'Allowed types file %r does not exist.' % options.types
            logger.error(msg)
            print msg
            sys.exit(1)

    # Optionally configure resources.
    if options.resources:
        # Import here to avoid import loop.
        from openmdao.main.resource import ResourceAllocationManager as RAM
        RAM.configure(options.resources)

    # Get address and create manager.
    if options.port >= 0:
        if options.address:  # Specify IPv4/hostname.
            address = (options.address, options.port)
        else:
            address = (platform.node(), options.port)
    else:
        if options.address:  # Specify pipename.
            address = options.address
        else:
            address = None

    logger.info('Starting FactoryManager %s %r', address, keytype(authkey))
    current_process().authkey = authkey
    bind_address = ('127.0.0.1', options.port) if options.tunnel else address
    manager = _FactoryManager(bind_address, authkey, name='Factory',
                              allowed_hosts=allowed_hosts,
                              allowed_users=allowed_users,
                              allow_tunneling=options.tunnel)

    # Set defaults for created ObjServerFactories.
    # There isn't a good method to propagate these through the manager.
    ObjServerFactory._address = address
    ObjServerFactory._allow_shell = options.allow_shell
    ObjServerFactory._allowed_types = allowed_types
    ObjServerFactory._allow_tunneling = options.tunnel

    # Get server, retry if specified address is in use.
    server = None
    retries = 0
    while server is None:
        try:
            server = manager.get_server()
        except socket.error as exc:
            if str(exc).find('Address already in use') >= 0:
                if retries < 10:
                    msg = 'Address %s in use, retrying...' % (address,)
                    logger.debug(msg)
                    print msg
                    time.sleep(5)
                    retries += 1
                else:
                    msg = 'Address %s in use, too many retries.' % (address,)
                    logger.error(msg)
                    print msg
                    sys.exit(1)
            else:
                raise

    # Record configuration.
    real_ip = None if address is None else address[0]
    write_server_config(server, _SERVER_CFG, real_ip)
    msg = 'Serving on %s' % (server.address,)
    logger.info(msg)
    print msg
    sys.stdout.flush()

    # And away we go...
    signal.signal(signal.SIGTERM, _sigterm_handler)
    try:
        server.serve_forever()
    finally:
        _cleanup()
    sys.exit(0)


def _sigterm_handler(signum, frame):  #pragma no cover
    """ Try to go down gracefully. """
    logging.getLogger().info('sigterm_handler invoked')
    print 'sigterm_handler invoked'
    sys.stdout.flush()
    _cleanup()
    sys.exit(1)


def _cleanup():  #pragma no cover
    """ Cleanup in preparation to shut down. """
    remove_remote_handlers()
    keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
    if not keep_dirs and os.path.exists(_SERVER_CFG):
        os.remove(_SERVER_CFG)


if __name__ == '__main__':  #pragma no cover
    main()

