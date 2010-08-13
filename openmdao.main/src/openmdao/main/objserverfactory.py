import logging
import os.path
import platform
import shutil

from openmdao.main.container import Container
from openmdao.main.component import SimulationRoot
from openmdao.main.factory import Factory
from openmdao.util.filexfer import pack_zipfile, unpack_zipfile
from openmdao.util.shellproc import ShellProc

from multiprocessing import util
#from multiprocessing import managers
from openmdao.main import mp_managers as managers


class ObjServerFactory(Factory):
    """
    An :class:`ObjServerFactory` creates :class:`ObjServers` which use
    :mod:`multiprocessing` for communication.  Note that :mod:`multiprocessing`
    is not a transparent distributed object protocol.  See the Python
    documentation for details.
    """

    def __init__(self):
        super(ObjServerFactory, self).__init__()

    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """
        Create an :class:`ObjServer` and return a proxy for it.

        typname : string
            Type of object to create. Currently not used.

        version : string or None
            Version of `typname` to create. Currently not used.

        server :
            Currently not used.

        res_desc : dict or None
            Required resources. Currently not used.

        ctor_args :
            Other constructor arguments.  If `name` is specified, that
            is used as the name of the ObjServer.
        """
        manager = managers.BaseManager()
        ObjServer.register(manager)
        manager.start()
        name = ctor_args.get('name', '')
        logging.debug("ObjServerFactory: new server for '%s' listening on %s",
                      name, manager.address)
        return manager.ObjServer(name=name, host=platform.node())

    
class ObjServer(object):
    """
    An object which knows how to load a model.
    Executes in a subdirectory of the parent factory's startup directory.
    All remote file accesses must be within the tree rooted there.
    """

    def __init__(self, name='', host=''):
        self.host = host
        self.pid = os.getpid()
        self.name = name or ('sim-%d' % self.pid)
        self.orig_dir = os.getcwd()
        self.root_dir = os.path.join(self.orig_dir, self.name)
        if os.path.exists(self.root_dir):
            logging.warning('%s: Removing existing directory %s',
                            self.name, self.root_dir)
            shutil.rmtree(self.root_dir)
        os.mkdir(self.root_dir)
        os.chdir(self.root_dir)
        util.Finalize(None, self.cleanup, exitpriority=-100)
        self._fix_logging()
        SimulationRoot.chroot(self.root_dir)
        self.tlo = None

    def _fix_logging(self):
        # Only want/need this for forked servers to reset log output.
        logging.root.handlers = []
        logging.basicConfig(level=logging.NOTSET, datefmt='%b %d %H:%M:%S',
            format='%(asctime)s %(levelname)s %(name)s: %(message)s',
            filename='openmdao_log.txt', filemode='w')

    def get_name(self):
        """ Return this server's :attr:`name`. """
        return self.name

    def get_pid(self):
        """ Return this server's :attr:`pid`. """
        return self.pid

    def get_host(self):
        """ Return this server's :attr:`host`. """
        return self.host

    def cleanup(self):
        """ Cleanup this server's directory. """
        logging.shutdown()
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            shutil.rmtree(self.root_dir)

    @staticmethod
    def echo(*args):
        """ Simply return our arguments. """
        logging.debug("echo %s", args)
        return args

    def execute_command(self, command, stdin, stdout, stderr, env_vars,
                        poll_delay, timeout):
        """
        Run `command` in subprocess.

        command : string
            Command line to be executed.

        stdin, stdout, stderr : string
            Filenames for the corresponding stream.

        env_vars : dict
            Environment variables for the command.

        poll_delay : float (seconds)
            Delay between polling subprocess for completion.

        timeout : float (seconds)
            Maximum time to wait for command completion. A value of zero
            implies no timeout.
        """
        logging.debug("execute_command '%s'", command)
        for arg in (stdin, stdout, stderr):
            if isinstance(arg, basestring):
                self._check_path(arg, 'execute_command')
        try:
            process = ShellProc(command, stdin, stdout, stderr, env_vars)
        except Exception as exc:
            logging.error('exception creating process: %s', exc)
            raise

        logging.debug('    PID = %d', process.pid)
        return_code, error_msg = process.wait(poll_delay, timeout)
        process.close_files()
        logging.debug('    returning %s', (return_code, error_msg))
        return (return_code, error_msg)

    def load_model(self, egg_filename):
        """
        Load model  and return top-level object.

        egg_filename : string
            Filename of egg to be loaded.
        """
        logging.debug('%s load_model %s', self.name, egg_filename)
        self._check_path(egg_filename, 'load')
        if self.tlo:
            self.tlo.pre_delete()
        self.tlo = Container.load_from_eggfile(egg_filename, install=False)
        return self.tlo

    def pack_zipfile(self, patterns, filename):
        """
        Create ZipFile of files matching `patterns`.

        patterns : list
            List of :mod:`glob`-style patterns.

        filename : string
            Name of ZipFile to create.
        """
        logging.debug("%s pack_zipfile '%s'", self.name, filename)
        self._check_path(filename, 'pack_zipfile')
        return pack_zipfile(patterns, filename, logging.getLogger())

    def unpack_zipfile(self, filename):
        """
        Unpack ZipFile `filename`.

        filename : string
            Name of ZipFile to unpack.
        """
        logging.debug("%s unpack_zipfile '%s'", self.name, filename)
        self._check_path(filename, 'unpack_zipfile')
        return unpack_zipfile(filename, logging.getLogger())

    def chmod(self, path, mode):
        """
        Returns ``os.chmod(path, mode)`` if `path` is legal.

        path : string
            Path to file to modify.

        mode : int
            New mode bits (permissions).
        """
        logging.debug("%s chmod '%s' %s", self.name, path, mode)
        self._check_path(path, 'chmod')
        try:
            return os.chmod(path, mode)
        except Exception as exc:
            logging.error('%s chmod %s %s in %s failed %s', self.name, path,
                          mode, os.getcwd(), exc)
            raise

    def open(self, filename, mode='r', bufsize=-1):
        """
        Returns ``open(filename, mode, bufsize)`` if `filename` is legal.

        filename : string
            Name of file to open.

        mode : string
            Accees mode.

        bufsize : int
            Size of buffer to use.
        """
        logging.debug("%s open '%s' %s %s", self.name, filename, mode, bufsize)
        self._check_path(filename, 'open')
        try:
            return open(filename, mode, bufsize)
        except Exception as exc:
            logging.error('%s open %s %s %s in %s failed %s', self.name,
                          filename, mode, bufsize, os.getcwd(), exc)
            raise

    def stat(self, path):
        """
        Returns ``os.stat(path)`` if `path` is legal.

        path : string
            Path to file to interrogate.
        """
        logging.debug("%s stat '%s'", self.name, path)
        self._check_path(path, 'stat')
        try:
            return os.stat(path)
        except Exception as exc:
            logging.error('%s stat %s in %s failed %s', self.name, path,
                          os.getcwd(), exc)
            raise

    def _check_path(self, path, operation):
        """ Check if path is allowed to be used. """
        path = os.path.abspath(path)
        if not path.startswith(self.root_dir):
            raise RuntimeError("Can't %s, %s doesn't start with %s",
                               operation, path, self.root_dir)

    @staticmethod
    def register(manager):
        """
        Register :class:`ObjServer` proxy info with `manager`.
        Not typically called by user code.

        manager : Manager
            :mod:`multiprocessing` Manager to register with.
        """
        name = 'ObjServer'
        method_to_typeid = {
            'load_model': 'LoadedObject',
            'open': 'file',
        }
        manager.register('LoadedObject', None)
        manager.register('file', None)
        manager.register(name, ObjServer, method_to_typeid=method_to_typeid)

