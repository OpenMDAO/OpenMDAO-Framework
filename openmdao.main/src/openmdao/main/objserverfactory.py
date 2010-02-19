import logging
import os.path
import platform
import shutil

from openmdao.main.api import Container, Factory, SimulationRoot

from multiprocessing import util
#from multiprocessing import managers
from openmdao.main import mp_managers as managers

__all__ = ('ObjServerFactory', 'ObjServer')


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
        """ Create an :class:`ObjServer` and return a proxy for it. """
        manager = managers.BaseManager()
        ObjServer.register(manager)
        manager.start()
        name = ctor_args.get('name', '')
        logging.debug("ObjServerFactory: new server for '%s' listening on %s",
                      name, manager.address)
        return manager.ObjServer(name=name, host=platform.node())

    
class ObjServer(object):
    """
    Object which knows how to load a model.
    Executes in a subdirectory of the startup directory.
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

    def load_model(self, egg_filename):
        """ Load model from `egg_filename`. """
        logging.debug('%s load_model %s', self.name, egg_filename)
        if os.path.isabs(egg_filename):
            if not egg_filename.startswith(self.root_dir):
                raise RuntimeError("Can't load, %s doesn't start with %s",
                                   egg_filename, self.root_dir)
        if self.tlo:
            self.tlo.pre_delete()
        self.tlo = Container.load_from_eggfile(egg_filename, install=False)
        return self.tlo

    def open(self, filename, mode='r', bufsize=-1):
        """ Open `filename`. """
        if os.path.isabs(filename):
            if not filename.startswith(self.root_dir):
                raise RuntimeError("Can't open, %s doesn't start with %s",
                                   filename, self.root_dir)
        try:
            return open(filename, mode, bufsize)
        except Exception as exc:
            logging.debug('%s open %s %s in %s failed %s', self.name, filename,
                          mode, os.getcwd(), exc)
            raise

    def chmod(self, path, mode):
        """ Change file permissions for `path` to `mode`. """
        if os.path.isabs(path):
            if not path.startswith(self.root_dir):
                raise RuntimeError("Can't chmod, %s doesn't start with %s",
                                   path, self.root_dir)
        return os.chmod(path, mode)

    @staticmethod
    def register(manager):
        """
        Register :class:`ObjServer` proxy info with `manager`.
        Not typically called by user code.
        """
        name = 'ObjServer'
        method_to_typeid = {
            'load_model': 'LoadedObject',
            'open': 'file',
        }
        manager.register('LoadedObject', None)
        manager.register('file', None)
        manager.register(name, ObjServer, method_to_typeid=method_to_typeid)

