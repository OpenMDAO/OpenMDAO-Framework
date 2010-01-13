"""
An ObjServerFactory creates ObjServers which use the multiprocessing module
for communication.  Note that the multiprocessing module is not a transparent
distributed object protocol.  See the Python documentation for details.
"""

import logging
from multiprocessing import managers, util
import os.path
import platform
import shutil

from openmdao.main.api import Container, Factory, SimulationRoot

__all__ = ('ObjServerFactory', 'ObjServer')


class ObjServerFactory(Factory):
    """ Creates ObjServers. """

    def __init__(self):
        super(ObjServerFactory, self).__init__()

    def create(self, typname, name='', version=None, server=None, 
               res_desc=None, **ctor_args):
        manager = managers.BaseManager()
        ObjServer.register(manager)
        manager.start()
        logging.debug('ObjServerFactory: new server listening on %s',
                      manager.address)
        return manager.ObjServer(name=name, host=platform.node())

    
class ObjServer(object):
    """ Object which knows how to load a model. """

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
        self.fix_logging()
        SimulationRoot.chroot(self.root_dir)
        self.tlo = None

    def fix_logging(self):
        # Only want/need this for forked servers to reset log output.
        logging.root.handlers = []
        logging.basicConfig(level=logging.NOTSET, datefmt='%b %d %H:%M:%S',
            format='%(asctime)s %(levelname)s %(name)s: %(message)s',
            filename='openmdao_log.txt', filemode='w')

    def get_name(self):
        return self.name

    def get_pid(self):
        return self.pid

    def get_host(self):
        return self.host

    def cleanup(self):
        """ Cleanup our directory. """
        logging.shutdown()
        os.chdir(self.orig_dir)
        if os.path.exists(self.root_dir):
            shutil.rmtree(self.root_dir)

    def load_model(self, egg_filename):
        """ Load model from egg file. """
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
        """ Open file. """
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
        """ Change file permissions. """
        if os.path.isabs(path):
            if not path.startswith(self.root_dir):
                raise RuntimeError("Can't chmod, %s doesn't start with %s",
                                   path, self.root_dir)
        return os.chmod(path, mode)

    @staticmethod
    def register(manager):
        """ Register ObjServer proxy info with `manager`. """
        name = 'ObjServer'
        method_to_typeid = {
            'load_model': 'LoadedObject',
            'open': 'file',
        }
        manager.register('LoadedObject', None)
        manager.register('file', None)
        manager.register(name, ObjServer, method_to_typeid=method_to_typeid)

