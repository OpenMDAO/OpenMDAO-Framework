
#public symbols
__all__ = ["ProjDirFactory"]

import os
import sys
import fnmatch

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

import openmdao.main.api
import openmdao.main.datatypes.api

from openmdao.main.factory import Factory
from openmdao.util.dep import PythonSourceTreeAnalyser, find_files, plugin_groups


class PyWatcher(FileSystemEventHandler):

    def __init__(self, factory):
        super(PyWatcher, self).__init__()
        self.factory = factory

    def on_modified(self, event):
        if not event.is_directory and fnmatch.fnmatch(event.src_path, '*.py'):
            self.factory.on_modified(event.src_path)
    
    def on_moved(self, event):
        if event._src_path and (event.is_directory or fnmatch.fnmatch(event._src_path, '*.py')):
            self.factory.on_deleted(event._src_path)
        
        if fnmatch.fnmatch(event._dest_path, '*.py'):
            self.factory.on_modified(event._dest_path)
    
    def on_deleted(self, event):
        if event.is_directory or fnmatch.fnmatch(event.src_path, '*.py'):
            self.factory.on_deleted(event.src_path)
            
    
_startmods = [
    'api',
    'datatypes.api',
    'component',
    'container',
    'driver',
    'arch',
    'assembly',
    'variable',
    'vartree',
    ]

# predicate functions for selecting available types
def is_plugin(name, meta):
    return True

#

class ProjDirFactory(Factory):
    """A Factory that watches a Project directory and dynamically keeps
    the set of available types up-to-date as project files change.
    """
    def __init__(self, watchdir):
        super(ProjDirFactory, self).__init__()
        self.watchdir = watchdir
        self.imported = {}  # imported files vs (module, ctor dict)
        startfiles = [sys.modules['openmdao.main.'+n].__file__.replace('.pyc','.py') 
                          for n in _startmods]
        self.analyzer = PythonSourceTreeAnalyser(startfiles=startfiles)
        self._baseset = set(self.analyzer.graph.nodes())
        
        for pyfile in find_files(self.watchdir, "*.py"):
            self.on_modified(pyfile)

        self.observer = Observer()
        self.observer.schedule(PyWatcher(self), path=watchdir, recursive=True)
        self.observer.daemon = True
        self.observer.start()

    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Create and return an instance of the specified type, or None if
        this Factory can't satisfy the request.
        """
        if server is None and res_desc is None and typ in self.analyzer.class_file_map:
            fpath = self.analyzer.class_file_map[typ]
            if fpath not in self.imported:
                modpath = self.analyzer.fileinfo[fpath].modpath
                sys.path = [os.path.dirname(fpath)] + sys.path
                try:
                    __import__(modpath)
                except ImportError as err:
                    return None
                finally:
                    sys.path = sys.path[1:]
                mod = sys.modules[modpath]
                visitor = self.analyzer.fileinfo[fpath]
                self.imported[fpath] = (mod, {})
                for cname in visitor.classes.keys():
                    self.imported[fpath][1][cname] = getattr(mod, cname.split('.')[-1])
            try:
                ctor = self.imported[fpath][1][typ]
            except KeyError:
                return None
            return ctor(**ctor_args)
        return None

    def get_available_types(self, predicate=is_plugin):
        """Return a list of available types that cause predicate(classname, metadata) to
        return True.
        """
        typset = set(self.analyzer.graph.nodes()) - self._baseset
        types = []
        graph = self.analyzer.graph
        
        for typ in typset:
            meta = graph.node[typ]['classinfo'].meta
            if predicate(typ, meta):
                types.append((typ, meta))
        return types

    def on_modified(self, fpath):
        if os.path.isdir(fpath):
            return
        if fpath in self.analyzer.fileinfo:
            if fpath in self.imported:
                sys.path = [os.path.dirname(fpath)] + sys.path
                try:
                    m = reload(self.imported[fpath][0])
                except ImportError as err:
                    return None
                finally:
                    sys.path = sys.path[1:]
                self.imported[fpath] = (m, self.imported[fpath][1])
            self.on_deleted(fpath)
        visitor = self.analyzer.analyze_file(fpath)

    def on_deleted(self, fpath):
        if os.path.isdir(fpath):
            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_deleted(pyfile)
        else:
            try:
                del self.imported[fpath]
            except KeyError:
                pass
            self.analyzer.remove_file(fpath)

    def cleanup(self):
        """If this factory is removed from the FactoryManager during execution, this function
        will stop the watchdog observer thread.
        """
        self.observer.stop()
        self.observer.join()

if __name__ == '__main__':
    import time
    event_handler = PyWatcher()
    observer = Observer()
    observer.schedule(event_handler, path='.', recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(.1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()