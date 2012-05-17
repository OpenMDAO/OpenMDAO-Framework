
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
        startfiles = [sys.modules['openmdao.main.'+n].__file__.replace('.pyc','.py') 
                          for n in _startmods]
        self.analyzer = PythonSourceTreeAnalyser(startfiles=startfiles)
        self._baseset = set(self.analyzer.graph.nodes())
        
        self._typeinfo = {}  # dict of typename vs tuples (ctor, metadata, loaded)
        
        for pyfile in find_files(self.watchdir, "*.py"):
            self.on_modified(pyfile)

        self.observer = Observer()
        self.observer.schedule(PyWatcher(self), path=watchdir, recursive=True)
        self.observer.start()

    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """
        """
        pass

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
        visitor = self.analyzer.analyze_file(fpath)

    def on_deleted(self, fpath):
        if os.path.isdir(fpath):
            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_deleted(pyfile)
        else:
            for modpath, info in self.analyzer.fileinfo.items():
                if info.fname == fpath:
                    self.analyzer.graph.remove_nodes_from(info.classes.keys())

    def cleanup(self):
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