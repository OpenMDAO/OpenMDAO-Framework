
#public symbols
__all__ = ["ProjDirFactory"]

import os
import sys
import fnmatch

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

import openmdao.main.api
import openmdao.main.datatypes.api
from openmdao.main.interfaces import IContainer, IComponent, IAssembly, IDriver, \
                                     IDOEgenerator, ISurrogate, ICaseFilter, ICaseIterator, ICaseRecorder, \
                                     IArchitecture, IDifferentiator

from openmdao.main.factory import Factory
from openmdao.main.factorymanager import get_available_types
from openmdao.util.dep import PythonSourceTreeAnalyser, find_files, plugin_groups
from openmdao.util.fileutil import get_module_path, get_ancestor_dir
from openmdao.main.publisher import Publisher
from openmdao.gui.util import packagedict
from openmdao.util.fileutil import dbg_to_file


class PyWatcher(FileSystemEventHandler):

    def __init__(self, factory):
        super(PyWatcher, self).__init__()
        self.factory = factory
        dbg_to_file("PyWatcher: init")

    def on_modified(self, event):
        dbg_to_file("PyWatcher: on_modified")
        added_set = set()
        changed_set = set()
        deleted_set = set()
        if not event.is_directory and fnmatch.fnmatch(event.src_path, '*.py'):
            self.factory.on_modified(event.src_path, added_set, changed_set, deleted_set)
            self.factory.publish_updates(added_set, changed_set, deleted_set)
            dbg_to_file("PyWatcher: update published")
    
    def on_moved(self, event):
        dbg_to_file("PyWatcher: on_moved")
        added_set = set()
        changed_set = set()
        deleted_set = set()
        
        publish = False
        if event._src_path and (event.is_directory or fnmatch.fnmatch(event._src_path, '*.py')):
            self.factory.on_deleted(event._src_path, deleted_set)
            publish = True
        
        if fnmatch.fnmatch(event._dest_path, '*.py'):
            self.factory.on_modified(event._dest_path, added_set, changed_set, deleted_set)
            publish = True
            
        if publish:
            self.factory.publish_updates(added_set, changed_set, deleted_set)
            dbg_to_file("PyWatcher: update published")

    
    def on_deleted(self, event):
        dbg_to_file("PyWatcher: on_deleted")
        added_set = set()
        changed_set = set()
        deleted_set = set()
        if event.is_directory or fnmatch.fnmatch(event.src_path, '*.py'):
            self.factory.on_deleted(event.src_path, deleted_set)
            self.factory.publish_updates(added_set, changed_set, deleted_set)
            
    
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

plugin_ifaces = set([
    'IContainer', 
    'IComponent', 
    'IAssembly', 
    'IDriver', 
    'IDOEgenerator', 
    'ISurrogate', 
    'ICaseFilter', 
    'ICaseIterator', 
    'ICaseRecorder',
    'IArchitecture', 
    'IDifferentiator',
])

# predicate functions for selecting available types
def is_plugin(name, meta):
    return 'ifaces' in meta and plugin_ifaces.intersection(meta['ifaces'])

#

class ProjDirFactory(Factory):
    """A Factory that watches a Project directory and dynamically keeps
    the set of available types up-to-date as project files change.
    """
    def __init__(self, watchdir, use_observer=True):
        dbg_to_file("ProjDirFactory ctor")
        super(ProjDirFactory, self).__init__()
        self.watchdir = watchdir
        self.imported = {}  # imported files vs (module, ctor dict)
        startfiles = [sys.modules['openmdao.main.'+n].__file__.replace('.pyc','.py') 
                          for n in _startmods]
        try:
            self.analyzer = PythonSourceTreeAnalyser(startfiles=startfiles)
            self._baseset = set(self.analyzer.graph.nodes())
            
            added_set = set()
            changed_set = set()
            deleted_set = set()
            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_modified(pyfile, added_set, changed_set, deleted_set)
        except Exception as err:
            dbg_to_file(str(err))
            
        if use_observer:
            dbg_to_file("using observer")
            self._start_observer()
            self.publish_updates(added_set, changed_set, deleted_set)
        else:
            dbg_to_file("NOT using observer")
            self.observer = None  # sometimes for debugging/testing it's easier to turn observer off

    def _start_observer(self):
        dbg_to_file("starting observer for dir %s" % self.watchdir)
        self.observer = Observer()
        self.observer.schedule(PyWatcher(self), path=self.watchdir, recursive=True)
        self.observer.daemon = True
        self.observer.start()
        dbg_to_file("observer started. watching %s" % self.watchdir)
        
    def _get_mod_ctors(self, mod, fpath, visitor):
        self.imported[fpath] = (mod, {})
        for cname in visitor.classes.keys():
            self.imported[fpath][1][cname] = getattr(mod, cname.split('.')[-1])
        
    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Create and return an instance of the specified type, or None if
        this Factory can't satisfy the request.
        """
        if server is None and res_desc is None and typ in self.analyzer.class_file_map:
            fpath = self.analyzer.class_file_map[typ]
            if fpath not in self.imported:
                modpath = self.analyzer.fileinfo[fpath].modpath
                sys.path = [get_ancestor_dir(fpath, len(modpath.split('.')))] + sys.path
                try:
                    __import__(modpath)
                except ImportError as err:
                    return None
                finally:
                    sys.path = sys.path[1:]
                mod = sys.modules[modpath]
                visitor = self.analyzer.fileinfo[fpath]
                self._get_mod_ctors(mod, fpath, visitor)
            try:
                ctor = self.imported[fpath][1][typ]
            except KeyError:
                return None
            return ctor(**ctor_args)
        return None

    def get_available_types(self, groups=None):
        """Return a list of available types that cause predicate(classname, metadata) to
        return True.
        """
        dbg_to_file("ProjDirFactory: get_available_types - groups = %s" % groups)
        graph = self.analyzer.graph
        typset = set(graph.nodes()) - self._baseset
        types = []
        
        if groups is None:
            ifaces = set([v[0] for v in plugin_groups.values()])
        else:
            ifaces = set([v[0] for k,v in plugin_groups.items() if k in groups])
        
        dbg_to_file("ifaces = %s" % list(ifaces))
        dbg_to_file("typset = %s" % list(typset))
        
        for typ in typset:
            meta = graph.node[typ]['classinfo'].meta
            dbg_to_file("meta = %s" % meta)
            if 'ifaces' in meta and ifaces.intersection(meta['ifaces']): 
                types.append((typ, meta))
        dbg_to_file("returning types: %s" % types)
        return types

    def on_modified(self, fpath, added_set, changed_set, deleted_set):
        dbg_to_file("ProjDirFactory:on_modified: %s" % fpath)
        if os.path.isdir(fpath):
            return
        
        imported = False
        if fpath in self.analyzer.fileinfo: # file has been previously scanned
            visitor = self.analyzer.fileinfo[fpath]
            pre_set = set(visitor.classes.keys())
            
            if fpath in self.imported:  # we imported it earlier
                imported = True
                sys.path = [os.path.dirname(fpath)] + sys.path # add fpath location to sys.path
                try:
                    m = reload(self.imported[fpath][0])
                except ImportError as err:
                    return None
                finally:
                    sys.path = sys.path[1:]  # restore original sys.path
                self.imported[fpath] = (m, self.imported[fpath][1])
            self.on_deleted(fpath, set())
        else:  # it's a new file
            pre_set = set()

        visitor = self.analyzer.analyze_file(fpath)
        post_set = set(visitor.classes.keys())

        deleted_set.update(pre_set - post_set)
        added_set.update(post_set - pre_set)
        if imported:
            changed_set.update(pre_set.intersection(post_set))

    def on_deleted(self, fpath, deleted_set):
        dbg_to_file("ProjDirFactory:on_deleted: %s" % fpath)
        if os.path.isdir(fpath):
            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_deleted(pyfile, deleted_set)
        else:
            try:
                del self.imported[fpath]
            except KeyError:
                pass
            
            visitor = self.analyzer.fileinfo[fpath]
            deleted_set.update(visitor.classes.keys())

            self.analyzer.remove_file(fpath)
            
    def publish_updates(self, added_set, changed_set, deleted_set):
        dbg_to_file("ProjDirFactory:publish updates")
        publisher = Publisher.get_instance()
        if publisher:
            types = get_available_types()
            types.extend(self.get_available_types())
            dbg_to_file("publishing types: %s" % types)
            publisher.publish('types', 
                              [
                                  packagedict(types),
                                  list(added_set),
                                  list(changed_set),
                                  list(deleted_set),
                              ])

    def cleanup(self):
        """If this factory is removed from the FactoryManager during execution, this function
        will stop the watchdog observer thread.
        """
        if self.observer:
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