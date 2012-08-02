
#public symbols
__all__ = ["ProjDirFactory"]

import os
import sys
import threading
import traceback
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
from openmdao.util.log import logger
from openmdao.main.publisher import Publisher
from openmdao.gui.util import packagedict


class PyWatcher(FileSystemEventHandler):
    """
    Watches files and dispatches to :class:`ProjDirFactory`.
    Exceptions are caught and reported here so the daemon thread continues
    to run.
    """

    def __init__(self, factory):
        super(PyWatcher, self).__init__()
        self.factory = factory

    def on_modified(self, event):
        try:
            added_set = set()
            changed_set = set()
            deleted_set = set()
            if not event.is_directory and fnmatch.fnmatch(event.src_path, '*.py'):
                compiled = event.src_path+'c'
                if os.path.exists(compiled):
                    os.remove(compiled)
                self.factory.on_modified(event.src_path, added_set, changed_set, deleted_set)
                self.factory.publish_updates(added_set, changed_set, deleted_set)
        except Exception:
            traceback.print_exc()

    on_created = on_modified
    
    def on_moved(self, event):
        try:
            added_set = set()
            changed_set = set()
            deleted_set = set()
        
            publish = False
            if event._src_path and (event.is_directory or fnmatch.fnmatch(event._src_path, '*.py')):
                if not event.is_directory:
                    compiled = event._src_path+'c'
                    if os.path.exists(compiled):
                        os.remove(compiled)
                self.factory.on_deleted(event._src_path, deleted_set)
                publish = True
        
            if fnmatch.fnmatch(event._dest_path, '*.py'):
                self.factory.on_modified(event._dest_path, added_set, changed_set, deleted_set)
                publish = True
            
            if publish:
                self.factory.publish_updates(added_set, changed_set, deleted_set)
        except Exception:
            traceback.print_exc()

    def on_deleted(self, event):
        try:
            added_set = set()
            changed_set = set()
            deleted_set = set()
            if event.is_directory or fnmatch.fnmatch(event.src_path, '*.py'):
                compiled = event.src_path+'c'
                if os.path.exists(compiled):
                    os.remove(compiled)
                self.factory.on_deleted(event.src_path, deleted_set)
                self.factory.publish_updates(added_set, changed_set, deleted_set)
        except Exception:
            traceback.print_exc()

            
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
    return len(plugin_ifaces.intersection(meta.get('ifaces',[]))) > 0

def _find_module_attr(modpath):
    """Return an attribute from a module based on the given modpath.
    Import the module if necessary.
    """
    parts = modpath.split('.')
    if len(parts) <= 1:
        return None
    
    mname = '.'.join(parts[:-1])
    mod = sys.modules.get(mname)
    if mod is None:
        try:
            __import__(mname)
            mod = sys.modules.get(mname)
        except ImportError:
            pass
    if mod:
        return getattr(mod, parts[-1])
    
    # try one more level down in case attr is nested
    obj = _find_module_attr(mname)
    if obj:
        obj = getattr(obj, parts[-1], None)
    return obj
    
class ProjDirFactory(Factory):
    """A Factory that watches a Project directory and dynamically keeps
    the set of available types up-to-date as project files change.
    """
    def __init__(self, watchdir, use_observer=True, observer=None):
        super(ProjDirFactory, self).__init__()
        self._lock = threading.RLock()
        self.watchdir = watchdir
        try:
            self.analyzer = PythonSourceTreeAnalyser()
            
            added_set = set()
            changed_set = set()
            deleted_set = set()
            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_modified(pyfile, added_set, changed_set, deleted_set)
            
            if use_observer:
                self._start_observer(observer)
                self.publish_updates(added_set, changed_set, deleted_set)
            else:
                self.observer = None  # sometimes for debugging/testing it's easier to turn observer off
        except Exception as err:
            logger.error(str(err))

    def _start_observer(self, observer):
        if observer is None:
            self.observer = Observer()
            self._ownsobserver = True
        else:
            self.observer = observer
            self._ownsobserver = False
        self.observer.schedule(PyWatcher(self), path=self.watchdir, recursive=True)
        if self._ownsobserver:
            self.observer.daemon = True
            self.observer.start()
        
    def create(self, typ, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Create and return an instance of the specified type, or None if
        this Factory can't satisfy the request.
        """
        logger.error("attempting to create a %s" % typ)
        import pprint
        logger.error(pprint.pformat(self.analyzer.class_map))
        if server is None and res_desc is None and typ in self.analyzer.class_map:
            logger.error("found it in class_map")
            with self._lock:
                fpath = self.analyzer.class_map[typ].fname
                modpath = self.analyzer.fileinfo[fpath][0].modpath
                if modpath in sys.modules:
                    if os.path.getmtime(fpath) > self.analyzer.fileinfo[fpath][1]:
                        reload(sys.modules[modpath])
                else:
                    logger.error("adding %s to sys.path" % get_ancestor_dir(fpath, len(modpath.split('.'))))
                    sys.path = [get_ancestor_dir(fpath, len(modpath.split('.')))] + sys.path
                    try:
                        __import__(modpath)
                    except ImportError as err:
                        logger.error("import failed")
                        return None
                    finally:
                        sys.path = sys.path[1:]

                mod = sys.modules[modpath]
                try:
                    ctor = _find_module_attr(typ)
                except KeyError:
                    logger.error("lookup of ctor failed")
                    return None
                return ctor(**ctor_args)
        return None
    
    def get_available_types(self, groups=None):
        """Return a list of available types that cause predicate(classname, metadata) to
        return True.
        """
        with self._lock:
            graph = self.analyzer.graph
            typset = set(graph.nodes())
            types = []
        
            if groups is None:
                ifaces = set([v[0] for v in plugin_groups.values()])
            else:
                ifaces = set([v[0] for k,v in plugin_groups.items() if k in groups])
        
            for typ in typset:
                if typ.startswith('openmdao.'): # don't include any standard lib types
                    continue
                if 'classinfo' in graph.node[typ]:
                    meta = graph.node[typ]['classinfo'].meta
                    if ifaces.intersection(self.analyzer.get_interfaces(typ)):
                        meta = meta.copy()
                        meta['_context'] = 'In Project'
                        types.append((typ, meta))
            return types

    def on_modified(self, fpath, added_set, changed_set, deleted_set):
        if os.path.isdir(fpath):
            return
        
        with self._lock:
            imported = False
            if fpath in self.analyzer.fileinfo: # file has been previously scanned
                visitor = self.analyzer.fileinfo[fpath][0]
                pre_set = set(visitor.classes.keys())
                modpath = get_module_path(fpath)
                if  modpath in sys.modules:  # we imported it earlier
                    mod = sys.modules[modpath]
                    imported = True
                    sys.path = [os.path.dirname(fpath)] + sys.path # add fpath location to sys.path
                    try:
                        reload(mod)
                    except ImportError as err:
                        return None
                    finally:
                        sys.path = sys.path[1:]  # restore original sys.path
                elif os.path.getmtime(fpath) > self.analyzer.fileinfo[fpath][1]:
                    modpath = get_module_path(fpath)
                    if modpath in sys.modules:
                        reload(sys.modules[modpath])
                self.on_deleted(fpath, set()) # clean up old refs
            else:  # it's a new file
                pre_set = set()

            try:
                visitor = self.analyzer.analyze_file(fpath)
            except Exception as exc:
                publisher = Publisher.get_instance()
                if publisher:
                    publisher.publish('file_errors', str(exc))
                else:
                    logger.error("no Publisher found")
                raise
            post_set = set(visitor.classes.keys())

            deleted_set.update(pre_set - post_set)
            added_set.update(post_set - pre_set)
            if imported:
                changed_set.update(pre_set.intersection(post_set))

    def on_deleted(self, fpath, deleted_set):
        with self._lock:
            if os.path.isdir(fpath):
                for pyfile in find_files(self.watchdir, "*.py"):
                    self.on_deleted(pyfile, deleted_set)
            else:
                visitor = self.analyzer.fileinfo[fpath][0]
                deleted_set.update(visitor.classes.keys())
                self.analyzer.remove_file(fpath)
            
    def publish_updates(self, added_set, changed_set, deleted_set):
        publisher = Publisher.get_instance()
        if publisher:
            types = get_available_types()
            types.extend(self.get_available_types())
            publisher.publish('types', 
                              [
                                  packagedict(types),
                                  list(added_set),
                                  list(changed_set),
                                  list(deleted_set),
                              ])
        else:
            logger.error("no Publisher found")

    def cleanup(self):
        """If this factory is removed from the FactoryManager during execution, this function
        will stop the watchdog observer thread.
        """
        if self.observer and self._ownsobserver:
            self.observer.unschedule_all()
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
