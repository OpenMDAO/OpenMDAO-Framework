
#public symbols
__all__ = ["ProjDirFactory"]

import os
import sys
import threading
import traceback
import fnmatch
import ast
from inspect import isclass, getmembers, getmro

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

from zope.interface import implementedBy

from openmdao.main.factory import Factory
from openmdao.main.factorymanager import get_available_types
from openmdao.util.dep import find_files, plugin_groups
from openmdao.util.fileutil import get_module_path
from openmdao.util.log import logger
from openmdao.main.publisher import publish
from openmdao.gui.util import packagedict
from openmdao.main.project import PROJ_DIR_EXT, parse


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
                compiled = event.src_path + 'c'
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

            pub = False
            if event._src_path and \
               (event.is_directory or fnmatch.fnmatch(event._src_path, '*.py')):
                if not event.is_directory:
                    compiled = event._src_path + 'c'
                    if os.path.exists(compiled):
                        os.remove(compiled)
                self.factory.on_deleted(event._src_path, deleted_set)
                pub = True

            if fnmatch.fnmatch(event._dest_path, '*.py'):
                self.factory.on_modified(event._dest_path, added_set, changed_set, deleted_set)
                pub = True

            if pub:
                self.factory.publish_updates(added_set, changed_set, deleted_set)
        except Exception:
            traceback.print_exc()

    def on_deleted(self, event):
        try:
            added_set = set()
            changed_set = set()
            deleted_set = set()
            if event.is_directory or fnmatch.fnmatch(event.src_path, '*.py'):
                compiled = event.src_path + 'c'
                if os.path.exists(compiled):
                    os.remove(compiled)
                self.factory.on_deleted(event.src_path, deleted_set)
                self.factory.publish_updates(added_set, changed_set, deleted_set)
        except Exception:
            traceback.print_exc()


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


class _ClassVisitor(ast.NodeVisitor):
    def __init__(self, fname):
        ast.NodeVisitor.__init__(self)
        self.classes = []

        with open(fname, 'Ur') as f:
            contents = f.read()
        self.visit(parse(contents, fname))

    def visit_ClassDef(self, node):
        self.classes.append(node.name)


class _FileInfo(object):
    def __init__(self, fpath):
        self.fpath = fpath
        self.modpath = get_module_path(fpath)
        self.modtime = os.path.getmtime(fpath)
        __import__(self.modpath)
        module = sys.modules[self.modpath]
        self.version = getattr(module, '__version__', None)
        self._update_class_info()

    def _update_class_info(self):
        self.classes = {}
        cset = set(_ClassVisitor(self.fpath).classes)
        module = sys.modules[self.modpath]
        for key, val in getmembers(module, isclass):
            if key in cset:
                fullname = '.'.join([self.modpath, key])
                self.classes[fullname] = {
                    'ctor': key,
                    'bases':  [klass.__name__ for klass in getmro(val)],
                    'ifaces': [klass.__name__ for klass in implementedBy(val)],
                    'version': self.version,
                    'modpath': self.modpath,
                }

    def _reload(self):
        mtime = os.path.getmtime(self.fpath)
        if self.modtime < mtime:
            cmpfname = os.path.splitext(self.fpath)[0] + '.pyc'
            # unless we remove the .pyc file, reload will just use it and won't
            # see any source updates.  :(
            if os.path.isfile(cmpfname):
                os.remove(cmpfname)
            logger.info("reloading module %s" % self.modpath)
            reload(sys.modules[self.modpath])
            self.modtime = mtime
        self._update_class_info()

    def update(self, added, changed, removed):
        """File may have changed on disk, update information and return
        sets of added, removed and (possibly) changed classes.
        """
        self.modpath = get_module_path(self.fpath)
        startset = set(self.classes.keys())

        self._reload()

        keys = set(self.classes.keys())
        added.update(keys - startset)
        changed.update(startset & keys)
        removed.update(startset - keys)


class ProjDirFactory(Factory):
    """A Factory that watches a Project directory and dynamically keeps
    the set of available types up-to-date as project files change.
    """
    def __init__(self, watchdir, use_observer=True, observer=None):
        super(ProjDirFactory, self).__init__()
        self._lock = threading.RLock()
        self.observer = None
        self.watchdir = watchdir
        self._files = {}    # mapping of file pathnames to _FileInfo objects
        self._classes = {}  # mapping of class names to _FileInfo objects
        try:
            added_set = set()
            changed_set = set()
            deleted_set = set()

            modeldir = watchdir + PROJ_DIR_EXT
            if modeldir not in sys.path:
                sys.path = [modeldir] + sys.path
                logger.info("added %s to sys.path" % modeldir)

            for pyfile in find_files(self.watchdir, "*.py"):
                self.on_modified(pyfile, added_set, changed_set, deleted_set)

            if use_observer:
                self._start_observer(observer)
                self.publish_updates(added_set, changed_set, deleted_set)
            else:
                # sometimes for debugging/testing it's easier to turn observer off
                self.observer = None
        except Exception as err:
            self._error(str(err))
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
        if server is None and res_desc is None:
            klass = self._lookup(typ, version)
            if klass is not None:
                return klass(**ctor_args)
        return None

    def _lookup(self, typ, version):
        """ Return class for `typ`. """
        try:
            cinfo = self._classes[typ].classes[typ]
            mod = sys.modules[cinfo['modpath']]
            return getattr(mod, cinfo['ctor'])
        except KeyError:
            return None

    def get_available_types(self, groups=None):
        """Return a list of available types."""
        with self._lock:
            typset = set(self._classes.keys())
            types = []

            if groups is None:
                ifaces = set([v[0] for v in plugin_groups.values()])
            else:
                ifaces = set([v[0] for k, v in plugin_groups.items() if k in groups])

            for typ in typset:
                finfo = self._classes[typ]
                meta = finfo.classes[typ]

                if ifaces.intersection(meta['ifaces']):
                    meta = {
                        'bases': meta['bases'],
                        'ifaces': meta['ifaces'],
                        'version': meta['version'],
                        '_context': 'In Project',
                    }
                    types.append((typ, meta))
            return types

    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary."""
        cls = self._lookup(typname, version)
        if cls is None:
            return None
        else:
            return self.form_signature(cls)

    def on_modified(self, fpath, added_set, changed_set, deleted_set):
        if os.path.isdir(fpath):
            return None
        with self._lock:
            finfo = self._files.get(fpath)
            if finfo is None:
                try:
                    fileinfo = _FileInfo(fpath)
                except Exception as err:
                    if isinstance(err, SyntaxError):
                        if err.offset:
                            msg = '%s%s^\n%s' % (err.text, ' ' * err.offset, str(err))
                        else:
                            msg = str(err)
                        self._error(msg)
                    else:
                        self._error(str(err))
                    return
                self._files[fpath] = fileinfo
                added_set.update(fileinfo.classes.keys())
                for cname in fileinfo.classes.keys():
                    self._classes[cname] = fileinfo
            else:  # updating a file that's already been imported
                try:
                    finfo.update(added_set, changed_set, deleted_set)
                except Exception as err:
                    if isinstance(err, SyntaxError):
                        if err.offset:
                            msg = '%s%s^\n%s' % (err.text, ' ' * err.offset, str(err))
                        else:
                            msg = str(err)
                        self._error(msg)
                    else:
                        self._error(str(err))
                    self._remove_fileinfo(fpath)
                    return
                for cname in added_set:
                    self._classes[cname] = finfo
                for cname in deleted_set:
                    del self._classes[cname]

    def on_deleted(self, fpath, deleted_set):
        with self._lock:
            if os.path.isdir(fpath):
                for pyfile in find_files(self.watchdir, "*.py"):
                    self.on_deleted(pyfile, deleted_set)
            else:
                finfo = self._files.get(fpath)
                if finfo:
                    deleted_set.update(finfo.classes.keys())
                    self._remove_fileinfo(fpath)

    def publish_updates(self, added_set, changed_set, deleted_set):
        types = get_available_types()
        try:
            publish('types',
                    [
                        packagedict(types),
                        list(added_set),
                        list(changed_set),
                        list(deleted_set),
                    ])
        except:
            logger.error("publish of types failed")

    def _error(self, msg):
        logger.error(msg)
        publish('console_errors', msg)
        publish('file_errors', msg)

    def _remove_fileinfo(self, fpath):
        """Clean up all data related to the given file. This typically occurs
        when there is some error during the import of the file.
        """
        finfo = self._files.get(fpath)
        if finfo:
            classes = [c for c, f in self._classes.items() if f is finfo]
            for klass in classes:
                del self._classes[klass]
            del self._files[fpath]
            return classes

    def cleanup(self):
        """If this factory is removed from the FactoryManager during execution,
        this function will stop the watchdog observer thread.
        """
        try:
            sys.path.remove(self.watchdir)
        except:
            pass
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
