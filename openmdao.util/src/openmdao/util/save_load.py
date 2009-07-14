"""
Save/load utilities.

Note that Pickle can't save references to functions that aren't defined at the
top level of a module, and there doesn't appear to be a viable workaround.
Normally pickle won't handle instance methods either, but there is code in
place to work around that.

When saving to an egg, the module named __main__ changes when reloading. This
requires finding the real module name and munging references to __main__.
References to old-style class types can't be restored correctly.

Also note that YAML format doesn't handle more than one layer of back-pointers, 
so it's only suitable for very flat object networks.
"""

import pickle
import cPickle
import yaml
try:
    from yaml import CLoader as Loader
    from yaml import CDumper as Dumper
    _libyaml = True
except ImportError:
    from yaml import Loader, Dumper
    _libyaml = False

import datetime
import inspect
import modulefinder
import os
import pkg_resources
import shutil
import site
import sys
import tempfile
import zc.buildout.easy_install
import zipfile

from openmdao.util import eggwriter

__all__ = ('save', 'save_to_egg',
           'load', 'load_from_eggfile', 'load_from_eggpkg',
           'SAVE_YAML', 'SAVE_LIBYAML', 'SAVE_PICKLE', 'SAVE_CPICKLE',
           'EGG_SERVER_URL')

__version__ = '0.1'

# Save formats.
SAVE_YAML    = 1
SAVE_LIBYAML = 2
SAVE_PICKLE  = 3
SAVE_CPICKLE = 4

EGG_SERVER_URL = 'http://torpedo.grc.nasa.gov:31001'

# Saved modulefinder results, keyed by module path.
_SAVED_FINDINGS = {}


class IMHolder(object):
    """ Holds an instancemethod object in a pickleable form. """

    def __init__(self, obj):
        # The inspect module notes 'class object that *asked* for this method'.
        # This obscure note and Pickle's reluctance to work on instancemethods
        # prompts the verification here...
        if not hasattr(obj.im_class, obj.__name__):
            raise RuntimeError('IMHolder: %r not a member of class (%s)'
                               % (obj, obj.im_class))
        self.name = obj.__name__
        self.im_self = obj.im_self
        if obj.im_self is not None:
            self.im_class = None  # Avoid possible __main__ issues.
        else:
            # TODO: handle __main__ for im_class.__module__.
            if obj.im_class.__module__ == '__main__':
                raise RuntimeError('IMHolder: %r with module __main__ (%s)'
                                   % (obj, obj.im_self))
            self.im_class = obj.im_class

    def method(self):
        """ Return instancemethod corresponding to saved state. """
        if self.im_self is not None:
            return getattr(self.im_self, self.name)
        else:
            return getattr(self.im_class, self.name)


class NullLogger(object):
    """ Used when the supplied logger is None. """

    def debug(self, msg, *args, **kwargs):
        pass

    def error(self, msg, *args, **kwargs):
        pass

    def exception(self, msg, *args, **kwargs):
        pass

    def info(self, msg, *args, **kwargs):
        pass

    def warning(self, msg, *args, **kwargs):
        pass


def save_to_egg(root, name, version=None, py_dir=None, src_dir=None,
                src_files=None, entry_pts=None, dst_dir=None,
                format=SAVE_CPICKLE, proto=-1, logger=None,
                use_setuptools=False):
    """
    Save state and other files to an egg.
    Analyzes the objects saved for distribution dependencies.
    Modules not found in any distribution are recorded in a '`name`.missing' file.
    Also creates and saves loader scripts for each entry point.

    - `root` is the root of the object graph to be saved.
    - `name` is the name of the package.
    - `version` defaults to a timestamp of the form 'YYYY.MM.DD.HH.mm'.
    - `py_dir` is the (root) directory for local Python files. \
      It defaults to the current directory.
    - `src_dir` is the root of all (relative) `src_files`.
    - 'entry_pts' is a list of (obj, obj_name) tuples for additional entries.
    - `dst_dir` is the directory to write the egg in.

    The resulting egg can be unpacked on UNIX via 'sh egg-file'.
    Returns the egg's filename.
    """
    if logger is None:
        logger = NullLogger()

    orig_dir = os.getcwd()

    if py_dir is None:
        py_dir = orig_dir
    elif not os.path.isabs(py_dir):
        py_dir = os.path.abspath(py_dir)
    else:
        py_dir = os.path.realpath(py_dir)  # In case it's a symlink.

    if src_dir and not os.path.isabs(src_dir):
        src_dir = os.path.abspath(src_dir)
    if src_files is None:
        src_files = set()

    if version is None:
        now = datetime.datetime.now()  # Could consider using utcnow().
        version = '%d.%02d.%02d.%02d.%02d' % \
                  (now.year, now.month, now.day, now.hour, now.minute)

    if dst_dir is None:
        dst_dir = orig_dir
    if not os.access(dst_dir, os.W_OK):
        raise IOError("Can't save to '%s', no write permission" % dst_dir)

    if entry_pts is None:
        entry_pts = []

    egg_name = eggwriter.egg_filename(name, version)
    logger.debug('Saving to %s in %s...', egg_name, orig_dir)

    # Put instance methods into pickleable form.
    # (do this now, rather than in save(), so fix_objects() can fix __main__)
    _fix_instancemethods(root)
    try:
        # Get a list of all objects we'll be saving.
        objs = _get_objects(root, logger)

        # Check that each object can be pickled.
        _check_objects(objs, logger)

        # Fixup objects, classes, & sys.modules for __main__ imports.
        fixup = _fix_objects(objs)
        _verify_objects(root, logger)
        tmp_dir = None
        try:
            # Determine distributions and local modules required.
            required_distributions, local_modules, missing_modules = \
                _get_distributions(objs, py_dir, logger)

            logger.debug('    py_dir: %s', py_dir)
            logger.debug('    src_dir: %s', src_dir)
            logger.debug('    local_modules:')
            for module in sorted(local_modules):
                mod = module
                if mod.startswith(py_dir):
                    mod = mod[len(py_dir)+1:]
                logger.debug('        %s', mod)

            # Move to scratch area.
            tmp_dir = tempfile.mkdtemp(prefix='Egg_', dir=tmp_dir)
            os.chdir(tmp_dir)
            cleanup_files = []

            buildout_name = 'buildout.cfg'
            buildout_path = os.path.join(name, buildout_name)
            buildout_orig = buildout_path+'-orig'
            try:
                if src_dir:
                    if py_dir != src_dir or sys.platform == 'win32':
                        # Copy original directory to object name.
                        shutil.copytree(src_dir, name)
                    else:
                        # Just link original directory to object name.
                        os.symlink(src_dir, name)
                else:
                    os.mkdir(name)

                # If py_dir isn't src_dir, copy local modules from py_dir.
                if py_dir != src_dir:
                    for path in local_modules:
                        if not os.path.exists(
                                   os.path.join(name, os.path.basename(path))):
                            if not os.path.isabs(path):
                                path = os.path.join(py_dir, path)
                            shutil.copy(path, name)

                # If any distributions couldn't be found, record them in a file.
                if missing_modules:
                    missing_name = '%s.missing' % name
                    missing_path = os.path.join(name, missing_name)
                    cleanup_files.append(missing_path)
                    out = open(missing_path, 'w')
                    for mod, path in sorted(missing_modules,
                                            key=lambda item: item[0]):
                        out.write(mod+'\n')
                    out.close()
                    src_files.add(missing_name)

                all_entries = [(root, name)]
                all_entries.extend(entry_pts)
                entry_info = []
                for obj_info in all_entries:
                    obj, obj_name = obj_info

                    # Save state of object hierarchy.
                    state_name, state_path = \
                        _write_state_file(name, obj, obj_name, format, proto,
                                          logger)
                    src_files.add(state_name)
                    cleanup_files.append(state_path)

                    # Create loader script.
                    loader = '%s_loader' % obj_name
                    loader_path = os.path.join(name, loader+'.py')
                    cleanup_files.append(loader_path)
                    _write_loader_script(loader_path, state_name, name,
                                         obj is root)

                    entry_info.append((obj_name, loader))

                # Create buildout.cfg
                if os.path.exists(buildout_path):
                    os.rename(buildout_path, buildout_orig)
                _create_buildout(name, EGG_SERVER_URL, required_distributions,
                                 buildout_path)
                src_files.add(buildout_name)

                # If needed, make an empty __init__.py
                init_path = os.path.join(name, '__init__.py')
                if not os.path.exists(init_path):
                    cleanup_files.append(init_path)
                    out = open(init_path, 'w')
                    out.close()

                # Save everything to an egg.
                doc = root.__doc__
                if doc is None:
                    doc = ''
                loader = entry_info[0][1]
                if use_setuptools:
                    eggwriter.write_via_setuptools(name, doc, version, loader,
                                                   src_files,
                                                   required_distributions,
                                                   dst_dir, logger,
                                                   entry_info[1:])
                else:
                    eggwriter.write(name, doc, version, loader,
                                    src_files, required_distributions,
                                    dst_dir, logger, entry_info[1:])
            finally:
                for path in cleanup_files:
                    if os.path.exists(path):
                        os.remove(path)
                if os.path.exists(buildout_orig):
                    os.rename(buildout_orig, buildout_path)
                elif os.path.exists(buildout_path):
                    os.remove(buildout_path)

        finally:
            os.chdir(orig_dir)
            if tmp_dir:
                shutil.rmtree(tmp_dir)
            _restore_objects(fixup)

    finally:
        _restore_instancemethods(root)

    return egg_name


def _fix_instancemethods(root, previsited=None):
    """ Replace references to instancemethods with IMHolders. """

    def _fix_im_recurse(obj, visited):
        """ Replace recursively. """
        visited.add(id(obj))
        if isinstance(obj, dict):
            for key2, obj2 in obj.items():
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[key2] = IMHolder(obj2)
                else:
                    _fix_im_recurse(obj2, visited)
        elif isinstance(obj, list) or isinstance(obj, set):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[i] = IMHolder(obj2)
                else:
                    _fix_im_recurse(obj2, visited)
        elif isinstance(obj, tuple):
            for obj2 in obj:
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    raise RuntimeError('_fix_im_recurse: tuple %r contains'
                                       ' reference to instance method %r'
                                       % (obj, obj2))
                else:
                    _fix_im_recurse(obj2, visited)
        elif hasattr(obj, '__dict__'):
            _fix_im_recurse(obj.__dict__, visited)

    visited = set()
    if previsited is None:
        pass
    elif isinstance(previsited, list):
        for obj in previsited:
            visited.add(id(obj))
    else:
        visited.add(id(previsited))
    _fix_im_recurse(root, visited)


def _restore_instancemethods(root, previsited=None):
    """ Restore references to instancemethods. """

    def _restore_im_recurse(obj, visited):
        """ Restore recursively. """
        visited.add(id(obj))
        if isinstance(obj, dict):
            for key2, obj2 in obj.items():
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    obj[key2] = obj2.method()
                else:
                    _restore_im_recurse(obj2, visited)
        elif isinstance(obj, list) or isinstance(obj, set):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    obj[i] = obj2.method()
                else:
                    _restore_im_recurse(obj2, visited)
        elif isinstance(obj, tuple):
            for obj2 in obj:
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    raise RuntimeError('_restore_im_recurse: tuple %r contains'
                                       ' reference to IMHolder %r'
                                       % (obj, obj2))
                else:
                    _restore_im_recurse(obj2, visited)
        elif hasattr(obj, '__dict__'):
            _restore_im_recurse(obj.__dict__, visited)

    visited = set()
    if previsited is None:
        pass
    elif isinstance(previsited, list):
        for obj in previsited:
            visited.add(id(obj))
    else:
        visited.add(id(previsited))
    _restore_im_recurse(root, visited)


def _get_objects(root, logger):
    """ Get objects to be saved. """

    def _recurse_get_objects(obj, objs, visited, logger):
        """ Use __getstate__(), or scan __dict__, or scan container. """
        try:
            state = obj.__getstate__()
        except AttributeError:
            try:
                state = obj.__dict__
            except AttributeError:
                if isinstance(obj, dict) or isinstance(obj, list) or \
                   isinstance(obj, set)  or isinstance(obj, tuple):
                    state = obj
                else:
                    return  # Some non-container primitive.
            container = state
        except Exception, exc:
            logger.error("During save_to_egg, _get_objects error %s: %s",
                         type(obj), exc)
            return
        else:
            # If possible, get the original container.
            # (__getstate__ often returns a (modified) copy)
            try:
                container = obj.__dict__
            except AttributeError:
                container = None

        if isinstance(state, dict):
            indices = state.keys()
            state = state.values()
        else:
            indices = range(len(state))

        for obj, index in zip(state, indices):
            if id(obj) in visited:
                continue
            visited.add(id(obj))
            objs.append((obj, container, index))
            if not inspect.isclass(obj):
                _recurse_get_objects(obj, objs, visited, logger)

    objs = [(root, None, None)]
    visited = set()
    visited.add(id(root))
    _recurse_get_objects(root, objs, visited, logger)
    return objs


def _check_objects(objs, logger):
    """ Check that each object can be pickled. """
    errors = 0
    for obj, container, index in objs:
        try:
            cls = obj.__class__
        except AttributeError:
            pass
        else:
            classname = cls.__name__
            if classname == 'function' and \
               not hasattr(sys.modules[obj.__module__], obj.__name__):
                logger.error(
                    "Can't save, can't pickle non-toplevel function %s:%s",
                    obj.__module__, obj.__name__)
                errors += 1
#        # (Debug) actually try to pickle.
#        try:
#            cPickle.dumps(obj)
#        except Exception, exc:
#            logger.error("Can't pickle obj %r %s:",
#                         obj, obj.__class__.__name__)
#            logger.error('    %s', exc)
#            errors += 1
    if errors:
        plural = 's' if errors > 1 else ''
        raise RuntimeError("Can't save, %d object%s cannot be pickled."
                           % (errors, plural))


def _verify_objects(root, logger):
    """ Verify no references to __main__ exist. """
    objs = _get_objects(root, logger)
    for obj, container, index in objs:
        try:
            mod = obj.__module__
        except AttributeError:
            continue

        if mod == '__main__':
            raise RuntimeError("Can't save, unable to patch __main__"
                               " module reference in obj %r, container %r"
                               " index %s", obj, container, index)


def _fix_objects(objs):
    """ Fixup objects, classes, & sys.modules for __main__ imports. """
    fixup_objects = []
    fixup_classes = {}
    fixup_modules = set()

    for obj, container, index in objs:
        if inspect.isclass(obj):
            cls = obj
        else:
            cls = obj.__class__
        classname = cls.__name__
        if classname == 'instancemethod':
            obj = obj.im_self
        try:
            mod = obj.__module__
        except AttributeError:
            continue  # No module entry to fix.

        if mod == '__main__':
            if classname in ('function', 'type'):
                raise RuntimeError("Can't save: reference to %s defined "
                                   "in main module %r" % (classname, obj))
            mod = cls.__module__
            if mod == '__main__' and (classname not in fixup_classes.keys()):
                mod, module = _find_module(classname)
                if mod:
                    old = cls
                    new = getattr(module, classname)
                    fixup_classes[classname] = (old, new)
                    fixup_modules.add(mod)
                else:
                    _restore_objects((fixup_objects, fixup_classes,
                                      fixup_modules))
                    raise RuntimeError("Can't find module for '%s'" % classname)

            if inspect.isclass(obj):
                if isinstance(container, tuple):
                    raise RuntimeError("Can't save: reference to class %s"
                                       " defined in main module is contained in"
                                       " a tuple." % classname)
                else:
                    container[index] = fixup_classes[classname][1]
            else:
                try:
                    obj.__class__ = fixup_classes[classname][1]
                except KeyError:
                    raise RuntimeError("Can't fix %r, classname %s, module %s"
                                       % (obj, classname, mod))
                obj.__module__ = obj.__class__.__module__
            fixup_objects.append((obj, container, index))

    return (fixup_objects, fixup_classes, fixup_modules)


def _find_module(attr):
    """ Try to determine 'real' module name for attribute in __main__. """
    if '.' not in sys.path:
        sys.path.append('.')
    for arg in sys.argv:
        if arg.endswith('.py'):
            mod = os.path.basename(arg)[:-3]
            try:
                module = __import__(mod, fromlist=[attr])
            except ImportError:
                pass
            else:
                return (mod, module)
    return (None, None)


def _restore_objects(fixup):
    """ Restore objects, classes, & sys.modules for __main__ imports. """
    fixup_objects, fixup_classes, fixup_modules = fixup

    for obj, container, index in fixup_objects:
        obj.__module__ = '__main__'
        if inspect.isclass(obj):
            classname = obj.__name__
            container[index] = fixup_classes[classname][0]
        else:
            classname = obj.__class__.__name__
            if classname != 'function':
                obj.__class__ = fixup_classes[classname][0]

    for classname in fixup_classes.keys():
        new = fixup_classes[classname][1]
        del new

    for mod in fixup_modules:
        del sys.modules[mod]


def _get_distributions(objs, py_dir, logger):
    """ Return (distributions, local_modules, missing) used by objs. """
    distributions = set()
    local_modules = set()
    missing = set()
    modules = ['__builtin__']
    prefixes = []
    site_lib = os.path.dirname(site.__file__)
    site_pkg = site_lib+os.sep+'site-packages'

    for obj, container, index in objs:
        try:
            name = obj.__module__
        except AttributeError:
            continue
        if name in modules:
            continue
        modules.append(name)

        # Skip modules in distributions we already know about.
        path = sys.modules[name].__file__
        if path.startswith(site_lib) and not path.startswith(site_pkg):
            continue
        found = False
        for prefix in prefixes:
            if path.startswith(prefix):
                found = True
                break
        if found:
            continue

        egg = path.find('.egg')
        if egg > 0:
            # We'll assume the egg has accurate dependencies.
            path = path[:egg+4]
            _process_egg(path, distributions, prefixes, logger)
        else:
            # Use module finder to get the modules this object requires.
            if path.endswith('.pyc') or path.endswith('.pyo'):
                path = path[:-1]
            if not os.path.exists(path):
                logger.warning("    module path '%s' does not exist", path)
                continue

            if not os.path.isabs(path):
                path = os.path.join(os.getcwd(), path)

            finder_items = None
            if path in _SAVED_FINDINGS.keys():
                logger.debug("    reusing analysis of '%s'", path)
                finder_items = _SAVED_FINDINGS[path]
            else:
                logger.debug("    analyzing '%s'...", path)
                finder = modulefinder.ModuleFinder()
                try:
                    finder.run_script(path)
                except Exception:
                    logger.exception("ModuleFinder for '%s'" % path)
                else:
                    finder_items = finder.modules.items()
                    _SAVED_FINDINGS[path] = finder_items

            if finder_items is not None:
                _process_found_modules(py_dir, finder_items, modules,
                                       distributions, prefixes, local_modules,
                                       missing, logger)

    distributions = sorted(distributions, key=lambda dist: dist.project_name)
    logger.debug('    required distributions:')
    for dist in distributions:
        logger.debug('        %s %s', dist.project_name, dist.version)
    return (distributions, local_modules, missing)


def _process_egg(path, distributions, prefixes, logger):
    """ Update distributions and prefixes based on egg data. """
    logger.debug("    processing '%s'", path)
    dist = pkg_resources.Distribution.from_filename(path)
    distributions.add(dist)
    prefixes.append(path)

    for req in dist.requires():
        logger.debug("    requires '%s'", req)
        dep = pkg_resources.get_distribution(req, logger)
        distributions.add(dep)
        loc = dep.location
        if loc.endswith('.egg') and loc not in prefixes:
            prefixes.append(loc)


def _process_found_modules(py_dir, finder_items, modules, distributions,
                           prefixes, local_modules, missing, logger):
    """ Use ModuleFinder data to update distributions and local_modules. """
    working_set = pkg_resources.WorkingSet()
    site_lib = os.path.dirname(site.__file__)
    site_pkg = site_lib+os.sep+'site-packages'
    py_version = 'python%s' % sys.version[:3]
    not_found = set()

    for name, module in sorted(finder_items, key=lambda item: item[0]):
        if name in modules:
            continue
        if name != '__main__':
            modules.append(name)
        try:
            path = module.__file__
        except AttributeError:
            continue
        if not path:
            continue

        dirpath = os.path.dirname(path)
        if dirpath == '.' or dirpath.startswith(py_dir):
            # May need to be copied later.
            local_modules.add(path)
            continue

        # Skip modules in distributions we already know about.
        if path.startswith(site_lib) and not path.startswith(site_pkg):
            continue
        found = False
        for prefix in prefixes:
            if path.startswith(prefix):
                found = True
                break
        if found:
            continue

        # Record distribution.
        for dist in working_set:
            loc = dist.location
            # Protect against a 'bare' location.
            if loc.endswith('site-packages') or loc.endswith(py_version):
                loc += os.sep+dist.project_name
            if path.startswith(loc):
                distributions.add(dist)
                if loc.endswith('.egg'):
                    prefixes.append(loc)
                break
        else:
            if dirpath not in not_found:
                if not dirpath.endswith('site-packages'):
                    not_found.add(dirpath)
                    path = dirpath
                logger.warning('No distribution found for %s', path)
                missing.add((name, path))


def _create_buildout(name, server_url, distributions, path):
    """ Create buildout.cfg """
    out = open(path, 'w')
    out.write("""\
[buildout]
parts = %(name)s
index = %(server)s
unzip = true

[%(name)s]
recipe = zc.recipe.egg:scripts
interpreter = python
eggs =
""" % {'name':name, 'server':server_url})
    for dist in distributions:
        out.write("    %s == %s\n" % (dist.project_name, dist.version))
    out.close()


def _write_state_file(dst_dir, root, name, format, proto, logger):
    """ Write state of `root` and its children. Returns (filename, path). """
    if format is SAVE_CPICKLE or format is SAVE_PICKLE:
        state_name = name+'.pickle'
    elif format is SAVE_LIBYAML or format is SAVE_YAML:
        state_name = name+'.yaml'
    else:
        raise RuntimeError("Unknown format '%s'." % format)
    state_path = os.path.join(dst_dir, state_name)
    try:
        save(root, state_path, format, proto, logger, fix_im=False)
    except Exception, exc:
        raise type(exc)("Can't save to '%s': %s" % (state_path, exc))

    return (state_name, state_path)


def _write_loader_script(path, state_name, package, top):
    """ Write script used for loading object(s). """
    if state_name.startswith(package):
        pkg_arg = ''
    else:
        pkg_arg = ", package='%s'" % package

    if top:
        top_arg = ''
    else:
        top_arg = ', top_obj=False'

    out = open(path, 'w')
    out.write("""\
import os
import sys
if not '.' in sys.path:
    sys.path.append('.')

try:
    from openmdao.main import Component
    from openmdao.main.constants import SAVE_CPICKLE, SAVE_LIBYAML
    from openmdao.main.log import enable_console
except ImportError:
    print 'No OpenMDAO distribution available.'
    if __name__ != '__main__':
        print 'You can unzip the egg to access the enclosed files.'
        print 'To get OpenMDAO, please visit openmdao.org'
    sys.exit(1)

def load():
    '''Create object(s) from state file.'''
    state_name = '%(name)s'
    if state_name.endswith('.pickle'):
        return Component.load(state_name, SAVE_CPICKLE%(pkg)s%(top)s)
    elif state_name.endswith('.yaml'):
        return Component.load(state_name, SAVE_LIBYAML%(pkg)s%(top)s)
    raise RuntimeError("State file '%%s' is not a pickle or yaml save file.",
                       state_name)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
""" % {'name':state_name, 'pkg':pkg_arg, 'top':top_arg})
    out.close()


def save(root, outstream, format=SAVE_CPICKLE, proto=-1, logger=None,
         fix_im=True):
    """
    Save the state of `root` and its children to `outstream`.
    The format can be supplied in case something other than cPickle is needed.
    Set `fix_im` False if no instancemethod objects need to be fixed.
    """
    if logger is None:
        logger = NullLogger()

    if isinstance(outstream, basestring):
        if format is SAVE_CPICKLE or format is SAVE_PICKLE:
            mode = 'wb'
        else:
            mode = 'w'
        try:
            outstream = open(outstream, mode)
        except IOError, exc:
            raise type(exc)("Can't save to '%s': %s" %
                            (outstream, exc.strerror))
    if fix_im:
        _fix_instancemethods(root)
    try:
        if format is SAVE_CPICKLE:
            cPickle.dump(root, outstream, proto) # -1 means use highest protocol
        elif format is SAVE_PICKLE:
            pickle.dump(root, outstream, proto)
        elif format is SAVE_YAML:
            yaml.dump(root, outstream)
        elif format is SAVE_LIBYAML:
            if _libyaml is False:
                logger.warning('libyaml not available, using yaml instead')
            yaml.dump(root, outstream, Dumper=Dumper)
        else:
            raise RuntimeError('cannot save object using format '+str(format))
    finally:
        if fix_im:
            _restore_instancemethods(root)
    

def load_from_eggfile(filename, entry_group, entry_name, install=True,
                      logger=None):
    """
    Extract files in egg to a subdirectory matching the saved object name,
    optionally install distributions the egg depends on, and then load object
    graph state by invoking the given entry point.  Returns the root object.
    """
    if logger is None:
        logger = NullLogger()
    logger.debug('Loading %s from %s in %s...',
                 entry_name, filename, os.getcwd())

    egg_dir, dist = _dist_from_eggfile(filename, install, logger)

    if not '.' in sys.path:
        sys.path.append('.')
    orig_dir = os.getcwd()
    os.chdir(egg_dir)
    try:
        return _load_from_distribution(dist, entry_group, entry_name, logger)
    finally:
        os.chdir(orig_dir)


def load_from_eggpkg(package, entry_group, entry_name, logger=None):
    """
    Load object graph state by invoking the given package entry point.
    Returns the root object.
    """
    if logger is None:
        logger = NullLogger()
    logger.debug('Loading %s from %s in %s...',
                 entry_name, package, os.getcwd())
    dist = pkg_resources.get_distribution(package)
    return _load_from_distribution(dist, entry_group, entry_name, logger)


def _load_from_distribution(dist, entry_group, entry_name, logger):
    """ Invoke entry point in distribution and return result. """
    logger.debug('    entry points:')
    maps = dist.get_entry_map()
    for group in maps.keys():
        logger.debug('        group %s:' % group)
        for entry_pt in maps[group].values():
            logger.debug('            %s', entry_pt)

    info = dist.get_entry_info(entry_group, entry_name)
    if info is None:
        raise RuntimeError("No '%s' '%s' entry point."
                           % (entry_group, entry_name))
    if info.module_name in sys.modules.keys():
        logger.debug("    removing existing '%s' in sys.modules",
                     info.module_name)
        del sys.modules[info.module_name]

    try:
        loader = dist.load_entry_point(entry_group, entry_name)
        return loader()
    except pkg_resources.DistributionNotFound, exc:
        logger.error('Distribution not found: %s', exc)
        visited = set()
        _check_requirements(dist, visited, logger)
        raise exc
    except pkg_resources.VersionConflict, exc:
        logger.error('Version conflict: %s', exc)
        visited = set()
        _check_requirements(dist, visited, logger)
        raise exc
    except Exception, exc:
        logger.exception('Loader exception:')
        raise exc


def _dist_from_eggfile(filename, install, logger):
    """ Create distribution by unpacking egg file. """
    if not os.path.exists(filename):
        raise ValueError("'%s' not found." % filename)

    if not zipfile.is_zipfile(filename):
        raise ValueError("'%s' is not an egg/zipfile." % filename)

    # Extract files.
    archive = zipfile.ZipFile(filename, 'r', allowZip64=True)
    name = archive.read('EGG-INFO/top_level.txt').split('\n')[0]
    logger.debug("    name '%s'", name)

    for info in archive.infolist():
        fname = info.filename
        if not fname.startswith(name) and not fname.startswith('EGG-INFO'):
            continue
        if fname.endswith('.pyc') or fname.endswith('.pyo'):
            continue  # Don't assume compiled OK for this platform.

        logger.debug("    extracting '%s' (%d bytes)...", fname, info.file_size)
        dirname = os.path.dirname(fname)
        if dirname == 'EGG-INFO':
            # Extract EGG-INFO as subdirectory.
            dirname = os.path.join(name, dirname)
            path = os.path.join(name, fname)
        else:
            path = fname
        if dirname and not os.path.exists(dirname):
            os.makedirs(dirname)
        # TODO: use 2.6 ability to extract to filename.
        out = open(path, 'w')
        out.write(archive.read(fname))
        out.close()

    # Create distribution from extracted files.
    location = os.getcwd()
    egg_info = os.path.join(location, name, 'EGG-INFO')
    provider = pkg_resources.PathMetadata(location, egg_info)
    dist = pkg_resources.Distribution.from_location(location,
                                                    os.path.basename(filename),
                                                    provider)

    logger.debug('    project_name: %s', dist.project_name)
    logger.debug('    version: %s', dist.version)
    logger.debug('    py_version: %s', dist.py_version)
    logger.debug('    platform: %s', dist.platform)
    logger.debug('    requires:')
    for req in dist.requires():
        logger.debug('        %s', req)

    if install:
        # Locate the installation (eggs) directory.
        install_dir = os.path.dirname(
                          os.path.dirname(
                              os.path.dirname(
                                  os.path.dirname(zc.buildout.__file__))))
        logger.debug('    installing in %s', install_dir)

        # Grab any distributions we depend on.
        try:
            zc.buildout.easy_install.install(
                [str(req) for req in dist.requires()], install_dir,
                index=EGG_SERVER_URL, always_unzip=True)
        except Exception, exc:
            raise RuntimeError("Install failed: '%s'" % exc)

    # If any module didn't have a distribution, check that we can import it.
    missing = os.path.join(name, '%s.missing' % name)
    if os.path.exists(missing):
        inp = open(missing, 'r')
        errors = 0
        for mod in inp.readlines():
            mod = mod.strip()
            logger.debug("    checking for 'missing' module: %s", mod)
            try:
                __import__(mod)
            except ImportError:
                logger.error("Can't import %s, which didn't have a known"
                             " distribution when the egg was written.", mod)
                errors += 1
        inp.close()
        if errors:
            plural = 's' if errors > 1 else ''
            raise RuntimeError("Couldn't import %d 'missing' module%s."
                               % (errors, plural))
    return (name, dist)


def _check_requirements(dist, visited, logger, level=1):
    """ Display requirements and note conflicts. """
    visited.add(dist)
    indent  = '    ' * level
    indent2 = '    ' * (level + 1)
    working_set = pkg_resources.WorkingSet()
    for req in dist.requires():
        logger.debug('%schecking %s', indent, req)
        dist = None
        try:
            dist = working_set.find(req)
        except pkg_resources.VersionConflict:
            dist = working_set.by_key[req.key]
            logger.debug('%sconflicts with %s %s', indent2,
                         dist.project_name, dist.version)
        else:
            if dist is None:
                logger.debug('%sno distribution found', indent2)
            else: 
                logger.debug('%s%s %s', indent2,
                             dist.project_name, dist.version)
                if not dist in visited:
                    _check_requirements(dist, visited, logger, level+1)


def load(instream, format=SAVE_CPICKLE, package=None, logger=None):
    """
    Load object(s) from the input stream (or filename).
    If `instream` is a string that is not an existing filename or
    absolute path, then it is searched for using pkg_resources.
    Returns the root object.
    """
    if logger is None:
        logger = NullLogger()

    if isinstance(instream, basestring):
        if not os.path.exists(instream) and not os.path.isabs(instream):
            # Try to locate via pkg_resources.
            if not package:
                dot = instream.rfind('.')
                if dot < 0:
                    raise ValueError("Bad state filename '%s'." % instream)
                package = instream[:dot]
            logger.debug("Looking for '%s' in package '%s'", instream, package)
            path = pkg_resources.resource_filename(package, instream)
            if not os.path.exists(path):
                raise IOError("State file '%s' not found." % instream)
            instream = path

            # The state file assumes a sys.path.
            package_dir = os.path.dirname(path)
            if not package_dir in sys.path:
                sys.path.append(package_dir)

        if format is SAVE_CPICKLE or format is SAVE_PICKLE:
            mode = 'rb'
        else:
            mode = 'r'
        instream = open(instream, mode)

    if format is SAVE_CPICKLE:
        top = cPickle.load(instream)
    elif format is SAVE_PICKLE:
        top = pickle.load(instream)
    elif format is SAVE_YAML:
        top = yaml.load(instream)
    elif format is SAVE_LIBYAML:
        if _libyaml is False:
            logger.warning('libyaml not available, using yaml instead')
        top = yaml.load(instream, Loader=Loader)
    else:
        raise RuntimeError('cannot load object using format %s' % format)

    # Restore instancemethods from IMHolder objects.
    _restore_instancemethods(top)
    return top

