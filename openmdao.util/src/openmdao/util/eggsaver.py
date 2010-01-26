"""
Egg save utilities.

Note that :mod:`pickle` can't save references to functions that aren't defined
at the top level of a module, and there doesn't appear to be a viable
workaround.  Normally :mod:`pickle` won't handle instance methods either,
but there is code in place to work around that.

When saving to an egg, the module named :mod:`__main__` changes when reloading.
This requires finding the real module name and munging references to
:mod:`__main__`.  References to old-style class types can't be restored
correctly.

Also note that YAML format doesn't handle more than one layer of back-pointers, 
so it's only suitable for very flat object networks.
"""

import pickle
import cPickle
import yaml
try:
    from yaml import CDumper as Dumper
    _libyaml = True
except ImportError:
    from yaml import Dumper
    _libyaml = False

import copy_reg
import datetime
import glob
import inspect
import modulefinder
import os.path
import pkg_resources
import shutil
import site
import sys
import tempfile
import types

import zc.buildout.easy_install

from openmdao.util.log import NullLogger
from openmdao.util import eggobserver, eggwriter

__all__ = ('save', 'save_to_egg',
           'SAVE_YAML', 'SAVE_LIBYAML', 'SAVE_PICKLE', 'SAVE_CPICKLE',
           'EGG_SERVER_URL')

# Save formats.
SAVE_YAML    = 1
SAVE_LIBYAML = 2
SAVE_PICKLE  = 3
SAVE_CPICKLE = 4

EGG_SERVER_URL = 'http://torpedo.grc.nasa.gov:31001'

_SITE_LIB = os.path.dirname(site.__file__)
_SITE_PKG = os.path.join(_SITE_LIB, 'site-packages')
if sys.platform == 'win32':
    _SITE_LIB = _SITE_LIB.lower()
    _SITE_PKG = _SITE_PKG.lower()

# Code from the net.  More flexible, but doesn't handle __main__ as well.
#
#def _pickle_method(method):
#    """ Pickles an instancemethod object. """
#    func_name = method.im_func.__name__
#    obj = method.im_self
#    cls = method.im_class
#    return _unpickle_method, (func_name, obj, cls)
#
#def _unpickle_method(func_name, obj, cls):
#    """ Unpickles an instancemethod object. """
#    for cls in cls.mro():
#        try:
#            func = cls.__dict__[func_name]
#        except KeyError:
#            pass
#        else:
#            break
#    return func.__get__(obj, cls)

def _pickle_method(method):
    """ Pickles an instancemethod object. """
    name = method.__name__
    im_self = method.im_self
    if im_self is not None:
        im_class = None  # Avoid possible __main__ issues.
    else:
        # TODO: handle __main__ for im_class.__module__.
        im_class = method.im_class
        if im_class.__module__ == '__main__':
            raise RuntimeError('_pickle_method: %r with module __main__ (%s)'
                               % (method, im_self))
    return _unpickle_method, (name, im_self, im_class)

def _unpickle_method(name, im_self, im_class):
    """ Unpickles an instancemethod object. """
    if im_self is not None:
        return getattr(im_self, name)
    else:
        return getattr(im_class, name)

# Register instancemethod handling.
copy_reg.pickle(types.MethodType, _pickle_method, _unpickle_method)


def save_to_egg(entry_pts, version=None, py_dir=None, src_dir=None,
                src_files=None, dst_dir=None, fmt=SAVE_CPICKLE, proto=-1,
                logger=None, use_setuptools=False, observer=None):
    """
    Save state and other files to an egg. Analyzes the objects saved for
    distribution dependencies.  Modules not found in any distribution are
    recorded in an 'egg-info/openmdao_orphans.txt' file.  Also creates and
    saves loader scripts for each entry point.

    - `entry_pts` is a list of (obj, obj_name, obj_group) tuples. \
      The first of these specifies the root object and package name.
    - `version` defaults to a timestamp of the form 'YYYY.MM.DD.HH.mm'.
    - `py_dir` is the (root) directory for local Python files. \
      It defaults to the current directory.
    - `src_dir` is the root of all (relative) `src_files`.
    - `dst_dir` is the directory to write the egg in.
    - `fmt` and `proto` are passed to save().
    - If 'use_setuptools` is True, then :func:`eggwriter.write_via_setuptools` \
      is called rather than :func:`eggwriter.write`.
    - `observer` will be called via an :class:`EggObserver` intermediary.

    Returns (egg_filename, required_distributions, orphan_modules).
    """
    root, name, group = entry_pts[0]
    logger = logger or NullLogger()
    observer = eggobserver.EggObserver(observer, logger)

    orig_dir = os.getcwd()

    if not py_dir:
        py_dir = orig_dir
    else:
        py_dir = os.path.realpath(py_dir)
    if sys.platform == 'win32':
        py_dir = py_dir.lower()

    if src_dir:
        src_dir = os.path.realpath(src_dir)
        if sys.platform == 'win32':
            src_dir = src_dir.lower()

    src_files = src_files or set()

    if not version:
        now = datetime.datetime.now()  # Could consider using utcnow().
        version = '%d.%02d.%02d.%02d.%02d' % \
                  (now.year, now.month, now.day, now.hour, now.minute)

    dst_dir = dst_dir or orig_dir
    if not os.access(dst_dir, os.W_OK):
        msg = "Can't save to '%s', no write permission" % dst_dir
        observer.exception(msg)
        raise IOError(msg)

    egg_name = eggwriter.egg_filename(name, version)
    logger.debug('Saving to %s in %s...', egg_name, orig_dir)

    # Get a list of all objects we'll be saving.
    objs = _get_objects(root, logger)

    # Check that each object can be pickled.
    _check_objects(objs, logger, observer)

    # Fixup objects, classes, & sys.modules for __main__ imports.
    fixup = _fix_objects(objs, observer)

    # Verify that the fixups are retained.
    _verify_objects(root, logger, observer)

    tmp_dir = None
    try:
        # Determine distributions and local modules required.
        required_distributions, local_modules, orphan_modules = \
            _get_distributions(objs, py_dir, logger, observer)

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

            # For each entry point...
            entry_info = []
            for obj, obj_name, obj_group in entry_pts:
                clean_name = obj_name
                if clean_name.startswith(name+'.'):
                    clean_name = clean_name[len(name)+1:]
                clean_name = clean_name.replace('.', '_')

                # Save state of object hierarchy.
                state_name, state_path = \
                    _write_state_file(name, obj, clean_name, fmt, proto,
                                      logger, observer)
                src_files.add(state_name)
                cleanup_files.append(state_path)

                # Create loader script.
                loader = '%s_loader' % clean_name
                loader_path = os.path.join(name, loader+'.py')
                cleanup_files.append(loader_path)
                _write_loader_script(loader_path, state_name, name, obj is root)

                entry_info.append((obj_group, obj_name, loader))

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
            doc = root.__doc__ or ''
            entry_map = _create_entry_map(entry_info)
            orphans = [mod for mod, path in orphan_modules]
            if use_setuptools:
                eggwriter.write_via_setuptools(name, version, doc,
                                               entry_map, src_files,
                                               required_distributions,
                                               orphans, dst_dir, logger,
                                               observer.observer)
            else:
                eggwriter.write(name, version, doc, entry_map,
                                src_files, required_distributions,
                                orphans, dst_dir, logger, observer.observer)
        finally:
            for path in cleanup_files:
                if os.path.exists(path):
                    os.remove(path)
            if os.path.exists(buildout_path):
                os.remove(buildout_path)
            if os.path.exists(buildout_orig):
                os.rename(buildout_orig, buildout_path)

    finally:
        os.chdir(orig_dir)
        if tmp_dir:
            shutil.rmtree(tmp_dir)
        _restore_objects(fixup)

    return (egg_name, required_distributions, orphan_modules)


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
                if isinstance(obj, (dict, list, set, tuple)):
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


def _check_objects(objs, logger, observer):
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
        msg = "Can't save, %d object%s cannot be pickled." % (errors, plural)
        observer.exception(msg)
        raise RuntimeError(msg)


def _verify_objects(root, logger, observer):
    """ Verify no references to __main__ exist. """
    objs = _get_objects(root, logger)
    for obj, container, index in objs:
        if obj.__class__.__name__ == 'instancemethod':
            continue  # Handled by _pickle_method() above.
        try:
            mod = obj.__module__
        except AttributeError:
            continue

        if mod == '__main__':
            msg = "Can't save, unable to patch __main__ module reference in" \
                  " obj %r, container %r index %s" % (obj, container, index)
            observer.exception(msg)
            raise RuntimeError(msg)


def _fix_objects(objs, observer):
    """ Fixup objects, classes, & sys.modules for __main__ imports. """
    fixup_objects = []
    fixup_classes = {}
    fixup_modules = set()

    for obj, container, index in objs:
        try:
            if obj.__class__.__name__ == 'instancemethod':
                continue  # Handled by _pickle_method() above.
            try:
                mod = obj.__module__
            except AttributeError:
                continue  # No module entry to fix.
            if inspect.isclass(obj):
                cls = obj
            else:
                cls = obj.__class__
            if mod is None:
                # Needed after switching to Traits for some reason.
                mod = cls.__module__
            if mod != '__main__':
                continue

            classname = cls.__name__
            if classname in ('function', 'type'):
                msg = "Can't save: reference to %s defined in main module %r" \
                      % (classname, obj)
                observer.exception(msg)
                raise RuntimeError(msg)
            mod = cls.__module__
            if mod == '__main__' and (classname not in fixup_classes.keys()):
                mod, module = _find_in_main(classname)
                if mod:
                    new = getattr(module, classname)
                    fixup_classes[classname] = (cls, new)
                    fixup_modules.add(mod)
                else:
                    msg = "Can't find module for '%s'" % classname
                    observer.exception(msg)
                    raise RuntimeError(msg)

            if inspect.isclass(obj):
                if isinstance(container, tuple):
                    msg = "Can't save: reference to class %s defined in main" \
                          " module is contained in a tuple." % classname
                    observer.exception(msg)
                    raise RuntimeError(msg)
                else:
                    container[index] = fixup_classes[classname][1]
            else:
                try:
                    obj.__class__ = fixup_classes[classname][1]
                except KeyError:
                    msg = "Can't fix %r, classname %s, module %s" \
                          % (obj, classname, mod)
                    observer.exception(msg)
                    raise RuntimeError(msg)
                obj.__module__ = obj.__class__.__module__

            fixup_objects.append((obj, container, index))
        except Exception:
            _restore_objects((fixup_objects, fixup_classes, fixup_modules))
            raise

    return (fixup_objects, fixup_classes, fixup_modules)


def _find_in_main(classname):
    """ Try to get 'real' module for __main__ class. """
    filename = sys.modules['__main__'].__file__
    if filename.endswith('.py'):
        if not '.' in sys.path:
            sys.path.append('.')
        mod = os.path.basename(filename)[:-3]
        try:
            module = __import__(mod, fromlist=[classname])
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


def _get_distributions(objs, py_dir, logger, observer):
    """ Return (distributions, local_modules, orphans) used by objs. """
    distributions = set()
    local_modules = set()
    orphans = set()
    modules = set(['__builtin__'])
    prefixes = set()

    if _get_distributions.excludes is None:
        # Exclude Python standard library from ModuleFinder analysis.
        _get_distributions.excludes = _get_standard_modules()

    for obj, container, index in objs:
        try:
            name = obj.__module__
        except AttributeError:
            continue
        if name is None or name in modules:
            continue
        modules.add(name)

        # Skip modules in distributions we already know about.
        try:
            path = os.path.realpath(sys.modules[name].__file__)
        except AttributeError:
            logger.debug('    module %s has no __file__', name)
            continue
        if sys.platform == 'win32':
            path = path.lower()
        if path.startswith(_SITE_LIB) and not path.startswith(_SITE_PKG):
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
            if path in _get_distributions.saved.keys():
                logger.debug("    reusing analysis of '%s'", path)
                finder_items = _get_distributions.saved[path]
            else:
                observer.analyze(path)
                finder = modulefinder.ModuleFinder(
                                          excludes=_get_distributions.excludes)
                try:
                    finder.run_script(path)
                except Exception:
                    logger.exception("ModuleFinder for '%s'" % path)
                else:
                    finder_items = finder.modules.items()
                    _get_distributions.saved[path] = finder_items

            if finder_items:
                _process_found_modules(py_dir, finder_items, modules,
                                       distributions, prefixes, local_modules,
                                       orphans, logger)

    distributions = sorted(distributions, key=lambda dist: dist.project_name)
    logger.debug('    required distributions:')
    for dist in distributions:
        logger.debug('        %s %s', dist.project_name, dist.version)
    return (distributions, local_modules, orphans)

_get_distributions.excludes = None  # Modules to exclude from analysis.
_get_distributions.saved = {}       # Saved results, keyed by module path.


def _get_standard_modules():
    """ Return list of module names in the Python standard library. """
    excl_dirs = []
    if sys.platform == 'win32':
        prefx = os.path.join(sys.prefix, 'Lib')
        excl_dirs = [prefx, os.path.join(sys.prefix, 'DLLs')]
        exts = ['*.py', '*.py?', '*.dll']
    else:
        prefx = os.path.join(sys.prefix, 'lib', 'python'+sys.version[0:3])
        excl_dirs = [prefx, os.path.join(prefx, 'lib-dynload'),
                     #prefx+'.zip',
                     os.path.join(prefx, 'plat-'+sys.platform),
                     os.path.join(prefx, 'site-packages')]
        exts = ['*.py', '*.py?', '*.so']
    excludes = set()
    for ext in exts:
        for edir in excl_dirs:
            pattern = os.path.join(edir, ext)
            excludes.update([os.path.basename(p)[:-(len(ext)-1)]
                             for p in glob.glob(pattern)])
          
    excludes.update(sys.builtin_module_names)
    return list(excludes)


def _process_egg(path, distributions, prefixes, logger):
    """ Update distributions and prefixes based on egg data. """
    logger.debug("    processing '%s'", path)
    dist = pkg_resources.Distribution.from_filename(path)
    distributions.add(dist)
    prefixes.add(path)

    for req in dist.requires():
        logger.debug("    requires '%s'", req)
        dep = pkg_resources.get_distribution(req, logger)
        distributions.add(dep)
        loc = dep.location
        if loc.endswith('.egg') and loc not in prefixes:
            prefixes.add(loc)


def _process_found_modules(py_dir, finder_items, modules, distributions,
                           prefixes, local_modules, orphans, logger):
    """ Use ModuleFinder data to update distributions and local_modules. """
    working_set = pkg_resources.WorkingSet()
    py_version = 'python%s' % sys.version[:3]
    not_found = set()

    for name, module in sorted(finder_items, key=lambda item: item[0]):
        if name in modules:
            continue
        if name != '__main__':
            modules.add(name)
        try:
            path = module.__file__
        except AttributeError:
            continue
        if not path:
            continue

        if sys.platform == 'win32':
            path = path.lower()
        dirpath = os.path.realpath(os.path.dirname(path))
        if dirpath.startswith(py_dir):
            # May need to be copied later.
            local_modules.add(path)
            continue

        # Skip modules in distributions we already know about.
        if path.startswith(_SITE_LIB) and not path.startswith(_SITE_PKG):
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
                loc = os.path.join(loc, dist.project_name)
            if path.startswith(loc):
                distributions.add(dist)
                if loc.endswith('.egg'):
                    prefixes.add(loc)
                break
        else:
            if dirpath not in not_found:
                if not dirpath.endswith('site-packages'):
                    not_found.add(dirpath)
                    path = dirpath
                # Verify name is valid. ModuleFinder can report a module
                # that was never successfully imported.
                try:
                    __import__(name)
                except ImportError:
                    logger.debug('Skipping %s, not importable.' % name)
                else:
                    logger.warning('No distribution found for %s.', name)
                    orphans.add((name, path))


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
        out.write("    %s\n" % dist.as_requirement())
    out.close()


def _write_state_file(dst_dir, root, name, fmt, proto, logger, observer):
    """ Write state of `root` and its children. Returns (filename, path). """
    if fmt is SAVE_CPICKLE or fmt is SAVE_PICKLE:
        state_name = name+'.pickle'
    elif fmt is SAVE_LIBYAML or fmt is SAVE_YAML:
        state_name = name+'.yaml'
    else:
        msg = "Unknown format '%s'." % fmt
        observer.exception(msg)
        raise RuntimeError(msg)

    state_path = os.path.join(dst_dir, state_name)
    try:
        save(root, state_path, fmt, proto, logger, fix_im=False)
    except Exception, exc:
        msg = "Can't save to '%s': %s" % (state_path, exc)
        observer.exception(msg)
        raise type(exc)(msg)

    return (state_name, state_path)


def _write_loader_script(path, state_name, package, top):
    """ Write script used for loading object(s). """
    if state_name.endswith('.pickle'):
        fmt = 'SAVE_CPICKLE'
    else:
        fmt = 'SAVE_LIBYAML'

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
    from openmdao.main.api import Component, %(fmt)s
except ImportError:
    print 'No OpenMDAO distribution available.'
    if __name__ != '__main__':
        print 'You can unzip the egg to access the enclosed files.'
        print 'To get OpenMDAO, please visit openmdao.org'
    sys.exit(1)

def load(**kwargs):
    '''Create object(s) from state file.'''
    return Component.load('%(name)s',
                          %(fmt)s%(pkg)s%(top)s, **kwargs)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
""" % {'name':state_name, 'fmt':fmt, 'pkg':pkg_arg, 'top':top_arg})
    out.close()


def _create_entry_map(entry_pts):
    """ Create entry point map from (group, name, loader) tuples. """
    pkg_name = entry_pts[0][1]
    pkg_loader = entry_pts[0][2]
    ldattr = ['load']
    entry_map = {}

    entry_group = {}
    entry_group['top'] = pkg_resources.EntryPoint('top', pkg_loader, ldattr)
    entry_map['openmdao.top'] = entry_group

    groups = set()
    for group, name, loader in entry_pts:
        groups.add(group)

    for grp in sorted(groups):
        entry_group = {}
        for group, name, loader in entry_pts:
            if group == grp:
                entry_group[name] = \
                    pkg_resources.EntryPoint(name, pkg_name+'.'+loader, ldattr)
        if entry_group:
            entry_map[grp] = entry_group

    return entry_map


def save(root, outstream, fmt=SAVE_CPICKLE, proto=-1, logger=None, fix_im=True):
    """
    Save the state of `root` and its children to an output stream (or filename).
    If `outstream` is a string, then it is used as a filename.
    The format can be supplied in case something other than :mod:`cPickle`
    is needed.  For the :mod:`pickle` formats, a `proto` of -1 means use the
    highest protocol.  Set `fix_im` False if no instance method objects need
    to be fixed.
    """
    logger = logger or NullLogger()

    if isinstance(outstream, basestring):
        if (fmt is SAVE_CPICKLE or fmt is SAVE_PICKLE) and proto != 0:
            mode = 'wb'
        else:
            mode = 'w'
        try:
            outstream = open(outstream, mode)
        except IOError, exc:
            raise type(exc)("Can't save to '%s': %s" %
                            (outstream, exc.strerror))
    if fmt is SAVE_CPICKLE:
        cPickle.dump(root, outstream, proto)
    elif fmt is SAVE_PICKLE:
        pickle.dump(root, outstream, proto)
    elif fmt is SAVE_YAML:
        yaml.dump(root, outstream)
    elif fmt is SAVE_LIBYAML:
        if _libyaml is False:
            logger.warning('libyaml not available, using yaml instead')
        yaml.dump(root, outstream, Dumper=Dumper)
    else:
        raise RuntimeError("Can't save object using format '%s'" % fmt)

