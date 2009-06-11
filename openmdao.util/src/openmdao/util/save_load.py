"""
Save/load utilities.

Note that Pickle format can't save references to functions that aren't defined
at the top level of a module, and there doesn't appear to be a viable
workaround.  Normally pickle won't handle instance methods either, but there is
code in place to work around that.

When saving to an egg, the module named __main__ changes when reloading. This
requires finding the real module name and munging references to __main__.
References to types defined in module __main__ can't be saved.  Also,
references to old-style class types can't be restored correctly.

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
import platform
import shutil
import site
import subprocess
import sys
import tempfile
import zc.buildout.easy_install
import zipfile

__all__ = ('save', 'save_to_egg', 'load', 'load_from_egg',
           'SAVE_YAML', 'SAVE_LIBYAML', 'SAVE_PICKLE', 'SAVE_CPICKLE',
           'EGG_SERVER_URL')

__version__ = '0.1'

# Save formats.
SAVE_YAML    = 1
SAVE_LIBYAML = 2
SAVE_PICKLE  = 3
SAVE_CPICKLE = 4

EGG_SERVER_URL = 'http://torpedo.grc.nasa.gov:31001'


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

    def debug(self, msg):
        pass

    def error(self, msg):
        pass

    def exception(self, msg):
        pass

    def info(self, msg):
        pass

    def warning(self, msg):
        pass


def save_to_egg(root, name, version=None, src_dir=None, src_files=None,
                dst_dir=None, format=SAVE_CPICKLE, proto=-1, tmp_dir=None,
                logger=None):
    """
    Save state and other files to an egg.

    - `root` is the root of the object graph to be saved.
    - `name` is the name of the package.
    - `version` defaults to a timestamp.
    - `src_dir` is the root of all (relative) `src_files`.
    - `dst_dir` is the directory to write the egg in.
    - `tmp_dir` is the directory to use for temporary files.

    The resulting egg can be unpacked on UNIX via 'sh egg-file'.
    Returns the egg's filename.
    """
    if logger is None:
        logger = NullLogger()

    orig_dir = os.getcwd()
    if src_dir and not os.path.isabs(src_dir):
        src_dir = os.path.abspath(src_dir)

    if version is None:
        now = datetime.datetime.now()  # Could consider using utcnow().
        version = '%d.%02d.%02d.%02d.%02d' % \
                  (now.year, now.month, now.day, now.hour, now.minute)

    if dst_dir is None:
        dst_dir = orig_dir
    if not os.access(dst_dir, os.W_OK):
        raise IOError("Can't save to '%s', no write permission" % dst_dir)

    py_version = platform.python_version_tuple()
    py_version = '%s.%s' % (py_version[0], py_version[1])
    egg_name = '%s-%s-py%s.egg' % (name, version, py_version)
    logger.debug('Saving to %s in %s...', egg_name, orig_dir)

    buildout = 'buildout.cfg'
    buildout_path = os.path.join(name, buildout)
    buildout_orig = buildout_path+'-orig'

    tmp_dir = None

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
        try:
            # Determine distributions and local modules required.
            required_distributions, local_modules = \
                _get_distributions(objs, logger)

            # Move to scratch area.
            tmp_dir = tempfile.mkdtemp(prefix='Egg_', dir=tmp_dir)
            os.chdir(tmp_dir)
            if src_dir:
                if sys.platform == 'win32':
                    # Copy original directory to object name.
                    shutil.copytree(src_dir, name)
                else:
                    # Just link original directory to object name.
                    os.symlink(src_dir, name)
            else:
                os.mkdir(name)

            # If orig_dir isn't src_dir, copy local modules from orig_dir.
            if orig_dir != src_dir:
                for path in local_modules:
                    path = orig_dir+os.sep+path
#                    logger.debug('    copy %s -> %s', path, name)
                    shutil.copy(path, name)

            # Save state of object hierarchy.
            if format is SAVE_CPICKLE or format is SAVE_PICKLE:
                state_name = name+'.pickle'
            elif format is SAVE_LIBYAML or format is SAVE_YAML:
                state_name = name+'.yaml'
            else:
                raise RuntimeError("Unknown format '%s'." % format)
            state_path = os.path.join(name, state_name)
            try:
                save(root, state_path, format, proto, logger, fix_im=False)
            except Exception, exc:
                if os.path.exists(state_path):
                    os.remove(state_path)
                raise type(exc)("Can't save to '%s': %s" % (state_path, exc))

            # Add state file to set of files to save.
            if src_files is None:
                src_files = set()
            src_files.add(state_name)

            # Create buildout.cfg
            if os.path.exists(buildout_path):
                os.rename(buildout_path, buildout_orig)
            _create_buildout(name, EGG_SERVER_URL, required_distributions,
                             buildout_path)
            src_files.add(buildout)

            # If needed, make an empty __init__.py
            init_path = os.path.join(name, '__init__.py')
            if not os.path.exists(init_path):
                remove_init = True
                out = open(init_path, 'w')
                out.close()
            else:
                remove_init = False

            # Create loader script.
            loader = '%s_loader' % name
            loader_path = os.path.join(name, loader+'.py')
            _write_loader_script(loader_path, state_name)

            # Save everything to an egg via setuptools.
            doc = root.__doc__
            if doc is None:
                doc = ''
            _write_egg_via_setuptools(name, doc, version, loader, src_files,
                                      required_distributions, dst_dir, logger)
            os.remove(state_path)
            os.remove(loader_path)
            if remove_init:
                os.remove(init_path)

        finally:
            if os.path.exists(buildout_orig):
                os.rename(buildout_orig, buildout_path)
            elif os.path.exists(buildout_path):
                os.remove(buildout_path)
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
        for key, val in obj.__dict__.items():
            if val is None or id(val) in visited:
                continue
            visited.append(id(val))
            if inspect.ismethod(val):
                obj.__dict__[key] = IMHolder(val)
            elif isinstance(val, dict):
                for key2, obj2 in val.items():
                    if obj2 is None or id(obj2) in visited:
                        continue
                    visited.append(id(obj2))
                    if inspect.ismethod(obj2):
                        val[key2] = IMHolder(obj2)
                    elif hasattr(obj2, '__dict__'):
                        _fix_im_recurse(obj2, visited)
            elif isinstance(val, list) or isinstance(val, set):
                for i, obj2 in enumerate(val):
                    if obj2 is None or id(obj2) in visited:
                        continue
                    visited.append(id(obj2))
                    if inspect.ismethod(obj2):
                        val[i] = IMHolder(obj2)
                    elif hasattr(obj2, '__dict__'):
                        _fix_im_recurse(obj2, visited)
            elif hasattr(val, '__dict__'):
                _fix_im_recurse(val, visited)

    if previsited is None:
        visited = []
    elif isinstance(previsited, list):
        visited = [id(obj) for obj in previsited]
    else:
        visited = [id(previsited)]
    _fix_im_recurse(root, visited)


def _restore_instancemethods(root, previsited=None):
    """ Restore references to instancemethods. """

    def _restore_im_recurse(obj, visited):
        """ Restore recursively. """
        for key, val in obj.__dict__.items():
            if val is None or id(val) in visited:
                continue
            visited.append(id(val))
            if isinstance(val, IMHolder):
                obj.__dict__[key] = val.method()
            elif isinstance(val, dict):
                for key2, obj2 in val.items():
                    if obj2 is None or id(obj2) in visited:
                        continue
                    visited.append(id(obj2))
                    if isinstance(obj2, IMHolder):
                        val[key2] = obj2.method()
                    elif hasattr(obj2, '__dict__'):
                        _restore_im_recurse(obj2, visited)
            elif isinstance(val, list) or isinstance(val, set):
                for i, obj2 in enumerate(val):
                    if obj2 is None or id(obj2) in visited:
                        continue
                    visited.append(id(obj2))
                    if isinstance(obj2, IMHolder):
                        val[i] = obj2.method()
                    elif hasattr(obj2, '__dict__'):
                        _restore_im_recurse(obj2, visited)
            elif hasattr(val, '__dict__'):
                _restore_im_recurse(val, visited)

    if previsited is None:
        visited = []
    elif isinstance(previsited, list):
        visited = [id(obj) for obj in previsited]
    else:
        visited = [id(previsited)]
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
                if isinstance(obj, dict) or \
                   isinstance(obj, list) or \
                   isinstance(obj, set)  or \
                   isinstance(obj, tuple):
                    state = obj
                else:
                    return  # Some non-container primitive.
        except Exception, exc:
            logger.error("During save_to_egg, _get_objects error %s: %s",
                         type(obj), exc)
            return

        if isinstance(state, dict):
            state = state.values()

        for obj in state:
            if id(obj) in visited:
                continue
            visited.append(id(obj))
            objs.append(obj)
            if not inspect.isclass(obj):
                _recurse_get_objects(obj, objs, visited, logger)

    objs = [root]
    visited = [id(root)]
    _recurse_get_objects(root, objs, visited, logger)
    return objs


def _check_objects(objs, logger):
    """ Check that each object can be pickled. """
    errors = 0
    for obj in objs:
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
        raise RuntimeError("Can't save, %d objects cannot be pickled." % errors)


def _fix_objects(objs):
    """ Fixup objects, classes, & sys.modules for __main__ imports. """
    fixup_objects = []
    fixup_classes = {}
    fixup_modules = set()

    for obj in objs:
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
            if mod == '__main__' and \
               (classname not in fixup_classes.keys()):
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
                obj.__module__ = mod
            else:
                try:
                    obj.__class__ = fixup_classes[classname][1]
                except KeyError:
                    raise RuntimeError("Can't fix %r, classname %s, module %s"
                                       % (obj, classname, mod))
                obj.__module__ = obj.__class__.__module__
            fixup_objects.append(obj)

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

    for obj in fixup_objects:
        obj.__module__ = '__main__'
        if not inspect.isclass(obj):
            classname = obj.__class__.__name__
            if classname != 'function':
                obj.__class__ = fixup_classes[classname][0]

    for classname in fixup_classes.keys():
        new = fixup_classes[classname][1]
        del new

    for mod in fixup_modules:
        del sys.modules[mod]


def _get_distributions(objs, logger):
    """ Return (distributions, local_modules) used by objs. """
#    working_set = pkg_resources.WorkingSet()
#    for dist in working_set:
#        logger.debug('    %s %s', dist.project_name, dist.version)
#        logger.debug('        %s', dist.location)
    distributions = set()
    local_modules = set()
    modules = ['__builtin__']
    prefixes = []
    site_lib = os.path.dirname(site.__file__)
    site_pkg = site_lib+os.sep+'site-packages'

    # For each object found...
    for obj in objs:
        try:
            name = obj.__module__
        except AttributeError:
            continue
#        logger.debug('    obj module %s', name)
        if name in modules:
#            logger.debug('        already known')
            continue
        modules.append(name)

        # Skip modules in distributions we already know about.
        path = sys.modules[name].__file__
        if path.startswith(site_lib) and not path.startswith(site_pkg):
#            logger.debug("    skipping(1) '%s'", path)
            continue
        found = False
        for prefix in prefixes:
            if path.startswith(prefix):
                found = True
#                logger.debug("    skipping(2) '%s'", path)
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
            logger.debug("    analyzing '%s'...", path)
            finder = modulefinder.ModuleFinder()
            try:
                finder.run_script(path)
            except Exception:
                logger.exception("ModuleFinder for '%s'" % path)
            else:
                _process_found_modules(finder, modules, distributions,
                                       prefixes, local_modules, logger)

    distributions = sorted(distributions,
                           key=lambda dist: dist.project_name)
    logger.debug('    required distributions:')
    for dist in distributions:
        logger.debug('        %s %s', dist.project_name, dist.version)
    return (distributions, local_modules)


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


def _process_found_modules(finder, modules, distributions, prefixes,
                           local_modules, logger):
    """ Use ModuleFinder data to update distributions and local_modules. """
    working_set = pkg_resources.WorkingSet()
    site_lib = os.path.dirname(site.__file__)
    site_pkg = site_lib+os.sep+'site-packages'
    py_version = 'python%d.%d' % (sys.version_info[0], sys.version_info[1])
    cwd = os.getcwd()
    not_found = set()

    for name, module in sorted(finder.modules.items(),
                               key=lambda item: item[0]):
#        logger.debug('    found %s', name)
        if name in modules:
#            logger.debug('        already known')
            continue
        if name != '__main__':
            modules.append(name)
        try:
            path = module.__file__
        except AttributeError:
#            logger.debug('        no __file__')
            continue
        if not path:
#            logger.debug('        null __file__')
            continue

        # Skip modules in distributions we already know about.
        if path.startswith(site_lib) and not path.startswith(site_pkg):
#            logger.debug("        skipping(1) '%s'", path)
            continue
        found = False
        for prefix in prefixes:
            if path.startswith(prefix):
                found = True
#                logger.debug("    skipping(2) '%s'", path)
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
#                logger.debug('        adding %s %s',
#                             dist.project_name, dist.version)
                break
        else:
            if not os.path.isabs(path):
                # No distribution expected.
                if os.path.dirname(path) == '.':
                    # May need to be copied later.
                    local_modules.add(path)
            elif path.startswith(cwd):
                # No distribution expected.
                if os.path.dirname(path) == cwd:
                    # May need to be copied later.
                    local_modules.add(path)
            else:
                dirpath = os.path.dirname(path)
                if dirpath not in not_found:
                    if not dirpath.endswith('site-packages'):
                        not_found.add(dirpath)
                        path = dirpath
                    logger.warning('        no distribution found for %s', path)


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


def _write_loader_script(path, state_name):
    """ Write script used for loading object(s). """
    out = open(path, 'w')
    out.write("""\
import logging
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
    state_name = '%s'
    if state_name.endswith('.pickle'):
        return Component.load(state_name, SAVE_CPICKLE)
    elif state_name.endswith('.yaml'):
        return Component.load(state_name, SAVE_LIBYAML)
    raise RuntimeError("State file '%%s' is not a pickle or yaml save file.",
                       state_name)

def eggsecutable():
    '''Unpack egg.'''
    install = os.environ.get('OPENMDAO_INSTALL', '1')
    if install:
        install = int(install)
    debug = os.environ.get('OPENMDAO_INSTALL_DEBUG', '1')
    if debug:
        debug = int(debug)
    if debug:
        enable_console()
        logging.getLogger().setLevel(logging.DEBUG)
    try:
        Component.load_from_egg(sys.path[0], install=install)
    except Exception, exc:
        print str(exc)
        sys.exit(1)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
""" % state_name)
    out.close()


def _write_egg_via_setuptools(name, doc, version, loader, src_files,
                              distributions, dst_dir, logger):
    """ Save everything to an egg via setuptools. """
    out = open('setup.py', 'w')

    out.write('import setuptools\n')

    out.write('\npackage_files = [\n')
    for filename in sorted(src_files):
        path = os.path.join(name, filename)
        if not os.path.exists(path):
            raise ValueError("Can't save, '%s' does not exist" % path)
        out.write("    '%s',\n" % filename)
    out.write(']\n')

    out.write('\nrequirements = [\n')
    for dist in distributions:
        out.write("    '%s == %s',\n" % (dist.project_name, dist.version))
    out.write(']\n')

    out.write("""
entry_points = {
    'openmdao.top' : [
        'top = %(loader)s:load',
    ],
    'openmdao.components' : [
        '%(name)s = %(loader)s:load',
    ],
    'setuptools.installation' : [
        'eggsecutable = %(name)s.%(loader)s:eggsecutable',
    ],
}

setuptools.setup(
    name='%(name)s',
    description='''%(doc)s''',
    version='%(version)s',
    packages=setuptools.find_packages(),
    package_data={'%(name)s' : package_files},
    zip_safe=False,
    install_requires=requirements,
    entry_points=entry_points,
)
""" % {'name':name, 'loader':loader, 'doc':doc.strip(), 'version':version})

    out.close()

    # Use environment since 'python' might not recognize '-u'.
    env = os.environ
    env['PYTHONUNBUFFERED'] = '1'
    proc = subprocess.Popen(['python', 'setup.py', 'bdist_egg',
                             '-d', dst_dir], env=env,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    output = []
    while proc.returncode is None:
        line = proc.stdout.readline()
        if line:
            line = line.rstrip()
            logger.debug('    '+line)
            output.append(line)
        proc.poll()
    line = proc.stdout.readline()
    while line:
        line = line.rstrip()
        logger.debug('    '+line)
        output.append(line)
        line = proc.stdout.readline()

    if proc.returncode != 0:
        for line in output:
            logger.error('    '+line)
        logger.error('save_to_egg failed due to setup.py error %d:',
                     proc.returncode)
        raise RuntimeError('setup.py failed, check log for info.')


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
    

def load_from_egg(filename, install=True, logger=None):
    """
    Extract files in egg to a subdirectory matching the saved object name,
    optionally install distributions the egg depends on, and then load object
    graph state.  Returns the top object.
    """
    if logger is None:
        logger = NullLogger()
    logger.debug('Loading from %s in %s...', filename, os.getcwd())
    if not os.path.exists(filename):
        raise ValueError("'%s' not found." % filename)

    # Check for a distribution.
    distributions = \
        [dist for dist in pkg_resources.find_distributions(filename, only=True)]
    if not distributions:
        raise RuntimeError("No distributions found in '%s'." % filename)

    dist = distributions[0]
    logger.debug('    project_name: %s', dist.project_name)
    logger.debug('    version: %s', dist.version)
    logger.debug('    py_version: %s', dist.py_version)
    logger.debug('    platform: %s', dist.platform)
    logger.debug('    requires:')
    for req in dist.requires():
        logger.debug('        %s', req)
    logger.debug('    entry points:')
    maps = dist.get_entry_map()
    for group in maps.keys():
        logger.debug('        group %s:' % group)
        for name in maps[group]:
            logger.debug('            %s', name)

    # Extract files.
    archive = zipfile.ZipFile(filename, 'r')
    name = archive.read('EGG-INFO/top_level.txt').split('\n')[0]
    logger.debug("    name '%s'", name)

    for info in archive.infolist():
        if not info.filename.startswith(name):
            continue  # EGG-INFO
        if info.filename.endswith('.pyc') or \
           info.filename.endswith('.pyo'):
            continue  # Don't assume compiled OK for this platform.

        logger.debug("    extracting '%s' (%d bytes)...",
                     info.filename, info.file_size)
        dirname = os.path.dirname(info.filename)
        if dirname and not os.path.exists(dirname):
            os.makedirs(dirname)
        # TODO: use 2.6 ability to extract to filename.
        out = open(info.filename, 'w')
        out.write(archive.read(info.filename))
        out.close()

    if install:
        # Locate the installation (eggs) directory.
# TODO: fix this hack!
        install_dir = \
            os.path.dirname(
                os.path.dirname(
                    os.path.dirname(
                        os.path.dirname(zc.buildout.__file__))))
        logger.debug('    installing in %s', install_dir)

        # Grab any distributions we depend on.
        try:
            zc.buildout.easy_install.install(
                [str(req) for req in dist.requires()], install_dir,
                index=EGG_SERVER_URL,
                always_unzip=True
                )
        except Exception, exc:
            raise RuntimeError("Install failed: '%s'" % exc)

    # Invoke the top object's loader.
    info = dist.get_entry_info('openmdao.top', 'top')
    if info is None:
        raise RuntimeError("No openmdao.top 'top' entry point.")
    if info.module_name in sys.modules.keys():
        logger.debug("    removing existing '%s' in sys.modules",
                     info.module_name)
        del sys.modules[info.module_name]
    if not '.' in sys.path:
        sys.path.append('.')
    orig_dir = os.getcwd()
    os.chdir(name)
    try:
        loader = dist.load_entry_point('openmdao.top', 'top')
        return loader()
    except pkg_resources.DistributionNotFound, exc:
        logger.error('Distribution not found: %s', exc)
        visited = []
        _check_requirements(dist, visited, logger)
        raise exc
    except pkg_resources.VersionConflict, exc:
        logger.error('Version conflict: %s', exc)
        visited = []
        _check_requirements(dist, visited, logger)
        raise exc
    except Exception, exc:
        logger.exception('Loader exception:')
        raise exc
    finally:
        os.chdir(orig_dir)


def _check_requirements(dist, visited, logger, level=1):
    """ Display requirements and note conflicts. """
    visited.append(dist)
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


def load(instream, format=SAVE_CPICKLE, logger=None):
    """ Load object(s) from the input stream. """
    if logger is None:
        logger = NullLogger()

    if isinstance(instream, basestring):
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

