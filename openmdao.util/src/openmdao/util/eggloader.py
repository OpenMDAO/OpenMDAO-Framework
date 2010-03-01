"""
Egg loading utilities.
"""

import pickle
import cPickle
import yaml
try:
    from yaml import CLoader as Loader
    _libyaml = True
except ImportError:
    from yaml import Loader
    _libyaml = False

import os.path
import pkg_resources
import sys
import zipfile

import zc.buildout.easy_install

from openmdao.util.log import NullLogger
from openmdao.util.eggobserver import EggObserver
from openmdao.util.eggsaver import SAVE_CPICKLE, SAVE_PICKLE, SAVE_YAML, \
                                   SAVE_LIBYAML, EGG_SERVER_URL

__all__ = ('load', 'load_from_eggfile', 'load_from_eggpkg',
           'check_requirements')


def load_from_eggfile(filename, entry_group, entry_name, install=False,
                      logger=None, observer=None):
    """
    Extract files in egg to a subdirectory matching the saved object name.
    Optionally installs distributions the egg depends on, and then loads object
    graph state by invoking the given entry point.  Returns the root object.
    """
    logger = logger or NullLogger()
    observer = EggObserver(observer, logger)
    logger.debug('Loading %s from %s in %s...',
                 entry_name, filename, os.getcwd())

    egg_dir, dist = _dist_from_eggfile(filename, install, logger, observer)

    if not '.' in sys.path:
        sys.path.append('.')
    orig_dir = os.getcwd()
    os.chdir(egg_dir)
    try:
        return _load_from_distribution(dist, entry_group, entry_name, None,
                                       logger, observer)
    finally:
        os.chdir(orig_dir)


def load_from_eggpkg(package, entry_group, entry_name, instance_name=None,
                     logger=None, observer=None):
    """
    Load object graph state by invoking the given package entry point.
    Returns the root object.
    """
    logger = logger or NullLogger()
    observer = EggObserver(observer, logger)
    logger.debug('Loading %s from %s in %s...',
                 entry_name, package, os.getcwd())
    try:
        dist = pkg_resources.get_distribution(package)
    except pkg_resources.DistributionNotFound, exc:
        logger.error('Distribution not found: %s', exc)
        raise exc
    return _load_from_distribution(dist, entry_group, entry_name, instance_name,
                                   logger, observer)


def _load_from_distribution(dist, entry_group, entry_name, instance_name,
                            logger, observer):
    """ Invoke entry point in distribution and return result. """
    logger.debug('    entry points:')
    maps = dist.get_entry_map()
    for group in sorted(maps.keys()):
        logger.debug('        group %s:' % group)
        for entry_pt in maps[group].values():
            logger.debug('            %s', entry_pt)

    info = dist.get_entry_info(entry_group, entry_name)
    if info is None:
        msg = "No '%s' '%s' entry point." % (entry_group, entry_name)
        logger.error(msg)
        raise RuntimeError(msg)

    if info.module_name in sys.modules.keys():
        logger.debug("    removing existing '%s' in sys.modules",
                     info.module_name)
        del sys.modules[info.module_name]

    try:
        loader = dist.load_entry_point(entry_group, entry_name)
        return loader(name=instance_name, observer=observer.observer)
    except pkg_resources.DistributionNotFound, exc:
        observer.exception('Distribution not found: %s' % exc)
        check_requirements(dist.requires(), logger=logger, indent_level=1)
        raise exc
    except pkg_resources.VersionConflict, exc:
        observer.exception('Version conflict: %s' % exc)
        check_requirements(dist.requires(), logger=logger, indent_level=1)
        raise exc
    except Exception, exc:
        observer.exception('Loader exception:')
        logger.exception('')
        raise exc


def _dist_from_eggfile(filename, install, logger, observer):
    """ Create distribution by unpacking egg file. """
    if not os.path.exists(filename):
        msg = "'%s' not found." % filename
        observer.exception(msg)
        raise ValueError(msg)

    if not zipfile.is_zipfile(filename):
        msg = "'%s' is not an egg/zipfile." % filename
        observer.exception(msg)
        raise ValueError(msg)

    # Extract files.
    archive = zipfile.ZipFile(filename, 'r', allowZip64=True)
    try:
        name = archive.read('EGG-INFO/top_level.txt').split('\n')[0]
        logger.debug("    name '%s'", name)

        if observer.observer is not None:
            # Collect totals.
            total_files = 0.
            total_bytes = 0.
            for info in archive.infolist():
                fname = info.filename
                if not fname.startswith(name) and not fname.startswith('EGG-INFO'):
                    continue
                if fname.endswith('.pyc') or fname.endswith('.pyo'):
                    continue  # Don't assume compiled OK for this platform.
                total_files += 1
                total_bytes += info.file_size
        else:
            total_files = 1.  # Avoid divide-by-zero.
            total_bytes = 1.

        files = 0.
        size = 0.
        for info in archive.infolist():
            fname = info.filename
            if not fname.startswith(name) and not fname.startswith('EGG-INFO'):
                continue
            if fname.endswith('.pyc') or fname.endswith('.pyo'):
                continue  # Don't assume compiled OK for this platform.

            observer.extract(fname, files/total_files, size/total_bytes)
            dirname = os.path.dirname(fname)
            if dirname == 'EGG-INFO':
                # Extract EGG-INFO as subdirectory.
                archive.extract(fname, name)
            else:
                archive.extract(fname)
            files += 1
            size += info.file_size

    finally:
        archive.close()

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
            msg = "Install failed: '%s'" % exc
            observer.exception(msg)
            raise RuntimeError(msg)

    # If any module didn't have a distribution, check that we can import it.
    if provider.has_metadata('openmdao_orphans.txt'):
        errors = 0
        orphan_names = []
        for mod in provider.get_metadata_lines('openmdao_orphans.txt'):
            mod = mod.strip()
            logger.debug("    checking for 'orphan' module: %s", mod)
            try:
                __import__(mod)
            except ImportError:
                logger.error("Can't import %s, which didn't have a known"
                             " distribution when the egg was written.", mod)
                orphan_names.append(mod)
                errors += 1
        if errors:
            plural = 's' if errors > 1 else ''
            msg = "Couldn't import %d 'orphan' module%s: %s." \
                  % (errors, plural, orphan_names)
            observer.exception(msg)
            raise RuntimeError(msg)

    return (name, dist)


def check_requirements(required, logger=None, indent_level=0):
    """
    Display requirements (if logger debug level enabled) and note conflicts.
    Returns a list of unavailable requirements.
    """
    def _recursive_check(required, logger, level, visited, working_set,
                         not_avail):
        indent  = '    ' * level
        indent2 = '    ' * (level + 1)
        for req in required:
            logger.debug('%schecking %s', indent, req)
            dist = None
            try:
                dist = working_set.find(req)
            except pkg_resources.VersionConflict:
                dist = working_set.by_key[req.key]
                logger.debug('%sconflicts with %s %s', indent2,
                             dist.project_name, dist.version)
                not_avail.append(req)
            else:
                if dist is None:
                    logger.debug('%sno distribution found', indent2)
                    not_avail.append(req)
                else: 
                    logger.debug('%s%s %s', indent2,
                                 dist.project_name, dist.version)
                    if not dist in visited:
                        visited.add(dist)
                        _recursive_check(dist.requires(), logger, level+1,
                                         visited, working_set, not_avail)

    logger = logger or NullLogger()
    not_avail = []
    _recursive_check(required, logger, indent_level, set(),
                     pkg_resources.WorkingSet(), not_avail)
    return not_avail


def load(instream, fmt=SAVE_CPICKLE, package=None, logger=None):
    """
    Load object(s) from an input stream (or filename).
    If `instream` is a string that is not an existing filename or
    absolute path, then it is searched for using :mod:`pkg_resources`.
    Returns the root object.
    """
    logger = logger or NullLogger()

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

        if fmt is SAVE_CPICKLE or fmt is SAVE_PICKLE:
            mode = 'rb'
        else:
            mode = 'rU'
        instream = open(instream, mode)

    if fmt is SAVE_CPICKLE:
        top = cPickle.load(instream)
    elif fmt is SAVE_PICKLE:
        top = pickle.load(instream)
    elif fmt is SAVE_YAML:
        top = yaml.load(instream)
    elif fmt is SAVE_LIBYAML:
        if _libyaml is False:
            logger.warning('libyaml not available, using yaml instead')
        top = yaml.load(instream, Loader=Loader)
    else:
        raise RuntimeError("Can't load object using format '%s'" % fmt)

    return top

