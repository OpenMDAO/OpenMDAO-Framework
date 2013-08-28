"""
Egg loading utilities.
"""

import pickle
import cPickle

import os.path
import pkg_resources
import sys
import zipfile

from openmdao.util.log import NullLogger, LOG_DEBUG2
from openmdao.util.eggobserver import EggObserver
from openmdao.util.eggsaver import SAVE_CPICKLE, SAVE_PICKLE

__all__ = ('load', 'load_from_eggfile', 'load_from_eggpkg',
           'check_requirements')


def load_from_eggfile(filename, entry_group, entry_name, logger=None,
                      observer=None):
    """
    Extracts files in egg to a subdirectory matching the saved object name.
    Then loads object graph state by invoking the given entry point.
    Returns the root object.

    filename: string
        Name of egg file.

    entry_group: string
        Name of group.

    entry_name: string
        Name of entry point in group.

    logger: Logger
        Used for recording progress, etc.

    observer: callable
        Called via an :class:`EggObserver`.
    """
    logger = logger or NullLogger()
    observer = EggObserver(observer, logger)
    logger.debug('Loading %s from %s in %s...',
                 entry_name, filename, os.getcwd())

    egg_dir, dist = _dist_from_eggfile(filename, logger, observer)

    # Just being defensive, '.' is typically in the path.
    if not '.' in sys.path:  #pragma no cover
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

    package: string
        Name of package to load from.

    entry_group: string
        Name of group.

    entry_name: string
        Name of entry point in group.

    instance_name: string
        Name for instance loaded.

    logger: Logger
        Used for recording progress, etc.

    observer: callable
        Called via an :class:`EggObserver`.
    """
    logger = logger or NullLogger()
    observer = EggObserver(observer, logger)
    logger.debug('Loading %s from %s in %s...',
                 entry_name, package, os.getcwd())
    try:
        dist = pkg_resources.get_distribution(package)
    except pkg_resources.DistributionNotFound as exc:
        logger.error('Distribution not found: %s', exc)
        raise exc
    return _load_from_distribution(dist, entry_group, entry_name, instance_name,
                                   logger, observer)


def _load_from_distribution(dist, entry_group, entry_name, instance_name,
                            logger, observer):
    """ Invoke entry point in distribution and return result. """
    logger.log(LOG_DEBUG2, '    entry points:')
    maps = dist.get_entry_map()
    for group in sorted(maps.keys()):
        logger.log(LOG_DEBUG2, '        group %s:' % group)
        for entry_pt in maps[group].values():
            logger.log(LOG_DEBUG2, '            %s', entry_pt)

    info = dist.get_entry_info(entry_group, entry_name)
    if info is None:
        msg = "No '%s' '%s' entry point." % (entry_group, entry_name)
        logger.error(msg)
        raise RuntimeError(msg)

    if info.module_name in sys.modules:
        logger.log(LOG_DEBUG2, "    removing existing '%s' in sys.modules",
                     info.module_name)
        del sys.modules[info.module_name]

    try:
        loader = dist.load_entry_point(entry_group, entry_name)
        return loader(name=instance_name, observer=observer.observer)
    # Difficult to generate egg in test process that causes this.
    except pkg_resources.DistributionNotFound as exc:  #pragma no cover
        observer.exception('Distribution not found: %s' % exc)
        check_requirements(dist.requires(), logger=logger, indent_level=1)
        raise exc
    # Difficult to generate egg in test process that causes this.
    except pkg_resources.VersionConflict as exc:  #pragma no cover
        observer.exception('Version conflict: %s' % exc)
        check_requirements(dist.requires(), logger=logger, indent_level=1)
        raise exc
    # Difficult to generate egg in test process that causes this.
    except Exception as exc:  #pragma no cover
        observer.exception('Loader exception:')
        logger.exception('')
        raise exc


def _dist_from_eggfile(filename, logger, observer):
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
        logger.log(LOG_DEBUG2, "    name '%s'", name)

        if observer.observer is not None:
            # Collect totals.
            total_files = 0.
            total_bytes = 0.
            for info in archive.infolist():
                fname = info.filename
                # Just being defensive.
                if not fname.startswith(name) and \
                   not fname.startswith('EGG-INFO'):  #pragma no cover
                    continue
                total_files += 1
                total_bytes += info.file_size
        else:
            total_files = 1.  # Avoid divide-by-zero.
            total_bytes = 1.

        files = 0.
        size = 0.
        for info in archive.infolist():
            fname = info.filename
            # Just being defensive.
            if not fname.startswith(name) and \
               not fname.startswith('EGG-INFO'):  #pragma no cover
                continue

            observer.extract(fname, files/total_files, size/total_bytes)
            dirname = os.path.dirname(fname)
            if dirname == 'EGG-INFO':
                # Extract EGG-INFO as subdirectory.
                archive.extract(fname, name)
            else:
                archive.extract(fname)
                if sys.platform != 'win32':
                    # Set permissions, extract() doesn't.
                    rwx = (info.external_attr >> 16) & 0777
                    if rwx:
                        os.chmod(fname, rwx)  # Only if something valid.
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

    logger.log(LOG_DEBUG2, '    project_name: %s', dist.project_name)
    logger.log(LOG_DEBUG2, '    version: %s', dist.version)
    logger.log(LOG_DEBUG2, '    py_version: %s', dist.py_version)
    logger.log(LOG_DEBUG2, '    platform: %s', dist.platform)
    logger.log(LOG_DEBUG2, '    requires:')
    for req in dist.requires():
        logger.log(LOG_DEBUG2, '        %s', req)

    # If any module didn't have a distribution, check that we can import it.
    if provider.has_metadata('openmdao_orphans.txt'):
        errors = 0
        orphan_names = []
        for mod in provider.get_metadata_lines('openmdao_orphans.txt'):
            mod = mod.strip()
            logger.log(LOG_DEBUG2, "    checking for 'orphan' module: %s", mod)
            try:
                __import__(mod)
            # Difficult to generate a distribution that can't be reloaded.
            except ImportError:  #pragma no cover
                logger.error("Can't import %s, which didn't have a known"
                             " distribution when the egg was written.", mod)
                orphan_names.append(mod)
                errors += 1
        # Difficult to generate a distribution that can't be reloaded.
        if errors:  #pragma no cover
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

    required: list
        List of package requirements.

    logger: Logger
        Used for recording progress, etc.

    indent_level: int
        Used to improve readability of log messages.
    """
    def _recursive_check(required, logger, level, visited, working_set,
                         not_avail):
        indent  = '    ' * level
        indent2 = '    ' * (level + 1)
        for req in required:
            logger.log(LOG_DEBUG2, '%schecking %s', indent, req)
            dist = None
            try:
                dist = working_set.find(req)
            # Difficult to generate a distribution that can't be reloaded.
            except pkg_resources.VersionConflict:  #pragma no cover
                dist = working_set.by_key[req.key]
                logger.debug('%sconflicts with %s %s', indent2,
                             dist.project_name, dist.version)
                not_avail.append(req)
            else:
                # Difficult to generate a distribution that can't be reloaded.
                if dist is None:  #pragma no cover
                    logger.debug('%sno distribution found', indent2)
                    not_avail.append(req)
                else: 
                    logger.log(LOG_DEBUG2, '%s%s %s', indent2,
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

    instream: file or string
        Stream or filename to load from.

    fmt: int
        Format of state data.

    package: string
        Name of package to use.

    logger: Logger
        Used for recording progress, etc.
    """
    logger = logger or NullLogger()

    new_stream = False
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
        new_stream = True

    try:
        if fmt is SAVE_CPICKLE:
            top = cPickle.load(instream)
        elif fmt is SAVE_PICKLE:
            top = pickle.load(instream)
        else:
            raise RuntimeError("Can't load object using format '%s'" % fmt)
    finally:
        if new_stream:
            instream.close()

    return top

