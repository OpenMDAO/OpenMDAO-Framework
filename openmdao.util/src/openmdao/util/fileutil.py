"""
Misc. file utility routines
"""

import os
import stat
from os import makedirs
import sys
import shutil
import warnings
import itertools
from fnmatch import fnmatch
from os.path import islink, isdir, join
from os.path import normpath, dirname, exists, isfile, abspath

class DirContext(object):
    """Supports using the 'with' statement in place of try-finally for
    entering a directory, executing a block, then returning to the 
    original directory.
    """
    def __init__(self, destdir):
        self.destdir = destdir

    def __enter__(self):
        self.startdir = os.getcwd()
        # convert destdir to absolute at enter time instead of init time
        # so relative paths will be relative to the current context
        self.destdir = os.path.abspath(self.destdir)
        os.chdir(self.destdir)
        return self.destdir

    def __exit__(self, exc_type, exc_val, exc_tb):
        os.chdir(self.startdir)

def expand_path(path):
    return os.path.abspath(os.path.expandvars(os.path.expanduser(path)))

def cleanup(*fnames, **kwargs):
    """delete the given files or directories if they exists"""
    for fname in fnames:
        if os.path.isfile(fname):
            os.remove(fname)
        elif os.path.isdir(fname):
            shutil.rmtree(fname, **kwargs)

def find_in_dir_list(fname, dirlist, exts=('',)):
    """Search the given list of directories for the specified file.
    Return the absolute path of the file if found, or None otherwise.
    
    fname: str
        Base name of file.
        
    dirlist: list of str
        List of directory paths, relative or absolute.
        
    exts: tuple of str
        Tuple of extensions (including the '.') to apply to fname for loop, 
        e.g., ('.exe','.bat').
    """
    for path in dirlist:
        for ext in exts:
            fpath = join(path, fname)+ext
            if isfile(fpath):
                return abspath(fpath)
    return None
    
def find_in_path(fname, pathvar=None, sep=os.pathsep, exts=('',)):
    """Search for a given file in all of the directories given
    in the pathvar string. Return the absolute path to the file
    if found, None otherwise.
    
    fname: str
        Base name of file.
        
    pathvar: str
        String containing search paths. Defaults to $PATH
        
    sep: str
        Delimiter used to separate paths within pathvar.
        
    exts: tuple of str
        Tuple of extensions (including the '.') to apply to fname for loop, 
        e.g., ('.exe','.bat').
    """
    if pathvar is None:
        pathvar = os.environ['PATH']
        
    return find_in_dir_list(fname, pathvar.split(sep), exts)

def _file_gen(dname):
    """A generator returning all files under the given directory."""
    for path, dirlist, filelist in os.walk(dname):
        for name in filelist:
            yield join(path, name)
            
def _file_dir_gen(dname):
    """A generator returning all files and directories under 
    the given directory.
    """
    for path, dirlist, filelist in os.walk(dname):
        for name in filelist:
            yield join(path, name)
        for name in dirlist:
            yield join(path, name)

def find_files(start, match=None, exclude=None, nodirs=True):
    """Return filenames (using a generator).
    
    start: str or list of str
        Starting directory or list of directories.
        
    match: str or predicate funct
        Either a string containing a glob pattern to match
        or a predicate function that returns True on a match.
        
    exclude: str or predicate funct
        Either a string containing a glob pattern to exclude
        or a predicate function that returns True to exclude.
        
    nodirs: bool
        If False, return names of files and directories.
        
    Walks all subdirectories below each specified starting directory.
    """
    startdirs = [start] if isinstance(start, basestring) else start
    
    gen = _file_gen if nodirs else _file_dir_gen
    if match is None:
        matcher = bool
    elif isinstance(match, basestring):
        matcher = lambda name: fnmatch(name,match)
    else:
        matcher = match

    if isinstance(exclude, basestring):
        fmatch = lambda name: matcher(name) and not fnmatch(name, exclude)
    elif exclude is not None:
        fmatch = lambda name: matcher(name) and not exclude(name)
    else:
        fmatch = matcher

    iters = [itertools.ifilter(fmatch, gen(d)) for d in startdirs]
    if len(iters) > 1:
        return itertools.chain(*iters)
    else:
        return iters[0]


def find_up(name, path=None):
    """Search upward from the starting path (or the current directory)
    until the given file or directory is found. The given name is
    assumed to be a basename, not a path.  Returns the absolute path
    of the file or directory if found, None otherwise.
    
    name: str
        Base name of the file or directory being searched for
        
    path: str (optional)
        Starting directory.  If not supplied, current directory is used.
    """
    if not path:
        path = os.getcwd()
    if not exists(path):
        return None
    while path:
        if exists(join(path, name)):
            return abspath(join(path, name))
        else:
            pth = path
            path = dirname(path)
            if path == pth:
                return None
    return None


def get_module_path(fpath):
    """Given a module filename, return its full Python name including
    enclosing packages. (based on existence of ``__init__.py`` files)
    """
    pnames = [os.path.splitext(os.path.basename(fpath))[0]]
    path = os.path.dirname(os.path.abspath(fpath))
    while os.path.isfile(os.path.join(path, '__init__.py')):
            path, pname = os.path.split(path)
            pnames.append(pname)
    return '.'.join(pnames[::-1])

def find_module(name, path=None):
    """Return the pathname of the file corresponding to the given module
    name, or None if it can't be found.  If path is set, search in path
    for the file, otherwise search in sys.path
    """
    if path is None:
        path = sys.path
    nameparts = name.split('.')
    
    endings = [os.path.join(*nameparts)]
    endings.append(os.path.join(endings[0], '__init__.py'))
    endings[0] += '.py'
    
    for entry in path:
        for ending in endings:
            f = os.path.join(entry, ending)
            if os.path.isfile(f):
                return f
    return None
   
def get_ancestor_dir(path, num_levels=1):
    """Return the name of the directory that is 'num_levels' levels
    above the specified path.  If num_levels is larger than the number
    of members in the path, then the root directory name will be returned.
    """
    for i in range(num_levels):
        path = os.path.dirname(path)
    return path


def copy(src, dest):
    """Copy a file or directory."""
    if isfile(src):
        shutil.copy(src, dest)
    elif isdir(src):
        shutil.copytree(src, dest) 


def build_directory(dct, force=False, topdir='.'):
    """Create a directory structure based on the contents of a nested dict.
    The directory is created in the specified top directory, or in the current
    working directory if one isn't specified. If a file being created already
    exists, a warning will be issued and the file will not be changed if force
    is False. If force is True, the file will be overwritten.
    
    The structure of the dict is as follows: if the value at a key is a
    dict, then that key is used to create a directory. Otherwise, the key is
    used to create a file and the value stored at that key is written to the
    file. All keys must be relative names or a RuntimeError will be raised.
    """
    #TODO: if a value stored in the dict is a callable, then call it and store
    #      its return value as the contents of the file
    startdir = os.getcwd()
    topdir = os.path.abspath(topdir)
    try:
        for key, val in dct.items():
            os.chdir(topdir)
            if os.path.isabs(key):
                raise RuntimeError("build_directory: key (%s) is not a relative name" % key)
            if isinstance(val, dict): # it's a dict, so this is a directory
                if not os.path.exists(key):
                    os.makedirs(key)
                os.chdir(key)
                build_directory(val, force)
            else:  # assume a string value. Use that value to create a file
                if os.path.exists(key) and force is False:
                    warnings.warn("File '%s' already exists and will not be overwritten." 
                                  % key, Warning)
                else:
                    dname = os.path.dirname(key)
                    if dname and not os.path.isdir(dname):
                        os.makedirs(dname)
                    with open(key, 'w') as f:
                        f.write(val)
    finally:
        os.chdir(startdir)
