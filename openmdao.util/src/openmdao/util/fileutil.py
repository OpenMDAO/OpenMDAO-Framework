"""
Misc. file utility routines
"""

import os
from os import makedirs
import sys
import shutil
import fnmatch
from os.path import islink, isdir, join
from os.path import normpath, dirname, exists, isfile, abspath

def find_in_dir_list(fname, dirlist, exts=('',)):
    """Search the given list of directories for the specified file.
    Return the absolute path of the file if found, or None otherwise.
    
    fname: str
        Base name of file.
        
    dirlist: list of str
        List of directory paths, relative or absolute.
        
    exts: tuple of str
        Tuple of extensions (including the '.') to apply to fname for, e.g., .exe.
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
        Tuple of extensions (including the '.') to apply to fname for, e.g., .exe.
    """
    if pathvar is None:
        pathvar = os.environ['PATH']
        
    return find_in_dir_list(fname, pathvar.split(sep), exts)


def makepath(path):
    """ Creates missing directories for the given path and returns a normalized absolute 
    version of the path.

    - If the given path already exists in the filesystem,
      the filesystem is not modified.

    - Otherwise makepath creates directories along the given path
      using the dirname() of the path. You may append
      a '/' to the path if you want it to be a directory path.

    from holger@trillke.net 2002/03/18
    """

    dpath = normpath(dirname(path))
    if not exists(dpath): makedirs(dpath)
    return normpath(abspath(path))


def find_files(pat, startdir):
    """Return a list of files (using a generator) that match
    the given glob pattern. Walks an entire directory structure.
    """
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist, pat):
            yield join(path, name)

def find_files_and_dirs(pat, startdir):
    """Return a list of files and directories (using a generator) that match
    the given glob pattern. Walks an entire directory structure.
    """
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist+dirlist, pat):
            yield join(path, name)

def find_up(name, path=None):
    """Search upward from the starting path (or the current directory)
    until the given file or directory is found. The given name is
    assumed to be a basename, not a path.  Returns the absolute path
    of the file or directory if found, None otherwise.
    
    name: str
        Base name of the file or directory being searched for
        
    path: str, optional
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

def rm(path):
    """Delete a file or directory."""
    if isdir(path):
        shutil.rmtree(path)
    else:
        os.remove(path)
        

def copy(src, dest):
    """Copy a file or directory."""
    if isfile(src):
        shutil.copy(src, dest)
    elif isdir(src):
        shutil.copytree(src, dest) 
    

def find_bzr(path=None):
    """ Return bzr root directory path or None. """
    if not path:
        path = os.getcwd()
    if not exists(path):
        return None
    while path:
        if exists(join(path, '.bzr')):
            return path
        else:
            pth = path
            path = dirname(path)
            if path == pth:
                return None
    return None
