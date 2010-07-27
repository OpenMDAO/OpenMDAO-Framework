"""
Misc. file utility routines
"""


import os
from os import makedirs
import sys
import shutil
import linecache
import fnmatch
from os.path import islink, isdir
from os.path import normpath,dirname,exists,abspath
from subprocess import Popen,PIPE,STDOUT


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
            yield os.path.join(path, name)


def rm(path):
    """Delete a file or directory."""
    if os.path.isdir(path):
        shutil.rmtree(path)
    else:
        os.remove(path)
        

def copy(src, dest):
    """Copy a file or directory."""
    if os.path.isfile(src):
        shutil.copy(src, dest)
    elif os.path.isdir(src):
        shutil.copytree(src, dest) 
    

def find_bzr(path=None):
    """ Return bzr root directory path or None. """
    if not path:
        path = os.getcwd()
    if not os.path.exists(path):
        return None
    while path:
        if os.path.exists(os.path.join(path, '.bzr')):
            return path
        else:
            pth = path
            path = os.path.dirname(path)
            if path == pth:
                return None
    return None
