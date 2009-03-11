"""
Misc. file utility routines
"""
import os,sys
import linecache
import fnmatch
from os.path import islink, isdir


def glob_walk(directory, include_list):
    """walk a directory tree, using a generator, including 
    specified files/directories based on a list of wildcards.
    """
    if isinstance(include_list, basestring):
        include_list = [include_list]

    def included(name, include_list):
       #True if the given name is matched by the include list
       for m in include_list:
          if fnmatch.fnmatchcase(name, m): 
              return True
       return False
       
    if included(directory, include_list):
       yield directory

    for f in os.listdir(directory):
        fullpath = os.path.join(directory,f)
        if os.path.isdir(fullpath) and not os.path.islink(f):
            # recurse into subdirectory
            for x in glob_walk(fullpath, include_list):  
                yield x
        elif os.path.isfile(fullpath) and included(fullpath, include_list):
            yield fullpath



def excluding_walk(ddir,excludeList):
    """walk a directory tree, using a generator, excluding specified files/dirs"""
    def included(name,excludeList):
       #False if the given name is part of the exlude list
       for m in excludeList:
          if fnmatch.fnmatchcase(name, m): return False
       return True
       
    if included(os.path.basename(ddir),excludeList):
       yield dir
       for f in os.listdir(ddir):
           if included(f,excludeList):
              fullpath = os.path.join(ddir,f)
              if isdir(fullpath) and not islink(fullpath):
                  for x in excluding_walk(fullpath,excludeList): 
                      yield x
              else:
                  yield fullpath
            


def dirtreegen(ddir):
    """walk a directory tree, returning file/directory 
    names relative to the starting dir. Links are not followed."""
    yield ddir
    for f in os.listdir(ddir):
       fullpath = os.path.join(ddir,f)
       if os.path.isdir(fullpath) and not os.path.islink(fullpath):
           for x in dirtreegen(fullpath):  # recurse into subdir
               yield x
       else:
           yield fullpath


def find_files(pat, startdir):
    """Return a list of files (using a generator) that match
    the given glob pattern.
    """
    for path, dirlist, filelist in os.walk(startdir):
        for name in fnmatch.filter(filelist, pat):
            yield os.path.join(path.name)


def traceit(frame, event, arg):
   """a function useful for tracing python execution. wherever you want the 
   tracing to start, insert a call to sys.settrace(traceit)"""
   if event == "line":
       lineno = frame.f_lineno
       filename = frame.f_globals["__file__"]
       if (filename.endswith(".pyc") or
           filename.endswith(".pyo")):
           filename = filename[:-1]
       name = frame.f_globals["__name__"]
       line = linecache.getline(filename, lineno)
       print "%s:%s: %s" % (name, lineno, line.rstrip())
   return traceit


