"""
Misc. file utility routines
"""
import os,sys
from fnmatch import fnmatchcase

def glob_walk(directory, include_list):
    """walk a directory tree, using a generator, including 
    specified files/directories based on a list of wildcards.
    """
    if isinstance(include_list, basestring):
        include_list = [include_list]

    def included(name, include_list):
       #True if the given name is matched by the include list
       for m in include_list:
          if fnmatchcase(name, m): 
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



