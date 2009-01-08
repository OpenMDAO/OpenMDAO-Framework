import os
import os.path
from fnmatch import fnmatchcase

def includingWalk(dir, includeList):
    """walk a directory tree, using a generator, including 
    specified files/dirs based on a list of wildcards
    """
    def included(name, includeList):
       #True if the given name is matched by the include list
       for m in includeList:
          if fnmatchcase(name, m): 
              return True
       return False
       
    if included(dir, includeList):
       yield dir

    for f in os.listdir(dir):
        fullpath = os.path.join(dir,f)
        if os.path.isdir(fullpath) and not os.path.islink(f):
            # recurse into subdir
            for x in includingWalk(fullpath, includeList):  
                yield x
        elif os.path.isfile(fullpath) and included(fullpath, includeList):
            yield fullpath


#for ff in includingWalk('.', ["*.dia"]):
#   print ff


