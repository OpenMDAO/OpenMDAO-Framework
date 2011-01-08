"""
Routines for doing plugin related things
"""

import os
from os import makedirs
import sys
import shutil
import fnmatch
from os.path import islink, isdir, join
from os.path import normpath, dirname, exists, isfile, abspath
from token import NAME, OP
from tokenize import generate_tokens

from openmdao.util.fileutil import find_files

class Foo(object):
    pass

class Bar(Foo):
    pass

def _parse_class(iterator):
    """after encountering 'class' during a parse, this function
    parses the rest of the inheritance specification up to the ':'
    """
    tok, s, _, _, _ = iterator.next()
    classinfo = [s]   # class name
    args = []
    pname = []
    while True:
        tok, s, _, _, _ = iterator.next()
        if tok == NAME:
            pname.append(s)
        elif s == ',' or s == ')':
            args.append('.'.join(pname))
            pname = []
        elif s == ':':
            if pname:
                args.append('.'.join(pname))
            break
    return classinfo+args
    
                
def get_class_decls(pyfile):
    """Gather class declaration info from the given python file
    and return it as a list of lists, where each list entry is:
    [class_name, baseclass1_name, baseclass2_name, ... ]
    """
    classes = []
    with open(pyfile, 'r') as f:
        inclass = False
        iterator = generate_tokens(f.readline)
        while True:
            try:
                tok, s, _, _, _ = iterator.next()
                if tok == NAME and s == 'class':
                    classes.append(_parse_class(iterator))
            except StopIteration:
                break
    return classes
            
def get_package_name(fpath):
    """Given a filename, return the name of the python package that
    contains it (based on existence of __init__.py files)
    """
    pnames = []
    path = os.path.dirname(os.path.abspath(fpath))
    while os.path.isfile(os.path.join(path, '__init__.py')):
            path, pname = os.path.split(path)
            pnames.append(pname)
    return '.'.join(pnames[::-1])
   
def get_inheritance_tree(class_list):
    idict = { 'object': [] }
    for entry in class_list:
        idict[entry[0]] = entry[1:]

                
if __name__ == '__main__':
    #get_class_decls(__file__)
    #print 'pname = ',get_package_name(__file__)
    for pyfile in find_files("*.py", sys.argv[1]):
        classes = get_class_decls(pyfile)
        if len(classes) > 0:
            print 'classes = ',classes
