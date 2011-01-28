"""
Routines to help out with obtaining debugging information
"""

import os
import sys
import re
import linecache
from inspect import getmembers, isbuiltin
from types import InstanceType, MethodType
import weakref

def traceit(frame, event, arg):
    """A function useful for tracing Python execution. Wherever you want the 
    tracing to start, insert a call to sys.settrace(traceit)."""
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


address_rgx = re.compile(' at 0x\w+')

def dumpit(obj, stream=sys.stdout, recurse=True, ignore_address=True):
    """Try to dump out the guts of an object, and optionally its children."""
    def _dumpit(obj, stream, recurse, indent, visited, ignore_address):
        if id(obj) not in visited:
            visited.add(id(obj))
            try:
                dct = obj.__dict__
            except AttributeError:
                return
            else:
                space = ' '*indent
                for key,value in sorted(dct.items(), cmp=lambda x,y: cmp(x[0],y[0])):
                    if isinstance(value, dict):
                        value = '{ ' + '\n'.join(['%s%s: %s' % (' '*(indent+3),k,v) 
                                         for k,v in sorted(value.items(), 
                                                           cmp=lambda x,y: cmp(x[0],y[0]))]) + ' }'
                    if ignore_address:
                        val = address_rgx.sub('', repr(value))
                    else:
                        val = value
                    stream.writelines(['%s%s: %s' % (space,key,val), '\n'])
                    if recurse:
                        _dumpit(value, stream, recurse, indent+3, visited, ignore_address)

    _dumpit(obj, stream, recurse, 0, set(), ignore_address)

    
class _objdiff(object):
    def __init__(self, o1names, o2names, diffdict):
        self.o1names = o1names
        self.o2names = o2names
        self.diffdict = diffdict

def dict_diff(d1, d2):
    """Recursive dict comparison.  Returns a dict of differences having two possible
    forms.  For differences between object instances, the difference value will be
    a tuple of the form: (d1only, d2only, common) where d1only and d2only are dicts
    containing values from d1 or d2 respectively that don't exist in the other. For
    differences between built-in types, the difference value will be a tuple of the 
    form (idx, v1, v2) where idx is the tuple index of a value that only exists in
    either d1 or d2. If idx is None that indicates that both values exist but are
    different. v1 will be the value from d1 if it exists and v2 will be the value
    from d2 if it exists.
    """
    obj1_set = set(d1.keys())
    obj2_set = set(d2.keys())
    
    common = obj1_set.intersection(obj2_set)
    obj1_only = obj1_set.difference(common)
    obj2_only = obj2_set.difference(common)
    
    del obj1_set
    del obj2_set

    diffs = {}
    for name in common:
        v1 = d1[name]
        v2 = d2[name]
        if not hasattr(v1, '__dict__') or not hasattr(v2, '__dict__'):
            if isinstance(v1, dict) and isinstance(v2, dict):
                diff = dict_diff(v1, v2)
            else:
                diff = obj_diff(v1, v2)
            if diff:
                diffs[name] = diff
        elif v1 != v2:
            diffs[name] = (None, v1, v2)
        
    for name in obj1_only:
        diffs[name] = (1, d1[name], None)
        
    for name in obj2_only:
        diffs[name] = (2, None, d2[name])
        
    return diffs


def obj_diff(obj1, obj2, recurse=True):
    """Find the difference between two objects of the same type and return it
    as a dict of difference tuples. Methods are not investigated beyond detecting whether
    they are present or absent, so two objects with methods of the same name
    having different bodies will not be detected.  
    """
    # first, see if objs have their own way of comparing
    if obj1 == obj2:
        return {}
    
    if hasattr(obj1, '__dict__') and hasattr(obj2, '__dict__'):
        return dict_diff(obj1.__dict__, obj2.__dict__)
    else:
        return (None, obj1, obj2)
