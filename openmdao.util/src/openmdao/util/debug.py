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


def obj_diff(obj1, obj2, recurse=True):
    """Find the difference between two objects of the same type and return it
    as a dict of dicts. Methods are not investigated beyond detecting whether
    they are present or absent, so two objects with methods of the same name
    having different bodies will not be detected.  Trying to diff two objects
    of different types will raise a TypeError.
    """
    def _filter(name, val):
        """Return True if value should be filtered"""
        if type(val) is MethodType or isbuiltin(val):
            return True
        if type(val).__name__ in ['method-wrapper','weakref']:
            return True
        return False
        
    if type(obj1) != type(obj2):
        raise TypeError('obj_diff: the two objects are not of the same type')
    
    ret = {}
    # first, see if objs have their own way of comparing
    try:
        if obj1 == obj2:
            return ret
    except:
        pass
    
    obj1_attrs = dict([(k,v) for k,v in getmembers(obj1)])
    obj2_attrs = dict([(k,v) for k,v in getmembers(obj2)])
    
    obj1_set = set(obj1_attrs.keys())
    obj2_set = set(obj2_attrs.keys())
    
    common = obj1_set.intersection(obj2_set)
    obj1_only = obj1_set.difference(common)
    obj2_only = obj2_set.difference(common)
    
    del obj1_set
    del obj2_set
    
    if obj1_only:
        ret['obj1'] = dict([(k,v) for k,v in obj1_attrs.items() if k in obj1_only and not _filter(k,v)])
    if obj2_only:
        ret['obj2'] = dict([(k,v) for k,v in obj2_attrs.items() if k in obj2_only and not _filter(k,v)])
    
    retcom = {}
    
    common.remove('__dict__')
    
    for name in common:
        o1 = obj1_attrs[name]
        o2 = obj2_attrs[name]
        if type(o1) == type(o2) and _filter(name, o1):
            continue
        elif type(o1) != type(o2):
            retcom[name] = (o1, o2)
        elif type(o1) is InstanceType and o1 != o2:
            diffdict = obj_diff(o1, o2)
            if diffdict:
                retcom[name] = diffdict
        elif o1 != o2:
            retcom[name] = (o1, o2)
    
    if retcom:
        ret['common'] = retcom
    
    return ret

