"""
Routines to help out with obtaining debugging information
"""

import os
import sys
import re
import linecache
import StringIO

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

def print_fuct_call(funct, *args, **kwargs):
    def quote_if_str(obj):
        if isinstance(obj, basestring):
            return "'%s'" % obj
        return str(obj)
    
    s = StringIO.StringIO()
    s.write(funct.__name__)
    s.write('(')
    for i,arg in enumerate(args):
        if i>0: s.write(',')
        s.write(quote_if_str(arg))
    if len(args) > 0:
        s.write(', ')
    for j,tup in enumerate(kwargs.items()):
        if j>0: s.write(', ')
        s.write("%s=%s" % (tup[0], quote_if_str(tup[1])))
    s.write(')')
    return s.getvalue()
    
