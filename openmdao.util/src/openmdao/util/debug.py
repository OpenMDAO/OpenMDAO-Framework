"""
Routines to help out with obtaining debugging information
"""
import sys

def dumpit(obj, stream=sys.stdout, recurse=True):
    def _dumpit(obj, stream, recurse, indent, visited):
        visited.add(id(obj))
        try:
            dct = obj.__dict__
        except AttributeError:
            return
        else:
            space = ' '*indent
            itemlist = sorted(dct.items(), cmp=lambda x,y: cmp(x[0],y[0]))
            for key,value in itemlist:
                stream.writelines(['%s%s: %s' % (space,key,value), '\n'])
                if id(value) not in visited:
                    visited.add(id(value))
                    if recurse:
                        _dumpit(value, stream, recurse, indent+3, visited)

    _dumpit(obj, stream, recurse, 0, set())


