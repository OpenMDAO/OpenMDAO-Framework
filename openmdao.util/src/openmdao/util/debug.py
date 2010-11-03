"""
Routines to help out with obtaining debugging information
"""
import sys
import re

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


