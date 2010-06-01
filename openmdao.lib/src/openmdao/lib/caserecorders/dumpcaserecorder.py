
import sys

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseRecorder

def _pprint_var(name, index, value):
    s = [name]
    if index:
        for entry in index:
            s.append('[%s]' % entry)
    s.append(' = %s' % value)
    return ''.join(s)

class DumpCaseRecorder(object):
    """"Dumps Cases to a file-like object called 'out' (defaults to sys.stdout)"""
    
    implements(ICaseRecorder)
    
    def __init__(self, out=sys.stdout):
        self.out = out

    def record(self, case):
        """Dump the given Case in a 'pretty' form."""
        if self.out:  # if self.out is None, just do nothing
            out = self.out
            out.write("Case: %s\n" % case.ident)
            out.write("   inputs:\n")
            for name,index,value in case.inputs:
                out.write('      %s\n' % _pprint_var(name, index, value))
            out.write("   outputs:\n")
            for name,index,value in case.outputs:
                out.write('      %s\n' % _pprint_var(name, index, value))
            out.write("   max_retries: %s, retries: %s\n" % (case.max_retries, 
                                                             case.retries))
            if case.msg:
                out.write('   msg: %s' % case.msg)
