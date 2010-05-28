
import sys

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseRecorder

class DumpCaseRecorder(object):
    """"Dumps Cases to a file-like attribute called 'out'"""
    
    implements(ICaseRecorder)
    
    def __init__(self, out=sys.stdout):
        self.out = out

    def record(self, case):
        """Dump the given Case"""
        out = self.out
        out.write("ident: %s\n" % case.ident)
        out.write("retries: %s\n" % case.retries)
        out.write("max_retries: %s\n" % case.max_retries)
        if case.msg:
            out.write("msg: %s\n" % case.msg)
        out.write("inputs:\n")
        for name,index,value in case.inputs:
            out.write("    %s" % name)
            if index:
                for entry in index:
                    out.write("[%s]" % entry)
            out.write(" = %s\n" % value)
        out.write("outputs:\n")
        for name,index,value in case.outputs:
            out.write("    %s" % name)
            if index:
                for entry in index:
                    out.write("[%s]" % entry)
            out.write(" = %s\n" % value)
        out.write("\n\n")
            