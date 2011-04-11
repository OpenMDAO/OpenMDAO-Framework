
import sys

from openmdao.lib.datatypes.api import implements

from openmdao.main.interfaces import ICaseRecorder

class DumpCaseRecorder(object):
    """Dumps cases in a "pretty" form to a file-like object called "out" (defaults to ``sys.stdout``).
    If out is None, cases will be ignored.
    """
    
    implements(ICaseRecorder)
    
    def __init__(self, out=sys.stdout):
        self.out = out

    def record(self, case):
        """Dump the given Case in a "pretty" form."""
        if self.out:  # if self.out is None, just do nothing
            out = self.out
            out.write("Case: %s" % case.ident)
            if case.parent_id:
                out.write(" (parent id %s)" % case.parent_id)
            out.write("\n   inputs:\n")
            inputs = dict(case.items(iotype='in'))
            for name,value in [(k,inputs[k]) for k in sorted(inputs.keys())]:
                out.write('      %s = %s\n' % (name, value))
            out.write("   outputs:\n")
            outputs = dict(case.items(iotype='out'))
            for name,value in [(k,outputs[k]) for k in sorted(outputs.keys())]:
                out.write('      %s = %s\n' % (name, value))
            out.write("   max_retries: %s, retries: %s\n" % (case.max_retries, 
                                                             case.retries))
            if case.msg:
                out.write('   msg: %s\n' % case.msg)
