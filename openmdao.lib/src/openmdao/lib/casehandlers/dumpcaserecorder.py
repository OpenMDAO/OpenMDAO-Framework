
import sys

from openmdao.main.interfaces import implements, ICaseRecorder

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
            self.out.write(str(case))
