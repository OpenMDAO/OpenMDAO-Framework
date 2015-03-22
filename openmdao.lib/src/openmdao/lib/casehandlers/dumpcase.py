import cStringIO
import StringIO
import sys
import time

from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.main.exceptions import traceback_str

from openmdao.lib.casehandlers.util import driver_map


class DumpCaseRecorder(object):
    """Dumps cases in a "pretty" form to `out`, which may be a string or a
    file-like object (defaults to ``stdout``). If `out` is ``stdout`` or
    ``stderr``, then that standard stream is used. Otherwise, if `out` is a
    string, then a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    implements(ICaseRecorder)

    def __init__(self, out='stdout'):
        if isinstance(out, basestring):
            if out == 'stdout':
                out = sys.stdout
            elif out == 'stderr':
                out = sys.stderr
            else:
                out = open(out, 'w')
        self.out = out
        self._cfg_map = {}

    def startup(self):
        """ Nothing needed for a dumpcase."""
        pass

    def register(self, driver, inputs, outputs):
        """Register names for later record call from `driver`."""
        self._cfg_map[driver] = driver_map(driver, inputs, outputs)

    def record_constants(self, constants):
        """Record constant data."""
        if not self.out:  # if self.out is None, just do nothing
            return

        write = self.out.write
        write("Constants:\n")
        for path in sorted(constants.keys()):
            write("   %s: %s\n" % (path, constants[path]))

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """Dump the given run data in a "pretty" form."""
        if not self.out:  # if self.out is None, just do nothing
            return

        in_names, out_names = self._cfg_map[driver]
        ins = sorted(zip(in_names, inputs))
        outs = sorted(zip(out_names, outputs))

        write = self.out.write
        write("Case:\n")
        write("   uuid: %s\n" % case_uuid)
        write("   timestamp: %15f\n" % time.time())
        if parent_uuid:
            write("   parent_uuid: %s\n" % parent_uuid)

        if ins:
            write("   inputs:\n")
            for name, val in ins:
                write("      %s: %s\n" % (name, val))
        if outs:
            write("   outputs:\n")
            for name, val in outs:
                write("      %s: %s\n" % (name, val))
        if exc:
            write("   exc: %s\n" % exc)
            write("        %s\n" % traceback_str(exc))

    def close(self):
        """Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`."""
        if self.out not in (None, sys.stdout, sys.stderr):
            if not isinstance(self.out,
                              (StringIO.StringIO, cStringIO.OutputType)):
                # Closing a StringIO deletes its contents.
                self.out.close()
            self.out = None

    def get_iterator(self):
        """Doesn't really make sense to have a case iterator for dump files, so
        just return None.
        """
        return None
