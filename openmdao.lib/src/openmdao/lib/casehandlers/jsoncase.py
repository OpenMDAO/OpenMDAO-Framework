import cStringIO
import json
import StringIO
import sys
import time

from openmdao.main.interfaces import implements, ICaseRecorder


class JSONCaseRecorder(object):
    """Dumps cases in JSON form to `out`, which may be a string or a
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
        self._name_map = {}

    def startup(self):
        """ Nothing needed for a dumpcase."""
        pass

    def register(self, src, inputs, outputs):
        """Register names for later record call from `src`."""
        self._name_map[src] = ([name for name, width in inputs],
                               [name for name, width in outputs])

    def record_constants(self, constants):
        """Record constant inputs."""
        if not self.out:  # if self.out is None, just do nothing
            return

        data = dict(constants=constants)
        self.out.write(json.dumps(data, indent=4, sort_keys=True))
        self.out.write('\n')

    def record(self, src, inputs, outputs, case_uuid, parent_uuid):
        """Dump the given run data in a "pretty" form."""
        if not self.out:  # if self.out is None, just do nothing
            return

        in_names, out_names = self._name_map[src]
        data = dict(uuid=case_uuid, parent_uuid=parent_uuid,
                    timestamp=time.time())
        data.update(zip(in_names, inputs))
        data.update(zip(out_names, outputs))

        data = dict(case=data)
        self.out.write(json.dumps(data, indent=4, sort_keys=True))
        self.out.write('\n')

    def close(self):
        """Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`."""
        if self.out not in (None, sys.stdout, sys.stderr):
            if not isinstance(self.out,
                              (StringIO.StringIO, cStringIO.OutputType)):
                # Closing a StringIO deletes its contents.
                self.out.close()
            self.out = None

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""

        attrs = {}
        attrs['type'] = type(self).__name__

        return attrs

    def get_iterator(self):
        """Just returns None."""
        return None
