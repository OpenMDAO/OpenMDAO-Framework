import cStringIO
import StringIO
import logging
import sys
import time

from json  import dumps, JSONEncoder
from numpy import ndarray
from uuid  import uuid1

from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.main.releaseinfo import __version__


class JSONCaseRecorder(object):
    """Dumps a run in JSON form to `out`, which may be a string or a
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
        self.indent = 4
        self.sort_keys = True
        self._cfg_map = {}
        self._uuid = None
        self._cases = None

    def startup(self):
        """Prepare for new run."""
        pass

    def register(self, driver, inputs, outputs):
        """Register names for later record call from `driver`."""
        self._cfg_map[driver] = (driver.get_pathname(),
                                 [name for name, width in inputs],
                                 [name for name, width in outputs])

    def record_constants(self, constants):
        """Record constant data."""
        if not self.out:  # if self.out is None, just do nothing
            return

        # Locate top level assembly from first driver registered.
        top = self._cfg_map.keys()[0].parent
        while top.parent:
            top = top.parent

        # Collect variable metadata.
        cruft = ('desc', 'framework_var', 'type', 'validation_trait')
        variable_metadata = {}
        for driver, (dname, ins, outs) in self._cfg_map.items():
            scope = driver.parent
            prefix = scope.get_pathname()
            if prefix:
                prefix += '.'

            for name in ins + outs:
                try:
                    metadata = scope.get_metadata(name)
                except AttributeError:
                    pass  # Response_0, etc.
                else:
                    metadata = metadata.copy()
                    for key in cruft:
                        if key in metadata:
                            del metadata[key]
                    variable_metadata[prefix+name] = metadata

        for name in constants:
            metadata = top.get_metadata(name).copy()
            for key in cruft:
                if key in metadata:
                    del metadata[key]
            variable_metadata[name] = metadata

        # Collect expression data.
        expressions = {}
        for driver, (dname, ins, outs) in sorted(self._cfg_map.items(),
                                                 key=lambda item: item[1][0]):
            prefix = driver.parent.get_pathname()
            if prefix:
                prefix += '.'

            if hasattr(driver, 'eval_objectives'):
                for obj in driver.get_objectives().values():
                    info = dict(data_type='Objective',
                                pcomp_name=prefix+obj.pcomp_name)
                    expressions[prefix+str(obj)] = info

            if hasattr(driver, 'eval_responses'):
                for response in driver.get_responses().values():
                    info = dict(data_type='Response',
                                pcomp_name=prefix+response.pcomp_name)
                    expressions[prefix+str(response)] = info

            constraints = []
            if hasattr(driver, 'get_ineq_constraints'):
                constraints.extend(driver.get_ineq_constraints().values())
            if hasattr(driver, 'get_eq_constraints'):
                constraints.extend(driver.get_eq_constraints().values())
            for con in constraints:
                info = dict(data_type='Constraint',
                            pcomp_name=prefix+con.pcomp_name)
                expressions[prefix+str(con)] = info

        # Write simulation info.
        self._uuid = str(uuid1())
        self._cases = 0

        info = dict(
            variable_metadata=variable_metadata,
            expressions=expressions,
            constants=constants,
            OpenMDAO_Version=__version__,
            uuid=self._uuid)

        write = self.out.write
        write('{\n')
        write('"simulation_info": ')
        try:
            write(dumps(info, indent=self.indent, sort_keys=self.sort_keys,
                        cls=Encoder))
        except Exception:
            # Has happened in past for 'validation_trait'.
            logging.error('JSON write failed for simulation_info:')
            for key in sorted(info):
                logging.error('    %s: %s', key, info[key])
            raise
        write('\n')

        # Write info for each driver.
        count = 0
        for driver, (dname, ins, outs) in sorted(self._cfg_map.items(),
                                                 key=lambda item: item[1][0]):
            info = dict(name=dname)
            if hasattr(driver, 'get_parameters'):
                info['parameters'] = \
                    [str(param) for param in driver.get_parameters().values()]
            if hasattr(driver, 'eval_objectives'):
                info['objectives'] = \
                    [key for key in driver.get_objectives()]
            if hasattr(driver, 'eval_responses'):
                info['responses'] = \
                    [key for key in driver.get_responses()]
            if hasattr(driver, 'get_ineq_constraints'):
                info['ineq_constraints'] = \
                    [str(con) for con in driver.get_ineq_constraints().values()]
            if hasattr(driver, 'get_eq_constraints'):
                info['eq_constraints'] = \
                    [str(con) for con in driver.get_eq_constraints().values()]

            count += 1
            write(', "driver_info_%s": ' % count)
            write(dumps(info, indent=self.indent, sort_keys=self.sort_keys,
                        cls=Encoder))
            write('\n')

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """Dump the given run data in a "pretty" form."""
        if not self.out:  # if self.out is None, just do nothing
            return

        # Collect case data.
        dname, in_names, out_names = self._cfg_map[driver]
        data = dict(zip(in_names, inputs))
        data.update(zip(out_names, outputs))

        # Write case.
        info = dict(
            _id=case_uuid,
            _parent_id=parent_uuid or self._uuid,
            _driver_id=dname,
            error_status=None,
            error_message=str(exc) if exc else '',
            timestamp=time.time(),
            data=data)

        self._cases += 1
        write = self.out.write
        write(', "iteration_case_%s": ' % self._cases)
        write(dumps(info, indent=self.indent, sort_keys=self.sort_keys,
                    cls=Encoder))
        write('\n')

    def close(self):
        """Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`."""
        if self.out is not None and self._cases is not None:
            self.out.write('}\n')

        if self.out not in (None, sys.stdout, sys.stderr):
            if not isinstance(self.out,
                              (StringIO.StringIO, cStringIO.OutputType)):
                # Closing a StringIO deletes its contents.
                self.out.close()
            self.out = None

        self._cases = None

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""

        attrs = {}
        attrs['type'] = type(self).__name__
        variables = []

        attr = {}
        attr['name'] = 'indent'
        attr['type'] = type(self.indent).__name__
        attr['value'] = str(self.indent)
        attr['connected'] = ''
        attr['desc'] = 'Number of spaces to indent each level.'
        variables.append(attr)

        attr = {}
        attr['name'] = 'sort_keys'
        attr['type'] = type(self.sort_keys).__name__
        attr['value'] = str(self.sort_keys)
        attr['connected'] = ''
        attr['desc'] = 'If True, sort dictionary keys.'
        variables.append(attr)

        attrs["Inputs"] = variables
        return attrs

    def get_iterator(self):
        """Just returns None."""
        return None


class Encoder(JSONEncoder):
    """Special encoder to deal with types not handled by default encoder."""

    def default(self, obj):
        if isinstance(obj, ndarray):
            return obj.tolist()
        else:
            super(Encoder, self).default(obj)

