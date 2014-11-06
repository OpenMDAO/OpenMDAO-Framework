"""
JSON/BSON Case Recording.
"""

import cStringIO
import StringIO
import logging
import sys
import time

import json
import bson

from numpy  import ndarray
from struct import pack
from uuid   import uuid1

from openmdao.main.api import VariableTree
from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.main.releaseinfo import __version__


class _BaseRecorder(object):
    """ Base class for JSONRecorder and BSONRecorder. """

    implements(ICaseRecorder)

    def __init__(self):
        self._cfg_map = {}
        self._uuid = None
        self._cases = None
        
        self._last_child_case_uuids = {} # keyed by driver id qqq

    def startup(self):
        """ Prepare for new run. """
        pass

    def register(self, driver, inputs, outputs):
        """ Register names for later record call from `driver`. """
        self._cfg_map[driver] = (inputs, outputs)

    def get_simulation_info(self, constants):
        """ Return simulation info dictionary. """
        # Locate top level assembly from first driver registered.
        top = self._cfg_map.keys()[0].parent
        while top.parent:
            top = top.parent
        prefix_drop = len(top.name)+1 if top.name else 0

        # Collect variable metadata.
        cruft = ('desc', 'framework_var', 'type', 'validation_trait')
        variable_metadata = {}
        for driver, (ins, outs) in self._cfg_map.items():
            scope = driver.parent
            prefix = '' if scope is top else scope.get_pathname()[prefix_drop:]
            if prefix:
                prefix += '.'

            for name in ins + outs:
                if '_pseudo_' in name or name.endswith('.workflow.itername'):
                    pass  # No metadata.
                else:
                    name, _, rest = name.partition('[')
                    try:
                        metadata = scope.get_metadata(name)
                    except AttributeError:
                        pass  # Error already logged.
                    else:
                        metadata = metadata.copy()
                        for key in cruft:
                            if key in metadata:
                                del metadata[key]
                        variable_metadata[prefix+name] = metadata

        for name in constants:
            name, _, rest = name.partition('[')
            metadata = top.get_metadata(name).copy()
            for key in cruft:
                if key in metadata:
                    del metadata[key]
            variable_metadata[name] = metadata

        # Collect expression data.
        expressions = {}
        for driver, (ins, outs) in sorted(self._cfg_map.items(),
                                          key=lambda item: item[1][0]):
            scope = driver.parent
            prefix = '' if scope is top else scope.get_pathname()[prefix_drop:]
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
            if hasattr(driver, 'get_eq_constraints'):
                constraints.extend(driver.get_eq_constraints().values())
            if hasattr(driver, 'get_ineq_constraints'):
                constraints.extend(driver.get_ineq_constraints().values())
            for con in constraints:
                info = dict(data_type='Constraint',
                            pcomp_name=prefix+con.pcomp_name)
                expressions[prefix+str(con)] = info

        self._uuid = str(uuid1())
        self._cases = 0

        dep_graph = top.get_graph(format='json')
        comp_graph = top.get_graph(components_only=True, format='json')

        return dict(variable_metadata=variable_metadata,
                    expressions=expressions,
                    constants=constants,
                    graph=dep_graph,
                    comp_graph=comp_graph,
                    name=top.name,
                    OpenMDAO_Version=__version__,
                    uuid=self._uuid)

    def get_driver_info(self):
        """ Return list of driver info dictionaries. """

        # Locate top level assembly from first driver registered.
        top = self._cfg_map.keys()[0].parent
        while top.parent:
            top = top.parent
        prefix_drop = len(top.name) + 1 if top.name else 0

        driver_info = []
        for driver, (ins, outs) in sorted(self._cfg_map.items(),
                                          key=lambda item: item[0].get_pathname()):
            name = driver.get_pathname()[prefix_drop:]
            info = dict(name=name, _id=id(driver), recording=ins+outs)
            if hasattr(driver, 'get_parameters'):
                info['parameters'] = \
                    [str(param) for param in driver.get_parameters().values()]
            if hasattr(driver, 'eval_objectives'):
                info['objectives'] = \
                    [key for key in driver.get_objectives()]
            if hasattr(driver, 'eval_responses'):
                info['responses'] = \
                    [key for key in driver.get_responses()]
            if hasattr(driver, 'get_eq_constraints'):
                info['eq_constraints'] = \
                    [str(con) for con in driver.get_eq_constraints().values()]
            if hasattr(driver, 'get_ineq_constraints'):
                info['ineq_constraints'] = \
                    [str(con) for con in driver.get_ineq_constraints().values()]
            driver_info.append(info)
        return driver_info

    def get_case_info(self, driver, inputs, outputs, exc,
                      case_uuid, parent_uuid):
        """ Return case info dictionary. """
        in_names, out_names = self._cfg_map[driver]
        
        data = dict(zip(in_names, inputs))
        data.update(zip(out_names, outputs))

        subdriver_last_case_uuids = {}
        for subdriver in driver.subdrivers():
            subdriver_last_case_uuids[ id(subdriver) ] = self._last_child_case_uuids[ id(subdriver) ]
        self._last_child_case_uuids[ id(driver) ] = case_uuid
        
        #qqq
        #data = data.values()
        
        return dict(_id=case_uuid,
                    _parent_id=parent_uuid or self._uuid,
                    _driver_id=id(driver),
                    #subdriver_last_case_uuids = subdriver_last_case_uuids,
                    error_status=None,
                    error_message=str(exc) if exc else '',
                    timestamp=time.time(),
                    data=data)


class JSONCaseRecorder(_BaseRecorder):
    """
    Dumps a run in JSON form to `out`, which may be a string or a file-like
    object (defaults to ``stdout``). If `out` is ``stdout`` or ``stderr``,
    then that standard stream is used. Otherwise, if `out` is a string, then
    a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    def __init__(self, out='cases.json', indent=4, sort_keys=True):
        super(JSONCaseRecorder, self).__init__()
        if isinstance(out, basestring):
            if out == 'stdout':
                out = sys.stdout
            elif out == 'stderr':
                out = sys.stderr
            else:
                out = open(out, 'w')
        self.out = out
        self.indent = indent
        self.sort_keys = sort_keys
        self._count = 0

    def record_constants(self, constants):
        """ Record constant data. """
        if not self.out:
            return

        info = self.get_simulation_info(constants)
        category = 'simulation_info'
        data = self._dump(info, category,
                          ('variable_metadata', 'expressions', 'constants'))
        self._count += 1
        self.out.write('{\n"__length_%s": %s\n, "%s": '
                       % (self._count, len(data), category))
        self.out.write(data)
        self.out.write('\n')

        for i, info in enumerate(self.get_driver_info()):
            category = 'driver_info_%s' % (i+1)
            data = self._dump(info, category)
            self._count += 1
            self.out.write(', "__length_%s": %s\n, "%s": '
                           % (self._count, len(data), category))
            self.out.write(data)
            self.out.write('\n')

        self.out.flush()

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """ Dump the given run data. """
        if not self.out:
            return

        info = self.get_case_info(driver, inputs, outputs, exc,
                                  case_uuid, parent_uuid)
        self._cases += 1
        category = 'iteration_case_%s' % self._cases
        data = self._dump(info, category, ('data',))
        self._count += 1
        self.out.write(', "__length_%s": %s\n, "%s": '
                       % (self._count, len(data), category))
        self.out.write(data)
        self.out.write('\n')
        self.out.flush()

    def _dump(self, info, category, subcategories=None):
        """ Return JSON data, report any bad keys & values encountered. """
        try:
            return json.dumps(info, indent=self.indent,
                              sort_keys=self.sort_keys,
                              cls=_Encoder, check_circular=False)
        except Exception as exc:
            # Log bad keys & values.
            bad = []
            for key in sorted(info):
                try:
                    json.dumps(info[key], indent=self.indent,
                               sort_keys=self.sort_keys,
                               cls=_Encoder, check_circular=False)
                except Exception:
                    bad.append(key)

            # If it's in a subcategory we only report the first subcategory.
            if subcategories is not None and bad[0] in subcategories:
                key = bad[0]
                category = '.'.join((category, key))
                info = info[key]
                bad = []
                for key in sorted(info):
                    try:
                        json.dumps(info[key], indent=self.indent,
                                   sort_keys=self.sort_keys,
                                   cls=_Encoder, check_circular=False)
                    except Exception:
                        bad.append(key)

            msg = 'JSON write failed for %s:' % category
            logging.error(msg)
            for key in bad:
                logging.error('    %s: %s', key, info[key])

            msg = '%s keys %s: %s' % (msg, bad, exc)
            raise RuntimeError(msg)

    def close(self):
        """
        Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`.
        """
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
        """ Return attribute dictionary for GUI. """
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
        """ Just returns None. """
        return None


class _Encoder(json.JSONEncoder):
    """ Special encoder to deal with types not handled by default encoder. """

    def default(self, obj):
        fixed = _fixup(obj)
        if fixed is obj:
            super(_Encoder, self).default(obj)
        return fixed


def _fixup(value):
    """
    Fix object for json encoder, also bson. Stock bson doesn't handle a lot
    of types, just skips them.
    """
    if isinstance(value, dict):
        for key, val in value.items():
            value[key] = _fixup(val)
        return value
    elif isinstance(value, (list, tuple, set, frozenset)):
        return [_fixup(val) for val in value]
    elif isinstance(value, ndarray):
        #return value.tolist()
        import cPickle
        from myjson_util import dumps, Binary
        return dumps(Binary( cPickle.dumps( value, protocol=2) ) )
    elif isinstance(value, VariableTree):
        return dict([(name, _fixup(getattr(value, name)))
                     for name in value.list_vars()])
    elif hasattr(value, 'json_encode') and callable(value.json_encode):
        return value.json_encode()
    elif hasattr(value, '__dict__'):
        return _fixup(value.__dict__)
    return value


class BSONCaseRecorder(_BaseRecorder):
    """
    Dumps a run in BSON form to `out`, which may be a string or a file-like
    object. If `out` is a string, then a file with that name will be opened
    in the current directory. If `out` is None, cases will be ignored.

    The resulting file can be read by code similar to this::

        from bson import loads
        from pprint import pprint
        from struct import unpack
        import sys

        sep = '-'*60

        with open(sys.argv[1], 'rb') as inp:
            reclen = unpack('<L', inp.read(4))[0]
            data = inp.read(reclen)
            obj = loads(data)  # simulation_info
            pprint(obj)
            print sep

            data = inp.read(4)
            while data:
                reclen = unpack('<L', data)[0]
                data = inp.read(reclen)
                obj = loads(data)  # driver_info or iteration_case
                pprint(obj)
                print sep

                data = inp.read(4)

    """

    def __init__(self, out='cases.bson'):
        super(BSONCaseRecorder, self).__init__()
        if isinstance(out, basestring):
            out = open(out, 'w')
        self.out = out

    def record_constants(self, constants):
        """ Record constant data. """
        if not self.out:
            return

        data = self._dump(self.get_simulation_info(constants))
        reclen = pack('<L', len(data))
        self.out.write(reclen)
        self.out.write(data)

        for info in self.get_driver_info():
            data = self._dump(info)
            reclen = pack('<L', len(data))
            self.out.write(reclen)
            self.out.write(data)

        self.out.flush()

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """ Dump the given run data in a "pretty" form. """
        if not self.out:
            return

        info = self.get_case_info(driver, inputs, outputs, exc,
                                  case_uuid, parent_uuid)
        data = self._dump(info)
        reclen = pack('<L', len(data))
        self.out.write(reclen)
        self.out.write(data)
        self.out.flush()

    def _dump(self, info):
        """ Return BSON data, report any bad keys & values encountered. """
        return bson.dumps(_fixup(info))

    def close(self):
        """
        Closes `out`. Note that a closed recorder will do nothing in
        :meth:`record`.
        """
        if self.out is not None:
            if not isinstance(self.out,
                              (StringIO.StringIO, cStringIO.OutputType)):
                # Closing a StringIO deletes its contents.
                self.out.close()
            self.out = None

        self._cases = None

    def get_attributes(self, io_only=True):
        """ Return attribute dictionary for GUI. """
        attrs = {}
        attrs['type'] = type(self).__name__
        return attrs

    def get_iterator(self):
        """ Just returns None. """
        return None
