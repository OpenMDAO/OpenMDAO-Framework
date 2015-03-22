import bson
import json
import logging
import cPickle
import StringIO
from struct import pack, unpack
from weakref import ref

from numpy import ndarray

from openmdao.main.api import Assembly, VariableTree
from openmdao.lib.casehandlers.pymongo_bson.json_util import loads, dumps
from openmdao.lib.casehandlers.pymongo_bson.binary import Binary
from openmdao.lib.casehandlers.jsoncase import _Encoder

_GLOBAL_DICT = dict(__builtins__=None)


class CaseDataset(object):
    """
    Reads case data from `filename` and allows queries on it.
    `format` should be ``bson`` or ``json``, indicating a
    :class:`BSONCaseRecorder` file or :class:`JSONCaseRecorder` file
    respectively.

    To get all case data::

        cds = CaseDataset('recorded.json', 'json')
        cases = cds.data.fetch()

    To get names of columns returned::

        names = cds.data.var_names().fetch()

    To select a specific set of variables::

        cases = cds.data.vars(['top.sub.comp.x, top.sub.comp.y']).fetch()

    To get a case and all its child cases::

        cases = cds.data.parent_case(parent_id).fetch()

    To get cases for a particular driver::

        cases = cds.data.driver(driver_name).fetch()

    To get cases for a particular run of a particular driver::

        cases = cds.data.parent_case(parent_id).driver(driver_name).fetch()

    or::

        cases = cds.data.driver(driver_name).parent_case(parent_id).fetch()

    Other possibilities exist, see :class:`Query`.

    To restore from the last recorded case::

        cds.restore(assembly, cds.data.fetch()[-1]['_id'])

    """

    def __init__(self, filename, format):
        format = format.lower()
        if format == 'bson':
            self._reader = _BSONReader(filename)
        elif format == 'json':
            self._reader = _JSONReader(filename)
        else:
            raise ValueError("dataset format must be 'json' or 'bson'")

        self._query_id = self._parent_id = self._driver_id = None
        self._case_ids = self._drivers = None

    @property
    def data(self):
        """ :class:`Query` object. """
        return Query(self)

    @property
    def drivers(self):
        """ List of driver info dictionaries. """
        return self._reader.drivers()

    @property
    def simulation_info(self):
        """ Simulation info dictionary. """
        return self._reader.simulation_info

    def _fetch(self, query):
        """ Return data based on `query`. """
        self._setup(query)

        metadata_names = ['_id', '_parent_id', '_driver_id', 'error_status',
                          'error_message', 'timestamp']
        if query.vnames:
            tmp = []
            for name in metadata_names:
                if name in query.vnames:
                    tmp.append(name)
            metadata_names = tmp
            names = query.vnames
        else:
            if query.driver_name:
                driver_info = self._drivers[self._driver_id]
                prefix = driver_info['prefix']
                all_names = [prefix+name
                             for name in driver_info['recording']]
            else:
                all_names = []
                for driver_info in self._drivers.values():
                    prefix = driver_info['prefix']
                    all_names.extend([prefix+name
                                      for name in driver_info['recording']])
            names = sorted(all_names+metadata_names)

        if query.names:
            # Returning single row, not list of rows.
            return names

        nan = float('NaN')
        rows = ListResult()
        state = {}  # Retains last seen values.
        for case_data in self._reader.cases():
            data = case_data['data']
            case_id = case_data['_id']
            case_driver_id = case_data['_driver_id']

            prefix = self._drivers[case_driver_id]['prefix']
            if prefix:
                # Make names absolute.
                pass
                #data = dict([(prefix+name, value)
                 #            for name, value in data.items()])
            else:
                data = data.copy()  # Don't modify reader version.

            state.update(data)

            # Filter on driver.
            if self._driver_id is not None and \
               case_driver_id != self._driver_id:
                continue

            # Filter on case.
            if self._case_ids is None or case_id in self._case_ids:
                for name in metadata_names:
                    data[name] = case_data[name]

                row = DictList(names)
                for name in names:
                    if query.local_only:
                        if name in metadata_names:
                            row.append(data[name])
                        else:
                            driver = self._drivers[case_driver_id]
                            lnames = [prefix+rec for rec in driver['recording']]
                            if name in lnames:
                                row.append(data[name])
                            else:
                                row.append(nan)
                    elif name in state:
                        row.append(state[name])
                    elif name in data:
                        row.append(data[name])
                    else:
                        row.append(nan)
                rows.append(row)

            if case_id == self._query_id or case_id == self._parent_id:
                break  # Parent is last case recorded.

        if self._query_id and not rows:
            raise ValueError('No case with _id %s' % self._query_id)

        if query.transpose:
            tmp = DictList(names)
            for i in range(len(rows[0])):
                tmp.append([row[i] for row in rows])
            # Keep CDS as attribute for post-processing
            tmp.cds = self
            return tmp

        # Keep CDS as attribute for post-processing
        rows.cds = self
        return rows

    def _write(self, query, out, format):
        """ Write data based on `query` to `out`. """
        if query.local_only:
            raise ValueError('data.local() invalid for write()')
        if query.names:
            raise ValueError('data.var_names() invalid for write()')
        if query.transpose:
            raise ValueError('data.by_variable() invalid for write()')

        self._setup(query)

        format = format.lower()
        if format == 'bson':
            writer = _BSONWriter(out)
        elif format == 'json':
            writer = _JSONWriter(out)
        else:
            raise ValueError("dataset format must be 'json' or 'bson'")

        drivers = self.drivers

        found = False
        simulation_info = self.simulation_info
        constants = simulation_info['constants']  # Updated to reflect start.

        for count, case_data in enumerate(self._reader.cases()):
            data = case_data['data']
            case_id = case_data['_id']
            case_driver_id = case_data['_driver_id']
            prefix = self._drivers[case_driver_id]['prefix']

            # Filter on driver.
            if self._driver_id is not None and \
               case_driver_id != self._driver_id:
                if not found:
                    # Update 'constants'.
                    for name, value in data.items():
                        constants[prefix+name] = value
                continue

            # Filter on case.
            if self._case_ids is None or case_id in self._case_ids:
                if not found:
                    writer.write('simulation_info', simulation_info)
                    for i, driver in enumerate(self.drivers):
                        # Remove unused variables from driver['recording'] data.
                        if query.vnames:
                            prefix, _, name = driver['name'].rpartition('.')
                            recording = [name for name in driver['recording']
                                               if prefix+name in query.vnames]
                            driver['recording'] = recording
                        writer.write('driver_info_%s' % (i+1), driver)
                    found = True

                if query.vnames:
                    # Filter on variable.
                    for name in data.keys():
                        if prefix+name not in query.vnames:
                            del data[name]

                writer.write('iteration_case_%s' % (count+1), case_data)

            elif not found:
                # Update 'constants'.
                for name, value in data.items():
                    constants[prefix+name] = value

            if case_id == self._query_id or case_id == self._parent_id:
                break  # Parent is last case recorded.

        if self._query_id and not found:
            raise ValueError('No case with _id %s' % self._query_id)
        elif not found:
            # write simulation_info even if no cases were found
            writer.write('simulation_info', simulation_info)
            for i, driver in enumerate(self.drivers):
                # Remove unused variables from driver['recording'] data.
                if query.vnames:
                    prefix, _, name = driver['name'].rpartition('.')
                    recording = [name for name in driver['recording']
                                       if prefix+name in query.vnames]
                    driver['recording'] = recording
                writer.write('driver_info_%s' % (i+1), driver)

        writer.close()

    def _setup(self, query):
        """ Setup for processing `query`. """
        if query.vnames is not None:
            bad = []
            metadata = self.simulation_info['variable_metadata']
            expressions = self.simulation_info['expressions']
            for name in query.vnames:
                if name not in metadata and name not in [ e['pcomp_name'] for e in expressions.values()]:
                    bad.append(name)
            if bad:
                raise RuntimeError('Names not found in the dataset: %s' % bad)

        self._drivers = {}
        self._driver_id = None
        for driver_info in self._reader.drivers():
            _id = driver_info['_id']
            name = driver_info['name']
            prefix, _, name = name.rpartition('.')
            if prefix:
                prefix += '.'
            driver_info['prefix'] = prefix
            self._drivers[_id] = driver_info
            if driver_info['name'] == query.driver_name:
                self._driver_id = _id

        if query.driver_name:
            if self._driver_id is None:
                raise ValueError('No driver named %r' % query.driver_name)

        self._case_ids = None
        self._query_id = None
        self._parent_id = None
        if query.case_id is not None:
            self._query_id = query.case_id
            self._case_ids = set((self._query_id,))
            self._driver_id = None  # Case specified, ignore driver.
        elif query.parent_id is not None:
            # Parent won't be seen until children are, so we have to pre-screen.
            # Collect tree of cases.
            self._parent_id = query.parent_id
            cases = {}
            for case_data in self._reader.cases():
                _id = case_data['_id']
                _driver_id = case_data['_driver_id']
                _parent_id = case_data['_parent_id']

                if _id in cases:
                    node = cases[_id]
                    node.driver_id = _driver_id
                    if node.parent is None:
                        if _parent_id in cases:
                            node.parent = cases[_parent_id]
                        else:
                            parent = _CaseNode(_parent_id)
                            parent.add_child(node)
                            node.parent = parent
                            cases[_parent_id] = parent
                else:
                    if _parent_id in cases:
                        parent = cases[_parent_id]
                    else:
                        parent = _CaseNode(_parent_id)
                        cases[_parent_id] = parent
                    child = _CaseNode(_id, _driver_id, parent)
                    cases[_id] = parent.add_child(child)

                if _id == self._parent_id:
                    break  # Parent is last case recorded.

            # Determine subtree of interest.
            if self._parent_id in cases:
                root = cases[self._parent_id]
                self._case_ids = set((self._parent_id,))
                for child in root.get_children():
                    self._case_ids.add(child.case_id)
            else:
                raise ValueError('No case with _id %s', self._parent_id)

    def restore(self, assembly, case_id):
        """ Restore case `case_id` into `assembly`. """
        case = self.data.case(case_id).fetch()[0]

        # Restore constant inputs.
        constants = self.simulation_info['constants']
        for name in sorted(constants):
            self._set(assembly, name, constants[name])

        # Restore case data.
        # Sorted order gets comp.arr set before comp.arr[[1, 3, 5]].
        metadata = self.simulation_info['variable_metadata']
        for name in sorted(case.keys()):
            value = case[name]
            if name in metadata:
                iotype = metadata[name]['iotype']
            elif '_pseudo_' in name:
                if not name.endswith('.out0'):
                    name += '.out0'
                iotype = 'out'
            else:
                continue

            try:
                self._set(assembly, name, value)
            except Exception as exc:
                raise RuntimeError("Can't set %s to %s: %s" % (name, value, exc))

            # Find connected inputs and set those as well.
            asm = assembly
            src = name
            for name in src.split('.')[:-1]:
                obj = getattr(asm, name)
                if not isinstance(obj, Assembly):
                    break
                asm = obj
            prefix = asm.get_pathname()
            if prefix:
                prefix += '.'
            src = src[len(prefix):]
            for src, dst in asm._depgraph.out_edges(src):
                if src.startswith(dst):
                    continue  # Weird VarTree edge.
                dst = prefix+dst
                try:
                    self._set(assembly, dst, value)
                except Exception as exc:
                    logging.warning("Can't set %s to %s: %s", dst, value, exc)

    def _set(self, assembly, name, value):
        """ Set `name` in `assembly` to `value`. """
        # Translating unicode to str to avoid issues like pyOpt option checks.
        if isinstance(value, dict):
            curr = assembly.get(name)
            if isinstance(curr, VariableTree):
                for key, val in value.items():
                    self._set(assembly, '.'.join((name, key)), val)
            elif '[' in name:
                if isinstance(value, unicode):
                    value = str(value)
                exec('assembly.%s = value' % name, _GLOBAL_DICT, locals())
            else:
                for key, val in value.items():
                    if isinstance(val, unicode):
                        value[key] = str(val)
                assembly.set(name, value)
        else:
            if isinstance(value, unicode):
                value = str(value)
            if '[' in name:
                exec('assembly.%s = value' % name, _GLOBAL_DICT, locals())
            else:
                assembly.set(name, value)


class Query(object):
    """
    Retains query information for a :class:`CaseDataset`. All methods other
    than :meth:`fetch` and :meth:`write` return ``self``, so operations are
    easily chained.  If the same method is called more than once, only the last
    call has an effect.
    """

    def __init__(self, dataset):
        self._dataset = dataset
        self.driver_name = None
        self.case_id = None
        self.parent_id = None
        self.vnames = None
        self.local_only = False
        self.names = False
        self.transpose = False

    def fetch(self):
        """ Return a list of rows of data, one for each selected case. """
        return self._dataset._fetch(self)

    def write(self, out, format=None):
        """
        Write filtered :class:`CaseDataset` to `out`, a filename or file-like
        object.  Default `format` is the format of the original data file.
        """
        if format is None:
            if isinstance(self._dataset._reader, _BSONReader):
                format = 'bson'
            else:
                format = 'json'
        self._dataset._write(self, out, format)

    def driver(self, driver_name):
        """ Filter the cases to those recorded by the named driver. """
        self.driver_name = driver_name
        return self

    def case(self, case_id):
        """ Return this case. """
        self.case_id = case_id
        self.parent_id = None
        return self

    def parent_case(self, parent_case_id):
        """ Filter the cases to only include this case and its children. """
        self.parent_id = parent_case_id
        self.case_id = None
        return self

    def vars(self, *args):
        """ Filter the variable columns returned in the row. """
        self.vnames = []
        for arg in args:
            if isinstance(arg, basestring):
                self.vnames.append(arg)
            else:
                self.vnames.extend(arg)
        return self

    def local(self):
        """
        Restrict the variables returned to only those in the specific driver's
        local set. This means that if there are cases from more than one driver,
        variables not local to that driver will be set to ``NaN``.
        """
        self.local_only = True
        return self

    def by_case(self):
        """
        Have :meth:`fetch` return data as ``[case][var]`` (the default).
        """
        self.transpose = False
        return self

    def by_variable(self):
        """
        Have :meth:`fetch` return data as ``[var][case]`` rather than the
        default of ``[case][var]``.
        """
        self.transpose = True
        return self

    def var_names(self):
        """ Return  a list of the names of the variables in the cases. """
        self.names = True
        return self


class DictList(list):
    """ List that can be indexed by index or 'var_name'. """

    def __init__(self, var_names, seq=None):
        if seq is None:
            super(DictList, self).__init__()
        else:
            super(DictList, self).__init__(seq)
        self.name_map = dict([(v, i) for i, v in enumerate(var_names)])

    def __getitem__(self, key):
        if isinstance(key, int):
            return super(DictList, self).__getitem__(key)
        else:
            return super(DictList, self).__getitem__(self.name_map[key])

    def keys(self):
        """ Return list of dictionary keys. """
        return self.name_map.keys()

    def items(self):
        """ Return list of ``(key, value)``. """
        return [(key, self[key]) for key in self.name_map]

    def values(self):
        """ Return values in key order. """
        return [self[key] for key in self.name_map]


class ListResult(list):
    """ Simply a list that allows us to save a reference to the
    original CaseDataSet.
    """
    pass


class _CaseNode(object):
    """ Represents a node in a tree of cases. """

    def __init__(self, case_id, driver_id=None, parent=None):
        self.case_id = case_id
        self.driver_id = driver_id
        self._parent = None
        self.parent = parent
        self.children = []

    @property
    def parent(self):
        """ Parent node. """
        return None if self._parent is None else self._parent()

    @parent.setter
    def parent(self, node):
        self._parent = None if node is None else ref(node)

    def add_child(self, child):
        """ Add child to this case. """
        self.children.append(child)
        return child

    def get_children(self):
        """ Return all child cases, recursively. """
        kids = []
        for child in self.children:
            kids.append(child)
            kids.extend(child.get_children())
        return kids


class _Reader(object):
    """ Base class for JSON/BSON readers. """

    def __init__(self, filename, mode):
        if isinstance(filename, StringIO.StringIO):
            self._inp = filename
        else:
            self._inp = open(filename, mode)
        self._simulation_info = self._next()
        self._state = 'drivers'
        self._info = None

    def _next(self):
        """ Return next dictionary of data. """
        raise NotImplementedError('_next')

    @property
    def simulation_info(self):
        """ Simulation info dictionary. """
        return self._simulation_info

    def drivers(self):
        """ Return list of 'driver_info' dictionaries. """
        if self._state != 'drivers':
            self._inp.seek(0)
            self._next()  # Re-read 'simulation_info'.

        driver_info = []
        info = self._next()
        while info:
            if '_driver_id' not in info:
                driver_info.append(info)
            else:
                self._info = info
                self._state = 'cases'
                return driver_info
            info = self._next()
        self._state = 'eof'
        return driver_info

    def cases(self):
        """ Return sequence of 'iteration_case' dictionaries. """
        if self._state != 'cases' or self._info is None:
            self.drivers()  # Read up to first case.
            if self._state != 'cases':
                return

        yield self._info  # Read when looking for drivers.
        self._info = None

        info = self._next()
        while info:
            yield info
            info = self._next()
        self._state = 'eof'


class _JSONReader(_Reader):
    """ Reads a :class:`JSONCaseRecorder` file. """

    def __init__(self, filename):
        super(_JSONReader, self).__init__(filename, 'rU')

    def _next(self):
        """ Return next dictionary of data. """
        data = self._inp.readline()
        while '__length_' not in data:
            if not data:
                return None
            data = self._inp.readline()

        key, _, value = data.partition(':')  # '"__length_1": NNN'
        reclen = int(value) - 1
        data = self._inp.readline()  # ', "dictname": {'
        data = '{\n' + self._inp.read(reclen)
        return json.loads(data,object_hook=object_hook)
        #return loads(data)

def object_hook(dct, compile_re=True):
    if "$binary" in dct:
        if isinstance(dct["$type"], int):
            dct["$type"] = "%02x" % dct["$type"]
        subtype = int(dct["$type"], 16)
        if subtype >= 0xffffff80:  # Handle mongoexport values
            subtype = int(dct["$type"][6:], 16)
        #return Binary(base64.b64decode(dct["$binary"].encode()), subtype)
        import base64
        return cPickle.loads(base64.b64decode(dct["$binary"].encode()))
        #return cPickle.loads(dct["$binary"].encode('utf-8'))
    return dct

class _BSONReader(_Reader):
    """ Reads a :class:`BSONCaseRecorder` file. """

    def __init__(self, filename):
        super(_BSONReader, self).__init__(filename, 'rb')

    def _next(self):
        """ Return next dictionary of data. """
        data = self._inp.read(4)
        if not data:
            return None
        reclen = unpack('<L', data)[0]
        return bson.loads(self._inp.read(reclen))


class _JSONWriter(object):
    """ Writes case data as JSON. """

    def __init__(self, out, indent=4, sort_keys=True):
        if isinstance(out, basestring):
            self._out = open(out, 'w')
        elif isinstance(out, StringIO.StringIO):
            self._out = out
        elif ('w' in out.mode or 'a' in out.mode) and 'b' not in out.mode:
            self._out = out
        else:
            raise ValueError("'out' must be a writable file-like object in"
                             " text mode")
        self._indent = indent
        self._sort_keys = sort_keys
        self._count = 0

    def write(self, category, data):
        """ Write `data` under `category`. """
        data = json.dumps(data, indent=self._indent, sort_keys=self._sort_keys, cls=_Encoder)
        #data = dumps(data, indent=self._indent, sort_keys=self._sort_keys)
        self._count += 1
        prefix = '{\n' if self._count == 1 else ', '
        self._out.write('%s"__length_%s": %s\n, "%s": '
                        % (prefix, self._count, len(data), category))
        self._out.write(data)
        self._out.write('\n')

    def close(self):
        """ Close file. """
        self._out.write('}\n')

        if isinstance(self._out, StringIO.StringIO):
            pass
        elif self._out.mode == 'w':
            self._out.close()


class _BSONWriter(object):
    """ Writes case data as BSON. """

    def __init__(self, out):
        if isinstance(out, basestring):
            self._out = open(out, 'wb')
        elif 'w' in out.mode and 'b' in out.mode:
            self._out = out
        else:
            raise ValueError("'out' must be a writable file-like object in"
                             " binary mode")

    def write(self, category, data):
        """ Write `data` under `category`. """
        data = bson.dumps(data)
        self._out.write(pack('<L', len(data)))
        self._out.write(data)

    def close(self):
        """ Close file. """
        self._out.close()
