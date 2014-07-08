import bson
import json

from struct import unpack
from weakref import ref

from openmdao.main.api import Assembly


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
    """

    def __init__(self, filename, format):
        format = format.lower()
        if format == 'bson':
            self._reader = _BSONReader(filename)
        elif format == 'json':
            self._reader = _JSONReader(filename)
        else:
            raise ValueError("dataset format must be 'json' or 'bson'")

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
        vnames = query.vnames
        if vnames is not None:
            bad = []
            metadata = self.simulation_info['variable_metadata']
            for name in vnames:
                if name not in metadata:
                    bad.append(name)
            if bad:
                raise RuntimeError('Names not found in the dataset: %s' % bad)

        local = query.local_only
        return_names = query.names

        all_names = []
        drivers = {}
        driver_id = None
        driver_names = None
        for driver_info in self._reader.drivers():
            _id = driver_info['_id']
            name = driver_info['name']
            prefix, _, name = name.rpartition('.')
            if prefix:
                prefix += '.'
            driver_info['prefix'] = prefix
            drivers[_id] = driver_info
            if query.driver_name and name == query.driver_name:
                driver_id = _id
                driver_names = [prefix+name
                                for name in driver_info['recording']]
            if not vnames and not query.driver_name:
                all_names.extend([prefix+name
                                  for name in driver_info['recording']])

        if query.driver_name:
            if driver_id is None:
                raise ValueError('No driver named %r' % query.driver_name)
            all_names = driver_names

        case_ids = None
        query_id = None
        parent_id = None
        if query.case_id is not None:
            query_id = query.case_id
            case_ids = set([query_id])
            driver_id = None
        elif query.parent_id is not None:
            # Parent won't be seen until children are, so we have to pre-screen.
            # Collect tree of cases.
            parent_id = query.parent_id
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

                if _id == parent_id:
                    break  # Parent is last case recorded.

            # Determine subtree of interest.
            if parent_id in cases:
                root = cases[parent_id]
                case_ids = set((parent_id,))
                if not vnames:
                    all_names = list(drivers[root.driver_id]['recording'])
                recorded = set()
                for child in root.get_children():
                    case_ids.add(child.case_id)
                    if not vnames and child.driver_id not in recorded:
                        all_names.extend(drivers[child.driver_id]['recording'])
                        recorded.add(child.driver_id)
            else:
                raise ValueError('No case with _id %s', parent_id)

        metadata_names = ['_id', '_parent_id', '_driver_id', 'error_status',
                          'error_message', 'timestamp']
        if vnames:
            tmp = []
            for name in metadata_names:
                if name in vnames:
                    tmp.append(name)
            metadata_names = tmp
            names = vnames
        else:
            names = sorted(all_names+metadata_names)

        if return_names:
            # Returning single row, not list of rows.
            return names

        nan = float('NaN')
        rows = []
        state = {}  # Retains last seen values.
        for case_data in self._reader.cases():
            data = case_data['data']
            case_id = case_data['_id']
            case_driver_id = case_data['_driver_id']

            prefix = drivers[case_driver_id]['prefix']
            if prefix:
                # Make names absolute.
                tmp = dict([(prefix+name, value)
                            for name, value in data.items()])
                data = tmp
            else:
                data = data.copy()  # Don't modify reader version.

            state.update(data)

            # Filter on driver.
            if driver_id is not None and case_driver_id != driver_id:
                continue

            if case_ids is None or case_id in case_ids:
                # Record this case.
                for name in metadata_names:
                    data[name] = case_data[name]

                row = DictList(names)
                for name in names:
                    if local:
                        if name in metadata_names:
                            row.append(data[name])
                        else:
                            driver = drivers[case_driver_id]
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

            if case_id == query_id or case_id == parent_id:
                break  # Parent is last case recorded.

        if query_id and not rows:
            raise ValueError('No case with _id %s', query_id)

        if query.transpose:
            tmp = DictList(names)
            for i in range(len(rows[0])):
                tmp.append([row[i] for row in rows])
            return tmp
        elif query_id:
            return rows[0]
        return rows

    def restore(self, assembly, case_id):
        """ Restore case `case_id` into `assembly`. """
        case = self.data.case(case_id).fetch()

        # Restore constant inputs.
        constants = self.simulation_info['constants']
        for name in sorted(constants.keys()):
            assembly.set(name, constants[name])

        # Restore case data.
        global_dict = dict(__builtins__=None)
        metadata = self.simulation_info['variable_metadata']
        for name, value in case.items():
            if name in metadata:
                iotype = metadata[name]['iotype']
            elif name.startswith('_pseudo_'):
                name += '.out0'
                iotype = 'out'
            else:
                continue

            if '[' in name:
                exec('assembly.%s = value' % name, global_dict, locals())
            else:
                assembly.set(name, value)

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
                dst = prefix+dst
                if '[' in dst:
                    exec('assembly.%s = value' % dst, global_dict, locals())
                else:
                    assembly.set(dst, value)


class Query(object):
    """
    Retains query information for a :class:`CaseDataset`. All methods other
    than :meth:`fetch` return ``self``, so operations are easily chained.
    If the same method is called more than once, only the last call has an
    effect.
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
        return self.name_map.keys()

    def items(self):
        return [(key, self[key]) for key in self.name_map]

    def values(self):
        return [self[key] for key in self.name_map]


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
        """ Return all child cases. """
        kids = []
        for child in self.children:
            kids.append(child)
            kids.extend(child.get_children())
        return kids


class _Reader(object):
    """ Base class for JSON/BSON readers. """

    def __init__(self, filename, mode):
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
        return json.loads(data)


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

