import bson
import json

from struct import unpack
from weakref import ref


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
        parent_id = None
        if query.parent_id is not None:
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

                row = []
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

            if case_id == parent_id:
                break  # Parent is last case recorded.

        if query.transpose:
            tmp = []
            for i in range(len(rows[0])):
                tmp.append([row[i] for row in rows])
            return tmp
        else:
            return rows


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

    def parent_case(self, parent_case_id):
        """ Filter the cases to only include this case and its children. """
        self.parent_id = parent_case_id
        return self

    def vars(self, vnames):
        """ Filter the variable columns returned in the row. """
        self.vnames = vnames
        return self

    def local(self):
        """
        Restrict the variables returned to only those in the specific driver's
        local set. This means that if there are cases from more than one driver,
        variables not local to that driver will be set to ``NaN``.
        """
        self.local_only = True
        return self

    def transposed(self):
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


class _JSONReader(object):
    """ Reads a :class:`JSONCaseRecorder` file. """

    def __init__(self, filename):
        with open(filename, 'r') as inp:
            self._run_data = json.load(inp)

    @property
    def simulation_info(self):
        """ Simulation info dictionary. """
        return self._run_data['simulation_info']

    def drivers(self):
        """ Return list of 'driver_info' dictionaries. """
        driver_info = []
        count = 0
        while True:
            count += 1
            driver_key = 'driver_info_%s' % count
            if driver_key not in self._run_data:
                break
            driver_info.append(self._run_data[driver_key])
        return driver_info

    def cases(self):
        """ Return sequence of 'iteration_case' dictionaries. """
        count = 0
        while True:
            count += 1
            case_key = 'iteration_case_%s' % count
            if case_key not in self._run_data:
                break
            yield self._run_data[case_key]


class _BSONReader(object):
    """ Reads a :class:`BSONCaseRecorder` file. """

    def __init__(self, filename):
        self._inp = open(filename, 'rb')
        reclen = unpack('<L', self._inp.read(4))[0]
        self._simulation_info = bson.loads(self._inp.read(reclen))
        self._state = 'drivers'
        self._info = None

    @property
    def simulation_info(self):
        """ Simulation info dictionary. """
        return self._simulation_info

    def drivers(self):
        """ Return list of 'driver_info' dictionaries. """
        if self._state != 'drivers':
            self._inp.seek(0)
            reclen = unpack('<L', self._inp.read(4))[0]
            self._inp.read(reclen)  # Re-read 'simulation_info'.

        driver_info = []
        data = self._inp.read(4)
        while data:
            reclen = unpack('<L', data)[0]
            info = bson.loads(self._inp.read(reclen))
            if '_driver_id' not in info:
                driver_info.append(info)
            else:
                self._info = info
                self._state = 'cases'
                return driver_info
            data = self._inp.read(4)
        self._state = 'eof'
        return driver_info

    def cases(self):
        """ Return sequence of 'iteration_case' dictionaries. """
        if self._state != 'cases':
            self.drivers()  # Read up to first case.

        yield self._info  # Read when looking for drivers.
        self._info = None

        data = self._inp.read(4)
        while data:
            reclen = unpack('<L', data)[0]
            yield bson.loads(self._inp.read(reclen))
            data = self._inp.read(4)
        self._state = 'eof'

