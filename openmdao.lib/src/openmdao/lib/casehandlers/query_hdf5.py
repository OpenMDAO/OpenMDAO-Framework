from weakref import ref

import numpy as np

from openmdao.main.api import  VariableTree
from openmdao.lib.casehandlers.query import  DictList, ListResult

_GLOBAL_DICT = dict(__builtins__=None)

class CaseDatasetHDF5(object):
    """
    Reads case data from `filename` and allows queries on it.

    To get all case data::

        cds = CaseDataset('recorded.hdf5', 'hdf5')
        cases = cds.data.fetch()

    To get names of columns returned::

        names = cds.data.var_names().fetch()

    To select a specific set of variables::

        cases = cds.data.vars(['top.sub.comp.x, top.sub.comp.y']).fetch()

    To get a case and all its child cases::

        cases = cds.data.parent_case(parent_itername).fetch()

    To get cases for a particular driver::

        cases = cds.data.driver(driver_name).fetch()

    To get cases for a particular run of a particular driver::

        cases = cds.data.driver(driver_name).parent_case(parent_itername).fetch()

    Other possibilities exist, see :class:`QueryHDF5`.


    """

    def __init__(self, filename, format):
        format = format.lower()
        if format == 'hdf5':
            self._reader = _HDF5Reader(filename)
        else:
            raise ValueError("dataset format must be 'hdf5'")

        # TODO: Clean up some of the old _id variables that we do not use any more
        self._query_id = self._query_itername = self._parent_id = self._parent_itername = self._driver_id = self._driver_name = None
        self._case_ids = self._drivers = self._case_iternames = None
        self.metadata_names = ['_id', '_parent_id', '_driver_id', '_driver_name', '_itername', 'error_status',
                          'error_message', 'timestamp']

    @property
    def data(self):
        """ :class:`Query` object. """
        return QueryHDF5(self)

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


        if query.vnames:
            tmp = []
            for name in self.metadata_names:
                if name in query.vnames:
                    tmp.append(name)
            self.metadata_names = tmp
            names = query.vnames
        else:
            if query.driver_name:
                #driver_info = self._drivers[self._driver_id]
                driver_info = self._drivers[self._driver_name]
                prefix = driver_info['prefix']
                all_names = [prefix+name
                             for name in driver_info['recording']]
            else:
                all_names = []
                for driver_info in self._drivers.values():
                    prefix = driver_info['prefix']
                    all_names.extend([prefix+name
                                      for name in driver_info['recording']])
            names = sorted(all_names+self.metadata_names)

        if query.names:
            # Returning single row, not list of rows.
            return names

        nan = float('NaN')
        rows = ListResult()
        state = {}  # Retains last seen values.
        for case_data in self._reader.cases():
            data = case_data['data']
            metadata = case_data['metadata']
            case_id = metadata['_id']
            case_driver_id = metadata['_driver_id']
            case_driver_name = metadata['_driver_name']
            case_itername = metadata['_itername']

            prefix = self._drivers[case_driver_name]['prefix']
            #prefix = self._drivers[case_driver_id]['prefix']
            if prefix:
                # Make names absolute.
                pass
                #data = dict([(prefix+name, value)
                 #            for name, value in data.items()])
            else:
                data = data.copy()  # Don't modify reader version.

            state.update(data)

            # Filter on driver.
            if self._driver_name is not None and \
               case_driver_name != self._driver_name:
                continue
            #if self._driver_id is not None and \
               #case_driver_id != self._driver_id:
                #continue

            ## Filter on case.
            #if self._case_ids is None or case_id in self._case_ids:
            if self._case_iternames is None or case_itername in self._case_iternames:
                for name in self.metadata_names:
                    data[name] = case_data['metadata'][name]

                row = DictList(names)
                for name in names:
                    if query.local_only:
                        if name in self.metadata_names:
                            row.append(data[name])
                        else:
                            driver = self._drivers[case_driver_name]
                            # driver = self._drivers[case_driver_id]
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

            #if case_id == self._query_id or case_id == self._parent_id:
                #break  # Parent is last case recorded.
            if case_itername == self._query_itername or case_itername == self._parent_itername:
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
        raise NotImplementedError

    def _setup(self, query):
        """ Setup for processing `query`. """
        if query.vnames is not None:
            bad = []
            metadata = self.simulation_info['variable_metadata']
            expressions = self.simulation_info['expressions']
            for name in query.vnames:
                if name not in metadata and name not in [ e['pcomp_name'] for e in expressions.values()] and name not in self.metadata_names:
                    bad.append(name)
            if bad:
                raise RuntimeError('Names not found in the dataset: %s' % bad)

        self._drivers = {}
        self._driver_id = None
        self._driver_name = None
        for driver_info in self._reader.drivers():
            _id = driver_info['_id']
            name = driver_info['name']
            prefix, _, name = name.rpartition('.')
            if prefix:
                prefix += '.'
            driver_info['prefix'] = prefix
            self._drivers[driver_info['name'] ] = driver_info
            if ( driver_info['name'] ) == query.driver_name:
                self._driver_name = query.driver_name
                #self._driver_id = _id

        # self._drivers = {}
        # self._driver_id = None
        # for driver_info in self._reader.drivers():
        #     _id = driver_info['_id']
        #     name = driver_info['name']
        #     prefix, _, name = name.rpartition('.')
        #     if prefix:
        #         prefix += '.'
        #     driver_info['prefix'] = prefix
        #     self._drivers[_id] = driver_info
        #     if driver_info['name'] == query.driver_name:
        #         self._driver_id = _id

        if query.driver_name:
            #if self._driver_id is None:
            if self._driver_name is None:
                raise ValueError('No driver named %r' % query.driver_name)

        self._case_ids = None
        self._query_id = None
        self._parent_id = None
        #if query.case_id is not None:
            #self._query_id = query.case_id
            #self._case_ids = set((self._query_id,))
            ##self._driver_id = None  # Case specified, ignore driver.
            #self._driver_name = None  # Case specified, ignore driver.
        if query.case_itername is not None:
            self._query_itername = query.case_itername
            self._case_iternames = set((self._query_itername,))
            #self._driver_id = None  # Case specified, ignore driver.
            self._driver_name = None  # Case specified, ignore driver.


        #elif query.parent_id is not None: # TODO - fix this
        elif query.parent_itername is not None: # TODO - fix this
            self._parent_itername = query.parent_itername
            self._case_iternames = set((self._parent_itername,))
            parent_itername_parts = self._parent_itername.split('-')
            for case_data in self._reader.cases():
                itername = case_data['metadata']['_itername']
                itername_parts = itername.split('-')
                if len(parent_itername_parts) + 1 == len(itername_parts) and itername_parts[:-1] == parent_itername_parts:
                    self._case_iternames.add(itername)


    def restore(self, assembly, case_id):
        """ Restore case `case_id` into `assembly`. """
        raise NotImplementedError


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


class QueryHDF5(object):
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
        self.case_itername = None
        self.parent_id = None
        self.parent_itername = None
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
        raise NotImplementedError
        #if format is None:
            #if isinstance(self._dataset._reader, _BSONReader):
                #format = 'bson'
            #else:
                #format = 'json'
        #self._dataset._write(self, out, format)

    def driver(self, driver_name):
        """ Filter the cases to those recorded by the named driver. """
        self.driver_name = driver_name
        return self

    def case(self, case_itername):
        """ Return this case. """
        self.case_itername = case_itername
        self.parent_itername = None
        return self

    def parent_case(self, parent_case_id):
        """ Filter the cases to only include this case and its children. """
        self.parent_id = parent_case_id
        self.parent_itername = parent_case_id
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




class _HDF5Reader(object):
    """ Reads a :class:`HDF5CaseRecorder` file. """

    def __init__(self, filename):
        import h5py  # import it here to get rid of autodoc warning
        self._inp = h5py.File(filename,'r')

        self._simulation_info = self.read_simulation_info()
        self._state = 'drivers'
        self._info = None

    @property
    def simulation_info(self):
        """ Simulation info dictionary. """
        return self._simulation_info

    def read_iteration_case_from_hdf5( self, hdf5file, driver_name, iteration_case_name ) :

        info = {}

        driver_grp = self._inp['/iteration_cases'][driver_name]
        iteration_grp = self._inp['/iteration_cases'][driver_name][iteration_case_name]

        info['metadata'] = self.read_from_hdf5(iteration_grp['metadata'])

        data_grp = iteration_grp['data']
        info['data'] = {}

        # read the names of the floats, ints and strings in the array_of_... arrays
        float_names = driver_grp['float_names']
        int_names = driver_grp['int_names']
        str_names = driver_grp['str_names']
        for i, name in enumerate(float_names):
            info['data'][name] = data_grp['array_of_floats'][i]
        for i, name in enumerate(str_names):
            info['data'][name] = data_grp['array_of_strs'][i]
        for i, name in enumerate(int_names):
            info['data'][name] = data_grp['array_of_ints'][i]

        for name in data_grp.keys():
            if name not in ['array_of_ints','array_of_strs', 'array_of_floats']:
                if '__vartree__' in data_grp[name].attrs:
                    info['data'][name] = {}
                    for n, v in data_grp[name].items():
                        info['data'][name][n] = self.read_from_hdf5(data_grp[name][n])
                info['data'][name] = self.read_from_hdf5(data_grp[name])

        return info

    def read_from_hdf5(self, value ):

        import h5py  # do it here to avoid warning from autodoc in Sphinx

        # If value is an HDF5 Group do
        if isinstance(value, h5py._hl.group.Group):
            d = {}
            group = value
            # Loop over what is inside that group
            for name, value in group.attrs.items() :
                d[ name ] = self.read_from_hdf5( value )
            for name, value in group.items() :
                d[ name ] = self.read_from_hdf5( value )
            return d
        elif value.dtype.names :    # compound type
            d = {}
            for name in value.dtype.names:
                d[ name ] = value[ name ][0]
            return d
        else: # it is just a value so return it
            return value[()]

    def read_simulation_info( self ):
        sim_info_grp = self._inp['simulation_info'] # the HDF5 simulation_info group

        # This group contains:
            # attributes
            # other groups
            # datasets

        sim_info = {}

        # Loop over attributes
        for name, value in sim_info_grp.attrs.items() :
            sim_info[ name ] = self.read_from_hdf5( value )

        # Loop over non attributes. e.g. datasets and groups?
        for name, value in sim_info_grp.items() :
            sim_info[ name ] = self.read_from_hdf5( value )

        return sim_info

        # to get at attributes use
        #    self._inp['simulation_info/expressions'].get('comp.x').attrs.keys()
        #    self._inp['simulation_info'].attrs.keys()


        # to get at non attributes
        #  self._inp['simulation_info/expressions'].keys()


    def drivers(self):
        """ Return list of 'driver_info' dictionaries. """

        driver_info = []

        # Loop over all the groups with names like '/driver_info_nnn'
        for name in self._inp.keys() :
            if name.startswith( 'driver_info_'):
                driver_info.append( self.read_from_hdf5( self._inp[name] ) )

        return driver_info

    def cases(self):
        """ Return sequence of 'iteration_case' dictionaries. """

        iteration_cases_grp = self._inp['/iteration_cases']
        case_timestamps = {}
        for driver_name in iteration_cases_grp:
            for iteration_case_name in iteration_cases_grp[driver_name] :
                if iteration_case_name.startswith('iteration_case_') :
                    timestamp = iteration_cases_grp[driver_name][iteration_case_name]['metadata']['timestamp'][0]
                    case_timestamps[timestamp] = ( driver_name, iteration_case_name )

        sorted_timestamps = sorted( case_timestamps )
        for timestamp in sorted_timestamps:
            driver_name, iteration_case_name = case_timestamps[ timestamp ]
            info = self.read_iteration_case_from_hdf5( self._inp, driver_name, iteration_case_name )
            yield info



    def _next(self):
        """ Return next dictionary of data. """
        pass
