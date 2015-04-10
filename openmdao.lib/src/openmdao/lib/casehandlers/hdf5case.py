"""
HDF5 Case Recording.
"""

import sys
import os
import time
from uuid import uuid1
import numpy as np

from openmdao.main.api import VariableTree
from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.main.mpiwrap import MPI
from openmdao.main.releaseinfo import __version__

def get_rank():
    '''for debugging'''
    if MPI is None:
        world_rank = 0
    else:
        world_rank = MPI.COMM_WORLD.rank

    return world_rank


def create_group(group, name): # TODO remove calls to this
    pass #print 'create_group', name, group.file.filename, group.file.driver
    #sys.stdout.flush()

def create_dataset(group, name,*arguments, **keywords): # TODO remove calls to this
    pass #print 'create_dataset', name, group.file.filename, group.file.driver
    #sys.stdout.flush()

def dp(s):
    pass #print s
    #sys.stdout.flush()

str_dtype = 'S50'

def write_to_hdf5( group, name, value ):

    filename = group.file.filename

    if isinstance(value,dict):
        dict_grp = group[name]
        for k, v in value.items():
            write_to_hdf5( dict_grp, k, v )
    elif isinstance( value, VariableTree):
        vtree_grp = group[name]
        for k in value.list_vars():
            write_to_hdf5( vtree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        dset = group[name]
        dset[:] = value[:]
    elif isinstance( value, list):
        if len( value ) > 0:
            if isinstance( value[0], str):
                dset = group[name]
                for i,v in enumerate(value): # TODO there must be a better way
                    dset[i] = value[i]
        else:
            pass # TODO How do we handle empty lists? Do not know type
    elif value == None :
        pass # TODO really need to write None here ?
    elif isinstance( value, (np.float64,float)):
        dset = group[name]
        dset[()] = value
    elif isinstance(value,int):
        dset = group[name]
        #dset[0,] = value
        dset[()] = value
    elif isinstance(value,str):
        dset = group[name]
        dset[()] = value
        sys.stdout.flush()
    elif isinstance(value,bool):
        dset = group[name]
        dset[()] = value


def write_groups_to_hdf5( group, name, value ):

    global str_dtype

    filename = group.file.filename

    if isinstance(value,dict):
        dict_grp = create_group(group, name)
        dict_grp = group.create_group(name)
        for k, v in value.items():
            write_groups_to_hdf5( dict_grp, k, v )
    elif isinstance( value, VariableTree): # TODO
        vartree_grp = create_group(group, name)
        vartree_grp = group.create_group(name)
        vartree_grp.attrs['__vartree__'] = True # to indicate this is a var tree
        for k in value.list_vars():
            write_groups_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        if group.file.driver == 'mpio': # cannot do compression when writing in parallel
            create_dataset(group, name, data=value)
            if value.shape == (0,) :
                group.create_dataset(name, data=value, maxshape=(1,)) # h5py does not like completely empty arrays
            else:
                group.create_dataset(name, data=value)
        else:
            create_dataset(group, name, data=value, compression='gzip', chunks=True)
            group.create_dataset(name, data=value, compression='gzip', chunks=True)
    elif isinstance( value, list):
        if len( value ) > 0:
            if isinstance( value[0], str):
                create_dataset(group, name, (len(value),),str_dtype ) # TODO check to make sure it fits
                group.create_dataset(name, (len(value),),str_dtype ) # TODO check to make sure it fits
        else:
            create_dataset(group, name,(0,)) # TODO How do we handle empty lists? Do not know type
            group.create_dataset(name,(0,)) # TODO How do we handle empty lists? Do not know type
    elif value == None : # TODO Need a better way to do this. When using h5diff, we get 'Not comparable' with these values
        create_dataset(group, name,(0,)) # TODO: This results in DATATYPE  H5T_IEEE_F32LE  DATASPACE  SIMPLE { ( 0 ) / ( 0 ) }
        group.create_dataset(name,(0,)) # TODO: This results in DATATYPE  H5T_IEEE_F32LE  DATASPACE  SIMPLE { ( 0 ) / ( 0 ) }
    elif isinstance( value, (np.float64,float)):
        create_dataset(group, name, (), dtype=np.float64)
        group.create_dataset(name, (), dtype=np.float64)
    elif isinstance(value,int):
        create_dataset(group, name, (), dtype=np.int64)
        group.create_dataset(name, (), dtype=np.int64)
    elif isinstance(value,str):
        dset = create_dataset(group, name, (), dtype=str_dtype)
        dset = group.create_dataset(name, (), dtype=str_dtype)
    elif isinstance(value,bool):
        dset = create_dataset(group, name, (), dtype=np.bool)
        dset = group.create_dataset(name, (), dtype=np.bool)






class HDF5CaseRecorder(object):
    """
    Dumps a run in HDF5 form to `out`, which may be a string or a file-like
    object (defaults to ``stdout``). If `out` is ``stdout`` or ``stderr``,
    then that standard stream is used. Otherwise, if `out` is a string, then
    a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    implements(ICaseRecorder)

    def __init__(self, filename='model.hdf5', indent=4, sort_keys=True, max_string_len=50 ): # TODO need an option for the size of the strings

        import h5py  # do it here to avoid warning from autodoc in Sphinx

        self._cfg_map = {}
        self._uuid = None
        self._cases = None

        self.max_string_len = max_string_len

        # not used yet but for getting values of variables
        #     from subcases
        self._last_child_case_uuids = {} # keyed by driver id

        self.filename_base = os.path.splitext(filename)[0]
        if MPI:
            self.hdf5_main_file_object = h5py.File(filename, "w", driver='mpio', comm=MPI.COMM_WORLD) # Has to be since all drivers will create links to their files
        else:
            self.hdf5_main_file_object = h5py.File(filename, "w")


        self.hdf5_case_record_file_objects = {}

        self.case_recording_filenames = {}

        self.is_variable_local_cache = {} # per driver

        self.indent = indent
        self.sort_keys = sort_keys
        self._count = 0

    def startup(self):
        """ Prepare for new run. """
        pass

    def register(self, driver, inputs, outputs):
        """ Register names for later record call from `driver`. """

        import h5py  # do it here to avoid warning from autodoc in Sphinx

        self._cfg_map[driver] = (inputs, outputs)
        scope = driver.parent
        prefix = scope.get_pathname()

        case_recording_filename = '%s_%s_%s.hdf5' % (self.filename_base, prefix, driver.name)

        self.case_recording_filenames[driver.get_pathname()] = case_recording_filename

        if not driver._system.is_active():
            return # do not want to open file if this driver not active on this process

        if driver.workflow._system.mpi.size > 1:
            communicator = driver.workflow._system.mpi.comm # Recommened by Bret. check to see if None, MPI.COMM_NULL
            self.hdf5_case_record_file_objects[driver] = h5py.File(case_recording_filename, "w",driver='mpio', comm=communicator)
        else:
            self.hdf5_case_record_file_objects[driver] = h5py.File(case_recording_filename, "w")


    def record_constants(self, constants):
        """ Record constant data. """

        info = self.get_simulation_info(constants)

        simulation_info_grp = create_group(self.hdf5_main_file_object, "simulation_info")
        simulation_info_grp = self.hdf5_main_file_object.create_group("simulation_info")

        # Can get away with setting the length because all processes participate in the writing of these.
        # TODO: Should do that for all of these values. Just doing it for the graphs since they can get really big
        dset = create_dataset(simulation_info_grp, 'comp_graph', (), dtype=np.dtype((np.str, len(info['comp_graph']))))
        dset = simulation_info_grp.create_dataset('comp_graph', (), dtype=np.dtype((np.str, len(info['comp_graph']))))
        dset = create_dataset(simulation_info_grp, 'graph', (), dtype=np.dtype((np.str, len(info['graph']))))
        dset = simulation_info_grp.create_dataset('graph', (), dtype=np.dtype((np.str, len(info['graph']))))
        write_groups_to_hdf5( simulation_info_grp, 'OpenMDAO_Version', info['OpenMDAO_Version'])
        write_groups_to_hdf5( simulation_info_grp, 'uuid', info['uuid'])
        write_groups_to_hdf5( simulation_info_grp, 'name', info['name'])

        #TODO: should only rank 0 write these data? Can rank 0 be the only one to write this file? Do we even need to open it mpio?
        write_to_hdf5( simulation_info_grp, 'OpenMDAO_Version', info['OpenMDAO_Version'])
        write_to_hdf5( simulation_info_grp, 'comp_graph', info['comp_graph'])
        write_to_hdf5( simulation_info_grp, 'graph', info['graph'])
        write_to_hdf5( simulation_info_grp, 'uuid', info['uuid'])
        write_to_hdf5( simulation_info_grp, 'name', info['name'])

        # Constants
        constants_grp = create_group(simulation_info_grp, "constants")
        constants_grp = simulation_info_grp.create_group("constants")
        for k,v in info['constants'].items():
            write_groups_to_hdf5( constants_grp, k, v )
            write_to_hdf5( constants_grp, k, v )

       #Expressions
        expressions_grp = create_group(simulation_info_grp, "expressions")
        expressions_grp = simulation_info_grp.create_group("expressions")
        for k,v in info['expressions'].items():
            write_groups_to_hdf5( expressions_grp, k, v )

        for k,v in info['expressions'].items():
           write_to_hdf5( expressions_grp, k, v )

        # Variable metadata
        variable_metadata_grp = create_group(simulation_info_grp, "variable_metadata")
        variable_metadata_grp = simulation_info_grp.create_group("variable_metadata")

        for k in sorted(info['variable_metadata']):
            v = info['variable_metadata'][k]
            write_groups_to_hdf5( variable_metadata_grp, k, v )

        for k,v in info['variable_metadata'].items():
            write_to_hdf5( variable_metadata_grp, k, v )

        # Drivers
        for i, info in enumerate(self.get_driver_info()):
            driver_info_name = 'driver_info_%s' % (i+1)
            driver_info_group = create_group(self.hdf5_main_file_object, driver_info_name)
            driver_info_group = self.hdf5_main_file_object.create_group(driver_info_name)
            for k,v in info.items():
                write_groups_to_hdf5( driver_info_group, k, v )
                write_to_hdf5( driver_info_group, k, v )


    def is_variable_local( self, driver, prefix, name ):
        '''Check to see if the given variable is available locally on this process'''

        if driver not in self.is_variable_local_cache:
            self.is_variable_local_cache[ driver ] = {}

        # Use the cached value if available
        if name in self.is_variable_local_cache[ driver ]:
            return self.is_variable_local_cache[ driver ][ name ]

        if prefix:
            name = name[ len(prefix) + 1 : ]

        if name.endswith('workflow.itername'):
            dname = name.replace('workflow.itername', 'itername')
            is_local = driver.workflow._system.is_variable_local( dname )
        else:
            is_local = driver.workflow._system.is_variable_local( name )

        self.is_variable_local_cache[ driver ][ name ] = is_local # save it away for next time
        return is_local

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """ Dump the given run data. """

        hdf5_file_object = self.hdf5_case_record_file_objects[driver]

        info = self.get_case_info(driver, inputs, outputs, exc,
                                  case_uuid, parent_uuid)

        self._cases += 1
        iteration_case_name = 'iteration_case_%s' % self._cases

        int_names = []
        float_names = []
        str_names = []

        # create dataset for the record metadata: _driver_id, _id, _parent_id, error_message, error_status, timestamp
        # From Python and HDF5 Book on Safari Online
        if not "/metadatatype" in hdf5_file_object: # TODO use actual string lengths
            hdf5_file_object['metadatatype'] =  np.dtype([
                ('_driver_id', 'i8'),
                ('_driver_name', np.str_, 40),
                ('_id', np.str_, 40),
                ('_parent_id', np.str_, 40),
                ('_itername', np.str_, 40),
                ('error_message', np.str_, 40),
                ('error_status', 'i8'),
                ('timestamp', 'f8')]
                )

        self._count += 1
        iteration_case_group = create_group(hdf5_file_object, iteration_case_name)
        iteration_case_group = hdf5_file_object.create_group(iteration_case_name)
        metadata_dset = create_dataset(iteration_case_group, "metadata",(1,), dtype=hdf5_file_object['metadatatype'])
        metadata_dset = iteration_case_group.create_dataset("metadata",(1,), dtype=hdf5_file_object['metadatatype'])
        data_grp = create_group( iteration_case_group, 'data' )
        data_grp = iteration_case_group.create_group( 'data' )
        for k,v in info.items():
            if k != 'data': # record metadata # TODO get rid of this clause
                # write_groups_to_hdf5( iteration_case_group, k, v )
                pass
            else:
                max_str_len = 0
                for name,value in v.items():
                    if isinstance(value,int):
                        int_names.append(name)
                    elif isinstance(value,( np.float64, float )):
                        float_names.append(name)
                    elif isinstance(value, str):
                        str_names.append(name)
                        max_str_len = max( max_str_len, len(value))
                    else:
                        write_groups_to_hdf5(data_grp, name, value )

        # only add this info once per record file
        if not "/float_names" in hdf5_file_object:
            create_dataset(hdf5_file_object, 'float_names', data=np.array( float_names ) )
            hdf5_file_object.create_dataset('float_names', data=np.array( float_names ) )
        if not "/int_names" in hdf5_file_object: # only add this info once per record file
            create_dataset(hdf5_file_object, 'int_names', data=np.array( int_names ) )
            hdf5_file_object.create_dataset('int_names', data=np.array( int_names ) )
        if not "/str_names" in hdf5_file_object: # only add this info once per record file
            create_dataset(hdf5_file_object, 'str_names', data=np.array( str_names ) )
            hdf5_file_object.create_dataset('str_names', data=np.array( str_names ) )

        # Create the datasets for the int and float and string arrays
        int_arrays_dset = create_dataset(data_grp, 'array_of_ints', (len(int_names),),dtype=np.int64)
        int_arrays_dset = data_grp.create_dataset('array_of_ints', (len(int_names),),dtype=np.int64)
        float_arrays_dset = create_dataset(data_grp, 'array_of_floats', (len(float_names),),dtype=np.float64)
        float_arrays_dset = data_grp.create_dataset('array_of_floats', (len(float_names),),dtype=np.float64)
        #str_arrays_dset = data_grp.create_dataset('array_of_strs', (len(str_names),),str_dtype )
        str_arrays_dset = create_dataset(data_grp, 'array_of_strs', (len(str_names),),str_dtype )
        str_arrays_dset = data_grp.create_dataset('array_of_strs', (len(str_names),),str_dtype )

        scope = driver.parent
        prefix = scope.get_pathname()

        ##### Write the datasets using only the data available to this process ######
        data_grp = iteration_case_group['data']

        dp( 'determine metadata' )
        metadata = []
        for name in [ '_driver_id', '_driver_name', '_id', '_parent_id', '_itername', 'error_message', 'error_status', 'timestamp'] :
            value = info[ name ]
            if name == 'error_status' and value == None :
                from sys import maxint
                value = maxint
            metadata.append( value )

        dp('set metadata_dset' )
        metadata_dset[()] = np.array([ tuple(metadata), ], dtype = hdf5_file_object['metadatatype'])

        dp('set values in data')
        for name, value in info[ 'data' ].items():

            print_var = self.is_variable_local( driver, prefix, name )

            if print_var:
                if isinstance(value,int):
                    idx = int_names.index(name) # where in the index is this value?
                    int_arrays_dset[idx] = value
                elif isinstance(value,(np.float64,float)):
                    idx = float_names.index(name) # where in the index is this value?
                    float_arrays_dset[idx] = value
                elif isinstance(value,str):
                    idx = str_names.index(name) # where in the index is this value?
                    str_arrays_dset[idx] = value
                else:
                    write_to_hdf5( data_grp, name, value )

        dp('exit record')


    def close(self):
        """
        Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`.
        """

        import h5py  # do it here to avoid warning from autodoc in Sphinx

        for hdf5_case_record_file in self.hdf5_case_record_file_objects.values() :
            hdf5_case_record_file.close()

        # if 1 or not MPI or get_rank() == 0 : # only rank 0 process needs to write the primary case recording file

        # add the individual case recording files to the main hdf5 file
        iteration_case_grp = create_group(self.hdf5_main_file_object, "iteration_cases")
        iteration_case_grp = self.hdf5_main_file_object.create_group("iteration_cases")

        for driver_path, filename in self.case_recording_filenames.items():
            # Create an external link to the root group "/" in the driver specific iteration cases
            # B['External'] = h5py.ExternalLink("dset.h5", "/dset")
            iteration_case_grp[driver_path] = h5py.ExternalLink(filename, "/") # root should work
        self.hdf5_main_file_object.close()

        self._cases = None

    def get_simulation_info(self, constants):
        """ Return simulation info dictionary. """
        # Locate top level assembly from first driver registered.
        top = self._cfg_map.keys()[0].parent
        while top.parent:
            top = top.parent
        #prefix_drop = len(top.name)+1 if top.name else 0
        prefix_drop = 0

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
        self._cases = 0 # TODO this has to be coordinated across processes???

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
        #prefix_drop = len(top.name) + 1 if top.name else 0
        prefix_drop = 0

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

        scope = driver.parent
        prefix = scope.get_pathname()
        if prefix:
            prefix += '.'
        in_names = [prefix+name for name in in_names]
        out_names = [prefix+name for name in out_names]

        data = dict(zip(in_names, inputs))
        data.update(zip(out_names, outputs))

        return dict(_id=case_uuid,
                    _parent_id=parent_uuid or self._uuid,
                    _driver_id=id(driver),
                    _itername = driver.workflow.itername,
                    _driver_name = driver.get_pathname(),
                    #subdriver_last_case_uuids = subdriver_last_case_uuids,
                    error_status=None,
                    error_message=str(exc) if exc else '',
                    timestamp=time.time(),
                    data=data)

    def get_iterator(self):
        """ Just returns None. """
        return None
