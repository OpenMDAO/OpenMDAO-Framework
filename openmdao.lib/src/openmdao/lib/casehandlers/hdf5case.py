"""
JSON/BSON Case Recording.
"""

import cStringIO
import StringIO
import logging
import sys
import time
import os
import inspect

from numpy  import ndarray
from struct import pack
from uuid   import uuid1

from openmdao.main.api import VariableTree
from openmdao.main.interfaces import implements, ICaseRecorder
from openmdao.main.releaseinfo import __version__
from openmdao.util.typegroups import real_types


import h5py
import numpy as np

from openmdao.main.mpiwrap import MPI

#from mpi4py import MPI


# print_metadata_log = False

# The string size for the names of the variables in 
#   the array of strings and also string variables themselves
str_dtype = 'S50'  



def write_to_hdf5( group, name, value ):
    


    filename = group.file.filename

    if MPI:
        rank = MPI.COMM_WORLD.rank
    else:  
        rank = 0

    if isinstance(value,dict):
        pass #print 'write_to_hdf5 dict', rank, name, value
        dict_grp = group[name]
        for k, v in value.items():
            write_to_hdf5( dict_grp, k, v )
    elif isinstance( value, VariableTree): #TODO
        #print "write_to_hdf5 VariableTree"
        vtree_grp = group[name]
        for k in value.list_vars():
            write_to_hdf5( vtree_grp, k, value.get(k) )
    #     pass #print 'create VariableTree', name, MPI.COMM_WORLD.rank
    #     vartree_grp = group.create_group(name)
    #     #QQQQQvartree_grp.attrs['__vartree__'] = True
    #     for k in value.list_vars():
    #         write_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        # print 'write_to_hdf5 np.ndarray', rank, filename, name, value.shape
        dset = group[name] 
        dset[:] = value[:]
    elif isinstance( value, list):
        if len( value ) > 0:
            if isinstance( value[0], str):
                # print 'write_to_hdf5 str list', rank, filename, name, len ( value )
                dset = group[name] 
                for i,v in enumerate(value): # TODO there must be a better way
                    dset[i] = value[i]
        else:
            pass #print 'write_to_hdf5 unknown list', rank, name, value
            # TODO How do we handle empty lists? Do not know type
    elif value == None :
        # print 'write_to_hdf5 None', rank, filename,  name
        #group.attrs[name] = np.array([]) # TODO really need to write None here
        pass
    elif isinstance( value, (np.float64,float)):
        # print 'write_to_hdf5 np.float64 or float', rank, filename, name, value
        dset = group[name] 
        dset[()] = value
    elif isinstance(value,int):
        # print 'write_to_hdf5 int', rank, filename, name, value
        dset = group[name] 
        #dset[0,] = value
        dset[()] = value
    elif isinstance(value,str):
        # print 'write_to_hdf5 string', rank, filename, name, len( value )
        dset = group[name] 
        #dset[0,] = value
        dset[()] = value
        sys.stdout.flush()
    elif isinstance(value,bool):
        # print 'write_to_hdf5 bool', rank, filename, name, value
        dset = group[name] 
        #dset[0,] = value
        dset[()] = value


def write_groups_to_hdf5( group, name, value ):

    # global print_metadata_log

    global str_dtype
    
    # print 'print_metadata_log', print_metadata_log

    filename = group.file.filename

    if MPI:
        rank = MPI.COMM_WORLD.rank
    else:  
        rank = 0

    # print name, value, type(value)

    # if print_metadata_log:
    #     print 'write_groups_to_hdf5_metadata', group.name, name, rank

    if isinstance(value,dict):
        # print 'write_groups_to_hdf5 group dict', filename, rank,  group.name, name
        dict_grp = group.create_group(name)
        #dict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
        for k, v in value.items():
            write_groups_to_hdf5( dict_grp, k, v )
    elif isinstance( value, VariableTree): # TODO
        #print "write_groups_to_hdf5 VariableTree", name

    #     pass #print 'create VariableTree', name, MPI.COMM_WORLD.rank
        vartree_grp = group.create_group(name)
        vartree_grp.attrs['__vartree__'] = True
        for k in value.list_vars():
            write_groups_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        # print 'write_groups_to_hdf5 dataset np.ndarray', filename, rank,  group.name, name
        if group.file.driver == 'mpio': # cannot do compression when writing in parallel
            group.create_dataset(name, data=value)
        else:
            group.create_dataset(name, data=value, compression='gzip', chunks=True)
    elif isinstance( value, list):
        if len( value ) > 0:
            if isinstance( value[0], str):
                # print 'write_groups_to_hdf5 dataset strlist', filename, rank,  group.name, name
                # dt = h5py.special_dtype(vlen=bytes)
                # group.create_dataset(name, (len(value),), dtype=dt)
                group.create_dataset(name, (len(value),),str_dtype ) # TODO check to make sure it fits
        else:
            # print 'write_groups_to_hdf5 dataset unknownlist', filename, rank,  group.name, name
            group.create_dataset(name,(0,)) # TODO How do we handle empty lists? Do not know type
    elif value == None : # TODO Need a better way to do this. When using h5diff, we get 'Not comparable' with these values
        # print 'write_groups_to_hdf5 dataset None', filename, rank,  group.name, name
        group.create_dataset(name,(0,)) # TODO: This results in DATATYPE  H5T_IEEE_F32LE  DATASPACE  SIMPLE { ( 0 ) / ( 0 ) }
    elif isinstance( value, (np.float64,float)):
        # print 'write_groups_to_hdf5 dataset np.float64 or float', filename, rank,  group.name, name
        #group.create_dataset(name, (1,), dtype=np.float64)
        group.create_dataset(name, (), dtype=np.float64)
    elif isinstance(value,int):
        # print 'write_groups_to_hdf5 dataset int', filename, rank,  group.name, name
        #group.create_dataset(name, (1,), dtype='i')
        group.create_dataset(name, (), dtype=np.int64)
    elif isinstance(value,str):
        # print 'write_groups_to_hdf5 dataset string', filename, rank,  group.name, name
        if len(value) > 50:
            raise ValueError('string will not fit in space allocated for in HDF5 file')
        dset = group.create_dataset(name, (), dtype=str_dtype)
        # dt = h5py.special_dtype(vlen=bytes)
        # #dset = group.create_dataset(name, (1,), dtype=dt)
        # dset = group.create_dataset(name, (), dtype=dt)
    elif isinstance(value,bool):
        # print 'write_groups_to_hdf5 dataset bool', filename, rank,  group.name, name
        #dset = group.create_dataset(name, (1,), dtype=np.bool)
        dset = group.create_dataset(name, (), dtype=np.bool)















def get_rank():
    if MPI is None:
        world_rank = 0
    else:
        world_rank = MPI.COMM_WORLD.rank

    return world_rank





import logging

logger = logging.getLogger()





class HDF5CaseRecorder(object):
    """
    Dumps a run in HDF5 form to `out`, which may be a string or a file-like
    object (defaults to ``stdout``). If `out` is ``stdout`` or ``stderr``,
    then that standard stream is used. Otherwise, if `out` is a string, then
    a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    implements(ICaseRecorder)

    def __init__(self, out='cases.hdf5', indent=4, sort_keys=True, max_string_len=50 ): # TODO need an option for the size of the strings
        self._cfg_map = {}
        self._uuid = None
        self._cases = None

        self.max_string_len = max_string_len

        # print 'in HDF5CaseRecorder.__init__', get_rank()

        # not used yet but for getting values of variables
        #     from subcases
        self._last_child_case_uuids = {} # keyed by driver id

        #self.hdf5_main_file_object = h5py.File(out, "w", driver='mpio', comm=MPI.COMM_WORLD)

        if MPI:
            pass #print 'type(MPI.COMM_WORLD)', type(MPI.COMM_WORLD)


        if MPI:
            logging.getLogger().info("MPI")
            self.hdf5_main_file_object = h5py.File(out, "w", driver='mpio', comm=MPI.COMM_WORLD) # Has to be since all drivers will create links to their files
        else:
            logging.getLogger().info("not MPI")
            self.hdf5_main_file_object = h5py.File(out, "w")


        self.hdf5_case_record_file_objects = {}

        self.case_recording_filenames = {}

        self.is_variable_local_cache = {} # per driver

        # print_metadata_log = False

        if MPI:
            pass #print 'COMM_WORLD', MPI.COMM_WORLD

        #self.hdf5_main_file_object.atomic = True 

        self.indent = indent
        self.sort_keys = sort_keys
        self._count = 0

    def startup(self):
        """ Prepare for new run. """
        pass

    #def register(self, driver, inputs, outputs,inputs_all_processes, outputs_all_processes):
    def register(self, driver, inputs, outputs):
        """ Register names for later record call from `driver`. """

        # import pprint
        # print 'in HDF5CaseRecorder.register', driver.name, get_rank()
        # print pprint.pprint( inputs ) 
        # print pprint.pprint( outputs ) 

        #import pdb; pdb.set_trace()
        self._cfg_map[driver] = (inputs, outputs)
        #self._cfg_map[driver] = (inputs_all_processes, outputs_all_processes)

        #self.inputs_all_processes = inputs_all_processes
        #self.outputs_all_processes = outputs_all_processes
        scope = driver.parent
        prefix = scope.get_pathname()

        # pass #print 'check if seg3.driver.directory local', driver.workflow._system.is_variable_local( 'seg3.driver.directory' )
        #pass #print 'check if seg3.driver.itername local', driver.get_pathname(), driver.workflow._system.is_variable_local( 'seg3.driver.itername`' ), MPI.COMM_WORLD.rank
        # pass #print 'check if s3 local', driver.workflow._system.is_variable_local( 's3' )

        pass #print 'driver.get_pathname()', driver.get_pathname()

        pass #print 'driver.workflow._system', driver.workflow._system
        pass #print 'driver.workflow._system.subsystems()', driver.workflow._system.subsystems()

        if MPI:
            pass #print 'isactive: driver, rank, active', driver.get_pathname(), MPI.COMM_WORLD.rank, driver._system.is_active()

        # from openmdao.main.systems import SerialSystem, ParallelSystem
        # communicator = None
        # for ss in driver.workflow._system.subsystems():
        #     if isinstance( ss, ParallelSystem):
        #         pass #print 'ParallelSystem comm', ss.mpi.comm
        #         communicator = ss.mpi.comm

        pass #print "for driver, in workflow", prefix + "."+driver.name, ",".join([c.name for c in driver.workflow])
        pass #print "for driver, recording", prefix + "."+driver.name, ",".join(inputs+outputs)

        if MPI:
            pass #print 'register driver, drivername, rank, system, prefix', driver, driver.name, MPI.COMM_WORLD.rank, driver.workflow._system, prefix
        #self.hdf5_file_objects[driver] = h5py.File('cases_%s_%s.hdf5' % (prefix, driver.name), "w", driver='mpio', comm=MPI.COMM_WORLD)

        case_recording_filename = 'cases_%s_%s.hdf5' % (prefix, driver.name)
        pass #print 'opening HDF5 file', case_recording_filename

        self.case_recording_filenames[driver.get_pathname()] = case_recording_filename

        if not driver._system.is_active():   
            return # do not want to open file if this driver not active on this process

        ##########   restore   
        if driver.workflow._system.get_req_cpus() > 1: 
            communicator = driver.workflow._system.mpi.comm # Recommened by Bret. check to see if None, MPI.COMM_NULL
            pass #print 'driver is  parallel', driver.get_pathname(), communicator, driver.workflow._system.get_req_cpus()
            self.hdf5_case_record_file_objects[driver] = h5py.File(case_recording_filename, "w",driver='mpio', comm=communicator)
        else:
            pass #print 'driver is not parallel', driver.get_pathname()
            self.hdf5_case_record_file_objects[driver] = h5py.File(case_recording_filename, "w")

        # print 'exiting HDF5CaseRecorder.register', get_rank()



    def record_constants(self, constants):
        """ Record constant data. """

        # global print_metadata_log

        # print 'in HDF5CaseRecorder.record_constants', get_rank()



        info = self.get_simulation_info(constants)



        simulation_info_grp = self.hdf5_main_file_object.create_group("simulation_info")
        

        ##### Just create group structure on all processes using the merged JSON info ######

        write_groups_to_hdf5( simulation_info_grp, 'OpenMDAO_Version', info['OpenMDAO_Version'])
        write_groups_to_hdf5( simulation_info_grp, 'comp_graph', info['comp_graph'])
        write_groups_to_hdf5( simulation_info_grp, 'graph', info['graph'])
        write_groups_to_hdf5( simulation_info_grp, 'uuid', info['uuid'])
        write_groups_to_hdf5( simulation_info_grp, 'name', info['name'])
       


        constants_grp = simulation_info_grp.create_group("constants")
        for k,v in info['constants'].items():
            write_groups_to_hdf5( constants_grp, k, v )




        expressions_grp = simulation_info_grp.create_group("expressions")
        for k,v in info['expressions'].items():
            write_groups_to_hdf5( expressions_grp, k, v )
            

        # print 'starting write_groups_to_hdf5_metadata'
        # print_metadata_log = True
        variable_metadata_grp = simulation_info_grp.create_group("variable_metadata")


        # for k,v in info['variable_metadata'].items():
        for k in sorted(info['variable_metadata']):
            v = info['variable_metadata'][k]
            write_groups_to_hdf5( variable_metadata_grp, k, v )
        # print_metadata_log = False
        # print 'ending write_groups_to_hdf5_metadata'



        ##### Write the datasets using only the data available to this process ######

        write_to_hdf5( simulation_info_grp, 'OpenMDAO_Version', info['OpenMDAO_Version'])
        write_to_hdf5( simulation_info_grp, 'comp_graph', info['comp_graph'])
        write_to_hdf5( simulation_info_grp, 'graph', info['graph'])
        write_to_hdf5( simulation_info_grp, 'uuid', info['uuid'])
        write_to_hdf5( simulation_info_grp, 'name', info['name'])
       
        for k,v in info['constants'].items():
            write_to_hdf5( constants_grp, k, v )

        for k,v in info['expressions'].items():
           write_to_hdf5( expressions_grp, k, v )
            
        # print_metadata_log = True
        for k,v in info['variable_metadata'].items():
            write_to_hdf5( variable_metadata_grp, k, v )
        # print_metadata_log = False


        ##### Just create group structure on all processes using the merged JSON info ######
        for i, info in enumerate(self.get_driver_info()):
            driver_info_name = 'driver_info_%s' % (i+1)
            driver_info_group = self.hdf5_main_file_object.create_group(driver_info_name)
            for k,v in info.items():
                write_groups_to_hdf5( driver_info_group, k, v )
                write_to_hdf5( driver_info_group, k, v ) 

        ##### Write the datasets using only the data available to this process ######
        # for i, info in enumerate(self.get_driver_info()):
        #     # import ppass #print 
        #     # pprint.pprint( info )
        #     driver_info_name = 'driver_info_%s' % (i+1)
        #     for k,v in info.items():
        #         write_to_hdf5( driver_info_group, k, v )

        # print 'in record_constants at end', get_rank()

    def is_variable_local( self, driver, prefix, name ):
        '''Check to see if the given variable is available locally on this process'''

        if driver not in self.is_variable_local_cache:
            self.is_variable_local_cache[ driver ] = {}

        # Use the cached value if available
        if name in self.is_variable_local_cache[ driver ]:
            return self.is_variable_local_cache[ driver ][ name ]

        if name.endswith( '.out0'):
            name_for_local_check = name[:-len('.out0')]
        else:
            name_for_local_check = name
        if prefix:
            name_for_local_check = name_for_local_check[ len(prefix) + 1 : ] 

        is_local = driver.workflow._system.is_variable_local( name_for_local_check )
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
        # TODO: Could make this dynamic so that if we change these in the future, we will not have to change this code

        # From Python and HDF5 Book on Safari Online
        if not "/metadatatype" in hdf5_file_object: 
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

        # comp_type = np.dtype([
        #     ('_driver_id', 'i8'), 
        #     ('_id', np.str_, 40), 
        #     ('_parent_id', np.str_, 40), 
        #     ('error_message', np.str_, 40), 
        #     ('error_status', 'i8'), 
        #     ('timestamp', 'f8')]
        #     )



        ##### Just create group structure on all processes using the merged JSON info ######
        self._count += 1
        iteration_case_group = hdf5_file_object.create_group(iteration_case_name)
        metadata_dset = iteration_case_group.create_dataset("metadata",(1,), dtype=hdf5_file_object['metadatatype'])
        #metadata_dset = iteration_case_group.create_dataset("metadata",(1,), comp_type)
        data_grp = iteration_case_group.create_group( 'data' )
        for k,v in info.items():
            if k != 'data': # record metadata
                # write_groups_to_hdf5( iteration_case_group, k, v ) 
                pass
            else:
                for name,value in v.items():
                    if isinstance(value,int):
                        int_names.append(name)
                    elif isinstance(value,( np.float64, float )):
                        float_names.append(name)
                    elif isinstance(value, str):
                        str_names.append(name)
                    else: 
                        write_groups_to_hdf5(data_grp, name, value )

        # only add this info once per record file
        if not "/float_names" in hdf5_file_object: 
            hdf5_file_object.create_dataset('float_names', data=np.array( float_names ) ) 
        if not "/int_names" in hdf5_file_object: # only add this info once per record file
            hdf5_file_object.create_dataset('int_names', data=np.array( int_names ) ) 
        if not "/str_names" in hdf5_file_object: # only add this info once per record file
            hdf5_file_object.create_dataset('str_names', data=np.array( str_names ) ) 

        # Create the datasets for the int and float arrays
        int_arrays_dset = data_grp.create_dataset('array_of_ints', (len(int_names),),dtype=np.int64)
        float_arrays_dset = data_grp.create_dataset('array_of_floats', (len(float_names),),dtype=np.float64)
        str_arrays_dset = data_grp.create_dataset('array_of_strs', (len(str_names),),str_dtype )

        scope = driver.parent
        prefix = scope.get_pathname()

        ##### Write the datasets using only the data available to this process ######
        data_grp = iteration_case_group['data']

        metadata = [] 
        for name in [ '_driver_id', '_driver_name', '_id', '_parent_id', '_itername', 'error_message', 'error_status', 'timestamp'] :
            value = info[ name ]
            if name == 'error_status' and value == None :
                from sys import maxint
                value = maxint
            metadata.append( value )
                
        metadata_dset[()] = np.array([ tuple(metadata), ], dtype = hdf5_file_object['metadatatype'])
        # metadata_dset[()] = np.array([ tuple(metadata), ], dtype = comp_type)

        for k,v in info.items():
            if k != 'data':
                #write_to_hdf5( iteration_case_group, k, v )
                pass
            else:
                for name,value in v.items():
                    if self.is_variable_local( driver, prefix, name ): # TODO should cache these. 
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



    def close(self):
        """
        Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`.
        """

        # if MPI:
        #     print "in close", MPI.COMM_WORLD.rank, get_rank()

        # if MPI:
        #     MPI.COMM_WORLD.Barrier()


        for hdf5_case_record_file in self.hdf5_case_record_file_objects.values() :
            #print 'whatisin at close', hdf5_case_record_file.filename, hdf5_case_record_file.keys(), get_rank()
            # print 'closing hdf5_case_record_file.filename', hdf5_case_record_file.filename, get_rank()
            hdf5_case_record_file.close()            

        # if MPI:
        #     MPI.COMM_WORLD.Barrier()

        if 1 or not MPI or get_rank() == 0 : # only rank 0 process needs to write the primary case recording file

            # print "closing hdf5_main_file_object", get_rank()
            sys.stdout.flush()


            ################# restore
            # # add the individual case recording files to the main hdf5 file
            iteration_case_grp = self.hdf5_main_file_object.create_group("iteration_cases")

            for driver_path, filename in self.case_recording_filenames.items():
                # Create an external link to the root group "/" in the driver specific iteration cases 
                # B['External'] = h5py.ExternalLink("dset.h5", "/dset")
                # print 'making links to', filename, driver_path, get_rank()
                sys.stdout.flush()
                # if filename != 'cases__driver.hdf5':
                #     continue
                iteration_case_grp[driver_path] = h5py.ExternalLink(filename, "/") # root should work
            self.hdf5_main_file_object.close()
            # print "closed main hdf5_case_recorder", get_rank()



        # if self.out is not None and self._cases is not None:
        #     self.out.write('}\n')

        # if self.out not in (None, sys.stdout, sys.stderr):
        #     if not isinstance(self.out,
        #                       (StringIO.StringIO, cStringIO.OutputType)):
        #         # Closing a StringIO deletes its contents.
        #         self.out.close()
        #     self.out = None

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

        #import pdb; pdb.set_trace()

        scope = driver.parent
        prefix = scope.get_pathname()
        if prefix:
            prefix += '.'
        in_names = [prefix+name for name in in_names]
        out_names = [prefix+name for name in out_names]

        data = dict(zip(in_names, inputs))
        data.update(zip(out_names, outputs))

        # print 'get_case_info'
        # import pprint
        # print pprint.pprint( data )

        #subdriver_last_case_uuids = {}
        #for subdriver in driver.subdrivers():
            #subdriver_last_case_uuids[ id(subdriver) ] = self._last_child_case_uuids[ id(subdriver) ]
        #self._last_child_case_uuids[ id(driver) ] = case_uuid


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

