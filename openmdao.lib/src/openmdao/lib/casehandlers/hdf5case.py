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

#from openmdao.main.mpiwrap import MPI

from mpi4py import MPI


def write_to_hdf5( group, name, value ):

    if isinstance(value,dict):
        print 'create dict', name, MPI.COMM_WORLD.rank
        #dict_grp = group.create_group(name)
        dict_grp = group[name]
        #QQQQQQdict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
        for k, v in value.items():
            write_to_hdf5( dict_grp, k, v )
    # elif isinstance( value, VariableTree):
    #     print 'create VariableTree', name, MPI.COMM_WORLD.rank
    #     vartree_grp = group.create_group(name)
    #     #QQQQQvartree_grp.attrs['__vartree__'] = True
    #     for k in value.list_vars():
    #         write_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        print 'create np.ndarray', name, MPI.COMM_WORLD.rank
        dset = group[name] 
        dset[:] = value[:]
        #group.create_dataset(name, data=value)
    elif isinstance( value, list):
        if len( value ) > 0:
            if isinstance( value[0], str):
                if name == 'parameters' :
                    #import pdb; pdb.set_trace()
                    pass
                dset = group[name] 
                for i,v in enumerate(value):
                    dset[i] = value[i]
    elif value == None :
        print 'create np.array([])', name, MPI.COMM_WORLD.rank
        ################group.create_dataset(name, data=np.array([]))
        #QQQQgroup.attrs[name] = np.array([])
        pass
    else:
        if isinstance( value, np.float64):
            print 'create np.float64', name, MPI.COMM_WORLD.rank
            dset = group[name] 
            dset[0,] = value
            #group.create_dataset(name, data=np.array([1.2345]))
            #group.create_dataset(name, (100,), dtype='i8')
            #print 'type numpy.float64', value
        else:
            print 'create non numpy scalar', type(value), name, MPI.COMM_WORLD.rank
            #group.create_dataset(name, (100,), dtype='i8')
            if isinstance(value,float):
                #group.create_dataset(name, data=np.array([value]))   
                dset = group[name] 
                dset[0,] = value
            elif isinstance(value,int):
                #group.create_dataset(name, data=np.array([value]))   
                dset = group[name] 
                #dset.attrs[name] = value
                dset[0,] = value
            elif isinstance(value,str):
                dset = group[name] 
                #dset = np.array([value])
                dset[0,] = value
                #dset.attrs[name] = value
                # dt = h5py.special_dtype(vlen=bytes)
                # dset = group.create_dataset(name, (1,), dtype=dt)

                # dset = f.create_dataset('test', (4,), dtype='i')

                # group.create_dataset(name, (1,), dtype='i')   
                # group.create_dataset(name, data=[value,])   
                #group[name] = [value,]

                # dset = group[name] 
                # dset = value


                #group.create_dataset(name, data=value)
                #QQQQgroup.attrs[name] = value
                pass


def write_groups_to_hdf5( group, name, value ):


    print 'write_groups_to_hdf5 group, name', group, name
    if name.startswith('/driver_info_'):
        import pdb; pdb.set_trace()

    data = {
    # '_pseudo_0.out0': 3.1086244689504383e-15, 
    # 'C1.exec_count': 0, 
    # 'C2.itername': '25-C2', 
    # 'driver.workflow.itername': '25', 
    # 'C2.derivative_exec_count': 0, 
    # 'C1.derivative_exec_count': 0, 
    # 'C1.y1': 0.0, 
    # 'C1.y2': 3.7551999999999977, 
    # '_pseudo_1.out0': 1.7763568394002505e-15, 
    # 'C2.y1': 3.1598617599999974, 
    # 'C2.y2': 3.7551999999999994, 
    # 'C1.itername': '', 
    # 'C2.exec_count': 25,
    }


    if name in data.keys() and group.name.startswith('/iteration_case_'):
        return

    if isinstance(value,dict):
        print 'create group for dict', name, MPI.COMM_WORLD.rank
        dict_grp = group.create_group(name)
        #QQQQQQdict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
        for k, v in value.items():
            write_groups_to_hdf5( dict_grp, k, v )
    # elif isinstance( value, VariableTree):
    #     print 'create VariableTree', name, MPI.COMM_WORLD.rank
    #     vartree_grp = group.create_group(name)
    #     #QQQQQvartree_grp.attrs['__vartree__'] = True
    #     for k in value.list_vars():
    #         write_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        print 'create np.ndarray', name, MPI.COMM_WORLD.rank
        pass
        group.create_dataset(name, data=value)
    elif isinstance( value, list):
        if name == 'parameters':
            #import pdb; pdb.set_trace()
            pass
        if len( value ) > 0:
            if isinstance( value[0], str):
                print "creating dataset for list", name
                group.create_dataset(name, (len(value),1),'S50' ) # TODO use variable length strings
        else:
            group.create_dataset(name,(0,)) # TODO How do we handle empty lists? Do not know type
    elif value == None :
        print 'create np.array([])', name, MPI.COMM_WORLD.rank
        group.create_dataset(name,(0,))
        #QQQQgroup.attrs[name] = np.array([])
        pass
    else:
        if isinstance( value, np.float64):
            print 'create np.float64', name, MPI.COMM_WORLD.rank
            group.create_dataset(name, (1,), dtype='f')
            #group.create_dataset(name, data=np.array([1.2345]))
            #group.create_dataset(name, (100,), dtype='i8')
            #print 'type numpy.float64', value
        else:
            print 'create non numpy scalar', type(value), name, MPI.COMM_WORLD.rank
            #group.create_dataset(name, (100,), dtype='i8')
            if isinstance(value,float):
                #group.create_dataset(name, data=np.array([value]))  
                group.create_dataset(name, (1,), dtype='f')
            elif isinstance(value,int):
                #group.create_dataset(name, data=np.array([value]))  
                group.create_dataset(name, (1,), dtype='i')
            elif isinstance(value,str):
                dt = h5py.special_dtype(vlen=bytes)
                #dset = group.create_dataset(name, (1,), dtype=dt)
                dset = group.create_dataset(name, (1,), dtype="S50")
            elif isinstance(value,bool):
                dset = group.create_dataset(name, (1,), dtype=np.bool)
        pass


def write_case_groups_to_hdf5( group, name, value ):


    data = {
    # '_pseudo_0.out0': 3.1086244689504383e-15,  # bad
    # 'C1.exec_count': 0, #bad
    # 'C2.itername': '25-C2', 
    # 'driver.workflow.itername': '25', 
    # 'C2.derivative_exec_count': 0, 
    # 'C1.derivative_exec_count': 0, 
    # 'C1.y1': 0.0, 
    # 'C1.y2': 3.7551999999999977, 
    # '_pseudo_1.out0': 1.7763568394002505e-15, 
    # 'C2.y1': 3.1598617599999974, 
    # 'C2.y2': 3.7551999999999994, 
    # 'C1.itername': '', 
    # 'C2.exec_count': 25,
    }


    #return

    # if group.name.startswith('/iteration_case_5') :
    #     return
    # if not ( group.name.startswith('/iteration_case_1') or group.name.startswith('/iteration_case_2') or group.name.startswith('/iteration_case_3') or group.name.startswith('/iteration_case_4') ):
    #     return
    #print 'write_case_groups_to_hdf5', MPI.COMM_WORLD.rank, name

    if name in data.keys() and group.name.startswith('/iteration_case_'):
        return

    if isinstance(value,dict):
        #print 'create group for dict', name, MPI.COMM_WORLD.rank
        print 'write_case_groups_to_hdf5 dict', MPI.COMM_WORLD.rank, name
        dict_grp = group.create_group(name)
        #QQQQQQdict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
        for k, v in value.items():
            write_case_groups_to_hdf5( dict_grp, k, v )
    # elif isinstance( value, VariableTree):
    #     print 'create VariableTree', name, MPI.COMM_WORLD.rank
    #     vartree_grp = group.create_group(name)
    #     #QQQQQvartree_grp.attrs['__vartree__'] = True
    #     for k in value.list_vars():
    #         write_to_hdf5( vartree_grp, k, value.get(k) )
    elif isinstance( value, np.ndarray):
        print 'write_case_groups_to_hdf5 np.ndarray', MPI.COMM_WORLD.rank, name
        print 'create np.ndarray', name, MPI.COMM_WORLD.rank
        pass
        group.create_dataset(name, data=value)
    elif isinstance( value, list):
        print 'write_case_groups_to_hdf5 list', MPI.COMM_WORLD.rank, name
        if len( value ) > 0:
            if isinstance( value[0], str):
                group.create_dataset(name, (len(value),1),'S50' ) # TODO use variable length strings
                pass
        else:
            group.create_dataset(name,(0,)) # TODO How do we handle empty lists? Do not know type
            pass
    elif value == None :
        print 'write_case_groups_to_hdf5 None', MPI.COMM_WORLD.rank, name
        print 'create np.array([])', name, MPI.COMM_WORLD.rank
        group.create_dataset(name,(0,))
        #QQQQgroup.attrs[name] = np.array([])
        pass
    else:
        if isinstance( value, np.float64):
            print 'write_case_groups_to_hdf5 np.float64', MPI.COMM_WORLD.rank, name
            group.create_dataset(name, (1,), dtype='f')
            print 'create np.float64', name, MPI.COMM_WORLD.rank
            #group.create_dataset(name, data=np.array([1.2345]))
            #group.create_dataset(name, (100,), dtype='i8')
            #print 'type numpy.float64', value
        else:
            print 'create non numpy scalar', type(value), name, MPI.COMM_WORLD.rank
            #group.create_dataset(name, (100,), dtype='i8')
            if isinstance(value,float):
                #group.create_dataset(name, data=np.array([value]))  
                print 'write_case_groups_to_hdf5 float', MPI.COMM_WORLD.rank, name
                group.create_dataset(name, (1,), dtype='f')
                pass
            elif isinstance(value,int):
                #group.create_dataset(name, data=np.array([value]))  
                print 'write_case_groups_to_hdf5 int', MPI.COMM_WORLD.rank, name
                group.create_dataset(name, (1,), dtype='i')
                pass
            elif isinstance(value,str):
                print 'write_case_groups_to_hdf5 str', MPI.COMM_WORLD.rank, name
                dt = h5py.special_dtype(vlen=bytes)
                #dset = group.create_dataset(name, (1,), dtype=dt)
                dset = group.create_dataset(name, (1,), dtype="S50") ### this is causing 'Symbol table node entry:Name out of order at'
            elif isinstance(value,bool):
                print 'write_case_groups_to_hdf5 bool', MPI.COMM_WORLD.rank, name
                dset = group.create_dataset(name, (1,), dtype=np.bool)
                pass
        pass


class HDF5CaseRecorder(object):
    """
    Dumps a run in HDF5 form to `out`, which may be a string or a file-like
    object (defaults to ``stdout``). If `out` is ``stdout`` or ``stderr``,
    then that standard stream is used. Otherwise, if `out` is a string, then
    a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    implements(ICaseRecorder)

    def __init__(self, out='cases.hdf5', indent=4, sort_keys=True):
        self._cfg_map = {}
        self._uuid = None
        self._cases = None

        # not used yet but for getting values of variables
        #     from subcases
        self._last_child_case_uuids = {} # keyed by driver id

        self.hdf5_file_object = h5py.File(out, "w", driver='mpio', comm=MPI.COMM_WORLD)

        self.hdf5_file_object.atomic = True 

        #self.hdf5_file_object = h5py.File(out, "w", driver='mpio' )

        #f = h5py.File('parallel_test.hdf5', 'w', driver='mpio', comm=MPI.COMM_WORLD)


        self.indent = indent
        self.sort_keys = sort_keys
        self._count = 0

    def startup(self):
        """ Prepare for new run. """
        pass

    #def register(self, driver, inputs, outputs,inputs_all_processes, outputs_all_processes):
    def register(self, driver, inputs, outputs):
        """ Register names for later record call from `driver`. """


        self._cfg_map[driver] = (inputs, outputs)
        #self._cfg_map[driver] = (inputs_all_processes, outputs_all_processes)

        #self.inputs_all_processes = inputs_all_processes
        #self.outputs_all_processes = outputs_all_processes

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

        #subdriver_last_case_uuids = {}
        #for subdriver in driver.subdrivers():
            #subdriver_last_case_uuids[ id(subdriver) ] = self._last_child_case_uuids[ id(subdriver) ]
        #self._last_child_case_uuids[ id(driver) ] = case_uuid


        return dict(_id=case_uuid,
                    _parent_id=parent_uuid or self._uuid,
                    _driver_id=id(driver),
                    #subdriver_last_case_uuids = subdriver_last_case_uuids,
                    error_status=None,
                    error_message=str(exc) if exc else '',
                    timestamp=time.time(),
                    data=data)

    def record_constants(self, constants):
        """ Record constant data. """

        info = self.get_simulation_info(constants)

        simulation_info_grp = self.hdf5_file_object.create_group("simulation_info")
        
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
            
        variable_metadata_grp = simulation_info_grp.create_group("variable_metadata")
        for k,v in info['variable_metadata'].items():
            write_groups_to_hdf5( variable_metadata_grp, k, v )

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
            
        for k,v in info['variable_metadata'].items():
            write_to_hdf5( variable_metadata_grp, k, v )


        ##### Just create group structure on all processes using the merged JSON info ######
        for i, info in enumerate(self.get_driver_info()):
            driver_info_name = 'driver_info_%s' % (i+1)
            #import pdb; pdb.set_trace()
            driver_info_group = self.hdf5_file_object.create_group(driver_info_name)
            for k,v in info.items():
                print 'driver key', k, v
                #import pdb; pdb.set_trace()
                write_groups_to_hdf5( driver_info_group, k, v )
                write_to_hdf5( driver_info_group, k, v ) # TODO really only rank 0 should do this
                # driver_info_group.attrs[k] = v

        ##### Write the datasets using only the data available to this process ######
        # for i, info in enumerate(self.get_driver_info()):
        #     # import pprint 
        #     # pprint.pprint( info )
        #     driver_info_name = 'driver_info_%s' % (i+1)
        #     for k,v in info.items():
        #         write_to_hdf5( driver_info_group, k, v )



    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """ Dump the given run data. """

        # qqqqqqqqqqqqqq
        #return

        # import pdb; pdb.set_trace()

        info = self.get_case_info(driver, inputs, outputs, exc,
                                  case_uuid, parent_uuid)
        self._cases += 1
        iteration_case_name = 'iteration_case_%s' % self._cases

        #print "iteration_case_name", iteration_case_name

        ##### Just create group structure on all processes using the merged JSON info ######
        self._count += 1
        iteration_case_group = self.hdf5_file_object.create_group(iteration_case_name)
        for k,v in info.items():
            print 'record case make group', MPI.COMM_WORLD.rank, k, v
            if k != 'dataqq':
                print 'writing iteration', iteration_case_group.name, k
                write_case_groups_to_hdf5( iteration_case_group, k, v ) ########################## even this causes problems

        #qqq
        #return

        ##### Write the datasets using only the data available to this process ######
        for k,v in info.items():
            print 'record case set values', MPI.COMM_WORLD.rank, k 
            if k != 'data':
                write_to_hdf5( iteration_case_group, k, v )
            else:
                print 'writing data'
                data_grp = iteration_case_group[k]
                for name,value in v.items():
                    if driver.workflow._system.is_variable_local( name ):
                        print "islocal true", MPI.COMM_WORLD.rank, name
                        write_to_hdf5( data_grp, name, value )
                    else:
                        print "islocal false", MPI.COMM_WORLD.rank, name



    # def write_to_hdf5_method(self, group, name, value ):

    #     #print 'write_to_hdf5', name, value, type(value)
    #     if isinstance(value,dict):
    #         #print 'create dict', name, MPI.COMM_WORLD.rank
    #         dict_grp = group.create_group(name)
    #         #dict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
    #         for k, v in value.items():
    #             self.write_to_hdf5( dict_grp, k, v )
    #     elif isinstance( value, VariableTree):
    #         #print 'create VariableTree', name, MPI.COMM_WORLD.rank
    #         vartree_grp = group.create_group(name)
    #         #vartree_grp.attrs['__vartree__'] = True
    #         for k in value.list_vars():
    #             self.write_to_hdf5( vartree_grp, k, value.get(k) )
    #     elif isinstance( value, np.ndarray):
    #         #print 'create np.ndarray', name, MPI.COMM_WORLD.rank
    #         group.create_dataset(name, data=value,compression="gzip")
    #     elif value == None :
    #         #print 'create np.array([])', name, type(value), MPI.COMM_WORLD.rank
    #         group.create_dataset(name, data=np.array([]))
    #         #group.attrs[name] = np.array([])
    #         pass
    #     else:
    #         if isinstance( value, np.float64):
    #             #print 'create np.float64', name, MPI.COMM_WORLD.rank
    #             group.create_dataset(name, data=np.array([value]))
    #             #group.create_dataset(name, (100,), dtype='i8')
    #             #print 'type numpy.float64', value
    #         else:
    #             #print 'create non numpy scalar', type(value), name, MPI.COMM_WORLD.rank
    #             #group.create_dataset(name, (100,), dtype='i8')
    #             if isinstance(value,float):
    #                 group.create_dataset(name, data=np.array([value]))   
    #             #group.create_dataset(name, data=value)
    #             #group.attrs[name] = value
    #         pass

    def close(self):
        """
        Closes `out` unless it's ``sys.stdout`` or ``sys.stderr``.
        Note that a closed recorder will do nothing in :meth:`record`.
        """

        self.hdf5_file_object.close()

        # if self.out is not None and self._cases is not None:
        #     self.out.write('}\n')

        # if self.out not in (None, sys.stdout, sys.stderr):
        #     if not isinstance(self.out,
        #                       (StringIO.StringIO, cStringIO.OutputType)):
        #         # Closing a StringIO deletes its contents.
        #         self.out.close()
        #     self.out = None

        self._cases = None

    def get_iterator(self):
        """ Just returns None. """
        return None


