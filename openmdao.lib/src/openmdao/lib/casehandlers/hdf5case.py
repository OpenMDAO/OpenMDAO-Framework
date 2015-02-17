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

class HDF5CaseRecorder(object):
    """
    Dumps a run in JSON form to `out`, which may be a string or a file-like
    object (defaults to ``stdout``). If `out` is ``stdout`` or ``stderr``,
    then that standard stream is used. Otherwise, if `out` is a string, then
    a file with that name will be opened in the current directory.
    If `out` is None, cases will be ignored.
    """

    implements(ICaseRecorder)

    def __init__(self, out='cases.json', indent=4, sort_keys=True):
        self._cfg_map = {}
        self._uuid = None
        self._cases = None

        # not used yet but for getting values of variables
        #     from subcases
        self._last_child_case_uuids = {} # keyed by driver id

        self.hdf5_file_object = h5py.File(out, "w")

        self.indent = indent
        self.sort_keys = sort_keys
        self._count = 0

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
        #if not self.hdf5_file_object:
            #return

        info = self.get_simulation_info(constants)

        simulation_info_grp = self.hdf5_file_object.create_group("simulation_info")

        self.write_to_hdf5( simulation_info_grp, 'OpenMDAO_Version', info['OpenMDAO_Version'])
        self.write_to_hdf5( simulation_info_grp, 'comp_graph', info['comp_graph'])
        self.write_to_hdf5( simulation_info_grp, 'graph', info['graph'])
        self.write_to_hdf5( simulation_info_grp, 'uuid', info['uuid'])
        self.write_to_hdf5( simulation_info_grp, 'name', info['name'])

       
        constants_grp = simulation_info_grp.create_group("constants")
        for k,v in info['constants'].items():
            self.write_to_hdf5( constants_grp, k, v )

            # if v == None :
            #     constants_grp.attrs[k] = np.array([])
            # elif isinstance(v,dict):
            #     dict_grp = constants_grp.create_group(k)
            #     for name, value in v.items():
            #         dict_grp.attrs[name] = value
            # else:
            #     constants_grp.attrs[k] = v

        expressions_grp = simulation_info_grp.create_group("expressions")
        for k,v in info['expressions'].items():
           self.write_to_hdf5( expressions_grp, k, v )
           # if not isinstance(v,dict):
           #      expressions_grp.attrs[k] = v
           #  else:
           #      dict_grp = expressions_grp.create_group(k)
           #      for name, value in v.items():
           #          dict_grp.attrs[name] = value
            
        variable_metadata_grp = simulation_info_grp.create_group("variable_metadata")
        for k,v in info['variable_metadata'].items():
            self.write_to_hdf5( variable_metadata_grp, k, v )
            # if not isinstance(v,dict):
            #     variable_metadata_grp.attrs[k] = v
            # else:
            #     dict_grp = variable_metadata_grp.create_group(k)
            #     for name, value in v.items():
            #         if value != None :
            #             dict_grp.attrs[name] = value
            #         else:
            #             dict_grp.attrs[name] = np.array([])

        for i, info in enumerate(self.get_driver_info()):
            driver_info_name = 'driver_info_%s' % (i+1)
            driver_info_group = self.hdf5_file_object.create_group(driver_info_name)
            for k,v in info.items():
                self.write_to_hdf5( driver_info_group, k, v )
                # driver_info_group.attrs[k] = v

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """ Dump the given run data. """
        info = self.get_case_info(driver, inputs, outputs, exc,
                                  case_uuid, parent_uuid)
        self._cases += 1
        iteration_case_name = 'iteration_case_%s' % self._cases
        self._count += 1
        iteration_case_group = self.hdf5_file_object.create_group(iteration_case_name)
        for k,v in info.items():
            self.write_to_hdf5( iteration_case_group, k, v )
            # if isinstance(v,dict):
            #     dict_grp = iteration_case_group.create_group(k)
            #     for name, value in v.items():
            #         if isinstance( value, np.ndarray):
            #             dict_grp.create_dataset(name, data=value,compression="gzip")
            #         else:
            #             dict_grp.attrs[name] = value
            # elif isinstance( v, np.ndarray):
            #     iteration_case_group.create_dataset(k, data=v,compression="gzip")
            # elif v == None :
            #     iteration_case_group.attrs[k] = np.array([])
            # else:
            #     iteration_case_group.attrs[k] = v

    def write_to_hdf5(self, group, name, value ):

        if isinstance(value,dict):
            dict_grp = group.create_group(name)
            dict_grp.attrs['__dict__'] = True # To indicate that this HDF5 group represents an actual Python dict
            for k, v in value.items():
                self.write_to_hdf5( dict_grp, k, v )
        elif isinstance( value, VariableTree):
            vartree_grp = group.create_group(name)
            vartree_grp.attrs['__vartree__'] = True
            for k in value.list_vars():
                self.write_to_hdf5( vartree_grp, k, value.get(k) )
        elif isinstance( value, np.ndarray):
            group.create_dataset(name, data=value,compression="gzip")
        elif value == None :
            group.attrs[name] = np.array([])
        else:
            group.attrs[name] = value

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


