"""
Post-processing function that takes a case_data_set and outputs a csv file

 

Should be able to pass tests of current csv case recorder (column ordering, meta column, etc...)

Assume query by case (not variable)


"""

import cStringIO
import StringIO
import logging
import sys
import time
import csv, datetime, glob, os, shutil, time


import json
import bson

from numpy  import ndarray
from struct import pack
from uuid   import uuid1

from openmdao.lib.casehandlers.api import CSVCaseIterator, CSVCaseRecorder, \
                                          DumpCaseRecorder

from openmdao.lib.casehandlers.api import CaseDataset, \
                                          JSONCaseRecorder, BSONCaseRecorder

from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator

def caseset_query_to_csv(data, cds, filename='cases.csv', delimiter=',', quotechar='"'):
    drivers = {}
    for driver in cds.drivers:
        drivers[driver['_id']] = driver['name']

    # Determine inputs & outputs, map pseudos to expression names.
    expressions = cds.simulation_info['expressions']
    metadata = cds.simulation_info['variable_metadata']
    constants_names = cds.simulation_info['constants'].keys()
    inputs = []
    outputs = []
    pseudos = {}
    #import pdb; pdb.set_trace()
    #for name in sorted(data[0].keys()):
    for name in sorted(metadata.keys()):
        if name in constants_names:
            continue
        if name in metadata:
            if metadata[name]['iotype'] == 'in':
                inputs.append(name)
            else:
                outputs.append(name)
        elif '_pseudo_' in name:
            for exp_name, exp_dict in expressions.items():
                if exp_dict['pcomp_name'] == name:
                    pseudos[name] = '%s(%s)' % (exp_dict['data_type'], exp_name)
                    break
            else:
                raise RuntimeError('Cannot find %r in expressions' % name)
            outputs.append(name)
        else:
            outputs.append(name)

    # Open CSV file
    outfile = open(filename, 'w')
    csv_writer = csv.writer(outfile, delimiter=delimiter,
                                     quotechar=quotechar,
                                     quoting=csv.QUOTE_NONNUMERIC)

    # Dump data.
    # data is a list of lists where the inner list is the values and metadata for a case
    # 
    #csv_data.append(row[var_names.index(name))
    #write("Case:\n")
    var_names = cds.data.var_names().fetch() # the list of names of the values in the case list
    for row in data:
        csv_data = []
        csv_data.append( row[ var_names.index( 'timestamp' ) ] )
        csv_data.append('')
        if inputs:
            for name in inputs:
                csv_data.append( row[var_names.index(name)] )
        #data.extend(sorted_input_values)
        csv_data.append('')
        if outputs:
            for name in outputs:
                if name == '_driver_id':
                    value = drivers[row[var_names.index(name)]]
                else:
                    value = row[var_names.index(name)]
                csv_data.append( row[var_names.index(name)] )
        case_uuid = row[ var_names.index( '_id' ) ]
        parent_uuid = row[ var_names.index( '_parent_id' ) ]
        msg = row[ var_names.index( 'error_message' ) ]
        csv_data.extend(['', case_uuid, parent_uuid, msg])


        csv_writer.writerow(csv_data)

    outfile.close()


