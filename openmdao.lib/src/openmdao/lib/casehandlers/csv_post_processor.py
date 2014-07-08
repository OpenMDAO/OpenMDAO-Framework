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

# class CSVCaseWriter(object):
#     """Stores cases in a csv file. Defaults to cases.csv."""

#     implements(ICaseRecorder)

#     def __init__(self, filename='cases.csv', append=False, delimiter=',',
#                  quotechar='"'):

#         self.delimiter = delimiter
#         self.quotechar = quotechar
#         self.append = append
#         self.outfile = None
#         self.csv_writer = None
#         self.num_backups = 5
#         self._header_size = 0
#         self._cfg_map = {}

#         #Open output file
#         self._write_headers = False
#         self._filename = None
#         self.filename = filename

#     def __getstate__(self):
#         """ Returns state as a dict. """
#         state = self.__dict__.copy()
#         # If already open, restored recorder will append to reopened file.
#         state['append'] = self.append or (self.outfile is not None)
#         state['outfile'] = None
#         state['csv_writer'] = None
#         return state

#     def __setstate__(self, state):
#         """ Restore state from `state`. """
#         self.__dict__.update(state)

#     @property
#     def filename(self):
#         """Get the name of the CSV file."""
#         return self._filename

#     @filename.setter
#     def filename(self, name):
#         """Set the CSV file name."""
#         self._filename = name

#     def startup(self):
#         """ Opens the CSV file for recording."""
#         if self.append:
#             self.outfile = open(self.filename, 'a')
#         else:
#             self.outfile = open(self.filename, 'w')

#             # Whenever we start a new CSV file, we need to insert a line
#             # of headers. These won't be available until the first
#             # case is passed to self.record.
#             self._write_headers = True
#             self._cfg_map = {}

#         self.csv_writer = csv.writer(self.outfile, delimiter=self.delimiter,
#                                      quotechar=self.quotechar,
#                                      quoting=csv.QUOTE_NONNUMERIC)

#     def register(self, driver, inputs, outputs):
#         """Register names for later record call from `driver`."""
#         self._cfg_map[driver] = driver_map(driver, inputs, outputs)

#     def record_constants(self, constants):
#         """Record constant data - currently ignored."""
#         pass


#     # inputs is a list of values
#     def write(self, inputs, outputs, exc, case_uuid, parent_uuid):
#         """Store the case in a csv file. The format for a line of data
#         follows:

#         Field 1      - timestamp
#         Field 2      - [Empty]
#         Field 3      - Input 1
#         ...
#         Field i+2    - Input i
#         Field i+3    - [Empty]
#         Field i+4    - Output 1
#         ...
#         Field i+j+4  - Output j
#         Field i+j+5  - [Empty]
#         Field i+j+6  - uuid
#         Field i+j+7  - parent_uuid
#         Field i+j+8  - msg
#         """
#         sorted_input_keys = []
#         sorted_input_values = []
#         sorted_output_keys = []
#         sorted_output_values = []

#         # input_keys = []
#         # input_values = []
#         # import pdb; pdb.set_trace() # qqq
#         # for name, obj in zip(in_cfg, inputs):
#         #     for key, value in flatten_obj(name, obj):
#         #         input_keys.append(key)
#         #         input_values.append(value)

#         # output_keys = []
#         # output_values = []
#         # for name, obj in zip(out_cfg, outputs):
#         #     for key, value in flatten_obj(name, obj):
#         #         output_keys.append(key)
#         #         output_values.append(value)

#         # # This should not be necessary, however python's csv writer
#         # # is not writing boolean variables correctly as strings.

#         # for index, item in enumerate(input_values):
#         #     if isinstance(item, bool):
#         #         input_values[index] = str(item)

#         # for index, item in enumerate(output_values):
#         #     if isinstance(item, bool):
#         #         output_values[index] = str(item)

#         # # Sort the columns alphabetically.

#         # if len(input_keys) > 0:
#         #     sorted_input_keys, sorted_input_values = \
#         #         (list(item) for item in zip(*sorted(zip(input_keys,
#         #                                                 input_values))))
#         # if len(output_keys) > 0:
#         #     sorted_output_keys, sorted_output_values = \
#         #         (list(item) for item in zip(*sorted(zip(output_keys,
#         #                                                 output_values))))
#         # if self.outfile is None:
#         #     raise RuntimeError('Attempt to record on closed recorder')

#         # if self._write_headers or self._header_size == 0:
#         #     headers = ['timestamp', '/INPUTS']
#         #     headers.extend(sorted_input_keys)
#         #     headers.append('/OUTPUTS')
#         #     headers.extend(sorted_output_keys)
#         #     headers.extend(['/METADATA', 'uuid', 'parent_uuid', 'msg'])

#         #     self.csv_writer.writerow(headers)
#         #     self._write_headers = False
#         #     self._header_size = len(headers)

#         msg = '' if exc is None else str(exc)

#         data = [time.time()]
#         data.append('')
#         data.extend(sorted_input_values)
#         data.append('')
#         data.extend(sorted_output_values)
#         data.extend(['', case_uuid, parent_uuid, msg])

#         # if self._header_size != len(data):
#         #     raise RuntimeError("number of data points (%d) doesn't match header"
#         #                        " size (%d) in CSV recorder"
#         #                        % (len(data), self._header_size))

#         self.csv_writer.writerow(data)



#     # inputs is a list of values
#     def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
#         """Store the case in a csv file. The format for a line of data
#         follows:

#         Field 1      - timestamp
#         Field 2      - [Empty]
#         Field 3      - Input 1
#         ...
#         Field i+2    - Input i
#         Field i+3    - [Empty]
#         Field i+4    - Output 1
#         ...
#         Field i+j+4  - Output j
#         Field i+j+5  - [Empty]
#         Field i+j+6  - uuid
#         Field i+j+7  - parent_uuid
#         Field i+j+8  - msg
#         """
#         sorted_input_keys = []
#         sorted_input_values = []
#         sorted_output_keys = []
#         sorted_output_values = []

#         in_cfg, out_cfg = self._cfg_map[driver]
#         input_keys = []
#         input_values = []
#         import pdb; pdb.set_trace() # qqq
#         for name, obj in zip(in_cfg, inputs):
#             for key, value in flatten_obj(name, obj):
#                 input_keys.append(key)
#                 input_values.append(value)

#         output_keys = []
#         output_values = []
#         for name, obj in zip(out_cfg, outputs):
#             for key, value in flatten_obj(name, obj):
#                 output_keys.append(key)
#                 output_values.append(value)

#         # This should not be necessary, however python's csv writer
#         # is not writing boolean variables correctly as strings.

#         for index, item in enumerate(input_values):
#             if isinstance(item, bool):
#                 input_values[index] = str(item)

#         for index, item in enumerate(output_values):
#             if isinstance(item, bool):
#                 output_values[index] = str(item)

#         # Sort the columns alphabetically.

#         if len(input_keys) > 0:
#             sorted_input_keys, sorted_input_values = \
#                 (list(item) for item in zip(*sorted(zip(input_keys,
#                                                         input_values))))
#         if len(output_keys) > 0:
#             sorted_output_keys, sorted_output_values = \
#                 (list(item) for item in zip(*sorted(zip(output_keys,
#                                                         output_values))))
#         if self.outfile is None:
#             raise RuntimeError('Attempt to record on closed recorder')

#         if self._write_headers or self._header_size == 0:
#             headers = ['timestamp', '/INPUTS']
#             headers.extend(sorted_input_keys)
#             headers.append('/OUTPUTS')
#             headers.extend(sorted_output_keys)
#             headers.extend(['/METADATA', 'uuid', 'parent_uuid', 'msg'])

#             self.csv_writer.writerow(headers)
#             self._write_headers = False
#             self._header_size = len(headers)

#         msg = '' if exc is None else str(exc)

#         data = [time.time()]
#         data.append('')
#         data.extend(sorted_input_values)
#         data.append('')
#         data.extend(sorted_output_values)
#         data.extend(['', case_uuid, parent_uuid, msg])

#         if self._header_size != len(data):
#             raise RuntimeError("number of data points (%d) doesn't match header"
#                                " size (%d) in CSV recorder"
#                                % (len(data), self._header_size))

#         self.csv_writer.writerow(data)

#     def close(self):
#         """Closes the file."""
#         if self.csv_writer is not None:
#             if not isinstance(self.outfile,
#                               (StringIO.StringIO, cStringIO.OutputType)):
#                 # Closing a StringIO deletes its contents.
#                 self.outfile.close()
#             self.outfile = None
#             self.csv_writer = None

#         # Save off a backup copy if requested.
#         if self.num_backups > 0:
#             timestamp = datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')

#             parts = self.filename.split('.')
#             if len(parts) > 1:
#                 name = ''.join(parts[:-1])
#                 extension = parts[-1]
#                 backup_name = '%s_%s.%s' % (name, timestamp, extension)
#                 globname = name
#             else:
#                 backup_name = '%s_%s' % (self.filename, timestamp)
#                 globname = self.filename

#             if os.path.isfile(self.filename):
#                 shutil.copyfile(self.filename, backup_name)

#             # Clean up old backups if we exceed our max
#             backups = glob.glob(globname + '_*')
#             if len(backups) > self.num_backups:
#                 sortbackups = sorted(backups)
#                 for item in sortbackups[:len(backups) - self.num_backups]:
#                     os.remove(item)

#     def get_iterator(self):
#         '''Return CSVCaseIterator that points to our current file.'''

#         # I think we can safely close the oufile if someone is
#         # requesting the iterator
#         self.close()

#         return CSVCaseIterator(self.filename)

#     def get_attributes(self, io_only=True):
#         """ We need a custom get_attributes because we aren't using Traits to
#         manage our changeable settings. This is unfortunate and should be
#         changed to something that automates this somehow."""

#         attrs = {}
#         attrs['type'] = type(self).__name__
#         variables = []

#         attr = {}
#         attr['name'] = "filename"
#         attr['id'] = attr['name']
#         attr['type'] = type(self.filename).__name__
#         attr['value'] = str(self.filename)
#         attr['connected'] = ''
#         attr['desc'] = 'Name of the CSV file to be output.'
#         variables.append(attr)

#         attr = {}
#         attr['name'] = "append"
#         attr['id'] = attr['name']
#         attr['type'] = type(self.append).__name__
#         attr['value'] = str(self.append)
#         attr['connected'] = ''
#         attr['desc'] = 'Set to True to append to the existing CSV file.'
#         variables.append(attr)

#         attr = {}
#         attr['name'] = "delimiter"
#         attr['id'] = attr['name']
#         attr['type'] = type(self.delimiter).__name__
#         attr['value'] = str(self.delimiter)
#         attr['connected'] = ''
#         attr['desc'] = 'CSV delimiter. Default is ",".'
#         variables.append(attr)

#         attr = {}
#         attr['name'] = "num_backups"
#         attr['id'] = attr['name']
#         attr['type'] = "int"
#         attr['value'] = str(self.num_backups)
#         attr['connected'] = ''
#         attr['desc'] = 'Number of csv files to keep from previous runs.'
#         variables.append(attr)

#         attrs["Inputs"] = variables
#         return attrs

def cwwwaseset_query_to_csv(data, cds, filename='cases.csv', delimiter=',', quotechar='"'):

    pass

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

def qqqcaseset_query_to_csv(cds, query, filename):
    """
    Create a csv file from the results of a case_data_set 
    query so the data can be imported to other tools

    query is a Query object
    """

    """
    The JSON records come in like this

    ['_driver_id', '_id', '_parent_id', u'_pseudo_0', u'_pseudo_1', u'_pseudo_2', u'_pseudo_3', u'comp1.a_array', u'comp1.a_array[2]', u'comp1.a_string', u'comp1.b_bool', u'comp1.derivative_exec_count', u'comp1.exec_count', u'comp1.itername', u'comp1.vt', u'comp1.x', u'comp1.x_array[1]', u'comp1.y', u'comp1.z', u'comp2.derivative_exec_count', u'comp2.exec_count', u'comp2.itername', u'comp2.z', u'driver.workflow.itername', 'error_message', 'error_status', 'timestamp']

    """

    ccw = CSVCaseWriter(filename)
    ccw.startup()
    for case in query.fetch():
        #import pdb; pdb.set_trace()
        print case
        inputs = []
        outputs = []
        exc = ""
        case_uuid = case[ '_id']
        parent_uuid = case[ '_parent_id']

        ccw.write(inputs, outputs, exc, case_uuid, parent_uuid)
    ccw.close()

    #print f

