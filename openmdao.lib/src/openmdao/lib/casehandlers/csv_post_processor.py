"""
    Post-processing function that takes a case_data_set and outputs a csv file
"""

import csv

# pylint: disable=E0611,F0401
from openmdao.main.case import flatten_obj

def caseset_query_to_csv(data, filename='cases.csv', delimiter=',', quotechar='"'):
    """
    Post-processing function that takes a case_data_set and outputs a csv
    file. Should be able to pass tests of current csv case recorder (column
    ordering, meta column, etc...) Assume query by case (not variable).

    Inputs:

    data - results of fetch on Query object

    """

    cds = data.cds
    drivers = {}
    for driver in cds.drivers:
        drivers[driver['_id']] = driver['name']

    # Determine inputs & outputs, map pseudos to expression names.
    expressions = cds.simulation_info['expressions']
    metadata = cds.simulation_info['variable_metadata']
    inputs = []
    outputs = []
    pseudos = {}
    for name in sorted(data[0].keys()):

        # All inputs and outputs that change.
        if name in metadata:
            if metadata[name]['iotype'] == 'in':
                inputs.append(name)
            else:
                outputs.append(name)

        # Include objectives and constraints from all simulation levels.
        elif '_pseudo_' in name and not name.endswith('.out0'):
            for exp_name, exp_dict in expressions.items():
                if exp_dict['pcomp_name'] == name:
                    pseudos[name] = '%s(%s)' % (exp_dict['data_type'], exp_name)
                    break
            else:
                raise RuntimeError('Cannot find %r in expressions' % name)
            outputs.append(name)

        # Allow private vars from components.
        elif '.' in name:
            outputs.append(name)

    # Open CSV file
    outfile = open(filename, 'wb')
    csv_writer = csv.writer(outfile, delimiter=delimiter,
                                     quotechar=quotechar,
                                     quoting=csv.QUOTE_NONNUMERIC)
    # No automatic data type conversion is performed unless the
    # QUOTE_NONNUMERIC format option is specified (in which case unquoted
    # fields are transformed into floats).

    # Write the data
    # data is a list of lists where the inner list is the values and metadata
    # for a case

    sorted_input_keys = []
    sorted_input_values = []
    sorted_output_keys = []
    sorted_output_values = []

    for i, row in enumerate( data ):

        input_keys = []
        input_values = []
        for name in inputs:
            obj = row[ row.name_map[ name ] ]
            for key, value in flatten_obj(name, obj):
                input_keys.append(key)
                input_values.append(value)

        output_keys = []
        output_values = []
        for name in outputs:

            obj = row[ row.name_map[ name ] ]
            if name in pseudos:
                name = pseudos[name]

            for key, value in flatten_obj(name, obj):
                output_keys.append(key)
                output_values.append(value)

        # This should not be necessary, however python's csv writer
        # is not writing boolean variables correctly as strings.

        for index, item in enumerate(input_values):
            if isinstance(item, bool):
                input_values[index] = str(item)

        for index, item in enumerate(output_values):
            if isinstance(item, bool):
                output_values[index] = str(item)

        # Sort the columns alphabetically.

        if len(input_keys) > 0:
            sorted_input_keys, sorted_input_values = \
                (list(item) for item in zip(*sorted(zip(input_keys,
                                                        input_values))))
        if len(output_keys) > 0:
            sorted_output_keys, sorted_output_values = \
                (list(item) for item in zip(*sorted(zip(output_keys,
                                                        output_values))))
        if outfile is None:
            raise RuntimeError('Attempt to record on closed recorder')

        if i == 0:
            headers = ['timestamp', '/INPUTS']
            headers.extend(sorted_input_keys)
            headers.append('/OUTPUTS')
            headers.extend(sorted_output_keys)
            headers.extend(['/METADATA', 'uuid', 'parent_uuid', 'msg'])

            csv_writer.writerow(headers)
            header_size = len(headers)


        timestamp = row[ row.name_map[ 'timestamp' ] ]
        csv_data = [timestamp]
        csv_data.append('')
        csv_data.extend(sorted_input_values)
        csv_data.append('')
        csv_data.extend(sorted_output_values)
        exc = row[ row.name_map[ 'error_message' ] ]
        msg = '' if exc is None else str(exc)
        case_uuid = row[ row.name_map[ '_id' ] ]
        parent_uuid = row[ row.name_map[ '_parent_id' ] ]
        csv_data.extend(['', case_uuid, parent_uuid, msg])

        if header_size != len(csv_data):
            raise RuntimeError("number of data points (%d) doesn't match header"
                               " size (%d) in CSV recorder"
                               % (len(csv_data), header_size))

        csv_writer.writerow(csv_data)

    outfile.close()


