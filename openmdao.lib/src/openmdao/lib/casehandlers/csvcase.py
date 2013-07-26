"""A CaseRecorder and CaseIterator that store the cases in a CSV file.
"""

import csv, datetime, glob, os, shutil
import cStringIO, StringIO

# pylint: disable-msg=E0611,F0401
from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator
from openmdao.main.case import Case

class CSVCaseIterator(object):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    
    Current limitations:
        Quote character in the input CSV file should be ``'`` or ``"``. Other
        choices don't seem to get identified by csv.Sniffer.
        
        All string data must be contained inside of quotes. This includes
        field headers.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, filename='cases.csv', headers=None):
        
        self.data = []
        self.headers = headers
        self.label_field = None
        
        #Open Input file
        self.filename = filename
        
    @property
    def filename(self):
        """Get the name of the CSV file."""
        
        return self._filename
    
    @filename.setter
    def filename(self, name):
        """Set the CSV file name."""
        
        self._filename = name
        
        with open(self.filename, 'r') as infile:
            # Sniff out the dialect
            #infile.seek(1)
            dialect = csv.Sniffer().sniff(infile.readline())
            infile.seek(0)
            reader = csv.reader(infile, dialect, quoting=csv.QUOTE_NONNUMERIC)
            
            self.data = []
            for row in reader:
                self.data.append(row)
           
            if self.headers is None:
                self.need_fieldnames = True
            else:
                self.need_fieldnames = False
                if 'label' in self.headers.values():
                    for key, value in self.headers.iteritems():
                        if value == 'label':
                            self.label_field = key
                            del self.headers[key]
                            break
            
    def __iter__(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which returns Cases one at a time. """
        
        # Default case label for external csv files that don't have labels.
        label = "External Case"
        
        retries = max_retries = 0
        parent_uuid = msg = ""
        retries_field = None
        if self.headers is None:
            input_fields = {}
        else:
            input_fields = self.headers
        output_fields = {}
        
        for row in self.data:
            
            # Get fieldnames from file
            if self.need_fieldnames:

                # OpenMDAO-style CSV file
                if len(row)>1 and row[1] == '/INPUTS':
                    
                    input_fields, output_fields = self._parse_fieldnames(row)
                        
                    self.label_field = 0
                    retries_field = row.index('/METADATA') + 1
                    max_retries_field = retries_field + 1
                    parent_uuid_field = retries_field + 2
                    msg_field = retries_field + 3
                    
                # Read headers from file
                elif self.headers is None:
                    for i, field in enumerate(row):
                        if field == 'label':
                            self.label_field = i
                        else:
                            input_fields[i] = field
                    
                self.need_fieldnames = False
                continue

            if self.label_field is not None:
                label = row[self.label_field]
                
            if retries_field is not None:
                retries = row[retries_field]
                max_retries = row[max_retries_field]
                parent_uuid = row[parent_uuid_field]
                msg = row[msg_field]
                
                # For some reason, default for these in a case is None
                if not retries:
                    retries = None
                if not max_retries:
                    max_retries = None
                
            inputs = []
            for i, field in input_fields.iteritems():
                
                # Convert bools from string back into bools
                # Note, only really need this for inputs.
                if row[i] in ['True', 'False']:
                    row[i] = bool(row[i])
                    
                inputs.append( (field, row[i]) )
            
            outputs = []
            for i, field in output_fields.iteritems():
                outputs.append( (field, row[i]) )
                
            yield Case(inputs=inputs, outputs=outputs, label=label, \
                       retries=retries, max_retries=max_retries, \
                       parent_uuid=parent_uuid, msg=msg)

        self.need_fieldnames = True

    def _parse_fieldnames(self, row):
        ''' Parse our input and output fieldname dictionaries
        '''
        
        input_fields = {}
        output_fields = {}
        
        # This file was generated by a CSVCaseRecorder
        if row[1] == '/INPUTS':
            
            in_start = 2
            out_start = row.index('/OUTPUTS') + 1
            out_end = row.index('/METADATA')
            
            if in_start < out_start-1:
                for i in range(in_start, out_start-1):
                    input_fields[i] = row[i]
                    
            if out_start < len(row)-1:
                for i in range(out_start, out_end):
                    output_fields[i] = row[i]
    
        # This file was generated externally
        else:
            pass

        return input_fields, output_fields

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""
        
        attrs = {}
        attrs['type'] = type(self).__name__
        variables = []
        
        attr = {}
        attr['name'] = "filename"
        attr['type'] = type(self.filename).__name__
        attr['value'] = str(self.filename)
        attr['connected'] = ''
        attr['desc'] = 'Name of the CSV file to be iterated.'
        variables.append(attr)
            
        attr = {}
        attr['name'] = "headers"
        attr['type'] = type(self.headers).__name__
        attr['value'] = str(self.headers)
        attr['connected'] = ''
        attr['desc'] = 'Optional dictionary of header labels, where the key is the column number.'
        variables.append(attr)
            
        attrs["Inputs"] = variables
        return attrs
        
        
        
class CSVCaseRecorder(object):
    """Stores cases in a csv file. Defaults to cases.csv."""
    
    implements(ICaseRecorder)
    
    def __init__(self, filename='cases.csv', append=False, delimiter=',',
                 quotechar = '"'):
        
        self.delimiter = delimiter
        self.quotechar = quotechar
        self.append = append
        self.outfile = None
        self.csv_writer = None
        self.num_backups = 5
        self._header_size = 0
        
        #Open output file
        self._write_headers = False
        self.filename = filename
        
    @property
    def filename(self):
        """Get the name of the CSV file."""
        
        return self._filename
    
    @filename.setter
    def filename(self, name):
        """Set the CSV file name."""
        
        self._filename = name
        
    def startup(self):
        """ Opens the CSV file for recording."""
        
        if self.append:
            self.outfile = open(self.filename, 'a')
        else:
            self.outfile = open(self.filename, 'w')
            
            # Whenever we start a new CSV file, we need to insert a line
            # of headers. These won't be available until the first
            # case is passed to self.record.
            self._write_headers = True

            self.csv_writer = csv.writer(self.outfile, delimiter=self.delimiter,
                                         quotechar=self.quotechar,
                                         quoting=csv.QUOTE_NONNUMERIC)


    def record(self, case):
        """Store the case in a csv file. The format for a line of data
        follows:
        
        Field 1      - label
        Field 2      - [Empty]
        Field 3      - Input 1
        ...
        Field i+2    - Input i
        Field i+3    - [Empty]
        Field i+4    - Output 1
        ...
        Field i+j+4  - Output j
        Field i+j+5  - [Empty]
        Field i+j+6  - retries
        Field i+j+7  - max_retries
        Field i+j+8  - parent_uuid
        Field i+j+9  - msg
        """
        
        sorted_input_keys = []
        sorted_input_values = []
        sorted_output_keys = []
        sorted_output_values = []
        
        input_keys = case.keys(iotype='in', flatten=True)
        input_values = case.values(iotype='in', flatten=True)
        output_keys = case.keys(iotype='out', flatten=True)
        output_values = case.values(iotype='out', flatten=True)
        
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
        if self.outfile is None:
            raise RuntimeError('Attempt to record on closed recorder')

        if self._write_headers:
            
            headers = ['label', '/INPUTS']
            
            headers.extend(sorted_input_keys)
                
            headers.append('/OUTPUTS')
            
            headers.extend(sorted_output_keys)
                
            headers.extend(['/METADATA', 'retries', 'max_retries', 'parent_uuid',
                            'msg'])
                    
            self.csv_writer.writerow(headers)
            self._write_headers = False
            self._header_size = len(headers)
            
        data = [case.label]
                
        data.append('')
        data.extend(sorted_input_values)
        data.append('')
        data.extend(sorted_output_values)
            
        data.extend(['', case.retries, case.max_retries, 
                     case.parent_uuid, case.msg])
        
        if self._header_size != len(data):
            raise RuntimeError("number of data points doesn't match header size in CSV recorder")
        
        self.csv_writer.writerow(data)

    def close(self):
        """Closes the file."""

        if self.csv_writer is not None:
            if not isinstance(self.outfile,
                              (StringIO.StringIO, cStringIO.OutputType)):
                # Closing a StringIO deletes its contents.
                self.outfile.close()
            self.outfile = None
            self.csv_writer = None
            
        # Save off a backup copy if requested.
        if self.num_backups > 0:
            
            timestamp = datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
            parts = self.filename.split('.')
            if len(parts) > 1:
                name = ''.join(parts[:-1])
                extension = parts[-1]
                backup_name = '%s_%s.%s' % (name, timestamp, extension)
                globname = name
            else:
                backup_name = '%s_%s' % (self.filename, timestamp)
                globname = self.filename
                
            shutil.copyfile(self.filename, backup_name)
            
            # Clean up old backups if we exceed our max
            backups = glob.glob(globname + '_*')
            if len(backups) > self.num_backups:
                sortbackups = sorted(backups)
                for item in sortbackups[:len(backups) - self.num_backups]:
                    os.remove(item)
        
    def get_iterator(self):
        '''Return CSVCaseIterator that points to our current file.'''
        
        # I think we can safely close the oufile if someone is
        # requesting the iterator
        self.close()
        
        return CSVCaseIterator(self.filename)

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""
        
        attrs = {}
        attrs['type'] = type(self).__name__
        variables = []
        
        attr = {}
        attr['name'] = "filename"
        attr['id'] = attr['name']
        attr['type'] = type(self.filename).__name__
        attr['value'] = str(self.filename)
        attr['connected'] = ''
        attr['desc'] = 'Name of the CSV file to be output.'
        variables.append(attr)
            
        attr = {}
        attr['name'] = "append"
        attr['id'] = attr['name']
        attr['type'] = type(self.append).__name__
        attr['value'] = str(self.append)
        attr['connected'] = ''
        attr['desc'] = 'Set to True to append to the existing CSV file.'
        variables.append(attr)
            
        attr = {}
        attr['name'] = "delimiter"
        attr['id'] = attr['name']
        attr['type'] = type(self.delimiter).__name__
        attr['value'] = str(self.delimiter)
        attr['connected'] = ''
        attr['desc'] = 'CSV delimiter. Default is ",".'
        variables.append(attr)
            
        attr = {}
        attr['name'] = "num_backups"
        attr['id'] = attr['name']
        attr['type'] = "int"
        attr['value'] = str(self.num_backups)
        attr['connected'] = ''
        attr['desc'] = 'Number of csv files to keep from previous runs.'
        variables.append(attr)
            
        attrs["Inputs"] = variables
        return attrs
        
