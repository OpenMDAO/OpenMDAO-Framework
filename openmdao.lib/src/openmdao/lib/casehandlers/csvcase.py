"""A CaseRecorder and CaseIterator that store the cases in a CSV file
"""

import csv

from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator
from openmdao.main.case import Case

class CSVCaseIterator(object):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, filename='cases.csv'):
        
        self.data = []
        
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
        
        infile = open(self.filename, 'r')
        
        # Sniff out the dialect
        infile.seek(1)
        dialect = csv.Sniffer().sniff(infile.readline())
        infile.seek(0)
        reader = csv.reader(infile, dialect)
        
        self.data = []
        for row in reader:
            self.data.append(row)
            
        self.need_fieldnames = True
        
    def __iter__(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which returns Cases one at a time. """
        
        
        for row in self.data:
            
            if self.need_fieldnames:
                self.need_fieldnames = False
                continue
            
            yield row


class CSVCaseRecorder(object):
    """Stores cases in a csv file. Defaults to cases.csv."""
    
    implements(ICaseRecorder)
    
    def __init__(self, filename='cases.csv', append=False, delimiter=',', \
                 quotechar = '"'):
        
        self.delimiter = delimiter
        self.quotechar = quotechar
        self.append = append
        
        #Open output file
        self.write_headers = False
        self.filename = filename
        
    @property
    def filename(self):
        """Get the name of the CSV file."""
        
        return self._filename
    
    @filename.setter
    def filename(self, name):
        """Set the CSV file name."""
        
        self._filename = name
        
        if self.append:
            self.outfile = open(self.filename, 'a')
        else:
            self.outfile = open(self.filename, 'w')
            
            # Whenver we start a new CSV file, we need to insert a line
            # of headers. These won't be available until the first
            # case is passed to self.record.
            self.write_headers = True
            
        self.csv_writer = csv.writer(self.outfile, delimiter=self.delimiter, \
                                     quotechar=self.quotechar, \
                                     quoting=csv.QUOTE_NONNUMERIC)


    def record(self, case):
        """Store the case in a csv file. The format for a line of data
        follows:
        
        Field 1     - uuid
        Field 2     - [Empty]
        Field 3     - Input 1
        ...
        Field i+2   - Input i
        Field i+3   - [Empty]
        Field i+4   - Output 1
        ...
        Field i+j+4 - Output j
        """
        
        if self.write_headers:
            
            headers = ['uuid', '/INPUTS']
            
            for name in case.keys(iotype='in'):
                headers.append(name)
                
            headers.append('/OUTPUTS')
            for name in case.keys(iotype='out'):
                headers.append(name)
                    
            self.csv_writer.writerow(headers)
            self.write_headers = False
            
        data = []
        
        data.append(case.uuid)
        
        data.append('')
        for value in case.values(iotype='in'):
            data.append(value)
            
        data.append('')
        for value in case.values(iotype='out'):
            data.append(value)
                
        self.csv_writer.writerow(data)

    def get_iterator(self):
        '''Return CSVCaseIterator that points to our current file'''
        
        # I think we can safely close the oufile if someone is
        # requesting the iterator
        self.outfile.close()
        
        return CSVCaseIterator(self.filename)