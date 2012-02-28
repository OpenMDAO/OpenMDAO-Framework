"""A CaseRecorder and CaseIterator that store the cases in a CSV file
"""

import csv

from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator

class CSVCaseIterator(object):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, cases):
        super(CSVCaseIterator, self).__init__(cases)


class CSVCaseRecorder(object):
    """Stores cases in a csv file. Defaults to cases.csv."""
    
    implements(ICaseRecorder)
    
    def __init__(self, filename='cases.csv', delimiter=',', append=False):
        
        self.delimiter = delimiter
        self.append = append
        
        #Open output file
        self.filename = filename
        
    def __len__(self):
        return len(self.cases)
    
    @property
    def filename(self):
        """Get the name of the CSV file."""
        
        return self._filename
    
    @filename.setter
    def filename(self, name):
        """Set the CSV file name."""
        
        self._filename = name
        
        if self.append:
            outfile = open(self.filename, 'a')
        else:
            outfile = open(self.filename, 'w')
            
        self.csvWriter = csv.writer(outfile, delimiter=self.delimiter, \
                                    quotechar='|', \
                                    quoting=csv.QUOTE_MINIMAL)

        # Whenver we start a new CSV file, we need to insert a line
        # of headers. These might not be available until run-time.
        self.write_headers = True
        

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
            
            headers = ['uuid', 'INPUTS']
            
            for name in case.keys(iotype='in'):
                headers.append(name)
                
            headers.append('OUTPUTS')
            for name in case.keys(iotype='out'):
                headers.append(name)
                    
            self.csvWriter.writerow(headers)
            self.write_headers = False
            
        data = []
        
        data.append(case.uuid)
        
        self.csvWriter.writerow(data)

    def get_iterator(self):
        return CSVCaseIterator(self.cases)