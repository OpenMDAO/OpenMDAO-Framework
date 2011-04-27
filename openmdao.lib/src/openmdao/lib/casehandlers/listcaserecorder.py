
import sys

from openmdao.lib.datatypes.api import implements
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator

from openmdao.main.interfaces import ICaseRecorder

class ListCaseRecorder(object):
    """Stores cases in a list."""
    
    implements(ICaseRecorder)
    
    def __init__(self):
        self.cases = []
        
    def __len__(self):
        return len(self.cases)

    def record(self, case):
        """Store the case in our internal list."""
        self.cases.append(case)

    def get_iterator(self):
        return ListCaseIterator(self.cases)