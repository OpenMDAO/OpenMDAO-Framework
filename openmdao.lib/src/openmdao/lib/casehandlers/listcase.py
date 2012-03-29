"""A Case Iterator and CaseRecorder that stores the cases in a list.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator


class ListCaseIterator(list):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, cases):
        super(ListCaseIterator, self).__init__(cases)


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

    def close(self):
        """Does nothing."""
        return

    def get_iterator(self):
        '''Return ListCaseIterator that uses our current list.'''
        return ListCaseIterator(self.cases)
    
