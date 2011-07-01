
from openmdao.main.interfaces import implements, ICaseIterator

class ListCaseIterator(list):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, cases):
        super(ListCaseIterator, self).__init__(cases)
