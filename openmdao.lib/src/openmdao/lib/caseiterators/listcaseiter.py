
from enthought.traits.api import HasTraits, implements

from openmdao.main.interfaces import ICaseIterator

class ListCaseIterator(HasTraits):
    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """
    
    implements(ICaseIterator)
    
    def __init__(self, cases):
        super(ListCaseIterator, self).__init__()
        self._cases = list(cases)

    def __getitem__(self, key):
        return self._cases[key]

    def __iter__(self):
        return self._next_case()
    
    def __len__(self):
        return len(self._cases)

    def _next_case(self):
        """ Generator which just returns list items in-order"""
        for case in self._cases:
            yield case

