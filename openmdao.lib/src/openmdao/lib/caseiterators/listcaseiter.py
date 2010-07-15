
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
        self._cases = []
        self._cases.extend(cases)

    def __iter__(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which just returns list items in-order, emptying
        the list as it goes.
        """
        while self._cases:
            yield self._cases.pop(0)

