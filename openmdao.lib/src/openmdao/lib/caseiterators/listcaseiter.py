
from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator

class ListCaseIterator(object):                
    """An iterator that returns :class:`Case` objects from a passed-in list
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.

    """

    implements(ICaseIterator)

    def __init__(self, cases):
        self._cases = []
        self._cases.extend(cases)

    def __iter__(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which just returns list items in-order. """
        while self._cases:
            yield self._cases.pop(0)

