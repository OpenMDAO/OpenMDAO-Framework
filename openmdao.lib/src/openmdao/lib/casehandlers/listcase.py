"""A Case Iterator and CaseRecorder that stores the cases in a list.
"""

# pylint: disable=E0611,F0401
from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator
from openmdao.main.case import Case


class ListCaseIterator(object):

    """An iterator that returns :class:`Case` objects from a passed-in iterator
    of cases. This can be useful for runtime-generated cases from an
    optimizer, etc.
    """

    implements(ICaseIterator)

    def __init__(self, cases):
        self._cases = cases

    def __getitem__(self, num):
        return self._cases[num]

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""

        attrs = {}
        attrs['type'] = type(self).__name__

        return attrs


class ListCaseRecorder(object):

    """Stores cases in a list."""

    implements(ICaseRecorder)

    def __init__(self):
        self.cases = []
        self._cfg_map = {}

    def __len__(self):
        return len(self.cases)

    def startup(self):
        """ Nothing needed for a list case."""
        pass

    def register(self, driver, inputs, outputs):
        """Register names for later record call from `driver`."""
        self._cfg_map[driver] = (inputs, outputs)

    def record_constants(self, constants):
        """Record constant data - currently ignored."""
        pass

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """Store the case in our internal list."""
        in_names, out_names = self._cfg_map[driver]
        self.cases.append(Case(zip(in_names, inputs), zip(out_names, outputs),
                               exc, case_uuid, parent_uuid))

    def close(self):
        """Does nothing."""
        return

    def get_iterator(self):
        '''Return ListCaseIterator that uses our current list.'''
        return ListCaseIterator(self.cases)

    def get_attributes(self, io_only=True):
        """ We need a custom get_attributes because we aren't using Traits to
        manage our changeable settings. This is unfortunate and should be
        changed to something that automates this somehow."""

        attrs = {}
        attrs['type'] = type(self).__name__

        return attrs
