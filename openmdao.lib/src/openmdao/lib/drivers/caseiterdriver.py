__all__ = ('CaseIteratorDriver',)
__version__ = '0.1'

from openmdao.main import Driver, Case
from openmdao.main.component import RUN_OK, RUN_FAILED, RUN_STOPPED
from openmdao.main.interfaces import ICaseIterator

class CaseIteratorDriver(Driver):
    """
    Just a stand-in until we have a concurrent-capable driver.
    The intent is that a set of cases to be run is provided by
    an ICaseIterator, and the system does its best to run those cases.
    """

    def __init__(self, name, parent=None, doc=None):
        super(CaseIteratorDriver, self).__init__(name, parent)
        self.add_socket('iterator', ICaseIterator, 'Cases to evaluate')
        self.add_socket('outerator', None, 'Something to append() to')

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

    def execute(self):
        """ Run each case in iterator and record results in outerator. """
        if not self.check_socket('iterator'):
            self.error('No iterator plugin')
            return RUN_FAILED

        if not self.check_socket('outerator'):
            self.error('No outerator plugin')
            return RUN_FAILED

        for case in self.iterator:
            if self._stop:
                return RUN_STOPPED

            for name, index, value in case.inputs:
                try:
                    self.parent.set(name, value, index)
                except Exception, exc:
                    msg = "Exception setting '%s': %s" % (name, str(exc))
                    self.error(msg)
                    self.outerator.append(Case(case.inputs, None,
                                               RUN_FAILED, msg))
                    break
            else:
                status = self.parent.workflow.run()
                if status == RUN_OK:
                    results = []
                    for name, index, value in case.outputs:
                        try:
                            value = self.parent.get(name, index)
                            results.append((name, index, value))
                        except Exception, exc:
                            msg = "Exception getting '%s': %s" % (name, str(exc))
                            self.error(msg)
                            self.outerator.append(Case(case.inputs, None,
                                                       RUN_FAILED, msg))
                            break
                    else:
                        self.outerator.append(Case(case.inputs, results,
                                                   RUN_OK, ''))
                else:
                    self.outerator.append(Case(case.inputs, None, status, ''))
        return RUN_OK

