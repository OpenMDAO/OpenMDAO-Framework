import os.path

from openmdao.main.api import Component, Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import ICaseIterator, ICaseRecorder
from openmdao.lib.api import Instance

class SimpleCaseIterDriver(Driver):
    """
    A Driver that runs a set of cases provided by an :class:`ICaseIterator`
    sequentially and records the results in a :class:`CaseRecorder`. This is
    intended for test cases or very simple models only. For a more full
    featured Driver with similar functionality, see
    :class:`CaseIteratorDriver`.

    - The `iterator` socket provides the cases to be evaluated.
    - The `recorder` socket is used to record results.
    
    For each case coming from the `iterator`, the parent workflow will
    be executed once.
    """

    iterator = Instance(ICaseIterator, desc='source of Cases', required=True)
    recorder = Instance(ICaseRecorder, desc='where Case results are recorded', 
                        required=True)
    
    def __init__(self, *args, **kwargs):
        super(SimpleCaseIterDriver, self).__init__(*args, **kwargs)
        self._iter = None  # Set to None when iterator is empty.
        self.on_trait_change(self._iterator_modified, 'iterator')

    def _iterator_modified(self, obj, name, value):
        self._call_execute = True
    
    def execute(self):
        """ Runs each case in `iterator` and records results in `recorder`. """
        for case in self.iterator:
            self._run_case(case)

    def _run_case(self, case):
        msg = ''
        case.apply(self.parent)
        try:
            self.parent.workflow.run()
        except Exception as err:
            msg = str(err)
        try:
            case.update(self.parent, msg)
        except Exception as err:
            case.msg = msg + " : " + str(err)
        self.recorder.record(case)
        
    def step(self):
        """ Evaluate the next case. """
        self._stop = False
        if self._iter is None:
            self._iter = self.iterator.__iter__()

        try:
            case = self._iter.next()
        except StopIteration:
            self._iter = None
            raise

