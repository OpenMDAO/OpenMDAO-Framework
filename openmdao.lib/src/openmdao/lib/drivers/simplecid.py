""" A simple driver that runs cases from a CaseIterator and records them
with a CaseRecorder. """

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Driver
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.datatypes.api import Slot
testdict = {}

class SimpleCaseIterDriver(Driver):
    """
    A Driver that sequentially runs a set of cases provided by an
    :class:`ICaseIterator` and optionally records the results in a
    :class:`CaseRecorder`. This is intended for test cases or very simple
    models only. For a more full-featured Driver with similar functionality,
    see :class:`CaseIteratorDriver`.

    - The `iterator` socket provides the cases to be evaluated.

    For each case coming from the `iterator`, the workflow will
    be executed once.
    """

    # pylint: disable-msg=E1101
    iterator = Slot(ICaseIterator, desc='Source of Cases.', required=True)

    def __init__(self):
        super(SimpleCaseIterDriver, self).__init__()
        self._iter = None  # Set to None when iterator is empty.
        self.on_trait_change(self._iterator_modified, 'iterator')

    def _iterator_modified(self, obj, name, value):
        self._call_execute = True

    def _pre_execute(self, force=False):
        super(SimpleCaseIterDriver, self)._pre_execute(force)

    def execute(self):
        """ Run each case in `iterator`. """
        for case in self.iterator:
            self._run_case(case)

    def _run_case(self, case):
        msg = None
        case.parent_uuid = self._case_id

        # Additional user-requested variables
        # These must be added here so that the outputs are in the cases
        # before they are in the server list.
        top = self.parent
        while top.parent:
            top = top.parent
        inputs, outputs = top.get_case_variables()
        for var, val in inputs:
            case.add_input(var, val)
        for var, val in outputs:
            case.add_output(var, val)
        case.add_output('%s.workflow.itername' % self.name, self.itername)

        case.apply_inputs(self.parent)
        try:
            self.workflow.run(case_id=case.uuid, record_case=False)
        except Exception as err:
            msg = str(err)
        try:
            case.update_outputs(self.parent, msg)
        except Exception as err:
            if msg is None:
                case.msg = str(err)
            else:
                case.msg = msg + ":" + str(err)

        top = self.parent
        while top.parent:
            top = top.parent
        for recorder in top.recorders:
            recorder.record(case)

