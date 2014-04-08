""" A simple driver that runs cases from a CaseIterator and records them
with a CaseRecorder. """

from openmdao.main.api import Driver
from openmdao.main.hasparameters import HasVarTreeParameters
from openmdao.main.hasresponses import HasVarTreeResponses
from openmdao.main.interfaces import IHasResponses, IHasParameters, implements

from openmdao.util.decorators import add_delegate


@add_delegate(HasVarTreeParameters, HasVarTreeResponses)
class SimpleCaseIterDriver(Driver):
    """
    A Driver that sequentially runs each parameter set. This is intended for
    test cases or very simple models only. For a more full-featured Driver with
    similar functionality, see :class:`CaseIteratorDriver`.
    """

    implements(IHasParameters, IHasResponses)

    def execute(self):
        """ Run each parameter set. """
        inputs = []
        values = []
        for path in self.get_parameters():
            inputs.append(path)
            values.append(self.get('case_inputs.'+path))

        length = len(values[0])
        self.init_responses(length)

        for i in range(length):
            for j, path in enumerate(inputs):
                self.parent.set(path, values[j][i])
            self.workflow.run()
            for path in self.get_responses():
                value = self.parent.get(path)
                self.set('case_outputs.'+path, value, index=(i,), force=True)

