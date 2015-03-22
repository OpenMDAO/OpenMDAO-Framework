""" A simple driver that runs cases from a CaseIterator and records them
with a CaseRecorder. """

from openmdao.main.api import Driver
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.hasparameters import HasVarTreeParameters
from openmdao.main.hasresponses import HasVarTreeResponses
from openmdao.main.interfaces import IHasResponses, IHasParameters, implements
from openmdao.main.variable import is_legal_name, make_legal_path
from openmdao.main.array_helpers import flattened_value

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

        # Prepare parameters and responses.
        exprs = {}
        case_paths = {}
        inputs = []
        values = []
        for path in self.get_parameters():
            if isinstance(path, tuple):
                for target in path:
                    inputs.append(target)
                    if not is_legal_name(target):
                        exprs[target] = ExprEvaluator(target)
                path = path[0]
            else:
                inputs.append(path)
                if not is_legal_name(path):
                    exprs[path] = ExprEvaluator(path)

            path = make_legal_path(path)
            values.append(self.get('case_inputs.'+path))

        for path in self.get_responses():
            if not is_legal_name(path):
                exprs[path] = ExprEvaluator(path)
            case_paths[path] = make_legal_path(path)

        length = len(values[0]) if values else 0
        self.init_responses(length)

        # Run each parameter set.
        for i in range(length):

            # Set inputs.
            for j, path in enumerate(inputs):
                value = values[j][i]
                expr = exprs.get(path)
                self.set_parameter_by_name(path, value)

            # Run workflow.
            self.run_iteration()

            # Get outputs.
            for path in self.get_responses():
                expr = exprs.get(path)
                if expr:
                    value = expr.evaluate(self.parent)
                else:
                    value = self.parent.get(path)
                path = case_paths[path]
                self.set('case_outputs.%s[%d]'%(path,i), value)
