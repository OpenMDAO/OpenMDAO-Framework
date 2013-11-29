"""
    sensitivity.py -- Driver to calculate the gradient of a workflow and return
    it as a driver output. OpenMDAO's gradient capability is utilized.

"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['SensitivityDriver']

from openmdao.main.numpy_fallback import zeros

from openmdao.main.datatypes.api import Array, List
from openmdao.main.driver_uses_derivatives import Driver
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import IHasParameters, IHasObjectives, \
                                     IHasConstraints, implements

@add_delegate(HasParameters, HasObjectives, HasConstraints)
class SensitivityDriver(Driver):
    """Driver to calculate the gradient of a workflow and return
    it as a driver output. The gradient is calculated from all
    inputs (Parameters) to all outputs (Objectives and Constraints).

    SensitivityDriver includes requires OpenMDAO to calculate a gradient.
    Fake Finite Difference is supported.
    """

    implements(IHasParameters, IHasObjectives, IHasConstraints)

    dF = Array(zeros((0, 0),'d'), iotype='out', desc='Sensitivity of the '
               'objectives with respect to the parameters. Index 1 is the '
               'objective output, while index 2 is the parameter input.')
    dG = Array(zeros((0, 0),'d'), iotype='out', desc='Sensitivity of the '
               'constraints with respect to the parameters. Index 1 is the '
               'constraint output, while index 2 is the parameter input.')

    F = Array(zeros((0, 0),'d'), iotype='out', desc='Values of the objectives '
               'which sensitivities are taken around.')
    G = Array(zeros((0, 0),'d'), iotype='out', desc='Values of the constraints '
               'which sensitivities are taken around.')

    dF_names = List([], iotype='out', desc='Objective names that '
                     'correspond to our array indices.')
    dG_names = List([], iotype='out', desc='Constraint names that '
                     'correspond to our array indices.')
    dx_names = List([], iotype='out', desc='Parameter names that '
                     'correspond to our array indices.')

    F = Array(zeros(0,'d'), iotype='out', desc='Objective baseline values '
                        'where sensitivity is evaluated.')
    G = Array(zeros(0,'d'), iotype='out', desc='Constraint baseline values '
                        'where sensitivity is evaluated.')
    x = Array(zeros(0,'d'), iotype='out', desc='Parameter baseline values '
                        'where sensitivity is evaluated.')

    def execute(self):
        """Calculate the gradient of the workflow."""

        self._check()

        # Run our iteration once, since we can't guarantee it has been.
        self.run_iteration()

        objs = self.get_objectives().keys()
        constraints = self.get_constraints().keys()

        inputs = self.list_param_group_targets()
        obj = self.list_objective_targets()
        con = self.list_constraint_targets()

        nparm = self.total_parameters()
        nobj = len(obj)
        ncon = len(con)

        self.dF = zeros((nobj, nparm), 'd')
        self.dG = zeros((ncon, nparm), 'd')

        self.dF_names = objs
        self.dG_names = constraints
        self.dx_names = inputs

        self.F = self.eval_objectives()
        self.G = self.eval_constraints(self.parent)
        self.x = self.eval_parameters(self.parent)

        # Finally, calculate gradient
        J = self.workflow.calc_gradient(inputs, obj + con)

        self.dF = J[0:nobj, :]

        n1 = nobj
        n2 = nobj + ncon
        self.dG = J[n1:n2, :]

        self.record_case()


    def _check(self):
        """Make sure we aren't missing inputs or outputs."""

        if self.total_parameters() < 1:
            msg = "Missing inputs for gradient calculation"
            self.raise_exception(msg, ValueError)

        if len(self.get_objectives()) + self.total_constraints() < 1:
            msg = "Missing outputs for gradient calculation"
            self.raise_exception(msg, ValueError)

