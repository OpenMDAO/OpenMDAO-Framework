"""
    sensitivity.py -- Driver to calculate the gradient of a workflow and return
    it as a driver output. OpenMDAO's gradient capability is utilized.

"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['SensitivityDriver']

from numpy import zeros

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
    """

    implements(IHasParameters, IHasObjectives, IHasConstraints)

    dF = Array(zeros((0, 0), 'd'), iotype='out', desc='Sensitivity of the '
               'objectives with respect to the parameters. Index 1 is the '
               'objective output, while index 2 is the parameter input.')
    dG = Array(zeros((0, 0), 'd'), iotype='out', desc='Sensitivity of the '
               'constraints with respect to the parameters. Index 1 is the '
               'constraint output, while index 2 is the parameter input.')

    dF_names = List([], iotype='out', desc='Objective names that '
                     'correspond to our array indices.')
    dG_names = List([], iotype='out', desc='Constraint names that '
                     'correspond to our array indices.')
    dx_names = List([], iotype='out', desc='Parameter names that '
                     'correspond to our array indices.')

    F = Array(zeros(0, 'd'), iotype='out', desc='Objective baseline values '
                        'where sensitivity is evaluated.')
    G = Array(zeros(0, 'd'), iotype='out', desc='Constraint baseline values '
                        'where sensitivity is evaluated.')
    x = Array(zeros(0, 'd'), iotype='out', desc='Parameter baseline values '
                        'where sensitivity is evaluated.')

    def execute(self):
        """Calculate the gradient of the workflow."""


        # Inital run to make sure the workflow executes
        self.run_iteration()

        inputs = self.list_param_group_targets()
        obj = self.list_objective_targets()
        con = self.list_constraint_targets()

        nobj = len(obj)
        ncon = self.total_constraints()

        self.dF_names = self.get_objectives().keys()
        self.dG_names = self.get_constraints().keys()
        self.dx_names = inputs

        self.F = self.eval_objectives()
        self.G = self.eval_constraints(self.parent)
        self.x = self.eval_parameters(self.parent)

        # Finally, calculate gradient
        J = self._calc_gradient(inputs, obj + con)

        self.dF = J[:nobj, :]
        self.dG = J[nobj:nobj+ncon, :]

    def init_var_sizes(self):
        """ Size up our outputs."""
        super(SensitivityDriver, self).init_var_sizes()
        self._check()

        n_param = len(self.eval_parameters())

        n_obj = 0
        for obj in self.list_objective_targets():
            n_obj += len(self.parent.get_flattened_value(obj))

        n_con = 0
        for con in self.list_constraint_targets():
            n_con += len(self.parent.get_flattened_value(con))

        self.x = zeros((n_param))
        self.F = zeros((n_obj))
        self.G = zeros((n_con))

        self.dF = zeros((n_obj, n_param))
        self.dG = zeros((n_con, n_param))

    def _check(self):
        """Make sure we aren't missing inputs or outputs."""

        if self.total_parameters() < 1:
            msg = "Missing inputs for gradient calculation"
            self.raise_exception(msg, ValueError)

        if len(self.get_objectives()) + self.total_constraints() < 1:
            msg = "Missing outputs for gradient calculation"
            self.raise_exception(msg, ValueError)

    def requires_derivs(self):
        return True
