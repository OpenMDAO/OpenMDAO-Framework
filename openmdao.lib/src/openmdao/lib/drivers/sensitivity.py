"""
    sensitivity.py -- Driver to calculate the gradient of a workflow, and return
    it as a driver output. 
    
    SensitivityDriver includes a differentiator slot where the differentiation
    method can be plugged. Fake finite difference is supported.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['SensitivityDriver']

# pylint: disable-msg=E0611,F0401
from numpy import zeros

from openmdao.lib.datatypes.api import Array, Float
from openmdao.main.api import Driver
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.uses_derivatives import UsesGradients
from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters, HasObjectives, HasConstraints, UsesGradients)
class SensitivityDriver(Driver):
    """Driver to calculate the gradient of a workflow, and return
    it as a driver output. The gradient is calculated from all
    inputs (Parameters) to all outputs (Objectives and Constraints).
    
    SensitivityDriver includes a differentiator slot where the differentiation
    method can be plugged. Fake finite difference is supported.
    """
    
    dF = Array(zeros((0,0),'d'), iotype='out', desc='Sensitivity of the '
               'objectives withrespect to the parameters.')
    dG = Array(zeros((0,0),'d'), iotype='out', desc='Sensitivity of the '
               'constraints withrespect to the parameters.')
    
    dF_names = Array(zeros((0,0),'d'), iotype='out', desc='Objective names that'
                     'correspond to our indices')
    dG_names = Array(zeros((0,0),'d'), iotype='out', desc='Sensitivity of the constraints with'
               'respect to the parameters.')
    
    def execute(self):
        """Calculate the gradient of the workflow."""
        
        self._check()
        
        # Calculate gradient of the workflow
        self.calc_derivatives(first=True)
        self.ffd_order = 1
        self.differentiator.calc_gradient()
        self.ffd_order = 0
            
        inputs = self.get_parameters().keys()
        objs = self.get_objectives().keys()
        constraints = list(self.get_eq_constraints().keys() + \
                           self.get_ineq_constraints().keys())
        
        self.dF = zeros((len(objs), len(inputs)), 'd')
        self.dG = zeros((len(objs), len(inputs)), 'd')

        for i, input_name in enumerate(inputs):
            for j, output_name in enumerate(objs):
                self.dF[j][i] = self.differentiator.get_derivative(output_name, 
                                                                   wrt=input_name)
                
            for j, output_name in enumerate(constraints):
                self.dG[j][i] = self.differentiator.get_derivative(output_name, 
                                                                   wrt=input_name)
                
        # Sensitivity is sometimes run sequentially using different submodels,
        # so we need to return the state to the baseline value.
        self.differentiator.reset_state()
                
        
    def _check(self):
        """Make sure we aren't missing inputs or outputs"""
        
        if len(self.get_parameters().values()) < 1:
            msg = "Missing inputs for gradient calculation"
            self.raise_exception(msg, ValueError)
        
        if len(self.get_objectives().values()) + \
           len(self.get_eq_constraints().values()) + \
           len(self.get_ineq_constraints().values()) < 1:
            msg = "Missing outputs for gradient calculation"
            self.raise_exception(msg, ValueError)

        
    