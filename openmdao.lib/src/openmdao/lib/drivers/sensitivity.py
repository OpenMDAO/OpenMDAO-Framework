"""
    sensitivity.py -- Driver to calculate the gradient of a workflow, and return
    it as a driver output. 
    
    SensitivityDriver includes a differentiator slot where the differentiation
    method can be plugged. Fake finite difference is supported.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['SensitivityDriver']

import logging

from openmdao.main.numpy_fallback import zeros

from openmdao.main.datatypes.api import Array, List
from openmdao.main.driver_uses_derivatives import DriverUsesDerivatives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import IHasParameters, IHasObjectives, IHasConstraints, implements

@add_delegate(HasParameters, HasObjectives, HasConstraints)
class SensitivityDriver(DriverUsesDerivatives):
    """Driver to calculate the gradient of a workflow, and return
    it as a driver output. The gradient is calculated from all
    inputs (Parameters) to all outputs (Objectives and Constraints).
    
    SensitivityDriver includes a differentiator slot where the differentiation
    method can be plugged. Fake finite difference is supported.
    """

    implements(IHasParameters, IHasObjectives, IHasConstraints)
    
    dF = Array(zeros((0, 0),'d'), iotype='out', desc='Sensitivity of the '
               'objectives withrespect to the parameters. Index 1 is the '
               'objective output, while index 2 is the parameter input')
    dG = Array(zeros((0, 0),'d'), iotype='out', desc='Sensitivity of the '
               'constraints withrespect to the parameters. Index 1 is the '
               'constraint output, while index 2 is the parameter input')
    
    F = Array(zeros((0, 0),'d'), iotype='out', desc='Values of the objectives '
               'which sensitivities are taken around.')
    G = Array(zeros((0, 0),'d'), iotype='out', desc='Values of the constraints '
               'which sensitivities are taken around.')
    
    dF_names = List([], iotype='out', desc='Objective names that'
                     'correspond to our array indices')
    dG_names = List([], iotype='out', desc='Constraint names that'
                     'correspond to our array indices')
    dx_names = List([], iotype='out', desc='Parameter names that'
                     'correspond to our array indices')
    
    F = Array(zeros(0,'d'), iotype='out', desc='Objective baseline values '
                        'where sensitivity is evaluated.')
    G = Array(zeros(0,'d'), iotype='out', desc='Constraint baseline values '
                        'where sensitivity is evaluated.')
    x = Array(zeros(0,'d'), iotype='out', desc='Parameter baseline values '
                        'where sensitivity is evaluated.')
    
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
        self.dG = zeros((len(constraints), len(inputs)), 'd')
        self.F = zeros(len(objs), 'd')
        self.G = zeros(len(constraints), 'd')
        self.x = zeros(len(inputs), 'd')
        self.dF_names = []
        self.dG_names = []
        self.dx_names = []
        
        for i, input_name in enumerate(inputs):
            
            self.dx_names.append(input_name)
            self.x[i] = self.differentiator.base_param[input_name]
            
            for j, output_name in enumerate(objs):
                self.dF[j][i] = self.differentiator.get_derivative(output_name, 
                                                                   wrt=input_name)
                self.dF_names.append(output_name)
                self.F[j] = self.differentiator.base_data[output_name]
                
            for j, output_name in enumerate(constraints):
                self.dG[j][i] = self.differentiator.get_derivative(output_name, 
                                                                   wrt=input_name)
                self.dG_names.append(output_name)
                self.G[j] = self.differentiator.base_data[output_name]
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

        
    