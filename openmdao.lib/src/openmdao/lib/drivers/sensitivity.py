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
from openmdao.lib.datatypes.api import Float
from openmdao.main.api import Driver
from openmdao.main.derivatives import derivative_name
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.uses_derivatives import UsesGradients
from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters, HasObjectives, UsesGradients, HasConstraints)
class SensitivityDriver(Driver):
    """Driver to calculate the gradient of a workflow, and return
    it as a driver output. The gradient is calculated from all
    inputs (Parameters) to all outputs (Objectives and Constraints).
    
    SensitivityDriver includes a differentiator slot where the differentiation
    method can be plugged. Fake finite difference is supported.
    """
    
    def create_outputs(self):
        """Creates OpenMDAO variables for the gradient outputs.
        
        The derivative outputs are named based on their location in
        the local hierarchy. Dots in the dotted path are replaced by
        underscores.
        
        For example, if our input is comp1.x and the output is comp1.y,
        then the derivative will be named:
        
        d__comp1_y__comp1_x
        
        Double underscores separate the numerator and denominator.
        """
        
        self._check()
        
        inputs = self.get_parameters().keys()
        objectives = self.get_objectives().keys()
        eq_con = self.get_eq_constraints().keys()
        ineq_con = self.get_ineq_constraints().keys()
        
        for input_name in inputs:
            
            for output_name in objectives:
                
                var_name = derivative_name(input_name, output_name)
                
                self.add_trait(var_name, Float(0.0, iostatus='out',
                           desc = 'Derivative output from SensitivityDriver'))
            
            for output_name in eq_con:
                
                var_name = derivative_name(input_name, output_name)
                
                self.add_trait(var_name, Float(0.0, iostatus='out',
                           desc = 'Derivative output from SensitivityDriver'))
            
            for output_name in ineq_con:
                
                var_name = derivative_name(input_name, output_name)
                
                self.add_trait(var_name, Float(0.0, iostatus='out',
                           desc = 'Derivative output from SensitivityDriver'))
            
    def execute(self):
        """Calculate the gradient of the workflow."""
        
        # Calculate gradient of the workflow
        self.calc_derivatives(first=True)
        self.ffd_order = 1
        self.differentiator.calc_gradient()
        self.ffd_order = 0
            
        inputs = self.get_parameters().keys()
        objectives = self.get_objectives().keys()
        eq_con = self.get_eq_constraints().keys()
        ineq_con = self.get_ineq_constraints().keys()
        
        for input_name in inputs:
            for output_name in list(objectives + eq_con + ineq_con):
                
                var_name = derivative_name(input_name, output_name)
                setattr(self, var_name,
                        self.differentiator.get_derivative(output_name, wrt=input_name))
                
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

        
    