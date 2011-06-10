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
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.uses_derivatives import UsesGradients
from openmdao.util.decorators import add_delegate

def _findname(input_name, output_name):
    """ Assemble the name string for a derivative output based on its input
    and output name."""
    
    # Sometimes a parameter is connected to multiple inputs.
    if isinstance(input_name, tuple):
        input_name = input_name[0]
    
    return "d__%s__%s" % (output_name.replace('.', '_'),
                          input_name.replace('.', '_'))


@add_delegate(HasParameters, HasObjectives, UsesGradients)
class SensitivityDriver(Driver):
    """Driver to calculate the gradient of a workflow, and return
    it as a driver output. The gradient is calculated from all
    inputs (Parameters) to all outputs (Objectives).
    
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
        outputs = self.get_objectives().keys()
        #outputs = ['Obj%s' % i for i in range(len(self.list_objectives()))]
        
        for input_name in inputs:
            for output_name in outputs:
                
                var_name = _findname(input_name, output_name)
                
                self.add_trait(var_name, Float(0.0, iostatus='out',
                           desc = 'Deritavive output from SensitivityDriver'))
            
    def execute(self):
        """Calculate the gradient of the workflow."""
        
        # Calculate gradient of the workflow
        self.calc_derivatives(first=True)
        self.ffd_order = 1
        self.differentiator.calc_gradient()
        self.ffd_order = 0
            
        inputs = self.get_parameters().keys()
        outputs = self.get_objectives().keys()
        #outputs = ['Obj%s' % i for i in range(len(self.list_objectives()))]
        
        for i, input_name in enumerate(inputs):
            for j, output_name in enumerate(outputs):
                
                var_name = _findname(input_name, output_name)
                
                setattr(self, var_name,
                        self.differentiator.gradient_obj[i,j])
                
        # Sensitivity is sometimes run sequentially using different submodels,
        # so we need to return the state to the baseline value.
        self.differentiator.reset_state()
                
        
    def _check(self):
        """Make sure we aren't missing input or output"""
        
        if len(self.get_parameters().values()) < 1:
            msg = "Missing inputs for gradient calculation"
            self.raise_exception(msg, ValueError)
        
        if len(self.get_objectives().values()) < 1:
            msg = "Missing outputs for gradient calculation"
            self.raise_exception(msg, ValueError)

        
    