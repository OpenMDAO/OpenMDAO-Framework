""" Differentiates a driver's workflow using the Chain Rule with Numerical 
Derivatives (CRND) method.
"""

from ordereddict import OrderedDict

from enthought.traits.api import HasTraits

from openmdao.lib.datatypes.api import Float
from openmdao.main.interfaces import implements, IDifferentiator
from openmdao.main.numpy_fallback import array


class ChainRule(HasTraits):
    """ Differentiates a driver's workflow using the Chain Rule with Numerical
    Derivatives (CRND) method"""

    implements(IDifferentiator)
    
    # Local FD might need a stepsize
    default_stepsize = Float(1.0e-6, iotype='in', desc='Default finite ' + \
                             'difference step size.')

    def __init__(self):

        # This gets set in the callback
        _parent = None
        
        self.param_names = []
        self.objective_names = []
        self.eqconst_names = []
        self.ineqconst_names = []
        
        self.gradient = {}
        self.hessian = {}
    
    def setup(self):
        """Sets some dimensions."""

        self.param_names = self._parent.get_parameters().keys()
        self.objective_names = self._parent.get_objectives().keys()
        
        try:
            self.ineqconst_names = self._parent.get_ineq_constraints().keys()
        except AttributeError:
            self.ineqconst_names = []
        try:
            self.eqconst_names = self._parent.get_eq_constraints().keys()
        except AttributeError:
            self.eqconst_names = []
        
        
    def get_derivative(self, output_name, wrt):
        """Returns the derivative of output_name with respect to wrt.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
            
        wrt: string
            Name of the input in the local OpenMDAO hierarchy. The
            derivative is with respect to this variable.
        """
        
        return self.gradient[wrt][output_name]

    
    def get_2nd_derivative(self, output_name, wrt):
        """Returns the 2nd derivative of output_name with respect to both vars
        in the tuple wrt.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
            
        wrt: tuple containing two strings
            Names of the inputs in the local OpenMDAO hierarchy. The
            derivative is with respect to these 2 variables.
        """
        
        return self.hessian[wrt[0]][wrt[1]][output_name]

    
    def get_gradient(self, output_name=None):
        """Returns the gradient of the given output with respect to all 
        parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """
        
        return array([self.gradient[wrt][output_name] for wrt in self.param_names])
        
        
    def get_Hessian(self, output_name=None):
        """Returns the Hessian matrix of the given output with respect to
        all parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """
        
        return array([self.hessian[in1][in2][output_name] for (in1,in2) in product(self.param_names, self.param_names)])


    def calc_gradient(self):
        """Calculates the gradient vectors for all outputs in this Driver's
        workflow."""
        
        self.setup()

        # Create our 2D dictionary the first time we execute.
        if not self.gradient:
            for name in self.param_names:
                self.gradient[name] = {}
        
        # Determine gradient of model outputs wrt each parameter
        for wrt in self.param_names:
                    
            derivs = { wrt: 1.0 }
            
            self._chain_workflow(derivs, self._parent)

            # Finite difference on the objectives.
            for obj_name, expr in self._parent.get_objectives().iteritems():
            
                obj_grad = expr.evaluate_gradient(scope=self._parent.parent,
                                                  wrt=derivs.keys())
                obj_deriv = 0.0
                for input_name, val in obj_grad.iteritems():
                    obj_deriv += val*derivs[input_name]
                    
                self.gradient[wrt][obj_name] = obj_deriv
                
            # Finite difference on the constraints.
            for con_name, constraint in self._parent.get_constraints().iteritems():
                
                lhs, rhs, comparator, _ = \
                    constraint.evaluate_gradient(scope=self._parent.parent,
                                                 wrt=derivs.keys())
                
                con_deriv = 0.0
                con_vals = {}
                if '>' in comparator:
                    for input_name, val in lhs.iteritems():
                        con_vals[input_name] = -val
                        
                    for input_name, val in rhs.iteritems():
                        if input_name in con_vals:
                            con_vals[input_name] += val
                        else:
                            con_vals[input_name] = val
                            
                else:
                    for input_name, val in lhs.iteritems():
                        con_vals[input_name] = val
                        
                    for input_name, val in rhs.iteritems():
                        if input_name in con_vals:
                            con_vals[input_name] -= val
                        else:
                            con_vals[input_name] = val

                for input_name, val in con_vals.iteritems():
                    con_deriv += val*derivs[input_name]
                    
                self.gradient[wrt][con_name] = con_deriv

    def _chain_workflow(self, derivs, scope):
        """Process a workflow, calculating all intermediate derivatives
        using the chain rule. This can be called recursively to handle
        nested assemblies."""

        # Loop through each comp in the workflow
        for node in scope.workflow.__iter__():
            
            node_name = node.name
    
            incoming_deriv_names = {}
            
            # This component can determine its derivatives.
            if hasattr(node, 'calculate_first_derivatives'):
                
                node.calc_derivatives(first=True)
                
                local_inputs = node.derivatives.in_names
                local_outputs = node.derivatives.out_names
                local_derivs = node.derivatives.first_derivatives
                
                for input_name in local_inputs:
                    
                    full_name = '.'.join([node_name, input_name])
                    
                    # Only look at connected local_inputs or parameters
                    if full_name in node.parent._depgraph._allsrcs or \
                       full_name in self.param_names:
                
                        source = node.parent._depgraph.get_source(full_name)
                        
                        # Only process inputs who are connected to outputs
                        # with derivatives in the chain
                        if source in derivs:
                            incoming_deriv_names[input_name] = source
                        # or who are connected to one of the parameters
                        elif full_name in derivs:
                            incoming_deriv_names[input_name] = full_name
                            
                            
            # This component must be finite differenced.
            else:
                raise NotImplementedError('Finite Difference in CRND')
            
            
            # CHAIN RULE
            # Propagate derivatives wrt parameter through current component
            for output_name in local_outputs:
                
                full_output_name = '.'.join([node_name, output_name])
                derivs[full_output_name] = 0.0
                
                for input_name, full_input_name in incoming_deriv_names.iteritems():
                    derivs[full_output_name] += \
                        local_derivs[output_name][input_name] * \
                        derivs[full_input_name]
                        

    def calc_hessian(self, reuse_first=False):
        """Returns the Hessian matrix for all outputs in the Driver's
        workflow.
        
        This method is not implemented yet.
        
        reuse_first: bool
            Switch to reuse some data from the gradient calculation so that
            we don't have to re-run some points we already ran (namely the
            baseline, +eps, and -eps cases.) Obviously you do this when the
            driver needs gradient and hessian information at the same point,
            and calls calc_gradient before calc_hessian.
        """
        
        #self.setup()
        
        # Create our 3D dictionary the first time we execute.
        #if not self.hessian:
        #    for name1 in self.param_names:
        #        self.hessian[name1] = {}
        #        for name2 in self.param_names:
        #            self.hessian[name1][name2] = {}
                
        #self.hessian_ondiag_case = OrderedDict()
        #self.hessian_offdiag_case = OrderedDict()

        raise NotImplementedError('Hessian calculation')

        
    
    def reset_state(self):
        """Local finite differences do not leave comps in a clean state. If you
        require one, then run this method."""
        
        pass
        
    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        name = find_name(self._parent, self)
        self._parent.raise_exception("%s: %s" % (name,msg), exception_class)
