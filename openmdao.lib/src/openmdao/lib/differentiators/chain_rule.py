""" Differentiates a driver's workflow using the Chain Rule with Numerical 
Derivatives (CRND) method.
"""

from ordereddict import OrderedDict

from enthought.traits.api import HasTraits

from openmdao.lib.datatypes.api import Float
from openmdao.main.interfaces import implements, IDifferentiator
from openmdao.main.api import Driver, Assembly
from openmdao.main.assembly import Run_Once
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
        
        return array([self.gradient[wrt][output_name] \
                      for wrt in self.param_names])
        
        
    def get_Hessian(self, output_name=None):
        """Returns the Hessian matrix of the given output with respect to
        all parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """
        
        return array([self.hessian[in1][in2][output_name] \
                      for (in1,in2) in product(self.param_names, self.param_names)])


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
            
            # Find derivatives for all component outputs in the workflow
            self._chain_workflow(derivs, self._parent, wrt)

            # Calculate derivative of the objectives.
            for obj_name, expr in self._parent.get_objectives().iteritems():
            
                obj_grad = expr.evaluate_gradient(scope=self._parent.parent,
                                                  wrt=derivs.keys())
                obj_deriv = 0.0
                for input_name, val in obj_grad.iteritems():
                    obj_deriv += val*derivs[input_name]
                    
                self.gradient[wrt][obj_name] = obj_deriv
                
            # Calculate derivatives of the constraints.
            for con_name, constraint in \
                self._parent.get_constraints().iteritems():
                
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

    def _chain_workflow(self, derivs, scope, param):
        """Process a workflow, calculating all intermediate derivatives
        using the chain rule. This can be called recursively to handle
        nested assemblies."""

        # Loop through each comp in the workflow
        for node in scope.workflow.__iter__():
            
            node_name = node.name
            #print "processing ", node_name
    
            incoming_deriv_names = {}
            incoming_derivs = {}
            
            # We don't handle nested drivers yet.
            if isinstance(node, Driver):
                raise NotImplementedError('Nested drivers')
            
            # Recurse into assemblies.
            elif isinstance(node, Assembly):
                
                if not isinstance(node.driver, Run_Once):
                    raise NotImplementedError('Nested drivers')
                
                self._recurse_assy(node, derivs, param)
                                     
            # This component can determine its derivatives.
            elif hasattr(node, 'calculate_first_derivatives'):
                
                node.calc_derivatives(first=True)
                
                local_inputs = node.derivatives.in_names
                local_outputs = node.derivatives.out_names
                local_derivs = node.derivatives.first_derivatives
                
                for input_name in local_inputs:
                    
                    full_name = '.'.join([node_name, input_name])

                    # Inputs who are hooked directly to the parameters
                    if full_name == param and \
                            full_name in derivs:
                            
                        incoming_deriv_names[input_name] = full_name
                        incoming_derivs[full_name] = derivs[full_name]
                        
                    # Inputs who are connected to something with a derivative
                    else:
                
                        sources = node.parent._depgraph.connections_to(full_name)
                        
                        # This list keeps track of duplicated sources, which
                        # are a biproduct of a connection to multiple inputs
                        # across a fake boundary node.
                        used_sources = []
                        
                        for source_tuple in sources:
                            
                            source = source_tuple[0]
                            expr_txt = node.parent._depgraph.get_source(source_tuple[1])
                            
                            # Variables on an assembly boundary
                            if source[0:4] == '@bin' and source.count('.') < 2:
                                source = source.replace('@bin.', '')
                            
                            # Only process inputs who are connected to outputs
                            # with derivatives in the chain
                            if expr_txt and source in derivs and \
                               source not in used_sources:
                                
                                # Need derivative of the expression
                                expr = node.parent._depgraph.get_expr(expr_txt)
                                expr_deriv = expr.evaluate_gradient(scope=node.parent,
                                                                    wrt=source)

                                incoming_deriv_names[input_name] = full_name
                                if full_name in incoming_derivs:
                                    incoming_derivs[full_name] += derivs[source] * \
                                        expr_deriv[source]
                                else:
                                    incoming_derivs[full_name] = derivs[source] * \
                                        expr_deriv[source]
                                    
                                used_sources.append(source)
                        
                            
                # CHAIN RULE
                # Propagate derivatives wrt parameter through current component
                for output_name in local_outputs:
                    
                    full_output_name = '.'.join([node_name, output_name])
                    derivs[full_output_name] = 0.0
                    
                    for input_name, full_input_name in incoming_deriv_names.iteritems():
                        derivs[full_output_name] += \
                            local_derivs[output_name][input_name] * \
                            incoming_derivs[full_input_name]
                            
            # This component must be finite differenced.
            else:
                raise NotImplementedError('CRND cannot Finite Difference subblocks yet.')
            

    def _recurse_assy(self, scope, upscope_derivs, upscope_param):
        """Enables assembly recursion by scope translation."""
        
        # Find all assembly boundary connections, and propagate
        # derivatives through the expressions.
        local_derivs = {}
        name = scope.name
        
        for item in scope._depgraph._depgraph.var_edges('@xin'):
            src = item[0].replace('@xin.','')
            upscope_src = src.replace('parent.','')
            dest = item[1]
            
            # Real connections on boundary
            if dest.count('.') < 2:
                dest = dest.split('.')[1]
                
            # Differentiate all expressions
            expr_txt = scope._depgraph.get_source(dest.replace('@bin.',''))
            expr = scope._depgraph.get_expr(expr_txt)
            expr_deriv = expr.evaluate_gradient(scope=scope,
                                                wrt=src)
            if dest in local_derivs:    
                local_derivs[dest] += upscope_derivs[upscope_src]*expr_deriv[src]
            else:
                local_derivs[dest] = upscope_derivs[upscope_src]*expr_deriv[src]
        
        param = upscope_param.split('.')
        if param[0] == name:
            param = param[1:].join('.')
        else:
            param = ''
        
        # Find derivatives for this assembly's workflow
        self._chain_workflow(local_derivs, scope.driver, param)
        
        # Convert scope and return gradient of connected components.
        for item in scope._depgraph._depgraph.var_in_edges('@bout'):
            src = item[0]
            upscope_src = '%s.%s' % (name, src)
            dest = item[1]
            
            # Real connections on boundary need expressions differentiated
            if dest.count('.') < 2:
                
                upscope_dest = dest.replace('@bout', name)
                dest = dest.replace('@bout.','')
                
                expr_txt = scope._depgraph.get_source(dest)
                expr = scope._depgraph.get_expr(expr_txt)
                expr_deriv = expr.evaluate_gradient(scope=scope,
                                                    wrt=src)
                
                upscope_derivs[upscope_dest] = local_derivs[src]*expr_deriv[src]
                
            # Fake connection, so just add source
            else:
                upscope_derivs[upscope_src] = local_derivs[src]
                
        # Finally, stuff in all our extra unconnected outputs because they may
        # be referenced by an objective or constraint at the outer scope.
        for key, value in local_derivs.iteritems():
            
            if key[0] == '@':
                continue
            
            target = '%s.%s' % (name, key)
            if target not in upscope_derivs:
                upscope_derivs[target] = value
            

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
