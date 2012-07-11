""" Differentiates a driver's workflow using the Chain Rule with Numerical 
Derivatives (CRND) method.
"""

from ordereddict import OrderedDict

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import HasTraits

from openmdao.lib.datatypes.api import Float
from openmdao.main.interfaces import implements, IDifferentiator, ISolver
from openmdao.main.api import Driver, Assembly
from openmdao.main.driver import Run_Once
from openmdao.main.container import find_name
from openmdao.main.mp_support import has_interface
from openmdao.main.numpy_fallback import array
from openmdao.units import convert_units

class ChainRule(HasTraits):
    """ Differentiates a driver's workflow using the Chain Rule with Numerical
    Derivatives (CRND) method."""

    implements(IDifferentiator)
    
    # pylint: disable-msg=E1101
    # Local FD might need a stepsize
    default_stepsize = Float(1.0e-6, iotype='in', desc='Default finite ' + \
                             'difference step size to use.')
    
    def __init__(self):

        # This gets set in the callback
        _parent = None
        
        self.param_names = []
        self.function_names = []
        
        self.gradient = {}
        self.hessian = {}
        
        # Stores the set of edges for which we need derivatives. Dictionary
        # key is the scope's pathname, since we need to store this for each
        # assembly recursion level.
        self.edge_dicts = OrderedDict()
    
    def setup(self):
        """Sets some dimensions."""

        self.param_names = self._parent.get_parameters().keys()
        objective_names = self._parent.get_objectives().keys()
        
        try:
            ineqconst_names = self._parent.get_ineq_constraints().keys()
        except AttributeError:
            ineqconst_names = []
        try:
            eqconst_names = self._parent.get_eq_constraints().keys()
        except AttributeError:
            eqconst_names = []
            
        self.function_names = objective_names + ineqconst_names + eqconst_names
        
        
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


    def _find_edges(self, scope, dscope):
        ''' Finds the minimum set of edges for which we need derivatives.
        These edges contain the inputs and outputs that are in the workflow
        of the driver, and whose source or target component has derivatives
        defined. Note that we only need one set of edges for all params.
        
        For each scope, we store a dictionary keyed by the components in the
        workflow. Each component has a tuple that contains the input and
        output varpaths that are active in the derivative calculation.
        
        A separate driver scope (dscope) can also be specified for drivers
        that are not the top driver ('driver') in the assembly's workflow.
        
        TODO - As part of this process, we will already need to know of any
        blocks that must be finite differenced together.
        '''
        
        scope_name = dscope.get_pathname()
        self.edge_dicts[scope_name] = OrderedDict()
        edge_dict = self.edge_dicts[scope_name]
        
        # Find our minimum set of edges part 1
        # Inputs and Outputs from interior edges
        driver_set = dscope.workflow.get_names()
        interior_edges = scope._parent._depgraph.get_interior_edges(driver_set)
        
        needed_in_edges = set([b for a, b in interior_edges])
        needed_edge_exprs = set([a for a, b in interior_edges])
        
        # Output edges are expressions, so we need to find the referenced
        # variables
        
        needed_edges = []
        for edge in needed_edge_exprs:
            expr = scope._parent._exprmapper.get_expr(edge)
            needed_edges.append(expr.refs().pop())
            
        needed_edges = set(needed_edges)
            
        # Find our minimum set of edges part 2
        # Inputs connected to parameters
        needed_in_edges = needed_in_edges.union(set(self.param_names))
        
        # Find our minimum set of edges part 3
        # Outputs connected to objectives
        for _, expr in self._parent.get_objectives().iteritems():
            varpaths = expr.get_referenced_varpaths()
            needed_edges = needed_edges.union(varpaths)
            
        # Find our minimum set of edges part 4
        # Outputs connected to constraints
        # Note: constraints have a left and right hand side expression.
        for _, expr in self._parent.get_constraints().iteritems():
            for item in [expr.lhs, expr.rhs]:
                varpaths = item.get_referenced_varpaths()
                needed_edges = needed_edges.union(varpaths)
                
        # Find our minimum set of edges part 5
        # If we are collecting edges for a solver-type driver, then we need
        # to add in the independents and dependents
        if has_interface(dscope, ISolver):
            
            params = dscope.get_parameters().keys()
            
            deps = []
            indeps = []
            for expr, constr in dscope.get_eq_constraints().iteritems():
                
                item1 = constr.lhs.get_referenced_varpaths()
                item2 = constr.rhs.get_referenced_varpaths()
                comps = list(item1.union(item2))
                    
                if comps[0] in params:
                    indep = comps[0]
                    dep = comps[1]
                elif comps[1] in params:
                    indep = comps[1]
                    dep = comps[0]
                else:
                    msg = "No independent in solver equation."
                    raise NotImplementedError(msg)
                
                deps.append(dep)
                indeps.append(indep)
            
            needed_edges = needed_edges.union(set(deps))
            needed_in_edges = needed_in_edges.union(set(indeps))
        
        # If we are at a deeper recursion level than the driver's assembly,
        # then we need to add the upscoped connected edges on the assembly
        # boundary
        if scope != self._parent:
            in_edges, out_edges = self._assembly_edges(scope)
            needed_edges = needed_edges.union(set(out_edges))
            needed_in_edges = needed_in_edges.union(set(in_edges))
        
        # Loop through each comp in the workflow and assemble our data
        # structures
        for node in dscope.workflow.__iter__():
    
            node_name = node.name
        
            # We don't handle nested drivers yet...
            # ... though the Analytic differentiator can handle solvers
            if isinstance(node, Driver):
                
                # There are no connections on an ISolver
                edge_dict[node_name] = ([], [])
            
            # Assemblies are tricky, but they should be able to calculate all
            # connected derivatives on their boundary.
            elif isinstance(node, Assembly):
                
                if not isinstance(node.driver, Run_Once):
                    raise NotImplementedError('Nested drivers')
                
                needed_inputs = []
                for edge in node.parent._depgraph.var_in_edges(node_name):
                    parts = edge[1].split('.')
                    edge = '.'.join(parts[1:])
                    needed_inputs.append(edge)
                    
                needed_outputs = []
                for edge_expr in node.parent._depgraph.var_edges(node_name):
                    expr = node._parent._exprmapper.get_expr(edge_expr[0])
                    edge = expr.refs().pop()
                    parts = edge.split('.')
                    edge = '.'.join(parts[1:])
                    needed_outputs.append(edge)
                    
                #needed_inputs = [b for a, b in \
                #                 node.parent._depgraph.var_in_edges(node_name)]
                #needed_outputs = [a for a, b in \
                #                  node.parent._depgraph.var_edges(node_name)]
                edge_dict[node_name] = (needed_inputs, needed_outputs)
                                         
            # This component can determine its derivatives. Only
            # derivatives that are defined should be added to our list of
            # edges.
            # NOTE - Derivatives in Functional form
            elif hasattr(node, 'calculate_first_derivatives'):
                
                # Note: if you haven't declared derivatives for some inputs
                # or outputs, then they are assumed to be zero, and are not
                # returned.
                local_inputs = node.derivatives.in_names
                local_outputs = node.derivatives.out_names
                
                needed_inputs = []
                for name in local_inputs:
                    full_name = '.'.join([node_name, name])
                    
                    if full_name in needed_in_edges:
                        needed_inputs.append(name)
                        
                needed_outputs = []
                for name in local_outputs:
                    full_name = '.'.join([node_name, name])
                    
                    if full_name in needed_edges:
                        needed_outputs.append(name)
                        
                edge_dict[node_name] = (needed_inputs, needed_outputs)
                
            # This component is part of a block that must be finite
            # differenced. These should provide all derivatives
            else:
                msg = 'CRND cannot Finite Difference subblocks yet.'
                raise NotImplementedError(msg)
            
        #print self.edge_dicts
                
    def _assembly_edges(self, scope):
        """ Helper function to return input and output edges for a nested assembly.
        Called recursively."""
        
        in_edges = []
        out_edges = []
        
        upscope = scope._parent.parent.driver
        scope_name = scope.get_pathname()
        upscope_name = upscope.get_pathname()
        parts = scope_name.split('.')
        assembly_name = '.'.join(parts[:-1])
        
        # All edges we need are stored in the higher assembly's edge list
        edge_dict = self.edge_dicts[upscope_name][assembly_name]
        
        # assembly inputs
        for item in edge_dict[0]:
            
            # If it has a dot in it, then it's a direct connection to an
            # interior object by the parent assy bypassing this assy.
            # TODO: What if we have a VarTree on the boundary?
            if '.' in item:
                in_edges.append(item)
                
            # Otherwise, real connections on assy boundary
            else:
                
                #find any inputs connected to this assembly input
                connects = scope._parent._depgraph.connections_to(item)
                for connect in connects:
                    if '@bin' in connect[0]:
                        in_edges.append(connect[1])
            
        # assembly outputs
        for item in edge_dict[1]:
                    
            # If it has a dot in it, then it's a direct connection to an
            # interior object by the parent assy bypassing this assy.
            # TODO: What if we have a VarTree on the boundary?
            if '.' in item:
                out_edges.append(item)
                
            # Otherwise, real connections on assy boundary
            else:
                
                #find any inputs connected to this assembly input
                connects = scope._parent._depgraph.connections_to(item)
                for connect in connects:
                    if '@bout' in connect[1]:
                        out_edges.append(connect[0])
                
        # Recurse into even higher subassies
        if upscope != self._parent:
            up_in_edges, up_out_edges = self._assembly_edges(upscope)
            
            in_edges += up_in_edges
            out_edges += up_out_edges
            
        return in_edges, out_edges
                 
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
        """Process a workflow calculating all intermediate derivatives
        using the chain rule. This can be called recursively to handle
        nested assemblies."""
        
        # Figure out what outputs we need
        scope_name = scope.get_pathname()
        if scope_name not in self.edge_dicts:
            self._find_edges(scope, scope)

        # Loop through each comp in the workflow
        for node in scope.workflow.__iter__():
            
            node_name = node.name
    
            incoming_deriv_names = {}
            incoming_derivs = {}
            ascope = node.parent
            
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
                
                local_inputs = self.edge_dicts[scope_name][node_name][0]
                local_outputs = self.edge_dicts[scope_name][node_name][1]
                local_derivs = node.derivatives.first_derivatives
                
                for input_name in local_inputs:
                    
                    full_name = '.'.join([node_name, input_name])

                    # Inputs who are hooked directly to the current param
                    if full_name == param:
                            
                        incoming_deriv_names[input_name] = full_name
                        incoming_derivs[full_name] = derivs[full_name]
                        
                    # Do nothing for inputs connected to the other params
                    elif full_name in self.param_names:
                        pass
                    
                    # Inputs who are connected to something with a derivative
                    else:
                
                        sources = ascope._depgraph.connections_to(full_name)
                        expr_txt = sources[0][0]
                        target = sources[0][1]
                        
                        # Variables on an assembly boundary
                        if expr_txt[0:4] == '@bin':
                            expr_txt = expr_txt.replace('@bin.', '')
                        
                        expr = ascope._exprmapper.get_expr(expr_txt)
                        source = expr.refs().pop()
                            
                        # Need derivative of the expression
                        expr = ascope._exprmapper.get_expr(expr_txt)
                        expr_deriv = expr.evaluate_gradient(scope=ascope,
                                                            wrt=source)
                        
                        # We also need the derivative of the unit
                        # conversion factor if there is one
                        metadata = expr.get_metadata('units')
                        source_unit = [x[1] for x in metadata if x[0] == source]
                        if source_unit and source_unit[0]:
                            dest_expr = ascope._exprmapper.get_expr(target)
                            metadata = dest_expr.get_metadata('units')
                            target_unit = [x[1] for x in metadata \
                                           if x[0] == target]

                            expr_deriv[source] = expr_deriv[source] * \
                                convert_units(1.0, source_unit[0], 
                                              target_unit[0])

                        # Store our derivatives to chain them
                        incoming_deriv_names[input_name] = full_name
                        if full_name in incoming_derivs:
                            incoming_derivs[full_name] += derivs[source] * \
                                expr_deriv[source]
                        else:
                            incoming_derivs[full_name] = derivs[source] * \
                                expr_deriv[source]
                        
                            
                # CHAIN RULE
                # Propagate derivatives wrt parameter through current component
                for output_name in local_outputs:
                    
                    full_output_name = '.'.join([node_name, output_name])
                    derivs[full_output_name] = 0.0
                    
                    for input_name, full_input_name in \
                        incoming_deriv_names.iteritems():
                        
                        derivs[full_output_name] += \
                            local_derivs[output_name][input_name] * \
                            incoming_derivs[full_input_name]
                            
            # This component must be finite differenced.
            else:
                msg = 'CRND cannot Finite Difference subblocks yet.'
                raise NotImplementedError(msg)
            

    def _recurse_assy(self, scope, upscope_derivs, upscope_param):
        """Enables assembly recursion by scope translation."""
        
        # Find all assembly boundary connections, and propagate
        # derivatives through the expressions.
        local_derivs = {}
        name = scope.name
        
        for item in scope._depgraph.var_edges('@xin'):
            src_expr = item[0].replace('@xin.','')
            expr = scope._exprmapper.get_expr(src_expr)
            src = expr.refs().pop()
            upscope_src = src.replace('parent.','')
            
            # Real connections on boundary
            dest = item[1]
            dest = dest.split('.')[1]
            dest_txt = dest.replace('@bin.','')
                
            # Differentiate all expressions
            expr_deriv = expr.evaluate_gradient(scope=scope,
                                                wrt=src)
            
            # We also need the derivative of the unit
            # conversion factor if there is one
            metadata = expr.get_metadata('units')
            source_unit = [x[1] for x in metadata if x[0] == src]
            if source_unit and source_unit[0]:
                dest_expr = scope._exprmapper.get_expr(dest_txt)
                metadata = dest_expr.get_metadata('units')
                target_unit = [x[1] for x in metadata if x[0] == dest_txt]

                expr_deriv[src] = expr_deriv[src] * \
                           convert_units(1.0, source_unit[0], target_unit[0])

            if dest in local_derivs:    
                local_derivs[dest] += \
                    upscope_derivs[upscope_src]*expr_deriv[src]
            else:
                local_derivs[dest] = \
                    upscope_derivs[upscope_src]*expr_deriv[src]
        
        param = upscope_param.split('.')
        if param[0] == name:
            param = param[1:].join('.')
        else:
            param = ''
        
        # Find derivatives for this assembly's workflow
        self._chain_workflow(local_derivs, scope.driver, param)
        
        # Convert scope and return gradient of connected components.
        for item in scope._depgraph.var_in_edges('@bout'):
            src = item[0]
            upscope_src = '%s.%s' % (name, src)
            dest = item[1]
            
            # Real connections on boundary need expressions differentiated
            if dest.count('.') < 2:
                
                upscope_dest = dest.replace('@bout', name)
                dest = dest.replace('@bout.','')
                
                expr_txt = scope._depgraph.get_source(dest)
                expr = scope._exprmapper.get_expr(expr_txt)
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
            driver needs gradient and Hessian information at the same point
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
        self._parent.raise_exception("%s: %s" % (name, msg), exception_class)
