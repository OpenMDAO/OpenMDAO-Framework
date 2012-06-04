""" Differentiates a driver's workflow using an analytic method.
"""

# pylint: disable-msg=E0611,F0401
try:
    from numpy import array, zeros, dot, linalg
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    
    # to keep class decl from barfing before being stubbed out
    array = lambda *args, **kwargs: None 
    zeros = lambda *args, **kwargs: None 
    
from openmdao.lib.datatypes.api import Enum, Bool
from openmdao.lib.differentiators.chain_rule import ChainRule
from openmdao.main.api import Driver, Assembly
from openmdao.main.assembly import Run_Once
from openmdao.main.interfaces import implements, IDifferentiator
from openmdao.units import convert_units
from openmdao.util.decorators import stub_if_missing_deps

@stub_if_missing_deps('numpy')
class Analytic(ChainRule):
    """ Differentiates a driver's workflow using one of the analytic
    methods."""

    implements(IDifferentiator)
    
    mode = Enum('direct', ['direct', 'adjoint'], iotype = 'in',
                 desc='Coose forward or adjoint mode')
    
    approach = Enum('functional', ['functional', 'residual', 'hybrid'],
                     iotype = 'in', desc = 'approach for assembling the ' + \
                     'problem.\n' + \
                     'functional - convert all comps to functional form' + \
                     'residual - convert all comps to residual form' + \
                     'hybrid - no conversion, each comps uses what it has')
                     
    sparse = Bool(False, iotype = 'in', desc='Set to True for sparse ' + \
                  'storage of matrices.')
    
    def __init__(self):
        
        super(Analytic, self).__init__()
        
        # Left hand and right hand sides of our linear system.
        self.LHS = zeros((0, 0), 'd')
        self.RHS = zeros((0, 0), 'd')
        self.EQS = zeros((0, 0), 'd')
        self.EQS_zero = zeros((0, 0), 'd')
        self.n_var = 0
        
        # Overrides definition in parent
        # Store as matrix instead of dict
        self.gradient = zeros((0, 0), 'd')
        
        # Bookkeeping index/name
        self.var_list = []
        
    def get_derivative(self, output_name, wrt):
        """Returns the derivative of output_name with respect to wrt.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
            
        wrt: string
            Name of the input in the local OpenMDAO hierarchy. The
            derivative is with respect to this variable.
        """
        
        i_param = self.param_names.index(wrt)
        i_func = self.function_names.index(output_name)
        
        return self.gradient[i_func][i_param]

    
    def get_gradient(self, output_name=None):
        """Returns the gradient of the given output with respect to all 
        parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """
        
        i_func = self.function_names.index(output_name)
        
        return self.gradient[i_func][:]
        
        
    def setup(self):
        """ Determine problem dimension and allocate arrays. (unless sparse)
        """
        
        # Parent class method assembles all of our driver connections
        super(Analytic, self).setup()
        
        # Traverses the workflow
        self._find_edges(self._parent)
        
        # Direct mode: count outputs
        # Adjoint mode: count inputs
        index = 0 if self.mode == 'adjoint' else 1
        
        n_var = 0
        self.var_list = []
        scope = self._parent
        scope_name = scope.get_pathname()
        
        # Number of unknowns = number of edges in our edge_dict
        for edge_dict in self.edge_dicts.values():
            for name, edges in edge_dict.iteritems():
                n_var += len(edges[index])
                
                # Assemblies are counted in their own scope, so skip them
                comp = scope.parent.get(name)
                if isinstance(comp, Assembly):
                    continue
                
                # Save the names for all our unknowns while we are here
                for output_name in edges[index]:
                    output_full = "%s.%s" % (name, output_name)
                    self.var_list.append(output_full)
                
        n_param = len(self.param_names)
        n_eq = len(self.function_names)
        
        self.LHS = zeros((n_var, n_var), 'd')
        self.RHS = zeros((n_var, n_param), 'd')
        self.EQS = zeros((n_eq, n_var), 'd')
        self.EQS_zero = zeros((n_eq, n_param), 'd')
        
        self.gradient = zeros((n_eq, n_param), 'd')
        
        # The EQS only needs to be calculated once
        # Objectives first
        i_eq = 0
        wrt = self.var_list + self.param_names
        for obj_name, expr in self._parent.get_objectives().iteritems():
            
            obj_grad = expr.evaluate_gradient(scope=self._parent.parent,
                                                  wrt=wrt)
            for input_name, val in obj_grad.iteritems():
                
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] = val
                    
                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] = val
                    
            i_eq += 1
            
        # Constraints next
        for con_name, constraint in \
            self._parent.get_constraints().iteritems():
            
            lhs, rhs, comparator, _ = \
                constraint.evaluate_gradient(scope=self._parent.parent,
                                             wrt=wrt)
            
            sign = -1.0 if '>' in comparator else 1.0
                
            for input_name, val in lhs.iteritems():
                val = sign*val
                    
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] += val
                    
                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] += val
                        
            for input_name, val in rhs.iteritems():
                val = -sign*val
                        
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] += val
                    
                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] += val
                        
            i_eq += 1
        
    def calc_gradient(self):
        """Calculates the gradient vectors for all outputs in this Driver's
        workflow."""
        
        if self._parent not in self.edge_dicts:
            self.setup()
            
        if self.mode == 'direct':
            self._assemble_direct()
        
        self._solve()
        
        
    def _assemble_direct(self):
        """Assembles the system matrices for the direct problem."""
        
        scope = self._parent
        scope_name = scope.get_pathname()
        
        i_eq = 0
        for node in scope.workflow.__iter__():
             
            # We don't handle nested drivers yet.
            if isinstance(node, Driver):
                raise NotImplementedError('Nested drivers')
             
            # Recurse into assemblies.
            elif isinstance(node, Assembly):
                 
                if not isinstance(node.driver, Run_Once):
                    raise NotImplementedError('Nested drivers')
                 
                raise NotImplementedError('Nested drivers')
                #self._recurse_assy(node, derivs, param)
                                      
            # This component can determine its derivatives.
            elif hasattr(node, 'calculate_first_derivatives'):
                 
                node.calc_derivatives(first=True)
             
                node_name = node.name
                edge_dict = self.edge_dicts[scope_name][node_name]
                local_derivs = node.derivatives.first_derivatives
                 
                # Each output gives us an equation
                for output_name in edge_dict[1]:
                     
                    self.LHS[i_eq][i_eq] = 1.0
                    output_full = "%s.%s" % (node_name, output_name)
                    #self.var_list.append(output_full)
                     
                    # Each input provides a term for LHS or RHS
                    for input_name in edge_dict[0]:
                         
                        # Direct connection to parameter goes in RHS
                        input_full = "%s.%s" % (node_name, input_name)
                        if input_full in self.param_names:
                             
                            i_param = self.param_names.index(input_full)
                             
                            self.RHS[i_eq][i_param] = \
                                local_derivs[output_name][input_name]
                             
                        # Input connected to other outputs goes in LHS
                        else:
                            
                            sources = scope.parent._depgraph.connections_to(input_full)

                            source = sources[0][0]
                            target = sources[0][1]
                            
                            i_var = self.var_list.index(source)
                             
                            expr_txt = scope.parent._depgraph.get_source(target)
                            
                            # Variables on an assembly boundary
                            #if source[0:4] == '@bin' and source.count('.') < 2:
                            #    source = source.replace('@bin.', '')
                            
                            # Need derivative of the expression
                            expr = scope.parent._exprmapper.get_expr(expr_txt)
                            expr_deriv = expr.evaluate_gradient(scope=scope.parent,
                                                                wrt=source)
                            
                            # We also need the derivative of the unit
                            # conversion factor if there is one
                            metadata = expr.get_metadata('units')
                            source_unit = [x[1] for x in metadata if x[0]==source]
                            if source_unit and source_unit[0]:
                                dest_expr = scope.parent._exprmapper.get_expr(target)
                                metadata = dest_expr.get_metadata('units')
                                target_unit = [x[1] for x in metadata if x[0]==target]

                                expr_deriv[source] = expr_deriv[source] * \
                                    convert_units(1.0, source_unit[0], target_unit[0])

                            # Chain together deriv from var connection and comp
                            self.LHS[i_eq][i_var] = \
                                -local_derivs[output_name][input_name] * \
                                 expr_deriv[source]
                     
                    i_eq += 1
            
    def _solve(self):
        """Solve the linear system.
        
        Direct mode: solves for dy/d(param)
        Adjoint mode: solves for d(obj,constr)/dx
        """
        
        total_derivs = linalg.solve(self.LHS, self.RHS)
        self.gradient = self.EQS_zero + dot(self.EQS, total_derivs)
        
