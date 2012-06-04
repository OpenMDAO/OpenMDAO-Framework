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
        
        # Bookkeeping index/name
        self.var_list = []
        
    def setup(self):
        """ Determine problem dimension and allocate arrays. (unless sparse)
        """
        
        # Parent class method assembles all of our driver connections
        super(Analytic, self).setup()
        
        # Traverses the workflow
        self._find_edges(self._parent)
        
        # Forward mode: count outputs
        # Adjoint mode: count inputs
        index = 1 if self.mode == 'adjoint' else 0
        
        n_var = 0
        for edge_dict in self.edge_dicts.values():
            for comp in edge_dict.values():
                n_var += len(comp[index])
                
        n_param = len(self.param_names)
        n_eq = len(self.objective_names) + \
               len(self.ineqconst_names) + \
               len(self.eqconst_names)
        
        self.LHS = zeros((n_var, n_var), 'd')
        self.RHS = zeros((n_var, n_param), 'd')
        self.EQS = zeros((n_eq, n_var), 'd')
        self.EQS_zero = zeros((n_eq, n_param), 'd')
        
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
        self.var_list = []
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
                    self.var_list.append(output_full)
                     
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
                            pass
                     
                    i_eq += 1
                    
        # EQS only needs to be calculated once
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
            
            
    def _solve(self):
        """Solve the linear system.
        
        Forward mode: solves for dy/d(obj)
        Adjoint mode: solves for d(obj,constr)/dx
        """
        
        total_derivs = linalg.solve(self.LHS, self.RHS)
        func_derivs = self.EQS_zero + dot(self.EQS, total_derivs)
        
        # Pack the solution into the differentitator data structure
        pass