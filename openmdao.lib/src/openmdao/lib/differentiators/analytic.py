""" Differentiates a driver's workflow using an analytic method.
"""

# pylint: disable-msg=E0611,F0401
try:
    from numpy import zeros, dot, linalg
except ImportError as err:
    import logging
    logging.warn("In %s: %r" % (__file__, err))
    
from openmdao.lib.datatypes.api import Enum, Bool
from openmdao.lib.differentiators.chain_rule import ChainRule
from openmdao.main.api import Driver, Assembly
from openmdao.main.driver import Run_Once
from openmdao.main.interfaces import implements, IDifferentiator, ISolver
from openmdao.main.mp_support import has_interface
from openmdao.units import convert_units
from openmdao.util.decorators import stub_if_missing_deps


def _d_conn(expr_txt, target, ascope):
    """ Evaluates the derivative of a source-target variable connection.
    This includes the derivative of the expression as well as the
    derivative of the unit conversion factor."""
    
    expr = ascope._exprmapper.get_expr(expr_txt)
    source = expr.refs().pop()
        
    # Need derivative of the expression
    expr_deriv = expr.evaluate_gradient(scope=ascope,
                                        wrt=source)
    
    # We also need the derivative of the unit
    # conversion factor if there is one
    metadata = expr.get_metadata('units')
    source_unit = [x[1] for x in metadata if x[0] == source]
    if source_unit and source_unit[0]:
        dest_expr = ascope._exprmapper.get_expr(target)
        metadata = dest_expr.get_metadata('units')
        target_unit = [x[1] for x in metadata if x[0] == target]

        expr_deriv[source] = expr_deriv[source] * \
            convert_units(1.0, source_unit[0], target_unit[0])
        
    return source, expr_deriv


@stub_if_missing_deps('numpy')
class Analytic(ChainRule):
    """ Differentiates a driver's workflow using one of the analytic
    methods."""

    implements(IDifferentiator)
    
    # pylint: disable-msg=E1101
    mode = Enum('direct', ['direct', 'adjoint'], iotype = 'in',
                 desc='Choose forward or adjoint mode.')
    
    approach = Enum('functional', ['functional', 'residual', 'hybrid'],
                     iotype = 'in', desc = 'Approach for assembling the ' + \
                     'problem.\n' + \
                     'functional - convert all comps to functional form; ' + \
                     'residual - convert all comps to residual form; ' + \
                     'hybrid - no conversion, each comps uses what it has.')
                     
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
        """ Determine problem dimension and allocate arrays (unless sparse).
        """
        
        # Parent class method assembles all of our driver connections
        super(Analytic, self).setup()
        
        # Direct mode: count outputs
        # Adjoint mode: count outputs as input edges
        #index = 0 if self.mode == 'adjoint' else 1
        index = 1
        
        self.var_list = []

        # Count recursively to get n_var and var_list
        self._edge_counter(self._parent, self._parent, index)
        n_var = len(self.var_list)
                
        n_param = len(self.param_names)
        n_eq = len(self.function_names)
        
        self.LHS = zeros((n_var, n_var), 'd')
        
        #if self.mode == 'adjoint':
        #    self.EQS = zeros((n_var, n_param), 'd')
        #    self.RHS = zeros((n_var, n_eq), 'd')
        #else:
        self.EQS = zeros((n_eq, n_var), 'd')
        self.RHS = zeros((n_var, n_param), 'd')
            
        self.EQS_zero = zeros((n_eq, n_param), 'd')
        
        self.gradient = zeros((n_eq, n_param), 'd')
        
        # For direct mode, the EQS only needs to be calculated once
        # For adjoint mode, the RHS may need recalculation because of
        # connection derivatives.
        # Objectives first
        i_eq = 0
        wrt = self.var_list + self.param_names + \
            [item for sublist in self.grouped_param_names for item in sublist]
        for expr in self._parent.get_objectives().values():
            
            obj_grad = expr.evaluate_gradient(scope=self._parent.parent,
                                                  wrt=wrt)
            for input_name, val in obj_grad.iteritems():
                
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] = val
                    
                elif input_name in self.grouped_param_names:
                        
                    grouped = self.grouped_param_names[input_name]
                    i_param = self.param_names.index(grouped)
                    self.EQS_zero[i_eq][i_param] = val
                        
                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] = val
                    
            i_eq += 1
            
        # Constraints next
        for constraint in self._parent.get_constraints().values():
            
            lhs, rhs, comparator, _ = \
                constraint.evaluate_gradient(scope=self._parent.parent,
                                             wrt=wrt)
            
            sign = -1.0 if '>' in comparator else 1.0
                
            for input_name, val in lhs.iteritems():
                val = sign*val
                    
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] += val
                    
                elif input_name in self.grouped_param_names:
                        
                    grouped = self.grouped_param_names[input_name]
                    i_param = self.param_names.index(grouped)
                    self.EQS_zero[i_eq][i_param] += val

                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] += val
                        
            for input_name, val in rhs.iteritems():
                val = -sign*val
                        
                if input_name in self.param_names:
                    
                    i_param = self.param_names.index(input_name)
                    self.EQS_zero[i_eq][i_param] += val
                    
                elif input_name in self.grouped_param_names:
                        
                    grouped = self.grouped_param_names[input_name]
                    i_param = self.param_names.index(grouped)
                    self.EQS_zero[i_eq][i_param] += val

                elif input_name in self.var_list:
                    
                    i_var = self.var_list.index(input_name)
                    self.EQS[i_eq][i_var] += val
                        
            i_eq += 1
        
    def _edge_counter(self, scope, dscope, index, head=''):
        """Helper function to figure out which edges in the edge dicts are
        our unknowns. Called recursively for assy or driver scopes."""

        # Traverse the workflow
        self._find_edges(scope, dscope)
        scope_name = dscope.get_pathname()
        
        if head:
            head = '%s.' % head
            
        index_bar = 0 if index == 1 else 1
            
        # Number of unknowns = number of edges in our edge_dict
        for name, edges in self.edge_dicts[scope_name].iteritems():
            
            # For assemblies, we need to traverse their workflows too
            node = scope.parent.get(name)
            
            if isinstance(node, Assembly):

                # Assembly inputs are also counted as unknowns. This makes
                # recursion easier so that you don't have to query out of
                # scope.
                for input_name in edges[index_bar]:
                    input_full = "%s%s.%s" % (head, name, input_name)
                    self.var_list.append(input_full)
                    
                node_scope_name = node.get_pathname()
                self._edge_counter(node.driver, node.driver, index,
                                   node_scope_name)
                
            elif isinstance(node, Driver):
                
                if not has_interface(node, ISolver):
                    msg = "Only nested solvers are supported"
                    raise NotImplementedError(msg)
                
                node_scope_name = node.get_pathname()
                self._edge_counter(scope, node, index, head)
            
            # Save the names for all our unknowns
            for item in edges[index]:
                full_name = "%s%s.%s" % (head, name, item)
                
                # Parameter edges in adjoint mode are not added to the
                # unknowns
                if full_name not in self.param_names and \
                   full_name not in self.grouped_param_names:
                    self.var_list.append(full_name)
                
        
    def calc_gradient(self):
        """Calculates the gradient vectors for all outputs in this Driver's
        workflow."""
        
        # This stuff only needs to run once
        if self._parent not in self.edge_dicts:
            self.setup()
        
        # Assemble matrices for problem every run
        ascope = self._parent.parent
        dscope = self._parent
        self._assemble_direct(ascope, dscope)
            
        self._solve()
        
        
    def _assemble_direct(self, ascope, dscope, eq_num=0, head='', 
                         solver_conns=None):
        """Assembles the system matrices for the direct problem.
        This is meant to be called recursively, with the assembly scope,
        driver scope, and current equation number as inputs."""
        
        scope_name = dscope.get_pathname()
        
        if head:
            head = '%s.' % head
            
        if solver_conns is None:
            solver_conns = []
            
        i_eq = eq_num
        for node_name in self.dworkflow[scope_name]:
            
            # If it's a list, then it's a set of components to finite
            # difference together.
            if not isinstance(node_name, list):
                node = dscope.parent.get(node_name)
                fdblock = False
            else:
                fdblock = True
            
            # Finite difference block
            if fdblock:
                
                fd = self.fdhelpers[scope_name][str(node_name)]
                
                input_dict = {}
                for item in fd.list_wrt():
                    input_dict[item] = dscope.parent.get(item)
                    
                output_dict = {}
                for item in fd.list_outs():
                    output_dict[item] = dscope.parent.get(item)
                        
                local_derivs = fd.run(input_dict, output_dict)
            
            # So far, we only handle nested solvers.
            elif isinstance(node, Driver):
                if not has_interface(node, ISolver):
                    msg = "Only nested solvers are supported"
                    raise NotImplementedError(msg)
             
                # Assemble a dictionary of the couplings that this solver
                # iterates.
                params = node.get_parameters().keys()
                
                solver_dict = {}
                for constr in node.get_eq_constraints().values():
                    
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
                    
                    solver_dict[indep] = dep
                    
                # Recurse
                i_eq = self._assemble_direct(ascope, node, i_eq, head, 
                                             solver_dict)
                    
                continue
            
            # Recurse into assemblies.
            elif isinstance(node, Assembly):
                 
                if not isinstance(node.driver, Run_Once):
                    raise NotImplementedError('Nested drivers')
                
                edge_dict = self.edge_dicts[scope_name][node_name]
                
                # Assembly inputs are unknowns, so they get equations
                for input_name in edge_dict[0]:
                    
                    self.LHS[i_eq][i_eq] = 1.0
                    input_full = "%s.%s" % (node_name, input_name)
                
                    # Assy input conected to parameter goes in RHS
                    if input_full in self.param_names:
                         
                        i_param = self.param_names.index(input_full)
                         
                        self.RHS[i_eq][i_param] = 1.0
                         
                    elif input_full in self.grouped_param_names:
                             
                        grouped = self.grouped_param_names[input_full]
                        i_param = self.param_names.index(grouped)
                             
                        self.RHS[i_eq][i_param] = 1.0
                             
                    # Assy Input connected to other outputs goes in LHS
                    else:
                        
                        sources = ascope._depgraph.connections_to(input_full)

                        expr_txt = sources[0][0]
                        target = sources[0][1]
                        
                        # Variables on an assembly boundary
                        if expr_txt[0:4] == '@bin':
                            expr_txt = expr_txt.replace('@bin.', '')
                        
                        # Need derivative of the expression
                        source, expr_deriv = _d_conn(expr_txt, target, 
                                                     ascope)
                        
                        # Chain together deriv from var connection and comp
                        i_var = self.var_list.index("%s%s" % (head, source))
                         
                        self.LHS[i_eq][i_var] = -expr_deriv[source]
                 
                    i_eq += 1
                
                # Recurse
                assy_scope_name = node.get_pathname()
                i_eq = self._assemble_direct(node, node.driver, i_eq, 
                                             assy_scope_name)
                                      
                # Assembly outputs are unknowns, so they get equations
                sub_scope = ascope.get(node_name)
                for output_name in edge_dict[1]:
                    
                    self.LHS[i_eq][i_eq] = 1.0
                
                    sources = sub_scope._depgraph.connections_to(output_name)
                    for connect in sources:
                        if '@bout' in connect[1]:
                            expr_txt = connect[0]
                            target = connect[1]
                            break
                    
                    # Variables on an assembly boundary
                    if target[0:5] == '@bout':
                        target = target.replace('@bout.', '')
                    
                    # Need derivative of the expression
                    source, expr_deriv = _d_conn(expr_txt, target, 
                                                 sub_scope)
                    
                    # Chain together deriv from var connection and comp
                    i_var = self.var_list.index("%s%s.%s" % (head, 
                                                             node_name, 
                                                             source))
                     
                    self.LHS[i_eq][i_var] = -expr_deriv[source]
                 
                    i_eq += 1
                
                continue

            # This component can determine its derivatives.
            else:
                 
                node.calc_derivatives(first=True)
                local_derivs = node.derivatives.first_derivatives
                node_name = [node_name]
                 
            # The following executes for components with derivatives and
            # blocks that are finite-differenced
            for item in node_name:
                edge_dict = self.edge_dicts[scope_name][item]
                
                # Calculate derivatives of incoming connections
                # I moved this out of the double loop so that it isn't
                # repeated needlessly x(num_outputs)
                conn_data = {}
                for input_name in edge_dict[0]:
                    input_full = "%s.%s" % (item, input_name)
                    
                    if input_full not in self.param_names and \
                       input_full not in self.grouped_param_names and \
                       input_full not in solver_conns:
                
                        sources = ascope._depgraph.connections_to(input_full)
                        expr_txt = sources[0][0]
                        target = sources[0][1]
                        
                        # Variables on an assembly boundary
                        if expr_txt[0:4] == '@bin':
                            expr_txt = expr_txt.replace('@bin.', '')
                        
                        # Need derivative of the expression
                        source, expr_deriv = _d_conn(expr_txt, target, 
                                                     ascope)
                        
                        # Chain together deriv from var connection and comp
                        i_var = self.var_list.index("%s%s" % (head, source))
                         
                        conn_data[input_full] = (i_var, expr_deriv[source])
    
                # Each output gives us an equation
                for output_name in edge_dict[1]:
                     
                    self.LHS[i_eq][i_eq] = 1.0
                     
                    if fdblock:
                        local_out = "%s.%s" % (item, output_name)
                    else:
                        local_out = output_name
                    
                    # Each input provides a term for LHS or RHS
                    for input_name in edge_dict[0]:
                        
                        input_full = "%s.%s" % (item, input_name)
                        if fdblock:
                            local_in = input_full
                        else:
                            local_in = input_name
                            
                        # Direct connection to parameter goes in RHS
                        if input_full in self.param_names:
                             
                            i_param = self.param_names.index(input_full)
                             
                            self.RHS[i_eq][i_param] = \
                                local_derivs[local_out][local_in]
                             
                        elif input_full in self.grouped_param_names:
                                 
                            grouped = self.grouped_param_names[input_full]
                            i_param = self.param_names.index(grouped)
                                 
                            self.RHS[i_eq][i_param] = \
                                local_derivs[local_out][local_in]
                                 
                        # Input is a dependent in a solver loop
                        elif input_full in solver_conns:
                            
                            source = solver_conns[input_full]
                            i_dep = self.var_list.index(source)
                             
                            self.LHS[i_eq][i_dep] = \
                                -local_derivs[local_out][local_in]
                            
                        # Input connected to other outputs goes in LHS
                        else:
                            
                            # Already calculated connection deriatives
                            i_var = conn_data[input_full][0]
                            expr_deriv = conn_data[input_full][1]
                            
                            self.LHS[i_eq][i_var] = \
                                -local_derivs[local_out][local_in] * \
                                 expr_deriv
                     
                    i_eq += 1
            
        return i_eq
    
    def _solve(self):
        """Solve the linear system.
        
        Direct mode: solves for dy/d(param)
        Adjoint mode: solves for d(obj,constr)/dx
        """
        
        if self.mode == 'adjoint':
            total_derivs = linalg.solve(self.LHS.T, self.EQS.T)
            self.gradient = self.EQS_zero + dot(total_derivs.T, self.RHS)
        else:
            total_derivs = linalg.solve(self.LHS, self.RHS)
            self.gradient = self.EQS_zero + dot(self.EQS, total_derivs)
        
