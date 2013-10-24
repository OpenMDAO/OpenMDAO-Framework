""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

import networkx as nx
import sys

from openmdao.main.array_helpers import flattened_size, flattened_value, \
                                        flattened_names
from openmdao.main.derivatives import calc_gradient, calc_gradient_adjoint, \
                                      applyJ, applyJT, recursive_components, \
                                      applyMinvT, applyMinv, edge_dict_to_comp_list
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudoassembly import PseudoAssembly
from openmdao.main.pseudocomp import PseudoComponent
from openmdao.main.vartree import VariableTree

from openmdao.main.workflow import Workflow
from openmdao.main.ndepgraph import find_related_pseudos, is_input_node, \
                                    get_inner_edges
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface

try:
    from numpy import ndarray, zeros
except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, zeros

__all__ = ['SequentialWorkflow']


class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""

    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        self._explicit_names = [] # names the user adds
        self._names = None  # names the user adds plus names required 
                            # for params, objectives, and constraints
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
        # Bookkeeping for calculating the residual.
        self._edges = None
        self._severed_edges = []
        self._additional_edges = []
        self._hidden_edges = set()
        self._driver_edges = None
        self.res = None
        
        self.derivative_iterset = None
        self._collapsed_graph = None
        self._topsort = None
        self._find_nondiff_blocks = True
        self._input_outputs = []
        self._interior_edges = None
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components(full=True))

    def __len__(self):
        if self._names:
            return len(self._names)
        else:
            return len(self._explicit_names)

    def __contains__(self, comp):
        return comp in self.get_names(full=True)

    def index(self, comp):
        """Return index number for a component in this workflow."""
        return self.get_names().index(comp)

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names

    def __ne__(self, other):
        return not self.__eq__(other)

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(SequentialWorkflow, self).config_changed()
        self._severed_edges = []
        self._additional_edges = []
        self._hidden_edges = set()
        self._driver_edges = None
        self._find_nondiff_blocks = True
        self.derivative_iterset = None
        self._collapsed_graph = None
        self._topsort = None
        self._input_outputs = []
        self._names = None
        self._interior_edges = None

    def sever_edges(self, edges):
        """Temporarily remove the specified edges but save
        them and their metadata for later restoration. 
        """
        self.scope._depgraph.sever_edges(edges)

    def unsever_edges(self):
        self.scope._depgraph.unsever_edges(self._parent.get_expr_scope())
        
    def get_names(self, full=False):
        """Return a list of component names in this workflow.  
        If full is True, include hidden pseudo-components in the list.
        """
        if self._names is None:
            comps = [getattr(self.scope, n) 
                               for n in self._explicit_names]
            drivers = [c for c in comps if has_interface(c, IDriver)]
            self._names = self._explicit_names[:]

            if len(drivers) == len(comps): # all comps are drivers
                iterset = set()
                for driver in drivers:
                    iterset.update(driver.iteration_set())
                added = set([n for n in 
                           self._parent._get_required_compnames() 
                              if n not in iterset]) - set(self._names)
                self._names.extend(added)
                          
        if full:
            allnames = self._names[:]
            fullset = set(self._parent.list_pseudocomps())
            fullset.update(find_related_pseudos(self.scope._depgraph.component_graph(),
                                                self._names))
            allnames.extend(fullset - set(self._names))
            return allnames
        else:
            return self._names[:]

    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the end of the workflow by name. """
        if isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            nodes = compnames

        try:
            iter(nodes)
        except TypeError:
            raise TypeError("Components must be added by name to a workflow.")

        # We seem to need this so that get_attributes is correct for the GUI.
        self.config_changed()

        for node in nodes:
            if isinstance(node, basestring):

                if check:
                    # check whether each node is valid and if not then
                    # construct a useful error message.
                    name = self._parent.parent.name
                    if not name:
                        name = "the top assembly."

                    # Components in subassys are never allowed.
                    if '.' in node:
                        msg = "Component '%s' is not" % node + \
                              " in the scope of %s" % name
                        raise AttributeError(msg)

                    # Does the component really exist?
                    try:
                        target = self._parent.parent.get(node)
                    except AttributeError:
                        msg = "Component '%s'" % node + \
                              " does not exist in %s" % name
                        raise AttributeError(msg)

                    # Don't add yourself to your own workflow
                    if target == self._parent:
                        msg = "You cannot add a driver to its own workflow"
                        raise AttributeError(msg)

                    # Check for circular dependency in driver workflow
                    if hasattr(target, 'iteration_set'):
                        iterset = target.iteration_set()
                        if self._parent in iterset:
                            msg = "Driver recursion loop detected"
                            raise AttributeError(msg)

                if index is None:
                    self._explicit_names.append(node)
                else:
                    self._explicit_names.insert(index, node)
                    index += 1
            else:
                msg = "Components must be added by name to a workflow."
                raise TypeError(msg)

    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            msg = "Components must be removed by name from a workflow."
            raise TypeError(msg)
        try:
            self._explicit_names.remove(compname)
        except ValueError:
            pass
        self.config_changed()

    def clear(self):
        """Remove all components from this workflow."""
        self._explicit_names = []
        self.config_changed()

    def get_interior_edges(self):
        """ Returns an alphabetical list of all output edges that are
        interior to the set of components supplied. When used for derivative
        calculation, the parameter inputs and response outputs are also
        included. If there are non-differentiable blocks grouped in
        pseudo-assemblies, then those interior edges are excluded.
        """
        if self._interior_edges is None:
                   
            graph = self._parent.workflow_graph()
            comps = [comp.name for comp in self.__iter__()]# + \
                    #required_floating_vars
            edges = set(graph.get_interior_connections(comps))
            edges.update(self.get_driver_edges())
            edges.update(self._additional_edges)
            edges = edges - self._hidden_edges
                    
            # Sometimes we connect an input to an input (particularly with
            # constraints). These need to be rehooked to corresponding source
            # edges.
            
            self._input_outputs = set()
            for src, target in edges:
                if src == '@in' or target == '@out' or '_pseudo_' in src:
                    continue
                if is_input_node(graph, src):
                    self._input_outputs.add(src)
                    
            self._input_outputs = list(self._input_outputs)
                    
            self._interior_edges = sorted(list(edges))
            
        return self._interior_edges

    def get_driver_edges(self):
        '''Return all edges where our driver connects to any component that
        is owned by a subdriver's workflow. Also, include the extra parameter
        edges that don't show up in the parent assembly depgraph.
        '''
        
        # Cache it
        if self._driver_edges is not None:
            return self._driver_edges
        
        comps = self.get_names(full=True)
        rcomps = recursive_components(self.scope, comps)
        
        sub_edge = set()
        outcomps = [item[0].split('.')[0] for item in self._additional_edges]
        loose_vars = self._parent.parent.list_inputs()
        
        for comp in comps:
            
            pcomp = self.scope.get(comp)
            
            # Output edges
            if comp in outcomps and isinstance(pcomp, PseudoComponent) and \
                   pcomp._pseudo_type in ['objective','constraint']:
                for pcomp_edge in pcomp.list_connections():
                    src_edge = pcomp_edge[0].split('.')[0]
                    if src_edge	in rcomps or src_edge in loose_vars \
                        or src_edge.split('[')[0] in loose_vars:
                        sub_edge.add(pcomp_edge)

        self._driver_edges = sub_edge
        return self._driver_edges
        
    def initialize_residual(self, inputs, outputs):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        nEdge = 0
        
        for varnames in inputs+outputs:
            if not isinstance(varnames, list):
                varnames = [varnames]
            for varname in varnames:
                if varname not in self.scope._depgraph.node:
                    self.scope._depgraph.add_subvar(varname)
                
        self._edges = get_inner_edges(self.scope._depgraph, inputs, outputs)
        
        for src, targets in self._edges.iteritems():
            if '@in' in src:
                src = targets
                
                if isinstance(src, list):
                    src = src[0]
                
            val = self.scope.get(src)
            width = flattened_size(src, val, self.scope)
            self.set_bounds(src, (nEdge, nEdge+width))
            
            if isinstance(targets, list):
                for target in targets:
                    if '@out' not in target:
                        self.set_bounds(target, (nEdge, nEdge+width))
            else:
                if '@out' not in targets:
                    self.set_bounds(targets, (nEdge, nEdge+width))
                    
            nEdge += width

        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))

        print 'old iter:  ', self.get_interior_edges()
        print 'iterator:  ', get_inner_edges(self.scope._depgraph, inputs, outputs)
        print edge_dict_to_comp_list(self._edges)
        return nEdge

    def get_bounds(self, node):
        """ Return a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        itername = 'top.'+self._parent.itername
        return self.scope._depgraph.node[node]['bounds'][itername]
        
    def set_bounds(self, node, bounds):
        """ Set a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        itername = 'top.'+self._parent.itername
        
        try:
            meta = self.scope._depgraph.node[node]
            
        # Array indexed parameter nodes are not in the graph, so add them.
        except KeyError:
            self.scope._depgraph.add_subvar(node)
            meta = self.scope._depgraph.node[node]
        
        if 'bounds' not in meta:
            meta['bounds'] = {}
            
        meta['bounds'][itername] = bounds
        
    def calculate_residuals(self):
        """Calculate and return the vector of residuals based on the current
        state of the system in our workflow."""
        for edge in self.get_interior_edges():
            src, target = edge
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val).reshape(-1, 1)
            target_val = self.scope.get(target)
            target_val = flattened_value(target, target_val).reshape(-1, 1)
            i1, i2 = self.get_bounds(src)
            self.res[i1:i2] = src_val - target_val

        return self.res

    def set_new_state(self, dv):
        """Adds a vector of new values to the current model state at the
        input edges.

        dv: ndarray (nEdge, 1)
            Array of values to add to the model inputs.
        """
        for edge in self._severed_edges:
            src, target = edge
            i1, i2 = self.get_bounds(src)
            old_val = self.scope.get(target)

            if isinstance(old_val, float):
                new_val = old_val + float(dv[i1:i2])
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    new_val = old_val.flatten() + dv[i1:i2]
                    new_val = new_val.reshape(shape)
                else:
                    new_val = old_val + dv[i1:i2]
            elif isinstance(old_val, VariableTree):
                new_val = old_val.copy()
                self._update(target, new_val, dv[i1:i2])
            else:
                msg = "Variable %s is of type %s." % (target, type(old_val)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

            # Poke new value into the input end of the edge.
            self.scope.set(target, new_val, force=True)

            # Prevent OpenMDAO from stomping on our poked input.
            self.scope.set_valid([target.split('[',1)[0]], True)

            #(An alternative way to prevent the stomping. This is more
            #concise, but setting an output and allowing OpenMDAO to pull it
            #felt hackish.)
            #self.scope.set(src, new_val, force=True)

    def _update(self, name, vtree, dv, i1=0):
        """ Update VariableTree `name` value `vtree` from `dv`. """
        for key in sorted(vtree.list_vars()):  # Force repeatable order.
            value = getattr(vtree, key)
            if isinstance(value, float):
                setattr(vtree, key, value + float(dv[i1]))
                i1 += 1
            elif isinstance(value, ndarray):
                shape = value.shape
                size = value.size
                i2 = i1 + size
                if len(shape) > 1:
                    value = value.flatten() + dv[i1:i2]
                    value = value.reshape(shape)
                else:
                    value = value + dv[i1:i2]
                setattr(vtree, key, value)
                i1 += size
            elif isinstance(value, VariableTree):
                i1 = self._update('.'.join((name, key)), value, dv, i1)
            else:
                msg = "Variable %s is of type %s." % (name, type(value)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

        return i1

    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''
        
        comps = edge_dict_to_comp_list(self._edges)
        result = zeros(len(arg))
        
        # We can call applyJ on each component one-at-a-time, and poke the
        # results into the result vector.
        for compname, data in comps.iteritems():
            
            comp_inputs = data['inputs']
            comp_outputs = data['outputs']
            inputs = {}
            outputs = {}
            
            for varname in comp_inputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                inputs[varname] = arg[i1:i2].copy()
            
            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                inputs[varname] = arg[i1:i2].copy()
                # applyJ needs to know what derivatives are needed
                outputs[varname] = arg[i1:i2].copy()
                
            comp = self.scope.get(compname)
            
            # Preconditioning
            #if hasattr(comp, 'applyMinv'):
                #inputs = applyMinv(comp, inputs)
            
            applyJ(comp, inputs, outputs)
            
            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                result[i1:i2] = outputs[varname]
                
        # Each parameter adds an equation
        for src, targets in self._edges.iteritems():
            if '@in' in src:
                if not isinstance(targets, list):
                    targets = [targets]
                    
                for target in targets:
                    i1, i2 = self.get_bounds(target)
                    result[i1:i2] = arg[i1:i2]
                
        #print arg, result
        return result
        
    def matvecFWD_old(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''
        #print arg.max()
        #import sys
        #sys.stdout.flush()

        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}

        # Variables owned by containing assembly can be in our graph.
        # The will be stored in 'parent'
        inputs['parent'] = {}
        outputs['parent'] = {}

        # Start with zero-valued dictionaries containing keys for all inputs
        pa_ref = {}
        for comp in self.derivative_iter():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
            # Interior Edges use original names, so we need to know
            # what comps are in a pseudo-assy.
            if '~' in name:
                for item in comp.list_all_comps():
                    pa_ref[item] = name
                    
                # If our whole model is in a finite difference, then
                # assembly variables should also be contained
                if len(self.derivative_iterset) == 1:
                    pa_ref['parent'] = name
                    
        # Fill input dictionaries with values from input arg.
        edges = self.get_interior_edges()
        for edge in edges:
            src, targets = edge
            
            if src != '@in' and src not in self._input_outputs:
                comp_name, dot, var_name = src.partition('.')
                
                i1, i2 = self.get_bounds(src)
                
                # Free-floating variables
                if not var_name:
                    var_name = comp_name
                    if 'parent' in pa_ref:
                        comp_name = pa_ref['parent']
                    else:
                        comp_name = 'parent'
                    
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                    
                if var_name not in outputs:
                    outputs[comp_name][var_name] = arg[i1:i2].copy()
                else:
                    outputs[comp_name][var_name] += arg[i1:i2].copy()
                    
                if var_name not in inputs:
                    inputs[comp_name][var_name] = arg[i1:i2].copy()
                else:
                    inputs[comp_name][var_name] += arg[i1:i2].copy()
                    
            if targets != '@out':
                
                # Parameter group support
                if not isinstance(targets, tuple):
                    targets = [targets]
                    
                for target in targets:
                    
                    i1, i2 = self.get_bounds(target)
                    comp_name, dot, var_name = target.partition('.')
                    
                    # Free-floating variables
                    if not var_name:
                        var_name = comp_name
                        if 'parent' in pa_ref:
                            comp_name = pa_ref['parent']
                        else:
                            comp_name = 'parent'
                    
                    if comp_name in pa_ref:
                        var_name = '%s.%s' % (comp_name, var_name)
                        comp_name = pa_ref[comp_name]
                    inputs[comp_name][var_name] = arg[i1:i2]
            #print i1, i2, edge, '\n', inputs, '\n', outputs
            
        # Call ApplyMinv on each component (preconditioner)
        #for comp in self.derivative_iter():
            #name = comp.name
            #if hasattr(comp, 'applyMinv'):
                #inputs[name] = applyMinv(comp, inputs[name])
            
        # Call ApplyJ on each component
        for comp in self.derivative_iter():
            name = comp.name
            applyJ(comp, inputs[name], outputs[name])
            
        # Each parameter adds an equation
        for edge in self._additional_edges:
            if edge[0] == '@in':
                
                # Parameter group support
                p_edges = edge[1]
                if not isinstance(p_edges, tuple):
                    p_edges = [p_edges]
                    
                for p_edge in p_edges:
                    i1, i2 = self.get_bounds(p_edge)
                    comp_name, dot, var_name = p_edge.partition('.')
                    
                    # Free-floating variables
                    if not var_name:
                        var_name = comp_name
                        if 'parent' in pa_ref:
                            comp_name = pa_ref['parent']
                        else:
                            comp_name = 'parent'
                        
                    if comp_name in pa_ref:
                        var_name = '%s.%s' % (comp_name, var_name)
                        comp_name = pa_ref[comp_name]
                    outputs[comp_name][var_name] = arg[i1:i2]

        result = zeros(len(arg))
        
        # Reference back to the source for input-input connections.
        input_input_xref = {}
        edge_outs = [a for a, b in edges]
        for edge in edges:
            targ = edge[1]
            if not isinstance(targ, tuple):
                targ = [targ]
            for target in targ:
                if target in self._input_outputs:
                    input_input_xref[target] = edge
        
        # Poke results into the return vector
        #print inputs, '\n', outputs
        for edge in edges:
            src, target = edge
            if '@in' not in src:
                i1, i2 = self.get_bounds(src)
            else:
                if isinstance(target, tuple):
                    i1, i2 = self.get_bounds(target[0])
                else:
                    i1, i2 = self.get_bounds(target)
            
            if src == '@in':
                # Extra eqs for parameters contribute a 1.0 on diag
                result[i1:i2] = arg[i1:i2]
                continue
            else:
                result[i1:i2] = -arg[i1:i2]
                
            # Input-input connections are not in the jacobians. We need
            # to add the derivative using our cross reference.
            if src in self._input_outputs:
                if src in input_input_xref:
                    ref_edge = input_input_xref[src]
                    i3, i4 = self.get_bounds(ref_edge[0])
                    result[i1:i2] += arg[i3:i4]
                continue
                
            # Parameter group support
            if not isinstance(src, tuple):
                src = [src]
            
            for item in src:
                comp_name, dot, var_name = item.partition('.')
                
                # Free-floating variables
                if not var_name:
                    var_name = comp_name
                    if 'parent' in pa_ref:
                        comp_name = pa_ref['parent']
                    else:
                        comp_name = 'parent'
                
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                result[i1:i2] += outputs[comp_name][var_name]
                #print i1, i2, edge, comp_name, var_name, outputs[comp_name][var_name]
            
        #print arg, result
        return result
    
    def matvecREV(self, arg):
        '''Callback function for performing the transpose matrix vector
        product of the workflow's full Jacobian with an incoming vector
        arg.'''
        #print arg.max()
        #import sys
        #sys.stdout.flush()
        
        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}
        edges = self.get_interior_edges()

        # Variables owned by containing assembly can be in our graph.
        # The will be stored in 'parent'
        inputs['parent'] = {}
        outputs['parent'] = {}

        # Reference back to the source for input-input connections.
        input_input_xref = {}
        edge_outs = [a for a, b in edges]
        for edge in edges:
            targ = edge[1]
            if not isinstance(targ, tuple):
                targ = [targ]
            for target in targ:
                if target in self._input_outputs:
                    input_input_xref[target] = edge
            
        # Start with zero-valued dictionaries cotaining keys for all inputs
        pa_ref = {}
        for comp in self.derivative_iter():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
            # Interior Edges use original names, so we need to know
            # what comps are in a pseudo-assy.
            if '~' in name:
                for item in comp.list_all_comps():
                    pa_ref[item] = name
                    
        deriv_iter_comps = [comp.name for comp in self.derivative_iter()]

        # Fill input dictionaries with values from input arg.
        for edge in edges:
            src, targets = edge
            
            if src != '@in' and src not in self._input_outputs:
                comp_name, dot, var_name = src.partition('.')
                
                i1, i2 = self.get_bounds(src)
                
                # Free-floating variables
                if not var_name:
                    var_name = comp_name
                    if 'parent' in pa_ref:
                        comp_name = pa_ref['parent']
                    else:
                        comp_name = 'parent'
                
                elif comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                
                if var_name in inputs[comp_name]: 
                    inputs[comp_name][var_name] += arg[i1:i2]
                    outputs[comp_name][var_name] += arg[i1:i2]
                else:
                    inputs[comp_name][var_name] = arg[i1:i2].copy()
                    outputs[comp_name][var_name] = arg[i1:i2].copy()

            # Parameter group support
            if not isinstance(targets, tuple):
                targets = [targets]
                   
            for target in targets:
                if target != '@out':
                    i1, i2 = self.get_bounds(target)
                    comp_name, dot, var_name = target.partition('.')
                    
                    # Free-floating variables
                    if not var_name:
                        var_name = comp_name
                        if 'parent' in pa_ref:
                            comp_name = pa_ref['parent']
                        else:
                            comp_name = 'parent'
                    
                    elif comp_name in pa_ref:
                        var_name = '%s.%s' % (comp_name, var_name)
                        comp_name = pa_ref[comp_name]
                        
                    if edge[0] == '@in':
                        # Extra eqs for parameters contribute a 1.0 on diag
                        outputs[comp_name][var_name] = arg[i1:i2].copy()
                    else:
                        # Interior comp edges contribute a -1.0 on diag
                        outputs[comp_name][var_name] = -arg[i1:i2].copy()
                            
        # Call ApplyMinvT on each component (preconditioner)
        #for comp in self.derivative_iter():
            #name = comp.name
            #if hasattr(comp, 'applyMinvT'):
                #inputs[name] = applyMinvT(comp, inputs[name])
            
        # Call ApplyJT on each component
        for comp in self.derivative_iter():
            name = comp.name
            applyJT(comp, inputs[name], outputs[name])

        # Poke results into the return vector
        #print inputs, outputs
        result = zeros(len(arg))
        
        for edge in edges:
            src, target = edge
            if '@in' not in src:
                i1, i2 = self.get_bounds(src)
            
            # Input-input connections are not in the jacobians. We need
            # to add the contribution.
            if src in self._input_outputs:
                
                if src in input_input_xref:
                    ref_edge = input_input_xref[src]
                    i3, i4 = self.get_bounds(ref_edge[0])
                    result[i1:i2] = -arg[i1:i2]
                    result[i3:i4] = result[i3:i4] + arg[i1:i2]
                    
                    # This entry shouldn't have anything else in it.
                    if arg[i1:i2] != 0.0:
                        continue
            
            if target == '@out':
                target = src
                    
            # Parameter group support
            if not isinstance(target, tuple):
                target = [target]
            
            for item in target:
                i1, i2 = self.get_bounds(item)
                comp_name, dot, var_name = item.partition('.')
                
                # Free-floating variables
                if not var_name:
                    var_name = comp_name
                    if 'parent' in pa_ref:
                        comp_name = pa_ref['parent']
                    else:
                        comp_name = 'parent'
                
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                result[i1:i2] = result[i1:i2] + outputs[comp_name][var_name]
                
        #print arg, result
        return result
    
    def group_nondifferentiables(self):
        """Method to find all non-differentiable blocks. These blocks
        will be replaced in the differentiation workflow by a pseudo-
        assembly, which can provide its own Jacobian via finite difference.
        """
        
        nondiff = []
        for comp in self.get_components():
            if not hasattr(comp, 'apply_deriv') and \
               not hasattr(comp, 'apply_derivT') and \
               not hasattr(comp, 'provideJ'):
                nondiff.append(comp.name)
                
        if len(nondiff) == 0:
            return
        
        collapsed = self._get_collapsed_graph()

        # Groups any connected non-differentiable blocks. Each block is a set
        # of component names.
        nondiff_groups = []
        sub = collapsed.subgraph(nondiff)
        nd_graphs = nx.connected_component_subgraphs(sub.to_undirected())
        for item in nd_graphs:
            nondiff_groups.append(item.nodes())
                
        # We need to copy our graph, and put pseudoasemblies in place
        # of the nondifferentiable components.
        
        graph = nx.DiGraph(collapsed)
        dgraph = self.scope._depgraph
        pseudo_assemblies = {}
        
        # for cyclic workflows, remove cut edges.
        for edge in self._severed_edges:
            comp1, _, _ = edge[0].partition('.')
            comp2, _, _ = edge[1].partition('.')
            
            graph.remove_edge(comp1, comp2)
        
        for j, group in enumerate(nondiff_groups):
            pa_name = '~~%d' % j
            
            # Add the pseudo_assemblies:
            graph.add_node(pa_name)
            
            # Carefully replace edges
            inputs = set()
            outputs = set()
            all_edges = graph.edges()
            recursed_components = recursive_components(self.scope, group)
            
            for edge in all_edges:
                
                if edge[0] in group and edge[1] in group:
                    graph.remove_edge(edge[0], edge[1])
                    var_edge = dgraph.get_directional_interior_edges(edge[0], edge[1])
                    self._hidden_edges.update(var_edge)
                    
                elif edge[0] in group:
                    graph.remove_edge(edge[0], edge[1])
                    graph.add_edge(pa_name, edge[1])
                    
                    # Hack: deal with driver connections that connect to
                    # a component in a subdriver's workflow.
                    pcomp = getattr(self.scope, edge[1])
                    if isinstance(pcomp, PseudoComponent) and \
                       pcomp._pseudo_type in ['objective', 'constraint']:
                        var_edge = set()
                        for pcomp_edge in pcomp.list_connections():
                            src_edge = pcomp_edge[0].split('.')[0]
                            if src_edge in recursed_components:
                                var_edge.add(pcomp_edge)
                    else:
                        var_edge = dgraph.get_directional_interior_edges(edge[0], edge[1])
                    outputs.update(var_edge)
                    
                elif edge[1] in group:
                    graph.remove_edge(edge[0], edge[1])
                    graph.add_edge(edge[0], pa_name)
                    
                    var_edge = dgraph.get_directional_interior_edges(edge[0], edge[1])
                    inputs.update(var_edge)
                    
            # Input and outputs that crossed the cut line should be included
            # for the pseudo-assembly.
            for edge in self._severed_edges:
                comp1, _, _ = edge[0].partition('.')
                comp2, _, _ = edge[1].partition('.')
                
                if comp1 in group:
                    outputs.add(edge)
                if comp2 in group:
                    inputs.add(edge)
                    
                if edge in self._hidden_edges:
                    self._hidden_edges.remove(edge)
            
            # Remove old nodes
            graph.remove_nodes_from(group)

            # You don't need the whole edge.
            inputs  = [b for a, b in inputs]
            outputs = list(set([a for a, b in outputs]))
                
            # Boundary edges must be added to inputs and outputs
            for edge in list(self._additional_edges):
                src, targets = edge
                
                comp_name, dot, var_name = src.partition('.')
                
                if comp_name in group or comp_name in recursed_components:
                    outputs.append(src)
                 
                # Parameter-groups are tuples
                if not isinstance(targets, tuple):
                    targets = [targets]
                
                target_group = []    
                for target in targets:
                    comp_name, dot, var_name = target.partition('.')
                    
                    # Edges in the assembly
                    if not var_name and target != '@out':
                        target_group.append(target)
                        
                    elif comp_name in group or comp_name in recursed_components:
                        target_group.append(target)
                        
                if len(target_group) > 1:
                    inputs.append(tuple(target_group))
                elif len(target_group) > 0:
                    inputs.append(target_group[0])
                
            # Input to input connections lead to extra outputs.
            for item in self._input_outputs:
                if item in outputs:
                    outputs.remove(item)
            
            # Create pseudo_assy
            comps = [getattr(self.scope, name) for name in group]
            pseudo_assemblies[pa_name] = \
                PseudoAssembly(pa_name, comps, inputs, outputs, self,
                               recursed_components = recursed_components)
            
        # Execution order may be different after grouping, so topsort
        iterset = nx.topological_sort(graph)
        
        # Save off list containing comps and pseudo-assemblies
        self.derivative_iterset = []
        scope = self.scope
        for name in iterset:
            if '~' in name:
                self.derivative_iterset.append(pseudo_assemblies[name])
            else:
                self.derivative_iterset.append(getattr(scope, name))
                
        # Basically only returning the text list to make the test easy.
        return iterset

    def derivative_iter(self):
        """Return the iterator for differentiating this workflow. All
        non-differential groups are found in pseudo-assemblies.
        """
        if self.derivative_iterset is None:
            return [getattr(self.scope, n) for n in self.get_names(full=True)]
        return self.derivative_iterset

    def calc_derivatives(self, first=False, second=False, savebase=False,
                         extra_in=None, extra_out=None):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""

        self._stop = False
        for node in self.derivative_iter():
            node.calc_derivatives(first, second, savebase, extra_in, extra_out)
            if self._stop:
                raise RunStopped('Stop requested')

    def calc_gradient(self, inputs=None, outputs=None, fd=False, 
                      upscope=False, mode='auto'):
        """Returns the gradient of the passed outputs with respect to
        all passed inputs.
        """
        
        if inputs is None:
            if hasattr(self._parent, 'list_param_group_targets'):
                inputs = self._parent.list_param_group_targets()
            else:
                msg = "No inputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
            
        if outputs is None:
            outputs = []
            if hasattr(self._parent, 'get_objectives'):
                obj = ["%s.out0" % item.pcomp_name for item in \
                        self._parent.get_objectives().values()]
                outputs.extend(obj)
            if hasattr(self._parent, 'get_constraints'):
                con = ["%s.out0" % item.pcomp_name for item in \
                               self._parent.get_constraints().values()]
                outputs.extend(con)
                
            if len(outputs) == 0:
                msg = "No outputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)

        # Override to do straight finite-difference of the whole model, with
        # no fake fd.
        if fd is True:
            
            mode = 'forward'
            
            # Finite difference the whole thing by putting the whole workflow in a
            # pseudo-assembly. This requires being a little creative.
            comps = [comp for comp in self]
            comp_names = [comp.name for comp in comps]
            rcomps = recursive_components(self.scope, comp_names)            
            pseudo = PseudoAssembly('~Check_Gradient', comps, inputs, outputs, 
                                    self, recursed_components=rcomps)
            pseudo.ffd_order = 0
            graph = self._parent.workflow_graph()
            self._hidden_edges = set(graph.get_interior_connections(comp_names))
            
            # Hack: subdriver edges aren't in the assy depgraph, so we 
            # have to manually find and remove them.
            for dr_edge in self.get_driver_edges():
                dr_src = dr_edge[0].split('.',1)[0]
                dr_targ = dr_edge[1].split('.',1)[0]
                if '%s.in0' % dr_src in inputs:
                    pcomp = getattr(self.scope, dr_src)
                    self._hidden_edges.update(pcomp.list_connections())
                elif '%s.out0' % dr_targ in outputs:
                    pcomp = getattr(self.scope, dr_targ)
                    self._hidden_edges.update(pcomp.list_connections())
            
            self.derivative_iterset = [pseudo]

            # Allow for the rare case the user runs this manually, first, in
            # which case the in/out edges haven't been defined yet.
            if len(self._additional_edges) == 0:
                
                # New edges for all requested inputs and outputs
                input_edges = [('@in', a) for a in inputs]
                output_edges = [(a, '@out') for a in outputs]
                additional_edges = set(input_edges + output_edges)
                self._additional_edges = additional_edges

            # Make sure to undo our big pseudo-assembly next time we calculate the
            # gradient.
            self._find_nondiff_blocks = True
            
        # Only do this once: find additional edges and figure out our
        # non-differentiable blocks.
        elif self._find_nondiff_blocks:
            
            self._severed_edges = []
            self._additional_edges = set()
            self._hidden_edges = set()
            
            # New edges for all requested inputs and outputs
            
            input_edges = [('@in', a) for a in inputs]
            output_edges = [(a, '@out') for a in outputs]
            additional_edges = set(input_edges + output_edges)
            self._additional_edges = additional_edges
            
            self.group_nondifferentiables()
            
            self._find_nondiff_blocks = False
            
        # Some reorganization to add additional edges when scoped outside
        # the driver that owns this workflow.
        elif upscope:
            
            # New edges for parameters
            input_edges = [('@in', a) for a in inputs]
            additional_edges = set(input_edges)
            
            # New edges for responses
            out_edges = [a[0] for a in self.get_interior_edges()]
            for item in outputs:
                if item not in out_edges:
                    additional_edges.add((item, '@out'))
            
            newset = self._additional_edges.union(additional_edges)
            if len(newset) > len(self._additional_edges):
                self._additional_edges = newset
                self._hidden_edges = set()
                self.group_nondifferentiables()
            
            self._find_nondiff_blocks = False
        
        # Auto-determine which mode to use.
        if mode == 'auto' and fd is False:
            # TODO - additional determination based on presence of
            # apply_derivT
            
            # TODO: This is repeated in derivatives.calc_gradient for sizing.
            # We should cache it and only do it once.
            
            num_in = 0
            for item in inputs:
                
                # For parameter groups, only size the first
                if isinstance(item, tuple):
                    item = item[0]
                    
                val = self.scope.get(item)
                width = flattened_size(item, val)
                num_in += width
        
            num_out = 0
            for item in outputs:
                val = self.scope.get(item)
                width = flattened_size(item, val)
                num_out += width
                
            if num_in > num_out:
                mode = 'adjoint'
            else:
                mode = 'forward'
            
        if mode == 'adjoint':
            return calc_gradient_adjoint(self, inputs, outputs)
        else:
            return calc_gradient(self, inputs, outputs)
    
    def check_gradient(self, inputs=None, outputs=None, stream=None, adjoint=False):
        """Compare the OpenMDAO-calculated gradient with one calculated
        by straight finite-difference. This provides the user with a way
        to validate his derivative functions (ApplyDer and ProvideJ.)
        Note that fake finite difference is turned off so that we are
        doing a straight comparison.

        stream: file-like object or string
            Where to write to, default stdout. If a string is supplied,
            that is used as a filename.
        """
        stream = stream or sys.stdout
        if isinstance(stream, basestring):
            stream = open(stream, 'w')
            close_stream = True
        else:
            close_stream = False
    
        self.config_changed()
        if adjoint:
            J = self.calc_gradient(inputs, outputs, mode='adjoint')
        else:
            J = self.calc_gradient(inputs, outputs)
        
        self.config_changed()
        Jbase = self.calc_gradient(inputs, outputs, fd=True)

        print >> stream, 24*'-'
        print >> stream, 'Calculated Gradient'
        print >> stream, 24*'-'
        print >> stream, J
        print >> stream, 24*'-'
        print >> stream, 'Finite Difference Comparison'
        print >> stream, 24*'-'
        print >> stream, Jbase

        if inputs is None:
            if hasattr(self._parent, 'get_parameters'):
                inputs = []
                input_refs = []
                for key, param in self._parent.get_parameters().items():
                    inputs.extend(param.targets)
                    input_refs.extend([key for t in param.targets])
            # Should be caught in calc_gradient()
            else:  # pragma no cover
                msg = "No inputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
        else:
            input_refs = inputs
            
        if outputs is None:
            outputs = []
            output_refs = []
            if hasattr(self._parent, 'get_objectives'):
                obj = ["%s.out0" % item.pcomp_name for item in \
                        self._parent.get_objectives().values()]
                outputs.extend(obj)
                output_refs.extend(self._parent.get_objectives().keys())
            if hasattr(self._parent, 'get_constraints'):
                con = ["%s.out0" % item.pcomp_name for item in \
                               self._parent.get_constraints().values()]
                outputs.extend(con)
                output_refs.extend(self._parent.get_constraints().keys())
                
            if len(outputs) == 0:  # pragma no cover
                msg = "No outputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
        else:
            output_refs = outputs

        out_width = 0
        for output, oref in zip(outputs, output_refs):
            out_val = self.scope.get(output)
            out_names = flattened_names(oref, out_val)
            out_width = max(out_width, max([len(out) for out in out_names]))

        inp_width = 0
        for input, iref in zip(inputs, input_refs):
            inp_val = self.scope.get(input)
            inp_names = flattened_names(str(iref), inp_val)
            inp_width = max(inp_width, max([len(inp) for inp in inp_names]))

        label_width = out_width + inp_width + 4

        print >> stream
        print >> stream, label_width*' ', \
              '%-18s %-18s %-18s' % ('Calculated', 'FiniteDiff', 'RelError')
        print >> stream, (label_width+(3*18)+3)*'-'

        suspect_limit = 1e-5
        error_n = error_sum = 0
        error_max = error_loc = None
        suspects = []
        i = -1
        for output, oref in zip(outputs, output_refs):
            out_val = self.scope.get(output)
            for out_name in flattened_names(oref, out_val):
                i += 1
                j = -1
                for input, iref in zip(inputs, input_refs):
                    inp_val = self.scope.get(input)
                    for inp_name in flattened_names(iref, inp_val):
                        j += 1
                        calc = J[i, j]
                        finite = Jbase[i, j]
                        if finite:
                            error = (calc - finite) / finite
                        else:
                            error = calc
                        error_n += 1
                        error_sum += abs(error)
                        if error_max is None or abs(error) > abs(error_max):
                            error_max = error
                            error_loc = (out_name, inp_name)
                        if abs(error) > suspect_limit:
                            suspects.append((out_name, inp_name))
                        print >> stream, '%*s / %*s: %-18s %-18s %-18s' \
                              % (out_width, out_name, inp_width, inp_name,
                                 calc, finite, error)
        print >> stream
        print >> stream, 'Average RelError:', error_sum / error_n
        print >> stream, 'Max RelError:', error_max, 'for %s / %s' % error_loc
        if suspects:
            print >> stream, 'Suspect gradients (RelError > %s):' % suspect_limit
            for out_name, inp_name in suspects:
                print >> stream, '%*s / %*s' \
                      % (out_width, out_name, inp_width, inp_name) 
        print >> stream

        if close_stream:
            stream.close()

