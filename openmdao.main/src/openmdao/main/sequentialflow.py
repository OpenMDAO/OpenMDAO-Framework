""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

import networkx as nx
import sys
from math import isnan

from openmdao.main.array_helpers import flattened_size, \
                                        flattened_names, flatten_slice
from openmdao.main.derivatives import calc_gradient, calc_gradient_adjoint, \
                                      applyJ, applyJT, applyMinvT, applyMinv
                                      
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudoassembly import PseudoAssembly, to_PA_var, from_PA_var
from openmdao.main.vartree import VariableTree

from openmdao.main.workflow import Workflow
from openmdao.main.depgraph import find_related_pseudos, base_var, \
                                    mod_for_derivs, is_basevar_node, \
                                    edge_dict_to_comp_list, flatten_list_of_iters, \
                                    is_input_base_node, is_output_base_node, \
                                    is_subvar_node, edges_to_dict, is_boundary_node
from openmdao.main.interfaces import IDriver, IImplicitComponent, ISolver
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
        
        # Bookkeeping
        self._edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components(full=True))

    def __len__(self):
        if self._names is None:
            self.get_names()
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
        
        self._edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        self._names = None

    def sever_edges(self, edges):
        """Temporarily remove the specified edges but save
        them and their metadata for later restoration. 
        """
        if edges:
            params = self._parent.get_parameters()
            non_param_edges = [(src, targ) for (src, targ) in edges \
                               if targ not in params]
            self.scope._depgraph.sever_edges(non_param_edges)

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
                          
            self._fullnames = self._names[:]
            fullset = set(self._parent.list_pseudocomps())
            fullset.update(find_related_pseudos(self.scope._depgraph.component_graph(),
                                                self._names))
            self._fullnames.extend(fullset - set(self._names))

        if full:
            return self._fullnames[:]
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
        allnames = self.get_names(full=True)
        try:
            self._explicit_names.remove(compname)
        except ValueError:
            pass
        if compname in allnames:
            self.config_changed()

    def clear(self):
        """Remove all components from this workflow."""
        self._explicit_names = []
        self.config_changed()

    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        dgraph = self.derivative_graph()
        if 'mapped_inputs' in dgraph.graph:
            inputs = dgraph.graph['mapped_inputs']
        else:
            inputs = dgraph.graph['inputs']
            
        basevars = set()
        edges = self.edge_list()
        implicit_edges = self.get_implicit_info()
        sortedkeys = sorted(implicit_edges)
        sortedkeys.extend(sorted(self.edge_list().keys()))
        
        nEdge = 0
        for src in sortedkeys:
            
            if src in implicit_edges:
                targets = implicit_edges[src]
                is_implicit = True
            else:
                targets = edges[src]
                is_implicit = False

            if isinstance(targets, str):
                targets = [targets]

            # Implicit source edges are tuples.
            if is_implicit == True:
                impli_edge = nEdge
                for resid in src:
                    unmap_src = from_PA_var(resid)
                    val = self.scope.get(unmap_src)
                    width = flattened_size(unmap_src, val, self.scope)

                    if isinstance(val, ndarray):
                        shape = val.shape
                    else:
                        shape = 1

                    bound = (impli_edge, impli_edge+width)
                    self.set_bounds(resid, bound)
                    basevars.add(resid)
                    impli_edge += width

            # Regular components
            else:

                # Only need to grab the source (or first target for param) to
                # figure out the size for the residual vector
                measure_src = src
                if '@in' in src:
                    idx = int(src[3:].split('[')[0])
                    if inputs[idx][0] in dgraph:
                        measure_src = inputs[idx][0]
                    else:
                        measure_src = targets[0]
                elif src == '@fake':
                    for t in targets:
                        if not t.startswith('@'):
                            measure_src = t
                            break
                    else:
                        raise RuntimeError("malformed graph!")

                # Find our width, etc.
                unmap_src = from_PA_var(measure_src)
                val = self.scope.get(unmap_src)
                width = flattened_size(unmap_src, val, self.scope)
                if isinstance(val, ndarray):
                    shape = val.shape
                else:
                    shape = 1

                # Special poke for boundary node
                if is_boundary_node(dgraph, measure_src) or \
                   is_boundary_node(dgraph, base_var(dgraph, measure_src)):
                    bound = (nEdge, nEdge+width)
                    self.set_bounds(measure_src, bound)
                     
                src_noidx = src.split('[',1)[0]
                
                # Poke our source data
                
                # Array slice of src that is already allocated
                if '[' in src and src_noidx in basevars:
                    _, _, idx = src.partition('[')
                    basebound = self.get_bounds(src_noidx)
                    if not '@in' in src_noidx:
                        unmap_src = from_PA_var(src_noidx)
                        val = self.scope.get(unmap_src)
                        shape = val.shape
                    offset = basebound[0]
                    istring, ix = flatten_slice(idx, shape, offset=offset, 
                                                name='ix')
                    bound = (istring, ix)
                    # Already allocated
                    width = 0
                    
                # Input-input connection to implicit state
                elif src_noidx in basevars:
                    bound = self.get_bounds(src_noidx)
                    width = 0
                    
                # Normal src
                else:
                    bound = (nEdge, nEdge+width)
                    
                self.set_bounds(src, bound)
                basevars.add(src)
                
            # Poke our target data
            impli_edge = nEdge
            for target in targets:
                    
                # Handle States in implicit comps
                if is_implicit == True:
                    
                    if isinstance(target, str):
                        target = [target]
                        
                    unmap_targ = from_PA_var(target[0])
                    val = self.scope.get(unmap_targ)
                    imp_width = flattened_size(unmap_targ, val, self.scope)
                    if isinstance(val, ndarray):
                        shape = val.shape
                    else:
                        shape = 1
                        
                    for itarget in target:
                        bound = (impli_edge, impli_edge+imp_width)
                        self.set_bounds(itarget, bound)
                        basevars.add(itarget)
                        
                    impli_edge += imp_width
                    width = impli_edge - nEdge
                    
                elif not target.startswith('@'):
                    self.set_bounds(target, bound)
            
            #print input_src, src, target, bound,      
            nEdge += width
            impli_edge = nEdge
                
        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))

        return nEdge

    def get_bounds(self, node):
        """ Return a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        dgraph = self._derivative_graph
        i1, i2 = dgraph.node[node]['bounds'][self._parent.name]
        
        # Handle index slices
        if isinstance(i1, str):
            if ':' in i1:
                i3 = i2 + 1
            else:
                i2 = i2.tolist()
                i3 = 0
            return i2, i3
        else:
            i2 = i2
            
        return i1, i2
        
    def set_bounds(self, node, bounds):
        """ Set a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        dgraph = self._derivative_graph
        
        try:
            meta = dgraph.node[node]
            
        # Array indexed parameter nodes are not in the graph, so add them.
        except KeyError:
            dgraph.add_subvar(node)
            meta = dgraph.node[node]
        
        if 'bounds' not in meta:
            meta['bounds'] = {}
            
        meta['bounds'][self._parent.name] = bounds
        
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

    def mimic(self, src):
        self.clear()
        par = self._parent.parent
        if par is not None:
            self._explicit_names = [n for n in src._explicit_names if hasattr(par, n)]
        else:
            self._explicit_names = src._explicit_names[:]

    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''
        
        comps = edge_dict_to_comp_list(self._derivative_graph, self._edges,
                                       self.get_implicit_info())
        if '@fake' in comps:
            del comps['@fake']
        result = zeros(len(arg))
        
        # We can call applyJ on each component one-at-a-time, and poke the
        # results into the result vector.
        for compname, data in comps.iteritems():
            
            comp_inputs = data['inputs']
            comp_outputs = data['outputs']
            comp_residuals = data['residuals']
            
            inputs = {}
            outputs = {}
            
            for varname in comp_inputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    inputs[varname] = arg[i1].copy()
                else:
                    inputs[varname] = arg[i1:i2].copy()
            
            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    if varname in comp_residuals:
                        outputs[varname] = zeros((1, 1))
                    else:
                        inputs[varname] = arg[i1].copy()
                        outputs[varname] = arg[i1].copy()
                else:
                    if varname in comp_residuals:
                        outputs[varname] = zeros((i2-i1, 1))
                    else:
                        inputs[varname] = arg[i1:i2].copy()
                        outputs[varname] = arg[i1:i2].copy()
                
            if '~' in compname:
                comp = self._derivative_graph.node[compname]['pa_object']
            else:
                comp = self.scope.get(compname)
        
            # Preconditioning
            # Currently not implemented in forward mode, mostly because this
            # mode requires post multiplication of the result by the M after
            # you have the final gradient.
            #if hasattr(comp, 'applyMinv'):
                #inputs = applyMinv(comp, inputs)
            
            applyJ(comp, inputs, outputs, comp_residuals)
            #print inputs, outputs
            
            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    result[i1] = outputs[varname]
                else:
                    result[i1:i2] = outputs[varname]
                
        # Each parameter adds an equation
        for src, targets in self._edges.iteritems():
            if '@in' in src or '@fake' in src:
                if not isinstance(targets, list):
                    targets = [targets]
                    
                for target in targets:
                    i1, i2 = self.get_bounds(target)
                    result[i1:i2] = arg[i1:i2]
                
        #print arg, result
        return result
        
    def matvecREV(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''
        
        dgraph = self._derivative_graph
        comps = edge_dict_to_comp_list(dgraph, self._edges,
                                       self.get_implicit_info())
        result = zeros(len(arg))
        
        # We can call applyJ on each component one-at-a-time, and poke the
        # results into the result vector.
        for compname, data in comps.iteritems():
            if compname == '@fake':
                continue
            
            comp_inputs = data['inputs']
            comp_outputs = data['outputs']
            comp_residuals = data['residuals']

            inputs = {}
            outputs = {}
            
            for varname in comp_outputs:
                
                node = '%s.%s' % (compname, varname)
                
                # Ouputs define unique edges, so don't duplicate anything
                if is_subvar_node(dgraph, node):
                    if base_var(dgraph, node).split('.', 1)[1] in comp_outputs:
                        continue
                    
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    inputs[varname] = arg[i1].copy()
                    if varname not in comp_residuals:
                        outputs[varname] = zeros(len(i1))
                else:
                    inputs[varname] = arg[i1:i2].copy()
                    if varname not in comp_residuals:
                        outputs[varname] = zeros(i2-i1)
                    
            for varname in comp_inputs:
                node = '%s.%s' % (compname, varname)
                
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    outputs[varname] = zeros(len(i1))
                else:
                    outputs[varname] = zeros(i2-i1)
                
            allvars = outputs.keys()
                
            if '~' in compname:
                comp = self._derivative_graph.node[compname]['pa_object']
            else:
                comp = self.scope.get(compname)
            
            # Preconditioning
            if hasattr(comp, 'applyMinvT'):
                inputs = applyMinvT(comp, inputs)
            
            applyJT(comp, inputs, outputs, comp_residuals)
            #print inputs, outputs
            
            for varname in allvars:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    result[i1] += outputs[varname]
                else:
                    result[i1:i2] += outputs[varname]
                
        # Each parameter adds an equation
        for src, target in self._edges.iteritems():
            if '@in' in src or '@fake' in src:
                if isinstance(target, list):
                    target = target[0]
                    
                i1, i2 = self.get_bounds(target)
                result[i1:i2] += arg[i1:i2]
                        
        #print arg, result
        return result
        
    def derivative_graph(self, inputs=None, outputs=None, fd=False, 
                         severed=None, group_nondif=True):
        """Returns the local graph that we use for derivatives.
        
        inputs: list of strings or tuples of strings
            List of input variables that we are taking derivatives with respect
            to. They must be within this workflow's scope. If no inputs are 
            given, the parent driver's parameters are used. A tuple can be used
            to link inputs together.
            
        outputs: list of strings
            List of output variables that we are taking derivatives of.
            They must be within this workflow's scope. If no outputs are 
            given, the parent driver's objectives and constraints are used.
        
        fd: boolean
            set to True to finite difference the whole model together with
            fake finite difference turned off. This is mainly for checking
            your model's analytic derivatives.
            
        severed: list
            If a workflow has a cylic connection, some edges must be severed.
            When a cyclic workflow calls this function, it passes a list of
            edges so that they can be severed prior to the topological sort.

        group_nondif: bool
            If True, collapse parts of the graph into PseudoAssemblies when
            necessary.
        """
        
        if self._derivative_graph is None or group_nondif is False:
            # when we call with group_nondif = False, we want the union of the passed inputs/outputs
            # plus the inputs/outputs from the solver
            if group_nondif is False:
                tmp_inputs = inputs
                inputs = None
                tmp_outputs = outputs
                outputs = None
                
            # If inputs aren't specified, use the parameters
            if inputs is None:
                if hasattr(self._parent, 'list_param_group_targets'):
                    inputs = self._parent.list_param_group_targets()
                else:
                    msg = "No inputs given for derivatives."
                    self.scope.raise_exception(msg, RuntimeError)
                    
            if group_nondif is False:
                inputs = list(set(tmp_inputs).union(inputs))
        
            # If outputs aren't specified, use the objectives and constraints
            if outputs is None:
                outputs = []
                if hasattr(self._parent, 'get_objectives'):
                    outputs.extend(["%s.out0" % item.pcomp_name for item in \
                            self._parent.get_objectives().values()])
                if hasattr(self._parent, 'get_constraints'):
                    outputs.extend(["%s.out0" % item.pcomp_name for item in \
                                   self._parent.get_constraints().values()])
                    
            if group_nondif is False:
                outputs = list(set(tmp_outputs).union(outputs))
    
            if len(outputs) == 0:
                msg = "No outputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
                
            graph = self.scope._depgraph

            # make a copy of the graph because it will be
            # modified by mod_for_derivs
            dgraph = graph.subgraph(graph.nodes())
            mod_for_derivs(dgraph, inputs, outputs, self)
            
            if group_nondif:
                self._derivative_graph = dgraph
                self._group_nondifferentiables(fd, severed)
            else:
                return dgraph
            
        return self._derivative_graph
    
    def _group_nondifferentiables(self, fd=False, severed=None):
        """Method to find all non-differentiable blocks, and group them
        together, replacing them in the derivative graph with pseudo-
        assemblies that can finite difference their components together.
        
        fd: boolean
            set to True to finite difference the whole model together with
            fake finite difference turned off. This is mainly for checking
            your model's analytic derivatives.
            
        severed: list
            If a workflow has a cylic connection, some edges must be severed.
            When a cyclic workflow calls this function, it passes a list of
            edges so that they can be severed prior to the topological sort.
        """
        
        dgraph = self._derivative_graph
        scope = self.scope
        
        # If we have a cyclic workflow, we need to remove severed edges from
        # the derivatives graph.
        if severed is not None:
            for edge in severed:
                if edge in dgraph.edges():
                    dgraph.remove_edge(edge[0], edge[1])
            
        cgraph = dgraph.component_graph()
        comps = cgraph.nodes()
        nondiff_map = {}
        
        # Full model finite-difference, so all components go in the PA
        if fd == True:
            nondiff_groups = [comps]
            for c in comps:
                nondiff_map[c] = 0

        # Find the non-differentiable components
        else:
            
            # A component with no derivatives is non-differentiable
            nondiff = set()
            nondiff_groups = []
            
            for name in comps:
                comp = self.scope.get(name)
                if not hasattr(comp, 'apply_deriv') and \
                   not hasattr(comp, 'apply_derivT') and \
                   not hasattr(comp, 'provideJ'):
                    nondiff.add(name)
                elif comp.force_fd is True:
                    nondiff.add(name)
                elif dgraph.node[name].get('non-differentiable'):
                    nondiff.add(name)
                    
            # If a connection is non-differentiable, so are its src and 
            # target components.
            conns = dgraph.list_connections()
                
            for edge in conns:
                src = edge[0]
                target = edge[1]
                
                if '@' in src or '@' in target or '.' not in src:
                    continue
                
                # Default differentiable connections
                val = self.scope.get(src)
                if isinstance (val, (float, ndarray, VariableTree)):
                    continue
                
                # Custom differentiable connections
                meta = self.scope.get_metadata(src)
                if 'data_shape' in meta:
                    continue
                
                #Nothing else is differentiable
                else:
                    nondiff.add(src.split('.')[0])
                    nondiff.add(target.split('.')[0])
                        
            # Everything is differentiable, so return
            if len(nondiff) == 0:
                return
            
            # Groups any connected non-differentiable blocks. Each block is a set
            # of component names.
            sub = cgraph.subgraph(nondiff)
            nd_graphs = nx.connected_component_subgraphs(sub.to_undirected())
            for i, item in enumerate(nd_graphs):
                inodes = item.nodes()
                nondiff_groups.append(inodes)
                nondiff_map.update([(n,i) for n in inodes])
                
        meta_inputs = dgraph.graph['inputs']
        meta_outputs = dgraph.graph['outputs']
        map_inputs = meta_inputs[:]
        map_outputs = meta_outputs[:]
        dgraph.graph['mapped_inputs'] = map_inputs
        dgraph.graph['mapped_outputs'] = map_outputs
        
       # Add requested params that point to boundary vars
        for i, varpath in enumerate(meta_inputs):
            if isinstance(varpath, basestring):
                varpath = [varpath]
                
            mapped = []
            for path in varpath:
                compname, _, varname = path.partition('.')
                if varname and (compname in nondiff_map):
                    mapped.append(to_PA_var(path, '~~%d' % nondiff_map[compname]))
                else:
                    mapped.append(path)  # keep old value in that spot
            
            map_inputs[i] = tuple(mapped)
            
        # Add requested outputs
        for i, varpath in enumerate(meta_outputs):
            compname, _, varname = varpath.partition('.')
            if varname and (compname in nondiff_map):
                map_outputs[i] = to_PA_var(varpath, '~~%d' % nondiff_map[compname])

        for j, group in enumerate(nondiff_groups):
            pa_name = '~~%d' % j
            
            # First, find our group boundary
            allnodes = dgraph.find_prefixed_nodes(group)
            out_edges = nx.edge_boundary(dgraph, allnodes)
            in_edges = nx.edge_boundary(dgraph, 
                                        set(dgraph.nodes()).difference(allnodes))
            solver_states = []
            if fd is False:
                for comp in group:
                    solver_states.extend([node for node in dgraph.predecessors(comp) \
                                          if 'solver_state' in dgraph.node[node]])
            
            pa_inputs = edges_to_dict(in_edges).values()
            pa_inputs.extend(solver_states)
            pa_outputs = set([a for a, b in out_edges])          
                        
            # Create the pseudoassy
            pseudo = PseudoAssembly(pa_name, group, pa_inputs, pa_outputs, self)
            
            # for full-model fd, turn off fake finite difference
            if fd==True:
                pseudo.ffd_order = 0
            
            # Add pseudoassys to graph
            dgraph.add_node(pa_name, pa_object=pseudo, comp=True, 
                            pseudo='assembly', valid=True)
            
            renames = {}
            # Add pseudoassy inputs
            for varpath in list(flatten_list_of_iters(pa_inputs)) + list(pa_outputs):
                varname = to_PA_var(varpath, pa_name)
                if varpath in dgraph:
                    renames[varpath] = varname
                    if is_subvar_node(dgraph, varpath):
                        renames[base_var(dgraph, varpath)] = to_PA_var(base_var(dgraph, varpath), 
                                                                       pa_name)

            nx.relabel_nodes(dgraph, renames, copy=False)
            
            for oldname,newname in renames.items():
                if is_subvar_node(dgraph, newname):
                    # since we're changing basevar, we need to make our
                    # own copy of the metadata dict for this node to
                    # avoid messing up the top level depgraph
                    dgraph.node[newname] = dict(dgraph.node[newname].items())
                    dgraph.node[newname]['basevar'] = to_PA_var(dgraph.node[newname]['basevar'], pa_name)
                if is_input_base_node(dgraph, newname):
                    dgraph.add_edge(newname, pa_name)
                elif is_output_base_node(dgraph, newname):
                    dgraph.add_edge(pa_name, newname)
                        
            # Clean up the old nodes in the graph
            dgraph.remove_nodes_from(allnodes)
            
        return None

    def edge_list(self):
        """ Return the list of edges for the derivatives of this workflow. """
        
        self._edges = edges_to_dict(self.derivative_graph().list_connections())
            
        return self._edges

    def get_implicit_info(self):
        """ Return a dict of the form {(residuals) : [states]}
        """
        info = {}
        
        comps = self.derivative_graph().all_comps()
        
        # Full model finite difference = no implcit edges
        if len(comps) == 1 and '~~' in comps[0]:
            return info
        
        # Residuals and states for implicit components
        for cname in comps:
            
            if cname.startswith('~~'):
                continue
            
            comp = getattr(self.scope, cname)
            
            if has_interface(comp, IImplicitComponent):
                if not comp.eval_only:
                    key = tuple(['.'.join([cname, n]) 
                                     for n in comp.list_residuals()])
                    info[key] = ['.'.join([cname, n]) 
                                     for n in comp.list_states()]
                    
        # Nested solvers act implicitly.
        dgraph = self._derivative_graph
        pa_comps = [dgraph.node[item]['pa_object'] \
                    for item in dgraph.all_comps() if '~~' in item]
        for comp in self._parent.iteration_set():
            if has_interface(comp, ISolver):
                
                key = tuple(comp.list_eq_constraint_targets())
                unmapped_states = comp.list_param_group_targets()
                
                # Need to map the subdriver parameters to any existing
                # pseudoassemblies
                value = []
                for state_tuple in unmapped_states:
                    value_target = []
                    for state in state_tuple:
                        if state not in dgraph:
                            for pcomp in pa_comps:
                                if state in pcomp.inputs:
                                    value_target.append(to_PA_var(state, 
                                                                  pcomp.name))
                                    break
                        else:
                            value_target.append(state)
                            
                    value.append(tuple(value_target))
                        
                info[key] = value

        return info
        
    def calc_derivatives(self, first=False, second=False, savebase=False,
                         required_inputs=None, required_outputs=None):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""

        self._stop = False
        
        comps = edge_dict_to_comp_list(self.derivative_graph(required_inputs, required_outputs), 
                                       self.edge_list())
        for compname, data in comps.iteritems():
            if '~' in compname:
                node = self._derivative_graph.node[compname]['pa_object']
            elif compname.startswith('@'):
                continue
            else:
                node = self.scope.get(compname)

            inputs = data['inputs']
            outputs = data['outputs']
            node.calc_derivatives(first, second, savebase, inputs, outputs)
            if self._stop:
                raise RunStopped('Stop requested')

    def calc_gradient(self, inputs=None, outputs=None, upscope=False, mode='auto'):
        """Returns the gradient of the passed outputs with respect to
        all passed inputs.
        
        inputs: list of strings or tuples of strings
            List of input variables that we are taking derivatives with respect
            to. They must be within this workflow's scope. If no inputs are 
            given, the parent driver's parameters are used. A tuple can be used
            to link inputs together.
            
        outputs: list of strings
            List of output variables that we are taking derivatives of.
            They must be within this workflow's scope. If no outputs are 
            given, the parent driver's objectives and constraints are used.
            
        upscope: boolean
            This is set to True when our workflow is part of a subassembly that
            lies in a workflow that needs a gradient with respect to variables
            outside of this workflow, so that the caches can be reset.
            
        mode: string
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode,
            'fd' for full-model finite difference (with fake finite 
            difference disabled), or 'auto' to let OpenMDAO determine the
            correct mode.
        """
        
        # This function can be called from a parent driver's workflow for
        # assembly recursion. We have to clear our cache if that happens.
        # We also have to clear it next time we arrive back in our workflow.
        if upscope:
            self._derivative_graph = None
            self._edges = None
            self._upscoped = True
        elif self._upscoped:
            self._derivative_graph = None
            self._edges = None
            self._upscoped = False
            
        dgraph = self.derivative_graph(inputs, outputs, fd=(mode=='fd'))
       
        if 'mapped_inputs' in dgraph.graph:
            inputs = dgraph.graph['mapped_inputs']
            outputs = dgraph.graph['mapped_outputs']
        else:
            inputs = dgraph.graph['inputs']
            outputs = dgraph.graph['outputs']
        
        n_edge = self.initialize_residual()
        
        # Size our Jacobian
        num_in = 0
        for item in inputs:
            
            # For parameter groups, only size the first
            if not isinstance(item, basestring):
                item = item[0]
                
            i1, i2 = self.get_bounds(item)
            if isinstance(i1, list):
                num_in += len(i1)
            else:
                num_in += i2-i1
    
        num_out = 0
        for item in outputs:
            i1, i2 = self.get_bounds(item)
            if isinstance(i1, list):
                num_out += len(i1)
            else:
                num_out += i2-i1
                
        shape = (num_out, num_in)
            
        # Auto-determine which mode to use based on Jacobian shape.
        if mode == 'auto':
            # TODO - additional determination based on presence of
            # apply_derivT
            
            if num_in > num_out:
                mode = 'adjoint'
            else:
                mode = 'forward'
            
        if mode == 'adjoint':
            J = calc_gradient_adjoint(self, inputs, outputs, n_edge, shape)
        elif mode in ['forward', 'fd']:
            J = calc_gradient(self, inputs, outputs, n_edge, shape)
        else:
            msg = "In calc_gradient, mode must be 'forward', 'adjoint', " + \
                  "'auto', or 'fd', but a value of %s was given." % mode
            self.scope.raise_exception(msg, RuntimeError)
            
        # Finally, we need to untransform the jacobian if any parameters have 
        # scalers.
        
        if not hasattr(self._parent, 'get_parameters'):
            return J
        
        params = self._parent.get_parameters()
        
        if len(params) == 0:
            return J
        
        i = 0
        for group in inputs: 
            
            if isinstance(group, str):
                group = [group]
                
            name = group[0]
            if len(group) > 1:
                pname = tuple([from_PA_var(aname) for aname in group])
            else:
                pname = from_PA_var(name)
                
            i1, i2 = self.get_bounds(name)
            
            if isinstance(i1, list):
                width = len(i1)
            else:
                width = i2-i1
            
            if pname in params:
                scaler = params[pname].scaler
                if scaler != 1.0:
                    J[:, i:i+width] = J[:, i:i+width]*scaler
                    
            i = i + width
        #print J        
        return J
            
    
    def check_gradient(self, inputs=None, outputs=None, stream=None, mode='auto'):
        """Compare the OpenMDAO-calculated gradient with one calculated
        by straight finite-difference. This provides the user with a way
        to validate his derivative functions (apply_deriv and provideJ.)
        Note that fake finite difference is turned off so that we are
        doing a straight comparison.

        inputs: (optional) iter of str or None
            Names of input variables. The calculated gradient will be
            the matrix of values of the output variables with respect
            to these input variables. If no value is provided for inputs,
            they will be determined based on the parameters of
            the Driver corresponding to this workflow.
            
        outputs: (optional) iter of str or None
            Names of output variables. The calculated gradient will be
            the matrix of values of these output variables with respect
            to the input variables. If no value is provided for outputs,
            they will be determined based on the objectives and constraints
            of the Driver corresponding to this workflow.
            
        stream: (optional) file-like object or str
            Where to write to, default stdout. If a string is supplied,
            that is used as a filename.
            
        mode: (optional) str
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode, 
            or 'auto' to let OpenMDAO determine the correct mode.
            Defaults to 'auto'.
        """
        stream = stream or sys.stdout
        if isinstance(stream, basestring):
            stream = open(stream, 'w')
            close_stream = True
        else:
            close_stream = False
    
        self.config_changed()
        J = self.calc_gradient(inputs, outputs, mode=mode)
        
        self.config_changed()
        Jbase = self.calc_gradient(inputs, outputs, mode='fd')

        print >> stream, 24*'-'
        print >> stream, 'Calculated Gradient'
        print >> stream, 24*'-'
        print >> stream, J
        print >> stream, 24*'-'
        print >> stream, 'Finite Difference Comparison'
        print >> stream, 24*'-'
        print >> stream, Jbase

        # This code duplication is needed so that we print readable names for the
        # constraints and objectives.
        
        if inputs is None:
            if hasattr(self._parent, 'list_param_group_targets'):
                inputs = self._parent.list_param_group_targets()
                input_refs = []
                for item in inputs:
                    if len(item) < 2:
                        input_refs.append(item[0])
                    else:
                        input_refs.append(item)
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
        for input_tup, iref in zip(inputs, input_refs):
            if isinstance(input_tup, str):
                input_tup = [input_tup]
            inp_val = self.scope.get(input_tup[0])
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
                for input_tup, iref in zip(inputs, input_refs):
                    if isinstance(input_tup, str):
                        input_tup = [input_tup]
                        
                    inp_val = self.scope.get(input_tup[0])
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
                        if abs(error) > suspect_limit or isnan(error):
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
            
        return suspects  # return suspects to make it easier to check from a test


