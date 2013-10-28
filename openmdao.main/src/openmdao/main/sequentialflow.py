""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

import copy
import networkx as nx
import sys

from openmdao.main.array_helpers import flattened_size, flattened_value, \
                                        flattened_names, flatten_slice
from openmdao.main.derivatives import calc_gradient, calc_gradient_adjoint, \
                                      applyJ, applyJT, edge_dict_to_comp_list, \
                                      applyMinvT, applyMinv
                                      
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudoassembly import PseudoAssembly, to_PA_var, from_PA_var
from openmdao.main.vartree import VariableTree

from openmdao.main.workflow import Workflow
from openmdao.main.ndepgraph import find_related_pseudos, base_var, \
                                    get_inner_edges, is_basevar_node
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
        
        # Bookkeeping
        self._edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        
        self._severed_edges = []
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
        
        self._edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        
        self._severed_edges = []
        self._names = None
        self._interior_edges = None

    def sever_edges(self, edges):
        """Temporarily remove the specified edges but save
        them and their metadata for later restoration. 
        """
        self._derivative_graph.sever_edges(edges)

    def unsever_edges(self):
        self._derivative_graph.unsever_edges(self._parent.get_expr_scope())
        
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

    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        nEdge = 0
        dgraph = self.derivative_graph()
        
        basevars = set()
        for src, targets in self.edge_list().iteritems():
            
            # Only need to grab the source (or first target for param) to
            # figure out the size for the residual vector
            if '@in' in src:
                src = targets
                if isinstance(src, list):
                    src = src[0]
                
            if not is_basevar_node(dgraph, src) and base_var(dgraph, src) in basevars:
                base, _, idx = src.partition('[')
                offset, _ = self.get_bounds(base)
                shape = self.scope.get(base).shape
                istring, ix = flatten_slice(idx, shape, offset=offset, name='ix')
                bound = (istring, ix)
            else:
                val = self.scope.get(from_PA_var(src))
                width = flattened_size(src, val, self.scope)
                bound = (nEdge, nEdge+width)
                
            self.set_bounds(src, bound)
            basevars.add(src)
            
            if not isinstance(targets, list):
                targets = [targets]
                
            # Putting the metadata in the targets makes life easier later on
            for target in targets:
                if '@out' not in target:
                    self.set_bounds(target, bound)
                    
            nEdge += width

        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))

        return nEdge

    def get_bounds(self, node):
        """ Return a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        itername = 'top.'+self._parent.itername
        dgraph = self._derivative_graph
        i1, i2 = dgraph.node[node]['bounds'][itername]
        
        # Handle index slices
        if isinstance(i1, str):
            i3 = i2+1 if ':' in i1 else 0
            return i2, i3
            
        return i1, i2
        
    def set_bounds(self, node, bounds):
        """ Set a tuple containing the start and end indices into the
        residual vector that correspond to a given variable name in this
        workflow."""
        itername = 'top.'+self._parent.itername
        dgraph = self._derivative_graph
        
        try:
            meta = dgraph.node[node]
            
        # Array indexed parameter nodes are not in the graph, so add them.
        except KeyError:
            dgraph.add_subvar(node)
            meta = dgraph.node[node]
        
        if 'bounds' not in meta:
            meta['bounds'] = {}
            
        meta['bounds'][itername] = bounds
        
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
                
                if '~' in compname:
                    comp = self._derivative_graph.node[compname]['pa_object']
                else:
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
        
    def matvecREV(self, arg):
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
            
            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                inputs[varname] = arg[i1:i2].copy()
                outputs[varname] = arg[i1:i2].copy()*0
            
            for varname in comp_inputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                outputs[varname] = arg[i1:i2].copy()*0
                
            if '~' in compname:
                comp = self._derivative_graph.node[compname]['pa_object']
            else:
                comp = self.scope.get(compname)
            
            # Preconditioning
            #if hasattr(comp, 'applyMinvT'):
                #inputs = applyMinvT(comp, inputs)
            
            applyJT(comp, inputs, outputs)
            #print inputs, outputs
            
            for varname in comp_inputs+comp_outputs:
                node = '%s.%s' % (compname, varname)
                i1, i2 = self.get_bounds(node)
                result[i1:i2] += outputs[varname]
                
        # Each parameter adds an equation
        for src, target in self._edges.iteritems():
            if '@in' in src:
                if isinstance(target, list):
                    target = target[0]
                    
                i1, i2 = self.get_bounds(target)
                result[i1:i2] += arg[i1:i2]
                        
        #print arg, result
        return result
        
    def derivative_graph(self, inputs=None, outputs=None, fd=False):
        """Returns the local graph that we use for derivatives.
        """
        
        if self._derivative_graph is None:
        
            # If inputs aren't specified, use the parameters
            if inputs is None:
                if hasattr(self._parent, 'list_param_group_targets'):
                    inputs = self._parent.list_param_group_targets()
                else:
                    msg = "No inputs given for derivatives."
                    self.scope.raise_exception(msg, RuntimeError)
        
            # If outputs aren't specified, use the objectives and constraints
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
    
            graph = self.scope._depgraph
            
            # Inputs and outputs introduce subvars that aren't in the
            # parent graph, so they need to be added.
            for varnames in inputs+outputs:
                if isinstance(varnames, basestring):
                    varnames = [varnames]
                else:
                    varnames = list(varnames)
                for varname in varnames:
                    if varname not in self.scope._depgraph.node:
                        graph.add_subvar(varname)
            
            edges = get_inner_edges(graph, inputs, outputs)
            comps = edge_dict_to_comp_list(edges)
            
            dgraph = graph.full_subgraph(comps.keys())
            
            # We want our graph metadata to be stored in the copy, not in the
            # parent.
            dgraph.graph = {}
            
            dgraph.graph['inputs'] = inputs
            dgraph.graph['outputs'] = outputs
            if 'mapped_inputs' in dgraph.graph:
                dgraph.graph.pop('mapped_inputs', None)
                dgraph.graph.pop('mapped_outputs', None)
                
            self._derivative_graph = dgraph
            self._group_nondifferentiables(fd)
            
        return self._derivative_graph
    
    def _group_nondifferentiables(self, fd=False, comps = None):
        """Method to find all non-differentiable blocks. These blocks
        will be replaced in the differentiation workflow by a pseudo-
        assembly, which can provide its own Jacobian via finite difference.
        """
        
        dgraph = self._derivative_graph
        cgraph = dgraph.component_graph()

        nondiff = []
        comps = nx.topological_sort(cgraph)
        
        # Full model finite-difference, so all components go in the PA
        if fd == True:
            nondiff_groups = [comps]
            
        # Find the non-differentiable componentns
        else:
            for name in comps:
                comp = self.scope.get(name)
                if not hasattr(comp, 'apply_deriv') and \
                   not hasattr(comp, 'apply_derivT') and \
                   not hasattr(comp, 'provideJ'):
                    nondiff.append(comp.name)
                
            if len(nondiff) == 0:
                return
            
            # Groups any connected non-differentiable blocks. Each block is a set
            # of component names.
            nondiff_groups = []
            sub = cgraph.subgraph(nondiff)
            nd_graphs = nx.connected_component_subgraphs(sub.to_undirected())
            for item in nd_graphs:
                nondiff_groups.append(item.nodes())
                
        # for cyclic workflows, remove cut edges.
        #for edge in self._severed_edges:
        #    comp1, _, _ = edge[0].partition('.')
        #    comp2, _, _ = edge[1].partition('.')
        #    cgraph.remove_edge(comp1, comp2)
        
        dgraph.graph['mapped_inputs'] = copy.copy(dgraph.graph['inputs'])
        dgraph.graph['mapped_outputs'] = copy.copy(dgraph.graph['outputs'])
        meta_inputs = dgraph.graph['inputs']
        meta_outputs = dgraph.graph['outputs']
        map_inputs = dgraph.graph['mapped_inputs']
        map_outputs = dgraph.graph['mapped_outputs']
        
        for j, group in enumerate(nondiff_groups):
            pa_name = '~~%d' % j
            
            # First, find our group boundary
            allnodes = dgraph.find_prefixed_nodes(group)
            out_edges = nx.edge_boundary(dgraph, allnodes)
            in_edges = nx.edge_boundary(dgraph, 
                                        set(dgraph.nodes()).difference(allnodes))
            
            pa_inputs = [b for a, b in in_edges]
            pa_outputs = [a for a, b in out_edges]
            
            # Add requested params
            for i, varpath in enumerate(meta_inputs):
                if not isinstance(varpath, tuple):
                    varpath = [varpath]
                    
                mapped = []
                for path in varpath:
                    compname, _, varname = path.partition('.')
                    if varname and (compname in group):
                        pa_inputs.append(path)
                        mapped.append(to_PA_var(path, pa_name))
                
                if mapped:                      
                    map_inputs[i] = tuple(mapped)
                
            # Add requested outputs
            for i, varpath in enumerate(meta_outputs):
                compname, _, varname = varpath.partition('.')
                if varname and (compname in group):
                    pa_outputs.append(varpath)
                    map_outputs[i] = to_PA_var(varpath, pa_name)
                        
            # Create the pseudoassy
            pseudo = PseudoAssembly(pa_name, group, pa_inputs, pa_outputs, self)
            
            # for full-model fd, turn off fake finite difference
            if fd==True:
                pseudo.ffd_order = 0
            
            # Clean up the old stuff in the graph
            dgraph.remove_nodes_from(allnodes)
            
            # Add pseudoassys to graph
            dgraph.add_node(pa_name, pa_object=pseudo, comp=True, 
                            pseudo='assembly', valid=True)
            
            # Add pseudoassy inputs
            for varpath in pa_inputs:
                varname = to_PA_var(varpath, pa_name)
                dgraph.add_node(varname, var=True, iotype='in', valid=True)
                dgraph.add_edge(varname, pa_name)
                
            # Add pseudoassy outputs
            for varpath in pa_outputs:
                varname = to_PA_var(varpath, pa_name)
                dgraph.add_node(varname, var=True, iotype='out', valid=True)
                dgraph.add_edge(pa_name, varname)
            
            # Hook up the pseudoassemblies
            for src, dst in in_edges:
                dst = to_PA_var(dst, pa_name)
                dgraph.add_edge(src, dst, conn=True)
                
            for src, dst in out_edges:
                src = to_PA_var(src, pa_name)
                dgraph.add_edge(src, dst, conn=True)
            
            #print pseudo.name, pseudo.comps, pseudo.inputs, pseudo.outputs
        
        return None

    def edge_list(self):
        """ Return the list of edges for the derivatives of this workflow. """
        
        if self._edges == None:
            
            dgraph = self.derivative_graph()
            if 'mapped_inputs' in dgraph.graph:
                inputs = 'mapped_inputs'
                outputs = 'mapped_outputs'
            else:
                inputs = 'inputs'
                outputs = 'outputs'
                
            self._edges = get_inner_edges(dgraph, dgraph.graph[inputs],
                                          dgraph.graph[outputs])
            
        return self._edges
        
    def calc_derivatives(self, first=False, second=False, savebase=False,
                         required_inputs=None, required_outputs=None):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""

        self._stop = False
        
        comps = edge_dict_to_comp_list(self.edge_list())
        for compname, data in comps.iteritems():
            if '~' in compname:
                node = self._derivative_graph.node[compname]['pa_object']
            else:
                node = self.scope.get(compname)

            inputs = data['inputs']
            outputs = data['outputs']
            node.calc_derivatives(first, second, savebase, inputs, outputs)
            if self._stop:
                raise RunStopped('Stop requested')

    def calc_gradient(self, inputs=None, outputs=None, fd=False, 
                      upscope=False, mode='auto'):
        """Returns the gradient of the passed outputs with respect to
        all passed inputs.
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
        
        dgraph = self.derivative_graph(inputs, outputs, fd=fd)
        
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
            if isinstance(item, tuple):
                item = item[0]
                
            i1, i2 = self.get_bounds(item)
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
        if mode == 'auto' and fd is False:
            # TODO - additional determination based on presence of
            # apply_derivT
            
            if num_in > num_out:
                mode = 'adjoint'
            else:
                mode = 'forward'
            
        if mode == 'adjoint':
            return calc_gradient_adjoint(self, inputs, outputs, n_edge, shape)
        else:
            return calc_gradient(self, inputs, outputs, n_edge, shape)
    
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

        dgraph = self.derivative_graph()
        input_refs = dgraph.graph['inputs']
        output_refs = dgraph.graph['outputs']

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

