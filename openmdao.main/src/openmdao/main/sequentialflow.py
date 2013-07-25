""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

import networkx as nx
import sys

from openmdao.main.derivatives import flattened_size, flattened_value, \
                                      flattened_names, \
                                      calc_gradient, calc_gradient_adjoint, \
                                      applyJ, applyJT
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudoassembly import PseudoAssembly
from openmdao.main.vartree import VariableTree
from openmdao.main.workflow import Workflow
from openmdao.main.depgraph import find_pseudos

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
        self._names = []
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
        # Bookkeeping for calculating the residual.
        self._severed_edges = []
        self._additional_edges = []
        self._hidden_edges = set()
        self.res = None
        self.bounds = None
        
        self.derivative_iterset = None
        self._collapsed_graph = None
        self._topsort = None
        self._find_nondiff_blocks = True
        self._input_outputs = []

        # keep names of pseudocomponents that we need to add
        # to our local dependency graph, e.g., pseudocomps for
        # parameters
        self._pseudocomps = []
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components(full=True))

    def __len__(self):
        return len(self._names)

    def __contains__(self, comp):
        return comp in self._names

    def index(self, comp):
        """Return index number for a component in this workflow."""
        return self._names.index(comp)

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names

    def __ne__(self, other):
        return not self.__eq__(other)

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(SequentialWorkflow, self).config_changed()
        self._find_nondiff_blocks = True
        self.derivative_iterset = None
        self._collapsed_graph = None
        self._topsort = None
        self._find_nondiff_blocks = True
        self._input_outputs = []

    def add_pseudocomp(self, pcomp):
        self._pseudocomps.append(pcomp.name)
        self.config_changed()

    def remove_pseudocomp(self, pcomp):
        self._pseudocomps.remove(pcomp.name)
        self.config_changed()
        
    def get_names(self, full=False):
        """Return a list of component names in this workflow.  If full is True,
        include hidden pseudo-components in the list.
        """
        if full:
            return self._names + \
                   list(find_pseudos(self.scope._depgraph._graph, 
                                     self._names)) + \
                   self._pseudocomps
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
                    self._names.append(node)
                else:
                    self._names.insert(index, node)
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
            self._names.remove(compname)
            self.config_changed()
        except ValueError:
            pass

    def clear(self):
        """Remove all components from this workflow."""
        self._names = []
        self.config_changed()

    def get_interior_edges(self):
        """ Returns an alphabetical list of all output edges that are
        interior to the set of components supplied. When used for derivative
        calculation, the parameter inputs and response outputs are also
        included. If there are non-differentiable blocks grouped in
        pseudo-assemblies, then those interior edges are excluded.
        """
        
        graph = self.scope._depgraph
        edges = graph.get_interior_edges(self.get_names(full=True))
        edges = edges.union(self._additional_edges)
        edges = edges - self._hidden_edges
                
        # Somtimes we connect an input to an input (particularly with
        # constraints). These need to be rehooked to corresponding source
        # edges.
        
        self._input_outputs = []
        for src, target in edges:
            if src == '@in' or target == '@out' or '_pseudo_' in src:
                continue
            compname, _, var = src.partition('.')
            var = var.split('[')[0]
            comp = self.scope.get(compname)
            if var in comp.list_inputs():
                self._input_outputs.append(src)
                
        return sorted(list(edges))

    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        nEdge = 0
        self.bounds = {}
        for edge in self.get_interior_edges():
            if edge[0] == '@in':
                src = edge[1]
            else:
                src = edge[0]
            val = self.scope.get(src)
            width = flattened_size(src, val)
            self.bounds[edge] = (nEdge, nEdge+width)
            nEdge += width

        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))

        return nEdge

    def calculate_residuals(self):
        """Calculate and return the vector of residuals based on the current
        state of the system in our workflow."""
        for edge in self.get_interior_edges():
            src, target = edge
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val).reshape(-1, 1)
            target_val = self.scope.get(target)
            target_val = flattened_value(target, target_val).reshape(-1, 1)
            i1, i2 = self.bounds[edge]
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
            i1, i2 = self.bounds[edge]
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
            comp_name, dot, var_name = target.partition('.')
            comp = self.scope.get(comp_name)
            comp._valid_dict[var_name] = True

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

        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}

        # Start with zero-valued dictionaries cotaining keys for all inputs
        pa_ref = {}
        for comp in self.derivative_iter():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
            # Interior Edges use original names, so we need to know
            # what comps are in a pseudo-assy.
            if '~' in name:
                for item in comp.inputs + comp.outputs:
                    key = item.partition('.')[0]
                    pa_ref[key] = name

        # Fill input dictionaries with values from input arg.
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
            if src != '@in' and src not in self._input_outputs:
                comp_name, dot, var_name = src.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                outputs[comp_name][var_name] = arg[i1:i2].copy()
                inputs[comp_name][var_name] = arg[i1:i2]

            if target != '@out':
                comp_name, dot, var_name = target.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                inputs[comp_name][var_name] = arg[i1:i2]

        # Call ApplyMinv on each component (preconditioner)
        for comp in self.derivative_iter():
            name = comp.name
            if hasattr(comp, 'applyMinv'):
                pre_inputs = inputs[name].copy()
                comp.applyMinv(pre_inputs, inputs[name])
            
        # Call ApplyJ on each component
        for comp in self.derivative_iter():
            name = comp.name
            applyJ(comp, inputs[name], outputs[name])

        # Each parameter adds an equation
        for edge in self._additional_edges:
            if edge[0] == '@in':
                i1, i2 = self.bounds[edge]
                comp_name, dot, var_name = edge[1].partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                outputs[comp_name][var_name] = arg[i1:i2]

        # Poke results into the return vector
        result = zeros(len(arg))
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
            if src == '@in':
                src = target
                
            # Input-input connections are not in the jacobians. We need
            # to add the derivative (which is 1.0).
            elif src in self._input_outputs:
                comp_name, dot, var_name = src.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                result[i1:i2] = outputs[comp_name][var_name] + arg[i1:i2]
                continue
                
            comp_name, dot, var_name = src.partition('.')
            if comp_name in pa_ref:
                var_name = '%s.%s' % (comp_name, var_name)
                comp_name = pa_ref[comp_name]
            result[i1:i2] = outputs[comp_name][var_name]
        
        return result
    
    def matvecREV(self, arg):
        '''Callback function for performing the transpose matrix vector
        product of the workflow's full Jacobian with an incoming vector
        arg.'''

        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}

        # Start with zero-valued dictionaries cotaining keys for all inputs
        pa_ref = {}
        for comp in self.derivative_iter():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
            # Interior Edges use original names, so we need to know
            # what comps are in a pseudo-assy.
            if '~' in name:
                for item in comp.inputs + comp.outputs:
                    key = item.partition('.')[0]
                    pa_ref[key] = name

        # Fill input dictionaries with values from input arg.
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
            if src != '@in' and src not in self._input_outputs:
                comp_name, dot, var_name = src.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                inputs[comp_name][var_name] = arg[i1:i2]
                outputs[comp_name][var_name] = arg[i1:i2].copy()

            if target != '@out':
                comp_name, dot, var_name = target.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                if edge[0] == '@in':
                    # Extra eqs for parameters contribute a 1.0 on diag
                    outputs[comp_name][var_name] = arg[i1:i2].copy()
                else:
                    # Interior comp edges contribute a -1.0 on diag
                    outputs[comp_name][var_name] = -arg[i1:i2].copy()

        # Call ApplyMinvT on each component (preconditioner)
        for comp in self.derivative_iter():
            name = comp.name
            if hasattr(comp, 'applyMinvT'):
                pre_inputs = inputs[name].copy()
                comp.applyMinvT(pre_inputs, inputs[name])
            
        # Call ApplyJT on each component
        for comp in self.derivative_iter():
            name = comp.name
            applyJT(comp, inputs[name], outputs[name])

        # Poke results into the return vector
        result = zeros(len(arg))
        
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
            if target == '@out':
                target = src
                
            # Input-input connections are not in the jacobians. We need
            # to add the derivative (which is 1.0).
            elif target in self._input_outputs:
                comp_name, dot, var_name = target.partition('.')
                if comp_name in pa_ref:
                    var_name = '%s.%s' % (comp_name, var_name)
                    comp_name = pa_ref[comp_name]
                result[i1:i2] = outputs[comp_name][var_name] + arg[i1:i2]
                continue
                
            comp_name, dot, var_name = target.partition('.')
            if comp_name in pa_ref:
                var_name = '%s.%s' % (comp_name, var_name)
                comp_name = pa_ref[comp_name]
            result[i1:i2] = result[i1:i2] + outputs[comp_name][var_name]
        
        return result
    
    def group_nondifferentiables(self):
        """Method to find all non-differentiable blocks. These blocks
        will be replaced in the differentiation workflow by a pseudo-
        assembly, which can provide its own Jacobian via finite difference.
        """
        
        nondiff = []
        for comp in self.get_components(full=True):
            if not hasattr(comp, 'apply_deriv') and \
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
            for edge in graph.edges():
                
                dgraph = self.scope._depgraph
                
                if edge[0] in group and edge[1] in group:
                    graph.remove_edge(edge[0], edge[1])
                    var_edge = dgraph.get_interior_edges(edge)
                    self._hidden_edges = self._hidden_edges.union(var_edge)
                elif edge[0] in group:
                    graph.remove_edge(edge[0], edge[1])
                    graph.add_edge(pa_name, edge[1])
                    var_edge = dgraph.get_directional_interior_edges(edge[0], edge[1])
                    outputs = outputs.union(var_edge)
                elif edge[1] in group:
                    graph.remove_edge(edge[0], edge[1])
                    graph.add_edge(edge[0], pa_name)
                    var_edge = dgraph.get_directional_interior_edges(edge[0], edge[1])
                    inputs = inputs.union(var_edge)
                    
            # Input and outputs that crossed the cut line should be included
            # for the pseudo-assembly.
            for edge in self._severed_edges:
                comp1, _, _ = edge[0].partition('.')
                comp2, _, _ = edge[1].partition('.')
                
                if comp1 in group:
                    outputs = outputs.union([edge])
                if comp2 in group:
                    inputs = inputs.union([edge])
                    
                if edge in self._hidden_edges:
                    self._hidden_edges.remove(edge)
            
            # Remove old nodes
            for node in group:
                graph.remove_node(node)

            # You don't need the whole edge.
            inputs  = [b for a, b in inputs]
            outputs = [a for a, b in outputs]
                
            # Boundary edges must be added to inputs and outputs
            for edge in list(self._additional_edges):
                src, target = edge
                
                comp_name, dot, var_name = src.partition('.')
                if comp_name in group:
                    outputs.append(src)
                    
                comp_name, dot, var_name = target.partition('.')
                if comp_name in group:
                    inputs.append(target)
                
            # Input to input connections lead to extra outputs.
            for item in self._input_outputs:
                if item in outputs:
                    outputs.remove(item)

            # Create pseudo_assy
            comps = [getattr(self.scope, name) for name in group]
            pseudo_assemblies[pa_name] = PseudoAssembly(pa_name, comps, 
                                                        inputs, outputs, 
                                                        self)
                
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
            if hasattr(self._parent, 'get_parameters'):
                inputs = self._parent.get_parameters().keys()
            else:
                msg = "No inputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
            
        if outputs is None:
            outputs = []
            if hasattr(self._parent, 'get_objectives'):
                outputs.extend(self._parent.get_objectives().keys())
            if hasattr(self._parent, 'get_ineq_constraints'):
                outputs.extend(self._parent.get_ineq_constraints().keys())
            if hasattr(self._parent, 'get_eq_constraints'):
                outputs.extend(self._parent.get_eq_constraints().keys())
                
            if len(outputs) == 0:
                msg = "No outputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)

        # Override to do straight finite-difference of the whole model, with
        # no fake fd.
        if fd is True:
            
            # Finite difference the whole thing by putting the whole workflow in a
            # pseudo-assembly. This requires being a little creative.
            comps = [comp for comp in self]
            pseudo = PseudoAssembly('~Check_Gradient', comps, inputs, outputs, self)
            pseudo.ffd_order = 0
            graph = self.scope._depgraph
            self._hidden_edges = graph.get_interior_edges(self.get_names(full=True))
            self.derivative_iterset = [pseudo]

            # Allow for the rare case the user runs this manually, first, in
            # which case the in/out edges haven't been defined yet.
            if len(self._additional_edges) == 0:
                
                # New edges for parameters
                input_edges = [('@in', a) for a in inputs]
                additional_edges = set(input_edges)
                
                # New edges for responses
                out_edges = [a[0] for a in self.get_interior_edges()]
                for item in outputs:
                    if item not in out_edges:
                        additional_edges.add((item, '@out'))
                
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
            
            # New edges for parameters
            input_edges = [('@in', a) for a in inputs]
            additional_edges = set(input_edges)
            
            # New edges for responses
            out_edges = [a[0] for a in self.get_interior_edges()]
            for item in outputs:
                if item not in out_edges:
                    additional_edges.add((item, '@out'))
            
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
        if mode == 'auto':
            # TODO - determine based on size and presence of apply_derT
            mode = 'forward'
            
        if mode == 'adjoint':
            return calc_gradient_adjoint(self, inputs, outputs)
        else:
            return calc_gradient(self, inputs, outputs)
    
    def check_gradient(self, inputs=None, outputs=None, stream=None):
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
    
        J = self.calc_gradient(inputs, outputs)
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
                inputs = self._parent.get_parameters().keys()
            # Should be caught in calc_gradient()
            else:  # pragma no cover
                msg = "No inputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)
            
        if outputs is None:
            outputs = []
            if hasattr(self._parent, 'get_objectives'):
                outputs.extend(self._parent.get_objectives().keys())
            if hasattr(self._parent, 'get_ineq_constraints'):
                outputs.extend(self._parent.get_ineq_constraints().keys())
            if hasattr(self._parent, 'get_eq_constraints'):
                outputs.extend(self._parent.get_eq_constraints().keys())
            # Should be caught in calc_gradient()
            if len(outputs) == 0:  # pragma no cover
                msg = "No outputs given for derivatives."
                self.scope.raise_exception(msg, RuntimeError)

        out_width = 0
        for output in outputs:
            out_val = self.scope.get(output)
            out_names = flattened_names(output, out_val)
            out_width = max(out_width, max([len(out) for out in out_names]))

        inp_width = 0
        for input in inputs:
            inp_val = self.scope.get(input)
            inp_names = flattened_names(input, inp_val)
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
        for output in outputs:
            out_val = self.scope.get(output)
            for out_name in flattened_names(output, out_val):
                i += 1
                j = -1
                for input in inputs:
                    inp_val = self.scope.get(input)
                    for inp_name in flattened_names(input, inp_val):
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

