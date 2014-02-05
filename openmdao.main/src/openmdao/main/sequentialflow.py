""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

import networkx as nx
import sys
from math import isnan
from StringIO import StringIO

from openmdao.main.array_helpers import flattened_size, \
                                        flattened_names, flatten_slice
from openmdao.main.derivatives import calc_gradient, calc_gradient_adjoint, \
                                      applyJ, applyJT, applyMinvT, applyMinv

from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudoassembly import PseudoAssembly, to_PA_var, from_PA_var
from openmdao.main.vartree import VariableTree

from openmdao.main.workflow import Workflow
from openmdao.main.depgraph import find_related_pseudos, \
                                    mod_for_derivs, \
                                    is_subvar_node, is_boundary_node
from openmdao.main.interfaces import IDriver, IImplicitComponent, ISolver
from openmdao.main.mp_support import has_interface
from openmdao.util.graph import edges_to_dict

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
        self._comp_edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        self._J_cache = {}
        self._bounds_cache = {}
        self._shape_cache = {}

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
        self._comp_edges = None
        self._derivative_graph = None
        self.res = None
        self._upscoped = False
        self._names = None
        self._J_cache = {}
        self._bounds_cache = {}
        self._shape_cache = {}

    def sever_edges(self, edges):
        """Temporarily remove the specified edges but save
        them and their metadata for later restoration.
        """
        if edges:
            params = self._parent.get_parameters()
            non_param_edges = [(src, targ) for (src, targ) in edges
                                           if targ not in params]
            self.scope._depgraph.sever_edges(non_param_edges)

    def unsever_edges(self):
        self.scope._depgraph.unsever_edges(self._parent.get_expr_scope())

    def get_names(self, full=False):
        """Return a list of component names in this workflow.
        If full is True, include hidden pseudo-components in the list.
        """
        if self._names is None:
            comps = [getattr(self.scope, n) for n in self._explicit_names]
            drivers = [c for c in comps if has_interface(c, IDriver)]
            self._names = self._explicit_names[:]

            if len(drivers) == len(comps): # all comps are drivers
                iterset = set()
                for driver in drivers:
                    iterset.update([c.name for c in driver.iteration_set()])
                added = set([n for n in self._parent._get_required_compnames()
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

        # workflow deriv graph, etc. must be recalculated
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
        sortedkeys.extend(sorted(edges.keys()))

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
                    inp = inputs[idx]
                    if not isinstance(inp, basestring):
                        inp = inp[0]
                    if inp in dgraph:
                        measure_src = inp
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
                   is_boundary_node(dgraph, dgraph.base_var(measure_src)):
                    bound = (nEdge, nEdge+width)
                    self.set_bounds(measure_src, bound)

                src_noidx = src.split('[', 1)[0]

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
        bounds = self._bounds_cache.get(node)
        if bounds is None:
            dgraph = self._derivative_graph
            i1, i2 = dgraph.node[node]['bounds'][self._parent.name]

            # Handle index slices
            if isinstance(i1, str):
                if ':' in i1:
                    i3 = i2 + 1
                else:
                    i2 = i2.tolist()
                    i3 = 0
                bounds = (i2, i3)
            else:
                bounds = (i1, i2)

            self._bounds_cache[node] = bounds

        return bounds

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
            self._explicit_names = [n for n in src._explicit_names
                                            if hasattr(par, n)]
        else:
            self._explicit_names = src._explicit_names[:]

    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''

        comps = self._comp_edge_list()

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
            out_bounds = []

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
                out_bounds.append((varname, i1, i2))

                if isinstance(i1, list):
                    if varname in comp_residuals:
                        outputs[varname] = zeros((1, 1))
                    else:
                        inputs[varname] = arg[i1].copy()
                        outputs[varname] = arg[i1].copy()
                else:
                    if varname in comp_residuals:
                        outputs[varname] = zeros((i2-i1))
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

            applyJ(comp, inputs, outputs, comp_residuals,
                   self._shape_cache.get(compname), self._J_cache.get(compname))
            #print inputs, outputs

            for varname, i1, i2 in out_bounds:
                if isinstance(i1, list):
                    result[i1] = outputs[varname].copy()
                else:
                    result[i1:i2] = outputs[varname].copy()

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
        comps = self._comp_edge_list()
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
            out_bounds = []

            for varname in comp_outputs:
                node = '%s.%s' % (compname, varname)

                # Ouputs define unique edges, so don't duplicate anything
                if is_subvar_node(dgraph, node):
                    if dgraph.base_var(node).split('.', 1)[1] in comp_outputs:
                        continue

                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    inputs[varname] = arg[i1].copy()
                    if varname not in comp_residuals:
                        outputs[varname] = zeros(len(i1))
                        out_bounds.append((varname, i1, i2))
                else:
                    inputs[varname] = arg[i1:i2].copy()
                    if varname not in comp_residuals:
                        outputs[varname] = zeros(i2-i1)
                        out_bounds.append((varname, i1, i2))

            for varname in comp_inputs:
                node = '%s.%s' % (compname, varname)

                i1, i2 = self.get_bounds(node)
                if isinstance(i1, list):
                    outputs[varname] = zeros(len(i1))
                else:
                    outputs[varname] = zeros(i2-i1)
                out_bounds.append((varname, i1, i2))

            if '~' in compname:
                comp = self._derivative_graph.node[compname]['pa_object']
            else:
                comp = self.scope.get(compname)

            # Preconditioning
            if hasattr(comp, 'applyMinvT'):
                inputs = applyMinvT(comp, inputs, self._shape_cache)

            applyJT(comp, inputs, outputs, comp_residuals,
                    self._shape_cache, self._J_cache.get(compname))
            #print inputs, outputs

            for varname, i1, i2 in out_bounds:
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
            # when we call with group_nondif = False, we want the union of the
            # passed inputs/outputs plus the inputs/outputs from the solver
            if group_nondif is False:
                tmp_inputs = [] if inputs is None else inputs
                tmp_outputs = [] if outputs is None else outputs
                inputs = None
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
                    outputs.extend(["%s.out0" % item.pcomp_name for item in
                                    self._parent.get_objectives().values()])
                if hasattr(self._parent, 'get_constraints'):
                    outputs.extend(["%s.out0" % item.pcomp_name for item in
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
            mod_for_derivs(dgraph, inputs, outputs, self, fd)

            if group_nondif:
                self._derivative_graph = dgraph
                self._group_nondifferentiables(fd, severed)
            else:
                # we're being called to determine the deriv graph
                # for a subsolver, so get rid of @in and @out nodes
                dgraph.remove_nodes_from(['@in%d' % i for i in range(len(inputs))])
                dgraph.remove_nodes_from(['@out%d' % i for i in range(len(outputs))])
                dgraph.graph['inputs'] = inputs[:]
                dgraph.graph['outputs'] = outputs[:]
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

        # If we have a cyclic workflow, we need to remove severed edges from
        # the derivatives graph.
        if severed is not None:
            for edge in severed:
                if edge in dgraph.edges():
                    dgraph.remove_edge(edge[0], edge[1])

        cgraph = dgraph.component_graph()
        comps = cgraph.nodes()
        pas = [dgraph.node[n]['pa_object']
               for n in dgraph.nodes_iter()
                     if n.startswith('~') and '.' not in n]
        pa_excludes = set()
        for pa in pas:
            pa_excludes.update(pa._removed_comps)

        # Full model finite-difference, so all components go in the PA
        if fd == True:
            nondiff_groups = [comps]

        # Find the non-differentiable components
        else:

            # A component with no derivatives is non-differentiable
            nondiff = set()
            nondiff_groups = []

            for name in comps:
                if name.startswith('~') or name in pa_excludes:
                    continue  # don't want nested pseudoassemblies
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

                if src.startswith('~') or target.startswith('~'):
                    continue

                # Default differentiable connections
                val = self.scope.get(src)
                if isinstance(val, (float, ndarray, VariableTree)):
                    continue

                # Custom differentiable connections
                if self.scope.get_metadata(src).get('data_shape'):
                    continue

                #Nothing else is differentiable
                else:
                    nondiff.add(src.split('.')[0])
                    nondiff.add(target.split('.')[0])

            # Everything is differentiable, so return
            if len(nondiff) == 0:
                return

            # Groups any connected non-differentiable blocks. Each block is a
            # set of component names.
            sub = cgraph.subgraph(nondiff)
            nd_graphs = nx.connected_component_subgraphs(sub.to_undirected())
            for item in nd_graphs:
                inodes = item.nodes()
                nondiff_groups.append(inodes)

        for j, group in enumerate(nondiff_groups):
            pa_name = '~%d' % j

            # Create the pseudoassy
            pseudo = PseudoAssembly(pa_name, group,
                                    dgraph, self, fd)

            pseudo.add_to_graph(self.scope._depgraph, dgraph)
            pseudo.clean_graph(self.scope._depgraph, dgraph)

    def edge_list(self):
        """ Return the list of edges for the derivatives of this workflow. """

        if self._edges is None:
            self._edges = edges_to_dict(self.derivative_graph().list_connections())

        return self._edges

    def _comp_edge_list(self):
        """ Caches the result of edge_dict_to_comp_list. """
        if self._comp_edges is None:
            edge2comp = self._derivative_graph.edge_dict_to_comp_list
            self._comp_edges = edge2comp(self._edges, self.get_implicit_info())
        return self._comp_edges

    def get_implicit_info(self):
        """ Return a dict of the form {(residuals) : [states]}
        """
        info = {}

        dgraph = self.derivative_graph()
        comps = dgraph.component_graph().nodes()

        # Full model finite difference = no implcit edges
        if len(comps) == 1 and '~' in comps[0]:
            return info

        # Residuals and states for implicit components
        for cname in comps:

            if cname.startswith('~'):
                continue

            comp = getattr(self.scope, cname)

            if has_interface(comp, IImplicitComponent):
                if not comp.eval_only:
                    key = tuple(['.'.join([cname, n])
                                     for n in comp.list_residuals()])
                    info[key] = ['.'.join([cname, n])
                                     for n in comp.list_states()]

        # Nested solvers act implicitly.
        pa_comps = [dgraph.node[item]['pa_object']
                    for item in comps if '~' in item]
        for comp in self._parent.iteration_set(solver_only=True):
            if has_interface(comp, ISolver):

                key = tuple(comp.list_eq_constraint_targets())

                # For cyclic workflows in a solver, the edge is already there.
                if len(key) == 0:
                    continue

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
        in this workflow.
        """

        self._stop = False

        dgraph = self.derivative_graph(required_inputs, required_outputs)
        comps = dgraph.edge_dict_to_comp_list(edges_to_dict(dgraph.list_connections()))
        for compname, data in comps.iteritems():
            if '~' in compname:
                comp = self._derivative_graph.node[compname]['pa_object']
            elif compname.startswith('@'):
                continue
            else:
                comp = self.scope.get(compname)

            J = self._J_cache.get(compname)
            if compname not in self._shape_cache:
                self._shape_cache[compname] = {}
            if J is None:
                J = comp.calc_derivatives(first, second, savebase,
                                          data['inputs'], data['outputs'])
                if J is not None:
                    self._J_cache[compname] = J

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

        self._J_cache = {}

        # User may request full-model finite difference.
        if self._parent.gradient_options.force_fd == True:
            mode = 'fd'

        # This function can be called from a parent driver's workflow for
        # assembly recursion. We have to clear our cache if that happens.
        # We also have to clear it next time we arrive back in our workflow.
        if upscope or self._upscoped:
            self._derivative_graph = None
            self._edges = None
            self._comp_edges = None

            self._upscoped = upscope

        dgraph = self.derivative_graph(inputs, outputs, fd=(mode == 'fd'))

        if 'mapped_inputs' in dgraph.graph:
            inputs = dgraph.graph['mapped_inputs']
            outputs = dgraph.graph['mapped_outputs']
        else:
            inputs = dgraph.graph['inputs']
            outputs = dgraph.graph['outputs']

        n_edge = self.initialize_residual()

        # cache Jacobians for comps that return them from provideJ


        # Size our Jacobian
        num_in = 0
        for item in inputs:

            # For parameter groups, only size the first
            if not isinstance(item, basestring):
                item = item[0]

            try:
                i1, i2 = self.get_bounds(item)
                if isinstance(i1, list):
                    num_in += len(i1)
                else:
                    num_in += i2-i1
            except KeyError:
                val = self.scope.get(item)
                num_in += flattened_size(item, val, self.scope)

        num_out = 0
        for item in outputs:
            try:
                i1, i2 = self.get_bounds(item)
                if isinstance(i1, list):
                    num_out += len(i1)
                else:
                    num_out += i2-i1
            except KeyError:
                val = self.scope.get(item)
                num_out += flattened_size(item, val, self.scope)

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

            try:
                i1, i2 = self.get_bounds(name)
            except KeyError:
                continue

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


    def check_gradient(self, inputs=None, outputs=None, stream=sys.stdout, mode='auto'):
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
            that is used as a filename. If None, no output is written.

        mode: (optional) str
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode,
            or 'auto' to let OpenMDAO determine the correct mode.
            Defaults to 'auto'.

        Returns the finite difference gradient, the OpenMDAO-calculated
        gradient, and a list of suspect inputs/outputs.
        """
        # tuples cause problems
        if inputs:
            inputs = list(inputs)
        if outputs:
            outputs = list(outputs)

        if isinstance(stream, basestring):
            stream = open(stream, 'w')
            close_stream = True
        else:
            close_stream = False
            if stream is None:
                stream = StringIO()

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

        # This code duplication is needed so that we print readable names for
        # the constraints and objectives.

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
                obj = ["%s.out0" % item.pcomp_name for item in
                       self._parent.get_objectives().values()]
                outputs.extend(obj)
                output_refs.extend(self._parent.get_objectives().keys())
            if hasattr(self._parent, 'get_constraints'):
                con = ["%s.out0" % item.pcomp_name for item in
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

        io_pairs = []

        for output, oref in zip(outputs, output_refs):
            out_val = self.scope.get(output)
            for out_name in flattened_names(oref, out_val):
                i += 1
                j = -1
                for input_tup, iref in zip(inputs, input_refs):
                    if isinstance(input_tup, basestring):
                        input_tup = (input_tup,)

                    inp_val = self.scope.get(input_tup[0])
                    for inp_name in flattened_names(iref, inp_val):
                        j += 1
                        calc = J[i, j]
                        finite = Jbase[i, j]
                        if finite and calc:
                            error = (calc - finite) / finite
                        else:
                            error = calc - finite
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
                        io_pairs.append("%*s / %*s"
                                        % (out_width, out_name,
                                           inp_width, inp_name))
        print >> stream
        if error_n:
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

        # return arrays and suspects to make it easier to check from a test
        return Jbase.flatten(), J.flatten(), io_pairs, suspects

