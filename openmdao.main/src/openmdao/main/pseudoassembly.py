'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

import networkx as nx

from openmdao.main.finite_difference import FiniteDifference, DirectionalFD
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IAssembly
from openmdao.util.graph import flatten_list_of_iters, edges_to_dict


def to_PA_var(name, pa_name):
    ''' Converts an input to a unique input name on a pseudoassembly.'''
    assert not name.startswith('~')
    return pa_name + '.' + name.replace('.', '|')


def from_PA_var(name):
    ''' Converts a pseudoassembly input name back to the real input.'''

    if '~' in name:
        comp, _, xname = name.partition('.')
        if xname:
            name = xname.replace('|', '.')
        else:
            name = comp.lstrip('~')

    return name


class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""

    def __init__(self, name, comps, graph, wflow, fd=False,
                 drv_name=None, boundary_params=()):
        """Initialized with list of components, and the parent workflow."""

        inputs, outputs, renames = self._pre_init(name, comps, graph, fd,
                                                  boundary_params)

        self.comps = wflow.scope._depgraph.order_components(set(comps))

        self.name = name
        self.boundary_params = list(boundary_params)
        self.wflow = wflow
        self.inputs = list(inputs)
        self.outputs = list(outputs)
        self.renames = renames
        self.mapped_inputs = []
        self._removed_comps = []
        self._provideJ_bounds = None
        for varpath in self.inputs:
            if isinstance(varpath, basestring):
                val = to_PA_var(varpath, name).partition('.')[2]
            else:
                val = tuple([to_PA_var(vp, name).partition('.')[2]
                             for vp in varpath])
            self.mapped_inputs.append(val)

        self.mapped_outputs = [to_PA_var(varpath, name).partition('.')[2]
                               for varpath in self.outputs]
        self.itername = ''

        self.fd = None
        self.J = None
        self.ffd_cache = {}

        if fd:  # for full-model fd, turn off fake finite difference
            self.ffd_order = 0
        else:  # use fake finite difference on comps having derivatives
            # TODO: Fake Finite Difference has been disabled until we upgrade
            # it to support arrays and vartrees.
            self.ffd_order = 0

        if fd:

            # Support for taking the deriative of a subset of our
            # workflow. This breaks down if we have subdrivers,
            # so we have to revert back to full workflow if any
            # are relevant.
            itercomps = set([c.name for c in wflow])
            comps = set(self.comps)
            if comps.issubset(itercomps):
                self.itercomps = self.comps
            else:
                self.itercomps = [c.name for c in wflow]

        elif drv_name is not None:
            self.itercomps = [drv_name]
        else:
            self.itercomps = list(self.comps)

        #print "Created PseudoAssembly:", self.comps

    def _pre_init(self, pa_name, group, dgraph, fd, boundary_params):
        """Return a tuple of the form (pa_inputs, pa_outputs, renames)
        for the PseudoAssembly that would be created given the nodes in
        group and the given graph.
        """

        # First, find our group boundary
        self._orig_group_nodes = list(group) + list(boundary_params)
        allnodes = dgraph.find_prefixed_nodes(self._orig_group_nodes)
        out_edges = nx.edge_boundary(dgraph, allnodes)
        in_edges = nx.edge_boundary(dgraph,
                            set(dgraph.nodes()).difference(allnodes))
        solver_states = []
        if fd is False:
            for comp in group:

                # Keep any node marked 'solver_state'. Note, only inputs can
                # be solver_states.
                solver_states.extend([node for node in dgraph.find_prefixed_nodes([comp])
                                      if 'solver_state' in dgraph.node[node]])

        pa_inputs = edges_to_dict(in_edges).values()
        pa_inputs.extend(solver_states)
        pa_outputs = set([a for a, b in out_edges])

        renames = {}

        # Add pseudoassy inputs
        for varpath in list(flatten_list_of_iters(pa_inputs)) + \
                       list(pa_outputs):
            varname = to_PA_var(varpath, pa_name)
            if varpath in dgraph:
                renames[varpath] = varname
                old = dgraph.base_var(varpath)
                if old != varpath:
                    renames[old] = to_PA_var(old, pa_name)

        # make boundary params outputs of the PA
        pa_outputs.update(boundary_params)

        return pa_inputs, pa_outputs, renames

    def set_itername(self, name):
        """Comp API compatibility; allows iteration coord to be set in
        components."""
        self.itername = name

    def run(self, ffd_order=0):
        """Run all components contained in this assy. Used by finite
        difference."""

        # Override fake finite difference if requested. This enables a pure
        # finite-differences for check_derivatives.
        if self.ffd_order == 0:
            ffd_order = 0

        for name in self.itercomps:
            comp = self.wflow.scope.get(name)
            comp.set_itername(self.itername+'-fd')
            comp.run(ffd_order=ffd_order)

    def calc_derivatives(self, first=False, second=False, savebase=True,
                         required_inputs=None, required_outputs=None):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""

        # Directional finite difference doesn't pre-generate a Jacobian
        if self.wflow._parent.gradient_options.directional_fd == True:
            if self.fd is None:
                self.fd = DirectionalFD(self)
                self.apply_deriv = self._apply_deriv
            return None

        # We don't do this in __init__ because some inputs and outputs
        # are added after creation (for nested driver support).
        if self.fd is None:
            self.fd = FiniteDifference(self)

        if hasattr(self.wflow, '_severed_edges'):
            self.wflow.sever_edges(self.wflow._severed_edges)

        try:
            # First, linearize about operating point.
            # Note: Only needed for differentiable islands, which are handled
            # with Fake Finite Difference.
            # Don't do this for full-model finite difference.
            if first and self.ffd_order > 0:
                for name in self.comps:

                    # TODO: I think the cache is blown away each time before
                    # this is called
                    if name in self.wflow._J_cache:
                        continue

                    comp = self.wflow.scope.get(name)

                    # Assemblies need some required inputs and outputs
                    # to calculate the Jacobians
                    if has_interface(comp, IAssembly):

                        # Need to know which assy bdry variables are
                        # required, and pass them in. Cache this once.
                        if name not in self.ffd_cache:
                            dgraph = self.wflow.scope._depgraph
                            inputs = [dgraph.base_var(inp)
                                       for inp in flatten_list_of_iters(self.inputs)]
                            outputs = [dgraph.base_var(outp)
                                       for outp in self.outputs]
                            from openmdao.main.depgraph import _get_inner_edges
                            edges = _get_inner_edges(dgraph, inputs, outputs)

                            req_inputs = []
                            req_outputs = []
                            for inp in inputs:
                                comp_str, _, var_str = inp.partition('.')
                                if comp_str == name:
                                    req_inputs.append(var_str)

                            for inp in outputs:
                                comp_str, _, var_str = inp.partition('.')
                                if comp_str == name:
                                    req_outputs.append(var_str)

                            for edge in edges:
                                src, target = edge
                                comp_str, _, var_str = src.partition('.')
                                if comp_str == name:
                                    req_outputs.append(var_str)
                                comp_str, _, var_str = target.partition('.')
                                if comp_str == name:
                                    req_inputs.append(var_str)

                            self.ffd_cache[name] = (req_inputs, req_outputs)

                        req_inputs, req_outputs = self.ffd_cache[name]

                        comp.calc_derivatives(first, second, savebase=True,
                                              required_inputs=req_inputs,
                                              required_outputs=req_outputs)

                    # Comp list contains full graph, so don't double up on
                    # the subdrivers.
                    elif not has_interface(comp, IDriver):
                        comp.calc_derivatives(first, second, True)

            self.J = self.fd.calculate()
        finally:
            if hasattr(self.wflow, '_severed_edges'):
                self.wflow.unsever_edges()

        return self.J

    def provideJ(self):
        """Return Jacobian for this block"""
        return self.J

    def _apply_deriv(self, arg, result):
        """Matrix vector product only used if we are doing a directional
        derivative."""
        self.fd.calculate(arg, result)

    def list_deriv_vars(self):
        """Derivative inputs and outputs for this block"""
        return self.mapped_inputs, self.mapped_outputs

    def get(self, varname):
        """ Return the value of a variable in the Pseudoassembly. Used
        when sizing variables in the Jacobian."""

        return self.wflow.scope.get(from_PA_var(self.name+'.'+varname))

    def add_to_graph(self, startgraph, dgraph, excludes=()):
        """Add this PseudoAssembly to the given graph, absorbing
        nodes that represent components contained in this PA.
        """
        from openmdao.main.depgraph import is_subvar_node, \
                                 is_input_base_node, is_output_base_node

        self._removed_comps = [c for c in self.comps if c not in excludes]

        # Add pseudoassys to graph
        dgraph.add_node(self.name, pa_object=self, comp=True,
                        pseudo='assembly', valid=True)

        empty = {}
        for oldname, newname in self.renames.items():
            attrs = startgraph.node.get(oldname, empty).copy()
            if oldname in self.boundary_params:
                attrs['iotype'] = 'out'
            dgraph.add_node(newname, attr_dict=attrs)
            for u, v, data in dgraph.edges(oldname, data=True):
                dgraph.add_edge(newname, v, attr_dict=data.copy())
            for u, v, data in dgraph.in_edges(oldname, data=True):
                dgraph.add_edge(u, newname, attr_dict=data.copy())

            if is_subvar_node(dgraph, newname):
                dgraph.node[newname]['basevar'] = \
                    newname.split('[', 1)[0]
            if is_input_base_node(dgraph, newname):
                dgraph.add_edge(newname, self.name)
            elif is_output_base_node(dgraph, newname):
                dgraph.add_edge(self.name, newname)

    def clean_graph(self, startgraph, dgraph, excludes=()):
        """ Clean up the old nodes in the graph, leaving out ones that
        are excluded because they're used by ancestor drivers.
        """
        dgraph.remove_nodes_from(dgraph.find_prefixed_nodes([n for n in
                          self._orig_group_nodes if n not in excludes
                                                   and n in dgraph]))

        # if this PA represents a driver, remove the corresponding driver
        # node
        if self.name[1:] in dgraph:
            dgraph.remove_nodes_from(dgraph.find_prefixed_nodes([self.name[1:]]))

        # update mapped inputs/outputs in the graph
        map_inputs = dgraph.graph['mapped_inputs']
        map_outputs = dgraph.graph['mapped_outputs']

        for i, varpath in enumerate(map_inputs):
            if isinstance(varpath, basestring):
                varpath = [varpath]

            mapped = []
            for path in varpath:
                compname, _, varname = path.partition('.')
                if varname and (compname in self.comps):
                    mapped.append(to_PA_var(path, self.name))
                else:
                    mapped.append(path)  # keep old value in that spot

            map_inputs[i] = tuple(mapped)

        # Add requested outputs
        for i, varpath in enumerate(map_outputs):
            compname, _, varname = varpath.partition('.')
            if varname and (compname in self.comps):
                map_outputs[i] = to_PA_var(varpath, self.name)

    def set_complex_step(self):
        """Activate support for complex stepping in the comps in this PA."""
        for name in self.itercomps:
            comp = self.wflow.scope.get(name)
            comp._complex_step = True
