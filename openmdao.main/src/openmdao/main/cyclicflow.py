""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

import networkx as nx
from ordereddict import OrderedDict
from networkx.algorithms.components import strongly_connected_components

try:
    from numpy import ndarray, hstack, zeros
except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, hstack, zeros


from openmdao.main.array_helpers import flattened_value
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.pseudoassembly import from_PA_var, to_PA_var
from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.vartree import VariableTree

__all__ = ['CyclicWorkflow']

# SequentialWorkflow gives us the add and remove methods.
class CyclicWorkflow(SequentialWorkflow):
    """A CyclicWorkflow consists of a collection of Components that contains
    loops in the graph.
    """

    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """

        super(CyclicWorkflow, self).__init__(parent, scope, members)
        self.config_changed()

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(CyclicWorkflow, self).config_changed()
        self._workflow_graph = None
        self._topsort = None
        self._severed_edges = []
        self._mapped_severed_edges = []

    def __iter__(self):
        """Iterate through the nodes in some proper order."""

        # resolve all of the components up front so if there's a problem it'll
        # fail early and not waste time running components
        scope = self.scope
        return [getattr(scope, n) for n in self._get_topsort()].__iter__()

    def _get_topsort(self):
        """ Return a sorted list of components in the workflow.
        """

        if self._topsort is None:
            graph = nx.DiGraph(self._get_collapsed_graph())

            cyclic = True
            self._severed_edges = set()

            while cyclic:

                try:
                    self._topsort = nx.topological_sort(graph)
                    cyclic = False

                except nx.NetworkXUnfeasible:
                    strong = strongly_connected_components(graph)

                    # We may have multiple loops. We only deal with one at
                    # a time because multiple loops create some non-unique
                    # paths.
                    strong = strong[0]

                    # Break one edge of the loop.
                    # For now, just break the first edge.
                    # TODO: smarter ways to choose edge to break.
                    graph.remove_edge(strong[-1], strong[0])

                    # Keep a list of the edges we break, so that a solver
                    # can use them as its independents/dependents.
                    depgraph = self.scope._depgraph
                    edge_set = set(depgraph.get_directional_interior_edges(strong[-1],
                                                                            strong[0]))

                    self._severed_edges.update(edge_set)

        return self._topsort

    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it. This graph can be cyclic."""

        # Cached
        if self._workflow_graph is None:

            contents = self.get_components(full=True)

            # get the parent assembly's component graph
            scope = self.scope
            compgraph = scope._depgraph.component_graph()
            graph = compgraph.subgraph([c.name for c in contents])

            self._workflow_graph = graph

        return self._workflow_graph

    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """

        dgraph = self.derivative_graph()

        # We need to map any of our edges if they are in a
        # pseudo-assy
        comps = dgraph.edge_dict_to_comp_list(self.edge_list())
        pa_keys = [name for name in comps if '~' in name]

        if len(pa_keys) == 0:
            self._mapped_severed_edges = self._severed_edges
        else:
            self._mapped_severed_edges = []
            for src, target in self._severed_edges:

                compname, _, varname = src.partition('.')
                for pa_key in pa_keys:
                    pseudo = dgraph.node[pa_key]['pa_object']
                    if src in pseudo.outputs:
                        src = to_PA_var(src, pseudo.name)
                        break

                compname, _, varname = target.partition('.')
                for pa_key in pa_keys:
                    pseudo = dgraph.node[pa_key]['pa_object']
                    flat_inputs = set()
                    for item in pseudo.inputs:
                        subset = set(item)
                        flat_inputs = flat_inputs.union(subset)

                    if target in flat_inputs:
                        target = to_PA_var(target, pseudo.name)
                        break

                self._mapped_severed_edges.append((src, target))

        return super(CyclicWorkflow, self).initialize_residual()


    def derivative_graph(self, inputs=None, outputs=None, fd=False,
                         group_nondif=True):
        """Returns the local graph that we use for derivatives. For cyclic flows,
        we need to sever edges and use them as inputs/outputs.
        """

        if self._derivative_graph is None or group_nondif is False:

            if inputs == None:
                inputs = []

            if outputs == None:
                outputs = []

            # Solver can specify parameters
            if hasattr(self._parent, 'list_param_group_targets'):
                inputs = self._parent.list_param_group_targets()

            # Solver can specify equality constraints
            if hasattr(self._parent, 'get_eq_constraints'):
                outputs = ["%s.out0" % item.pcomp_name for item in \
                               self._parent.get_constraints().values()]

            # Cyclic flows need to be severed before derivatives are calculated.
            self._get_topsort()

            for src, target in self._severed_edges:
                inputs.append(target)
                outputs.append(src)

            dgraph = super(CyclicWorkflow, self).derivative_graph(inputs,
                            outputs, fd, self._severed_edges, group_nondif)

            if group_nondif is False:
                return dgraph

        return self._derivative_graph

    def edge_list(self):
        """ Return the list of edges for the derivatives of this workflow. """

        self._edges = super(CyclicWorkflow, self).edge_list()

        # TODO: Shouldn't have to do this everytime.
        if len(self._mapped_severed_edges) > 0:

            cyclic_edges = OrderedDict()
            for edge in self._mapped_severed_edges:
                cyclic_edges[edge[0]] = edge[1]

            # Finally, modify our edge list to include the severed edges, and exclude
            # the boundary edges.
            for src, targets in self._edges.iteritems():
                if '@in' not in src or \
                   not any(edge in cyclic_edges.values() for edge in targets):
                    if isinstance(targets, str):
                        targets = [targets]

                    newtargets = []
                    for target in targets:
                        if '@out' not in target or \
                           src not in cyclic_edges:
                            newtargets.append(target)

                    if len(newtargets) > 0:
                        cyclic_edges[src] = newtargets

            self._edges = cyclic_edges

        return self._edges

    def get_dependents(self, fixed_point=False):
        """Returns a list of current values of the dependents. This includes
        both constraints and severed sources.

        fixed_point: bool
            Set to True if we are doing fixed-point iteration instead of a more
            general solve. In such a case, we need to swap the order of the
            constraints to match the parameter order. We also may need to swap
            signs on the constraints.
        """

        deps = self._parent.eval_eq_constraints(self.scope)

        # Reorder for fixed point
        if fixed_point == True:
            newdeps = zeros(len(deps))
            eqcons = self._parent.get_eq_constraints()
            old_j = 0
            for key, value in eqcons.iteritems():
                con_targets = value.get_referenced_varpaths()
                new_j = 0
                for params in self._parent.list_param_group_targets():
                    if params[0] == value.rhs.text:
                        newdeps[new_j] = deps[old_j]
                    elif params[0] == value.lhs.text:
                        newdeps[new_j] = -deps[old_j]
                    new_j += 1
                old_j += 1
            deps = newdeps

        sev_deps = []
        for src, target in self._severed_edges:

            if not isinstance(target, str):
                target = target[0]

            target = from_PA_var(target)
            src = from_PA_var(src)
            src_val = self.scope.get(src)
            targ_val = self.scope.get(target)
            res = flattened_value(src, src_val) - flattened_value(target, targ_val)

            sev_deps.extend(res)

        return hstack((deps, sev_deps))

    def get_independents(self):
        """Returns a list of current values of the dependents. This includes
        both parameters and severed targets.
        """

        indeps = self._parent.eval_parameters(self.scope)
        sev_indeps = []
        for _, target in self._severed_edges:

            if not isinstance(target, str):
                target = target[0]

            target = from_PA_var(target)
            old_val = self.scope.get(target)

            sev_indeps.extend(flattened_value(target, old_val))

        return hstack((indeps, sev_indeps))

    def set_independents(self, val):
        """Sets all dependent variables to the values in the input array
        `val`. This includes both parameters and severed targets.
        """

        nparam = self._parent.total_parameters()
        if nparam > 0:
            self._parent.set_parameters(val[:nparam].flatten())

        if len(self._severed_edges) > 0:
            i = nparam
            for src, targets in self._mapped_severed_edges:
                if isinstance(targets, str):
                    targets = [targets]

                i1, i2 = self.get_bounds(src)
                if isinstance(i1, list):
                    width = len(i1)
                else:
                    width = i2-i1

                i1 = i
                i2 = i + width

                for target in targets:

                    target = from_PA_var(target)
                    old_val = self.scope.get(target)

                    if isinstance(old_val, float):
                        new_val = float(val[i1:i2])
                    elif isinstance(old_val, ndarray):
                        shape = old_val.shape
                        if len(shape) > 1:
                            new_val = val[i1:i2].copy()
                            new_val = new_val.reshape(shape)
                        else:
                            new_val = val[i1:i2].copy()
                    elif isinstance(old_val, VariableTree):
                        new_val = old_val.copy()
                        self._vtree_set(target, new_val, val[i1:i2], i1)
                    else:
                        msg = "Variable %s is of type %s." % (target, type(old_val)) + \
                              " This type is not supported by the MDA Solver."
                        self.scope.raise_exception(msg, RuntimeError)

                    i += width

                    # Poke new value into the input end of the edge.
                    self.scope.set(target, new_val, force=True)

                    # Prevent OpenMDAO from stomping on our poked input.
                    self.scope.set_valid([target.split('[',1)[0]], True)

    def _vtree_set(self, name, vtree, dv, i1=0):
        """ Update VariableTree `name` value `vtree` from `dv`. """
        for key in sorted(vtree.list_vars()):  # Force repeatable order.
            value = getattr(vtree, key)
            if isinstance(value, float):
                setattr(vtree, key, float(dv[i1]))
                i1 += 1
            elif isinstance(value, ndarray):
                shape = value.shape
                size = value.size
                i2 = i1 + size
                if len(shape) > 1:
                    value = dv[i1:i2]
                    value = value.reshape(shape)
                else:
                    value = dv[i1:i2]
                setattr(vtree, key, value)
                i1 += size
            elif isinstance(value, VariableTree):
                i1 = self._vtree_set('.'.join((name, key)), value, dv, i1)
            else:
                msg = "Variable %s is of type %s." % (name, type(value)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

        return i1

