from collections import deque
from itertools import chain
from ordereddict import OrderedDict

import networkx as nx

from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IVariableTree, \
                                     IImplicitComponent, ISolver, \
                                     IAssembly, IComponent
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.array_helpers import is_differentiable_var, is_differentiable_val
from openmdao.main.pseudoassembly import PseudoAssembly, from_PA_var, to_PA_var
from openmdao.main.case import flatteners
from openmdao.main.vartree import VariableTree
from openmdao.util.nameutil import partition_names_by_comp
from openmdao.util.graph import flatten_list_of_iters, list_deriv_vars

# # to use as a quick check for exprs to avoid overhead of constructing an
# # ExprEvaluator
_exprchars = set('+-/*()&| %<>!')

# metadata to pull from component and place in graph
_metasrch = ['data_shape', 'deriv_ignore', 'framework_var']

_missing = object()

def _is_expr(node):
    """Returns True if node is an expression that is not a simple
    variable reference, or a reference to a single array entry or
    vartree attribute.
    """
    return len(_exprchars.intersection(node)) > 0

def _sub_or_super(s1, s2):
    """Returns True if s1 is a subvar or supervar of s2."""
    if s2.startswith(s1 + '.'):
        return True
    if s2.startswith(s1 + '['):
        return True
    if s1.startswith(s2 + '.'):
        return True
    if s1.startswith(s2 + '['):
        return True
    return False

def unique(seq):
    """Return a list of unique values, preserving order"""
    seen = set()
    sadd = seen.add
    return [ x for x in seq if x not in seen and not sadd(x)]


# Explanation of node/edge metadata dict entries
#
# NODES:
#   comp    means it's a Component node
#   pseudo  means it's a PseudoComponent, and the value of the 'pseudo' entry can be
#             'units', 'multi_var_expr', 'constraint', or 'objective'
#   var     means it's a simple Variable node (no indexing or attr access)
#   basevar means it's some reference to a part of a Variable (like an array ref or attr access)
#              and the value of basevar is the name of the Variable it refers to
#   driver  means it's a Driver node
#   iotype  is present in var and subvar nodes and indicates i/o direction
#   boundary means it's a boundary variable node
#   valid   indicates validity of the node. can be True or False
#   invalidate  indicates whether a comp node has partial or full invalidation. allowed
#               values are ['partial', 'full']
#   fake    used to designate subvar nodes that are essentially metadata placeholders and
#           are not really part of the dataflow
#
# EDGES:
#   conn    means that the edge is a connection that was specified
#           by calling connect()
#   fake    edge is artificial and doesn't represent a real data connection


# NODE selectors

def is_valid_node(graph, node):
    return graph.node['valid']

def is_input_node(graph, node):
    if graph.node[node].get('iotype') == 'in':
        return True
    base = graph.node[node].get('basevar')
    return base is not None and graph.node[base].get('iotype') == 'in'

def is_input_base_node(graph, node):
    return graph.node[node].get('iotype') == 'in'

def is_output_node(graph, node):
    if graph.node[node].get('iotype') == 'out':
        return True
    base = graph.node[node].get('basevar')
    return base is not None and graph.node[base].get('iotype') == 'out'

def is_output_base_node(graph, node):
    return graph.node[node].get('iotype') == 'out'

def is_boundary_node(graph, node):
    return 'boundary' in graph.node.get(node, '')

def is_boundary_input_node(graph, node):
    return is_boundary_node(graph, node) and is_input_node(graph, node)

def is_boundary_output_node(graph, node):
    return is_boundary_node(graph, node) and is_output_node(graph, node)

def is_comp_node(graph, node):
    """Returns True for Component or PseudoComponent nodes."""
    return 'comp' in graph.node.get(node, '')

def is_driver_node(graph, node):
    return 'driver' in graph.node.get(node, '')

def is_var_node(graph, node):
    """Return True for all basevar and subvar
    nodes.
    """
    data = graph.node.get(node, '')
    return 'var' in data or 'basevar' in data

def is_basevar_node(graph, node):
    """Returns True if this node represents an
    actual input or output variable.
    """
    return 'var' in graph.node.get(node, '')

def is_subvar_node(graph, node):
    """Returns True if this node represents some
    subdivision of an input or output variable,
    e.g., an array index -   comp_1.x[2]
    """
    return 'basevar' in graph.node.get(node, '')

def is_fake_node(graph, node):
    return 'fake' in graph.node.get(node, '')

def is_var_node_with_solution_bounds(graph, node):
    """Returns True if this variable node stores metadata
    for calculating the gradient. The metadata whose keys
    are driver iternames, and whose values are a tuple
    containing the start and end index where this var's
    values get poked into the solution vector.
    """
    return 'bounds' in graph.node.get(node, '')

def is_pseudo_node(graph, node):
    return 'pseudo' in graph.node.get(node, '')

def is_objective_node(graph, node):
    return graph.node[node].get('pseudo') == 'objective'

def is_pseudo_output_node(graph, node):
    pseudo = graph.node[node].get('pseudo')
    return pseudo == 'objective' or pseudo == 'constraint'

def is_unit_node(graph, node):
    return graph.node[node].get('pseudo') == 'units'

def is_multivar_expr_node(graph, node):
    return graph.node[node].get('pseudo') == 'multi_var_expr'

def is_non_driver_pseudo_node(graph, node):
    pseudo = graph.node[node].get('pseudo')
    return pseudo == 'units' or pseudo == 'multi_var_expr'

def is_extern_node(graph, node):
    return node.startswith('parent.')

def is_extern_src(graph, node):
    return node.startswith('parent.') and graph.out_degree(node) > 0

def is_extern_dest(graph, node):
    return node.startswith('parent.') and graph.in_degree(node) > 0

def is_nested_node(graph, node):
    """Returns True if the given node refers to an attribute that
    is nested within the child of a Component in our scope, or
    within a boundary variable in our scope.  For
    example, if a Component 'comp1' is within our scope,
    a variable node referring to 'comp1.child.x' would be a
    nested node while a 'comp1.y' node would not.  If we had a boundary
    var called 'b', then 'b.x' would be a nested node.
    """
    base = graph.base_var(node)
    return '.' in node[len(base):]

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False

def is_drv_connection(graph, src, dest, driver):
    try:
        if driver is True: # True for any driver
            return 'drv_conn' in graph.edge[src][dest]
        else:  # only True for specific driver
            return graph.edge[src][dest]['drv_conn'] == driver
    except KeyError:
        return False

def _break_loop(graph, loop):
    src = loop[0]
    for dest in loop[1:]:
        if dest in graph[src]:
            graph.remove_edge(src, dest)
            return (src, dest)


class DependencyGraph(nx.DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()
        self._severed_edges = []
        self._allow_config_changed = True
        self.config_changed()

    def base_var(self, node):
        """Returns the name of the variable node that is the 'base' for
        the given node name.  For example, for the node A.b[4], the
        base variable is A.b.  For the node d.x.y, the base variable
        is d if d is a boundary variable node, or d.x otherwise.
        """
        if node in self:
            base = self.node[node].get('basevar')
            if base:
                return base
            elif 'var' in self.node[node]:
                return node

        parts = node.split('[', 1)[0].split('.')
        # for external connections, we don't do the error checking at
        # this level so ambiguity in actual base varname doesn't
        # matter.  So just return the full name and be done with it.
        if parts[0] == 'parent':
            return node

        base = parts[0]
        if base in self and 'var' in self.node[base]:
            return base

        return '.'.join(parts[:2])

    def sever_edges(self, edges):
        """Temporarily remove the specified edges but save
        them and their metadata for later restoration.
        """
        # Note: This will NOT call config_changed(), and if
        # component_graph() has not been called since the last
        # config_changed, it WILL create a temporary new
        # component graph which will be overwritten by the original
        # one when unsever_edges() is called.
        if self._severed_edges:
            raise RuntimeError("only one set of severed edges is permitted")

        # save old stuff to restore later
        self._saved_comp_graph = self._component_graph
        self._saved_loops = self._loops

        self._severed_edges = list(edges)

        self._allow_config_changed = False
        try:
            for u,v in edges:
                self.disconnect(u, v)
        finally:
            self._allow_config_changed = True

    def unsever_edges(self, scope):
        """Restore previously severed edges."""
        if not self._severed_edges:
            return

        self._allow_config_changed = False
        try:
            for u,v in self._severed_edges:
                self.connect(scope, u, v)
        finally:
            self._allow_config_changed = True

        self._severed_edges = []

        self._loops = self._saved_loops
        self._component_graph = self._saved_comp_graph

    def config_changed(self):
        if self._allow_config_changed:
            self._component_graph = None
            self._loops = None
            self._saved_loops = None
            self._saved_comp_graph = None
            self._chvars = {}
            self._bndryins = {}
            self._bndryouts = {}
            self._extrnsrcs = None
            self._extrndsts = None
            self._srcs = {}
            self._conns = {}
            self._indegs = {}
            self._dstvars = {}

    def child_config_changed(self, child, adding=True, removing=True):
        """A child has changed its input lists and/or output lists,
        so we need to update the graph.
        """
        cname = child.name
        old_ins  = set(self.list_inputs(cname))
        old_outs = set(self.list_outputs(cname))
        old_states = set([n for n in old_outs if self.node[n]['iotype'] == 'state'])
        old_resids = set([n for n in old_outs if self.node[n]['iotype'] == 'residual'])

        # remove states from old_ins
        old_ins -= old_states

        # removes states and residuals from old_outs
        old_outs -= old_states
        old_outs -= old_resids

        new_ins  = set(['.'.join([cname,n]) for n in child.list_inputs()])
        new_outs = set(['.'.join([cname,n]) for n in child.list_outputs()])

        if has_interface(child, IImplicitComponent):
            new_states = set(child.list_states())
            new_resids = set(child.list_residuals())
        else:
            new_states = set()
            new_resids = set()

        if adding:
            added_ins = new_ins - old_ins
            added_outs = new_outs - old_outs
            added_states = new_states - old_states
            added_resids = new_resids - old_resids

            # add new inputs/outputs/states/residuals to the graph
            self.add_nodes_from(added_ins,    var=True, valid=True,  iotype='in')
            self.add_nodes_from(added_outs,   var=True, valid=False, iotype='out')
            self.add_nodes_from(added_states, var=True, valid=False, iotype='state')
            self.add_nodes_from(added_resids, var=True, valid=False, iotype='residual')

            # add edges from the variables to their parent component
            self.add_edges_from([(v,cname) for v in chain(added_ins, added_states)])
            self.add_edges_from([(cname,v) for v in chain(added_outs, added_states,
                                                          added_resids)])

            if added_outs or added_states or added_resids:
                self.node[cname]['valid'] = False

            self._update_graph_metadata(child, cname,
                                        chain([s.split('.',1)[1] for s in added_ins],
                                              [s.split('.',1)[1] for s in added_outs]))

        if removing:
            rem_ins = old_ins - new_ins
            rem_outs = old_outs - new_outs
            rem_states = old_states - new_states
            rem_resids = old_resids - new_resids

            # for removed inputs/outputs/states/residuals, may need to
            # remove connections and subvars
            for n in chain(rem_ins, rem_outs, rem_states, rem_resids):
                self.remove(n)

    def add_component(self, cname, obj, **kwargs):
        """Create nodes in the graph for the component and all of
        its input and output variables. Any other named args that
        are passed will be placed in the metadata for the component
        node.
        """

        if has_interface(obj, IDriver):
            kwargs['driver'] = True
        if hasattr(obj, '_pseudo_type'):
            kwargs['pseudo'] = obj._pseudo_type
            if obj._pseudo_type == 'multi_var_expr':
                kwargs['srcexpr'] = obj._orig_src
                kwargs['destexpr'] = obj._orig_dest

        kwargs['invalidation'] = obj.get_invalidation_type()
        kwargs['comp'] = True
        kwargs['valid'] = False

        inputs  = ['.'.join([cname, v]) for v in obj.list_inputs()]
        outputs = ['.'.join([cname, v]) for v in obj.list_outputs()]

        self.add_node(cname, **kwargs)
        self.add_nodes_from(inputs, var=True, iotype='in', valid=True)
        self.add_nodes_from(outputs, var=True, iotype='out', valid=False)

        self.add_edges_from([(v, cname) for v in inputs])
        self.add_edges_from([(cname, v) for v in outputs])

        if has_interface(obj, IImplicitComponent):
            states = ['.'.join([cname, v]) for v in obj.list_states()]
            resids = ['.'.join([cname, v]) for v in obj.list_residuals()]
            self.add_nodes_from(states, var=True, iotype='state', valid=True)
            self.add_nodes_from(resids, var=True, iotype='residual', valid=False)

            self.add_edges_from([(cname, v) for v in chain(states, resids)])
            self.add_edges_from([(v, cname) for v in states])

        self._update_graph_metadata(obj, cname,
                                    chain(obj.list_inputs(), obj.list_outputs()))

    def _update_graph_metadata(self, obj, cname, names):
        for vname in names:
            if not hasattr(obj, 'get_metadata'):
                continue

            if cname:
                data = self.node['.'.join((cname, vname))]
            else:
                data = self.node[vname]
            meta = obj.get_metadata(vname)
            for mname in _metasrch:
                if mname in meta:
                    data[mname] = meta[mname]

            val = getattr(obj, vname, _missing)
            if val is not _missing and not is_differentiable_val(val):
                data['differentiable'] = False

    def add_boundary_var(self, obj, name, **kwargs):
        """Add a boundary variable, i.e., one not associated
        with any component in the graph.
        """

        if name in self:
            raise RuntimeError("'%s' is already in the graph." % name)
        if _is_expr(name):
            raise RuntimeError("can't add expression '%s'. as a Variable node."
                               % name)
        if '.' in name or '[' in name:
            raise RuntimeError("'%s' is not a valid boundary variable name."
                               % name)
        if 'iotype' not in kwargs:
            raise RuntimeError("variable '%s' can't be added because its iotype was not specified." % name)

        kwargs['var'] = True
        if kwargs['iotype'] == 'in':
            valid = True
        else:
            valid = False
        kwargs['valid'] = valid
        kwargs['boundary'] = True
        self.add_node(name, **kwargs)

        self._update_graph_metadata(obj, '', (name,))

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.' or 'C1['.
        """
        nodes = self.find_prefixed_nodes([name])
        self.disconnect(name)  # updates validity
        if nodes:
            self.remove_nodes_from(nodes)


    def check_connect(self, srcpath, destpath):
        """Raise an exception if the specified destination is already
        connected.
        """
        if is_extern_node(self, destpath):  # error will be caught at parent level
            return

        dpbase = self.base_var(destpath)

        dest_iotype = self.node[dpbase].get('iotype')
        if dest_iotype in ('out','residual') and \
           not is_boundary_node(self, dpbase) and not dpbase == srcpath:
            raise RuntimeError("'%s' must be an input variable" % destpath)

        connected = False
        conns = self._var_connections(dpbase, 'in')
        if conns:
            if destpath == dpbase:
                connected = True
            else:
                for u, v in conns:
                    if destpath == v:
                        connected = True
                        break
                    if is_basevar_node(self, v):
                        connected = True
                        break
                    if destpath != dpbase and _sub_or_super(v, destpath):
                        connected = True
                        break

        if connected:
            raise RuntimeError("'%s' is already connected to '%s'" %
                                  (conns[0][1], conns[0][0]))

    def connect(self, scope, srcpath, destpath,
                check=True, invalidate=True):
        """Create a connection between srcpath and destpath,
        and create any necessary additional subvars with connections to base
        variable nodes.  For example, connecting A.b[3] to
        B.c.x will create an edge between A.b[3] and B.c.x, and
        will also add an edge from base variable A.b to A.b[3],
        and another edge from B.c.x to base variable B.c.
        """

        base_src  = self.base_var(srcpath)
        base_dest = self.base_var(destpath)

        for v in [base_src, base_dest]:
            if not v.startswith('parent.'):
                if v not in self:
                    raise RuntimeError("Can't find variable '%s' in graph." % v)
                elif not is_var_node(self, v):
                    raise RuntimeError("'%s' is not a variable node" % v)

        # path is a list of tuples of the form (var, basevar)
        path = [(base_src, base_src)]

        if srcpath != base_src:  # srcpath is a subvar

            path.append((srcpath, base_src))

        if destpath != base_dest:  # destpath is a subvar
            path.append((destpath, base_dest))

        path.append((base_dest, base_dest))

        if check:
            self.check_connect(srcpath, destpath)

        for i in range(len(path)):
            dest, base = path[i]
            if dest not in self:  # create a new subvar if it's not already there
                self.add_node(dest, basevar=base, valid=True)
            if i > 0:
                src = path[i-1][0]
                try:
                    self[src][dest]
                except KeyError:
                    self.add_edge(src, dest)

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True

        # create expression objects to handle setting of
        # array indces, etc.
        sexpr = ConnectedExprEvaluator(srcpath, scope=scope, getter='get_attr')
        dexpr = ConnectedExprEvaluator(destpath, scope=scope, getter='get_attr', is_dest=True)

        self.edge[srcpath][destpath]['sexpr'] = sexpr
        self.edge[srcpath][destpath]['dexpr'] = dexpr

        if invalidate:
            self.invalidate_deps(scope, [srcpath])

    def add_subvar(self, subvar):
        """ Adds a subvar node for a model input. This node is used to
        represent parameters that are array slices, mainly for metadata
        storage and for defining edge iterators, but not for workflow
        execution. Subvars created using this function will be labeled
        as 'fake' in their metadata.
        """
        base = self.base_var(subvar)
        if base not in self:
            raise RuntimeError("can't find basevar '%s' in graph" % base)
        self.add_node(subvar, basevar=base, valid=True, fake=True)
        if is_boundary_node(self, base):
            if is_input_node(self, base):
                self.add_edge(base, subvar)
            else:
                self.add_edge(subvar, base)
        else:  # it's a var of a child component
            if is_input_node(self, base):
                self.add_edge(subvar, base)
            else:
                self.add_edge(base, subvar)

    def disconnect(self, srcpath, destpath=None):

        if destpath is None:
            if is_comp_node(self, srcpath):
                edges = self.edges(self.successors(srcpath))
                edges.extend(self.in_edges_iter(self.predecessors(srcpath)))

            elif is_subvar_node(self, srcpath):
                # for a single subvar, add all of its downstream
                # edges to the removal list so that we can ensure that
                # downstream inputs are properly validated, and the
                # fact that we're marking ALL of its downstream edges to
                # remove means that ultimately the subvar node will
                # be removed because it will be 'dangling'
                edges = self.successors(srcpath)

            elif is_boundary_node(self, srcpath):
                if is_input_node(self, srcpath):
                    edges = self.edges(srcpath)
                else:
                    edges = self.in_edges(srcpath)
            else:
                if is_input_node(self, srcpath):
                    edges = self.in_edges(srcpath)
                else:
                    edges = self.out_edges(srcpath)
        else:
            edges = [(srcpath, destpath)]

        for u,v in edges:
            # make unconnected destinations valid
            if is_subvar_node(self, v):
                for node in self.successors_iter(v):
                    self.node[node]['valid'] = True
                    if is_subvar_node(self, node):
                        for vv in self.successors_iter(node):
                            self.node[vv]['valid'] = True

            self.node[v]['valid'] = True

        self.remove_edges_from(edges)

        # now clean up dangling subvars
        for edge in edges:
            for node in edge:
                if is_subvar_node(self, node) and not is_fake_node(self, node):
                    if self.in_degree(node) < 1 or self.out_degree(node) < 1:
                        self.remove_node(node)

    def get_directional_interior_edges(self, comp1, comp2):
        """ Behaves like get_interior_edges, except that it only
        returns interior edges that originate in comp1 and and end in comp2.

        comp1: str
            Source component name

        comp2: str
            Dest component name
        """
        in_set = set()
        for inp in self.list_inputs(comp2):
            in_set.update(self._var_connections(inp, 'in'))

        out_set = set()
        for out in self.list_outputs(comp1):
            out_set.update(self._var_connections(out, 'out'))

        return in_set.intersection(out_set)

    def connections_to(self, path, direction=None):
        if is_comp_node(self, path):
            return self._comp_connections(path, direction)
        else:
            return self._var_connections(path, direction)

    def list_connections(self, show_passthrough=True,
                               show_external=False, driver=False):
        conns = self._conns.get((show_passthrough, 
                                 show_external, driver))
        if conns is None:
            if driver:
                conn_test = lambda g,u,v: is_connection(g,u,v) or \
                                     is_drv_connection(g,u,v,driver)
            else:
                conn_test = is_connection

            conns = [(u,v) for u,v in self.edges_iter()
                              if conn_test(self, u, v)]

            if show_passthrough is False:
                conns = [(u,v) for u,v in conns 
                           if not ('.' in u or '.' in v)]

            if show_external is False:
                conns = [(u,v) for u,v in conns
                              if not (u.startswith('parent.')
                                   or v.startswith('parent.'))]

            self._conns[(show_passthrough, show_external,
                         driver)] = conns
        return conns[:]

    def get_sources(self, name):
        """Return the node that's actually a source for the
        named node (as opposed to just a connected subvar).
        This should only be called for destination nodes (inputs
        or output boundary vars).
        """
        srcs = self._srcs.get(name)
        if srcs is None:
            srcs = []
            for u,v in self.in_edges_iter(name):
                if is_connection(self, u, v):
                    srcs.append(u)
                elif is_subvar_node(self, u):
                    for uu,vv in self.in_edges_iter(u):
                        if is_connection(self, uu, vv):
                            srcs.append(uu)
            self._srcs[name] = srcs
        return srcs[:]

    def _check_source(self, path, src):
        """Raise an exception if the specified source differs from
        the connected source.  This prevents the setting of a
        destination value by any object other than the source
        specified in the graph.
        """
        preds = self.predecessors(path)
        for pred in preds:
            if src == pred:
                return
            if pred.startswith(path):  # subvar
                for p in self.predecessors_iter(pred):
                    if src == p:
                        return
        if len(preds) > 0:
            raise RuntimeError(
                "'%s' is connected to source '%s' and cannot be "
                "set by source '%s'" %
                (path, preds[0], src))

    def _all_child_vars(self, node, direction=None):
        """Return a list of nodes containing all nodes that are one
        or two connections out from the starting node that are prefixed
        by the starting node.  This captures all var and subvar
        nodes associated with the starting node.
        """
        ret = self._chvars.get((node, direction))
        if ret is None:
            bunch = set()

            if direction != 'in':
                succ = self.successors(node)
                bunch.update(succ)
                for s in succ:
                    bunch.update(self.successors_iter(s))

            if direction != 'out':
                pred = self.predecessors(node)
                bunch.update(pred)
                for p in pred:
                    bunch.update(self.predecessors_iter(p))
                    # it's possible for some input basevars to have subvars thar are
                    # sucessors instead of predecessors
                    bunch.update(self.successors_iter(p))

            ndot = node+'.'
            nbrack = node+'['
            ret = [n for n in bunch if n.startswith(ndot)
                                     or n.startswith(nbrack)]
            self._chvars[(node, direction)] = ret
        return ret[:]

    def subvars(self, node):
        if is_basevar_node(self, node):
            return self._all_child_vars(node)
        else:
            return []

    def all_comps(self):
        """Returns a list of all component and PseudoComponent
        nodes.
        """
        return [n for n in self.nodes_iter() if is_comp_node(self, n)]

    def find_prefixed_nodes(self, nodes, data=False):
        """Returns a list of nodes including the given nodes and
        any node that is prefixed with the name of any of those
        nodes. This is useful for retrieving all variable and
        subvar nodes associated with a component node.
        """

        full = []
        for node in nodes:
            full.extend(self._all_child_vars(node))
            full.append(node)

        if data:
            sn = self.node
            return [(n, sn[n]) for n in full]
        else:
            return full

    def full_subgraph(self, nodes):
        """Returns the subgraph specified by the given component
        or pseudocomp nodes and any variable or expr nodes
        corresponding to those nodes.
        """
        return self.subgraph(self.find_prefixed_nodes(nodes))

    def _comp_connections(self, cname, direction=None):
        conns = []
        if direction != 'in':
            for out in self.list_outputs(cname):
                conns.extend(self._var_connections(out, 'out'))
        if direction != 'out':
            for inp in self.list_inputs(cname):
                conns.extend(self._var_connections(inp, 'in'))
        return conns

    def invalidate_deps(self, scope, vnames):
        """Walk through all dependent nodes in the graph, invalidating all
        variables that depend on the given variable names.

        scope: object
            Scoping object where the components objects referred to
            by name in the graph are found.

        vnames: list or set of str
            Names of var nodes.
        """

        if not vnames:
            return []

        outset = set()  # set of changed boundary outputs

        ndata = self.node
        stack = [(n, self.successors_iter(n), not is_comp_node(self, n))
                        for n in vnames]

        visited = set()
        while(stack):
            src, neighbors, checkvisited = stack.pop()
            if checkvisited and src in visited:
                continue
            else:
                visited.add(src)
            sdata = ndata[src]
            oldvalid = sdata['valid']
            if oldvalid is True:
                if src.startswith('parent.'):
                    ndata[src]['valid'] = False
                else:
                    indeg = self._indegs.get(src)
                    if indeg is None:
                        indeg = self.in_degree(src)
                        self._indegs[src] = indeg
                    if indeg:  # don't invalidate unconnected inputs
                        ndata[src]['valid'] = False
                if 'boundary' in sdata and sdata.get('iotype') == 'out':
                    outset.add(src)

            parsources = None
            for node in neighbors:
                ddata = ndata[node]
                if 'comp' in ddata:
                    if ddata['valid'] or ddata.get('invalidation')=='partial':
                        if parsources is None:
                            parsources = self.get_sources(src)
                        outs = getattr(scope, node).invalidate_deps(['.'.join(('parent', n))
                                                                      for n in parsources])
                        if outs is None:
                            stack.append((node, self.successors_iter(node), True))
                        elif outs: # partial invalidation
                            stack.append((node, ['.'.join((node,n)) for n in outs], False))
                else:
                    stack.append((node, self.successors_iter(node), True))

        return outset

    def get_boundary_inputs(self, connected=False):
        """Returns inputs that are on the component boundary.
        If connected is True, return a list of only those nodes
        that are connected externally.
        """
        ret = self._bndryins.get(connected)
        if ret is not None:
            return ret

        ins = []
        for node in self.nodes_iter():
            if is_boundary_node(self, node) and \
               is_input_base_node(self, node):
                    if connected:
                        if self.in_degree(node) > 0:
                            ins.append(node)
                    else:
                        ins.append(node)
        self._bndryins[connected] = ins
        return ins[:]

    def get_boundary_outputs(self, connected=False):
        """Returns outputs that are on the component boundary.
        If connected is True, return a list of only those nodes
        that are connected externally.
        """
        ret = self._bndryouts.get(connected)
        if ret is not None:
            return ret

        outs = []
        for node in self.nodes_iter():
            if is_boundary_node(self, node) and \
               is_output_base_node(self, node):
                    if connected:
                        if self.out_degree(node) > 0:
                            outs.append(node)
                    else:
                        outs.append(node)
        self._bndryouts[connected] = outs
        return outs[:]

    def get_extern_srcs(self):
        """Returns sources from external to our parent
        component that are connected to our boundary inputs.
        """
        if self._extrnsrcs is None:
            self._extrnsrcs = [n for n in self.nodes_iter()
                                      if is_extern_src(self, n)]
        return self._extrnsrcs[:]

    def get_extern_dests(self):
        """Returns destinations that are external to our parent
        component that are connected to our boundary outputs.
        """
        if self._extrndsts is None:
            self._extrndsts = [n for n in self.nodes_iter()
                                   if is_extern_dest(self, n)]
        return self._extrndsts[:]

    def list_inputs(self, cname, connected=False, invalid=False):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.  If
        invalid is True, return only invalid inputs.
        """
        if connected:
            if invalid:
                data = self.node
                return [n for n in self.pred[cname]
                           if data[n]['valid'] is False and self.in_degree(n)]
            else:
                return [n for n in self.pred[cname]
                                            if self.in_degree(n)]
        elif invalid:
            data = self.node
            return [n for n in self.pred[cname] if data[n]['valid'] is False]
        else:
            return self.pred[cname].keys()

    def list_input_outputs(self, cname):
        """Return a list of names of input or state nodes that are used
        as outputs.
        """
        return [n for n in self.pred[cname]
                             if self.out_degree(n)>1]

    def list_outputs(self, cname, connected=False):
        """Return a list of names of output, state or residual nodes for a component.
        If connected is True, return only connected outputs.
        """
        if connected:
            return [n for n in self.succ[cname]
                                            if self.out_degree(n)>0]
        else:
            return self.succ[cname].keys()

    def list_autopassthroughs(self):
        """Returns a list of autopassthrough connections as (src, dest)
        tuples.  Autopassthroughs are connections directly from a
        variable external to this graph to an internal (non-boundary)
        variable.
        """
        conns = []
        for n in self.nodes_iter():
            if is_extern_node(self, n):
                for p in self.predecessors_iter(n):
                    if self.has_edge(p, n) and '.' in p:
                        conns.append((p, n))
                for s in self.successors_iter(n):
                    if self.has_edge(n, s) and '.' in s:
                        conns.append((n, s))

        return conns

    def component_graph(self, driver=False):
        """Return a subgraph containing only Components
        and PseudoComponents and edges between them.
        """
        if self._component_graph is not None:
            return self._component_graph

        compset = set(self.all_comps())

        g = nx.DiGraph()
        for comp in compset:
            g.add_node(comp, self.node[comp].copy())

        for src, dest in self.list_connections(driver=driver):
            destcomp = dest.split('.', 1)[0]
            srccomp  =  src.split('.', 1)[0]
            if srccomp in compset and destcomp in compset:
                g.add_edge(srccomp, destcomp)
                g[srccomp][destcomp].setdefault('var_edges',[]).append((src,dest))

        self._component_graph = g
        return g

    def order_components(self, comps):
        """Return a list of the given components, sorted in
        dataflow order.
        """
        cgraph = self.component_graph()
        csub = cgraph.subgraph(comps)
        if len(csub) != len(comps):
            missing = [n for n in comps if n not in cgraph]
            if missing:
                raise RuntimeError("Components %s are missing from the graph" %
                                   missing)
        while True:
            loops = [s for s in nx.strongly_connected_components(csub)
                       if len(s) > 1]
            if not loops:
                break

            for group in loops:
                _break_loop(csub, group)

        return nx.topological_sort(csub)

    def get_loops(self):
        if self._loops is None:
            self._loops = [s for s in
                nx.strongly_connected_components(self.component_graph())
                if len(s)>1]
        return self._loops

    def _var_connections(self, path, direction=None):
        """Returns a list of tuples of the form (srcpath, destpath) for all
        variable connections to the specified variable or sub-variable.
        If direction is None, both ins and outs are included. Other allowed
        values for direction are 'in' and 'out'.
        """
        conns = []

        if direction != 'in':  # get 'out' connections
            for u,v in self.var_edge_iter(path):
                if is_connection(self, u, v):
                    conns.append((u, v))

        if direction != 'out':  # get 'in' connections
            for u,v in self.var_edge_iter(path, reverse=True):
                if is_connection(self, u, v):
                    conns.append((u, v))

        return conns

    def var_edge_iter(self, source, reverse=False):
        """Iterater over edges from the given source to the
        nearest basevars.
        """
        # Adapted from the networkx bfs_edges function
        G = self
        if reverse and isinstance(G, nx.DiGraph):
            neighbors = G.predecessors_iter
        else:
            neighbors = G.neighbors_iter
        visited=set()
        queue = deque([(source, neighbors(source))])

        while queue:
            parent, children = queue[0]
            try:
                child = next(children)
                if (parent,child) not in visited:
                    visited.add((parent,child))
                    if reverse:
                        yield child, parent
                    else:
                        yield parent, child
                    if not is_basevar_node(self, child) and not is_comp_node(self, child):
                        queue.append((child, neighbors(child)))
            except StopIteration:
                queue.popleft()

    def basevar_iter(self, nodes, reverse=False):
        """Given a group of nodes, return an iterator
        over all base variable nodes that are nearest in one
        direction.
        """
        if reverse:
            idx = 0
        else:
            idx = 1

        for node in nodes:
            for edge in self.var_edge_iter(node, reverse=reverse):
                if is_basevar_node(self, edge[idx]):
                    yield edge[idx]

    def child_run_finished(self, childname, outs=None):
        """Called by a child when it completes its run() function."""
        data = self.node
        data[childname]['valid'] = True

        if outs:
            if childname:
                outs = ['.'.join([childname,n]) for n in outs]
            for out in outs:
                data[out]['valid'] = True
                for var in self._all_child_vars(out, direction='out'):
                    data[var]['valid'] = True
        else:
            for var in self._all_child_vars(childname, direction='out'):
                data[var]['valid'] = True

    def update_boundary_outputs(self, scope):
        """Update destination vars on our boundary."""
        for out in self.get_boundary_outputs():
            self.update_destvar(scope, out)

    def update_destvar(self, scope, vname):
        """Update the value of the given variable in the
        given scope using upstream variables.
        """
        tup = self._dstvars.get(vname)
        if tup is None:
            valid_set = set(self.find_prefixed_nodes([vname]))
            sexprs = []
            dexprs = []
            for u,v,data in self.in_edges_iter(vname, data=True):
                if 'conn' in data:
                    dexprs.append(data['dexpr'])
                    sexprs.append(data['sexpr'])
                    valid_set.add(v)
                else:
                    for uu,vv,ddata in self.in_edges_iter(u, data=True):
                        if 'conn' in ddata:
                            dexprs.append(ddata['dexpr'])
                            sexprs.append(ddata['sexpr'])
                            valid_set.add(vv)
            self._dstvars[vname] = (sexprs, dexprs, valid_set)
        else:
            sexprs, dexprs, valid_set = tup

        try:
            for sexpr, dexpr in zip(sexprs, dexprs):
                dexpr.set(sexpr.evaluate(scope=scope), 
                                         src=sexpr.text, scope=scope)
        except Exception as err:
            raise err.__class__("cannot set '%s' from '%s': %s" %
                                 (dexpr.text, sexpr.text, str(err)))

        for node in valid_set:
            self.node[node]['valid'] = True

    def validate_boundary_vars(self):
        """Mark extern and boundary vars and their
        subvars as valid.
        """
        meta = self.node
        for inp in self.get_extern_srcs():
            meta[inp]['valid'] = True
            for n in self.successors_iter(inp):
                meta[n]['valid'] = True
                if is_subvar_node(self, n):
                    for var in self._all_child_vars(self.node[n]['basevar']):
                        meta[var]['valid'] = True

        for out in self.get_boundary_outputs():
            meta[out]['valid'] = True
            for n in self.successors_iter(out):
                meta[n]['valid'] = True
                if is_subvar_node(self, n):
                    for var in self._all_child_vars(out):
                        meta[var]['valid'] = True

        for out in self.get_extern_dests():
            meta[out]['valid'] = True

    def edge_dict_to_comp_list(self, edges, implicit_edges=None):
        """Converts inner edge dict into an ordered dict whose keys
        are component names, and whose values are lists of relevant
        (in the graph) inputs and outputs.
        """
        comps = OrderedDict()
        basevars = set()
        for src, targets in edges.iteritems():

            if not isinstance(targets, list):
                targets = [targets]

            for target in targets:
                if not target.startswith('@'):
                    comp, _, var = target.partition('.')
                    if var: # TODO: this adds VarTrees as well as comps. Should it?
                        if comp not in comps:
                            comps[comp] = {'inputs': [],
                                           'outputs': [],
                                           'residuals': [],
                                           'states': []}

                        basevar = self.base_var(target)
                        if basevar not in basevars:
                            comps[comp]['inputs'].append(var)

                            if target == basevar:
                                basevars.add(target)

            if not src.startswith('@'):
                comp, _, var = src.partition('.')
                if var: # TODO: this adds VarTrees as well as comps. Should it?
                    if comp not in comps:
                        comps[comp] = {'inputs': [],
                                       'outputs': [],
                                       'residuals': [],
                                       'states': []}

                    basevar = self.base_var(src)
                    if basevar not in basevars:
                        comps[comp]['outputs'].append(var)
                        if src == basevar:
                            basevars.add(src)

        # Implicit edges
        if implicit_edges is not None:
            for srcs, targets in implicit_edges.iteritems():
                for src in srcs:
                    comp, _, var = src.partition('.')
                    comps[comp]['residuals'].append(var)
                    comps[comp]['outputs'].append(var)
                for target in targets:
                    if isinstance(target, str):
                        target = [target]
                    for itarget in target:
                        comp, _, var = itarget.partition('.')
                        comps[comp]['states'].append(var)
                        comps[comp]['inputs'].append(var)

                        # Remove any states that came into outputs via
                        # input-input connections.
                        if var in comps[comp]['outputs']:
                            comps[comp]['outputs'].remove(var)

        return comps

    def prune_unconnected(self):
        """Removes all unconnected variable and component
        nodes in the graph.  The graph at this point
        should contain connections for all driver dependencies
        as well as all normal data connections.
        """
        # save the set of all vars used in a connection
        conns = self.list_connections(driver=True)
        save = set([u for u,v in conns])
        save.update([v for u,v in conns])
        base = self.base_var

        # save all basevars for any connected subvars
        save.update([base(v) for v in save])

        # finally, save all component nodes
        save.update(self.all_comps())

        # now do the pruning of variables
        self.remove_nodes_from([n for n in self.nodes_iter() 
                                        if n not in save])

        # now, prune any component with no connections
        self.remove_nodes_from([c for c in self.all_comps() 
                                 if self.degree(c)==0])

    def add_node(self, n, attr_dict=None, **attr):
        super(DependencyGraph, self).add_node(n,
                                              attr_dict=attr_dict,
                                              **attr)
        self.config_changed()

    def add_nodes_from(self, nodes, **attr):
        super(DependencyGraph, self).add_nodes_from(nodes, **attr)
        self.config_changed()

    def remove_node(self, n):
        super(DependencyGraph, self).remove_node(n)
        self.config_changed()

    def remove_nodes_from(self, nbunch):
        super(DependencyGraph, self).remove_nodes_from(nbunch)
        self.config_changed()

    def add_edge(self, u, v, attr_dict=None, **attr):
        super(DependencyGraph, self).add_edge(u, v,
                                      attr_dict=attr_dict, **attr)
        self.config_changed()

    def add_edges_from(self, ebunch, attr_dict=None, **attr):
        super(DependencyGraph, self).add_edges_from(ebunch,
                                        attr_dict=attr_dict,
                                        **attr)
        self.config_changed()

    def remove_edge(self, u, v):
        super(DependencyGraph, self).remove_edge(u, v)
        self.config_changed()

    def remove_edges_from(self, ebunch):
        super(DependencyGraph, self).remove_edges_from(ebunch)
        self.config_changed()


def find_related_pseudos(compgraph, nodes):
    """Return a set of pseudocomponent nodes not driver related and are
    attached to the given set of component nodes.
    """

    pseudos = set()

    for node in nodes:
        for upcomp in compgraph.predecessors_iter(node):
            if is_non_driver_pseudo_node(compgraph, upcomp):
                pseudos.add(upcomp)
        for dwncomp in compgraph.successors_iter(node):
            if is_non_driver_pseudo_node(compgraph, dwncomp):
                pseudos.add(dwncomp)

    return list(pseudos)

def find_all_connecting(graph, start, end):
    """Return the set of all component nodes along all paths
    between start and end. The start and end nodes are included
    in the set if they're connected.
    """
    if start == end:
        return set()

    Gsucc = graph.succ
    Gpred = graph.pred

    fwdset = set()
    backset = set()

    tmplst = [end]
    while tmplst:
        node = tmplst.pop()
        if node in backset:
            continue
        backset.add(node)
        tmplst.extend(Gpred[node].keys())

    tmplst = [start]
    while tmplst:
        node = tmplst.pop()
        if node in fwdset:
            continue
        fwdset.add(node)
        tmplst.extend(Gsucc[node].keys())

    return fwdset.intersection(backset)

def _dfs_connections(G, source, visited, reverse=False):
    """Produce connections in a depth-first-search starting at source."""
    # Slightly modified version of the networkx function dfs_edges

    if reverse:
        neighbors = G.predecessors_iter
    else:
        neighbors = G.successors_iter

    stack = []
    if source in G:
        stack.append((source, neighbors(source)))
    while stack:
        parent, children = stack[-1]
        try:
            child = next(children)
            if reverse:
                tup = (child, parent)
            else:
                tup = (parent, child)
            if tup not in visited:
                yield tup
                visited.add(tup)
                stack.append((child, neighbors(child)))
        except StopIteration:
            stack.pop()

def _get_inner_edges(G, srcs, dests):
    """Return the full set of edges between the
    given sources and destinations.

    srcs: iter of (str or tuple of str)
        Starting var or subvar nodes

    dests: iter of str
        Ending var or subvar nodes

    """
    fwdset = set()
    backset = set()
    for node in dests:
        backset.update(_dfs_connections(G, node, visited=backset, reverse=True))

    for node in srcs:
        fwdset.update(_dfs_connections(G, node, visited=fwdset))

    return fwdset.intersection(backset)

def _get_inner_connections(G, srcs, dests):
    """Return the set of edges that are actual connections (conn==True)
    between the given sources and destinations.

    srcs: iter of (str or tuple of str)
        Starting var or subvar nodes

    dests: iter of str
        Ending var or subvar nodes

    """
    data = G.edge
    return [(u,v) for u,v in _get_inner_edges(G, srcs, dests) if 'conn' in data[u][v]]

def get_subdriver_graph(graph, inputs, outputs, wflow, full_fd=False):
    """Update the given graph to replace non-solver subdrivers with
    PseudoAssemblies and promote edges up from sub-Solvers.
    Returns a list of names of drivers that were replaced, the set of
    additional inputs from subsolvers, and the set of additional outputs
    from subsolvers.
    """
    # set of comps being used by the current driver, so that
    # subdriver PAs won't remove them from the graph
    using = set(wflow.get_names(full=True))

    inputs = list(flatten_list_of_iters(inputs))
    outputs = list(flatten_list_of_iters(outputs))

    fd_drivers = []
    xtra_inputs = set()
    xtra_outputs = set()
    for comp in wflow:
        if has_interface(comp, IDriver):
            # Solvers are absorbed into the top graph
            if has_interface(comp, ISolver):
                dg = comp.workflow.derivative_graph(inputs=inputs, outputs=outputs,
                                                    group_nondif=False)
                xtra_inputs.update(flatten_list_of_iters(dg.graph['inputs']))
                xtra_outputs.update(flatten_list_of_iters(dg.graph['outputs']))
                for u,v,data in dg.edges_iter(data=True):
                    graph.add_edge(u, v, attr_dict=data)
                for param in comp.list_param_targets():
                    graph.node[param]['solver_state'] = True

            # However, we finite-difference all other drivers.
            else:
                fd_drivers.append(comp)

    pa_list = []

    # for all non-solver subdrivers, replace them with a PA
    if fd_drivers and not full_fd:
        # only create a copy of the graph if we have non-solver subdrivers
        startgraph = graph.subgraph(graph.nodes_iter())
        for drv in fd_drivers:

            pa_list.append(_create_driver_PA(drv, startgraph,
                                             graph, inputs, outputs,
                                             wflow, using))

            # The parameters of other drivers can propagate to our expressions
            # via input-input connections. These are relevant, so save them.
            sub_params = drv.list_param_targets()
            pa_name = pa_list[-1].name
            xtra_inputs.update([to_PA_var(v, pa_name) for v in sub_params])

        for pa in pa_list:
            pa.clean_graph(startgraph, graph, using)

    # return the list of names of subdrivers that were
    # replaced with PAs, along with any subsolver states/resids
    return [d.name for d in fd_drivers], xtra_inputs, xtra_outputs

def _create_driver_PA(drv, startgraph, graph, inputs, outputs,
                      wflow, ancestor_using):
    """Creates a PsuedoAssembly in the given graph for the specified
    Driver.  It adds nodes/edges to the graph but doesn't remove anything.
    """
    needed = set([c.name for c in drv.iteration_set()])
    for cname in needed:
        if cname in graph and cname not in ancestor_using:
            graph.node[cname] = graph.node[cname].copy() # don't pollute other graphs with nondiff markers
            graph.node[cname]['differentiable'] = False

    # get any boundary vars referenced by parameters of the subdriver or
    # any of its subdrivers
    srcs, dests = drv.get_expr_var_depends(recurse=True)
    boundary_params = [v for v in dests if is_boundary_node(startgraph, v)]

    pa = PseudoAssembly('~'+drv.name, list(needed), startgraph, wflow,
                        drv_name=drv.name,
                        boundary_params=boundary_params)

    pa.add_to_graph(startgraph, graph,
                    excludes=ancestor_using-set([drv.name]))
    return pa

def _remove_ignored_derivs(graph):
    to_remove = [n for n,data in graph.nodes_iter(data=True) if data.get('deriv_ignore')]
    graph.remove_nodes_from(to_remove)

def _is_false(item):
    return not item

def _check_for_missing_derivs(scope, comps):
    ''' we have the edges that are actually needed for the derivatives, so
    check all of the corresponding components now to see if they are
    supplying the needed derivatives.'''
    remove = []
    vt_flattener = flatteners[VariableTree]
    for cname, vnames in comps.items():
        vnames = set(vnames) # vnames may have dups
        comps[cname] = vnames
        if cname is None or cname.startswith('~'):
            # skip boundary vars and pseudoassemblies
            continue
        comp = getattr(scope, cname)

        # Skip comp if we are forcing it to fd
        if getattr(comp, 'force_fd', False):
            continue

        if not has_interface(comp, IComponent): # filter out vartrees
            continue
        if has_interface(comp, IAssembly):

            # Assemblies need to call into provideJ so that we can determine
            # what derivatives are available. Note that boundary variables
            # that are unconnected on the interior need a missing_deriv_policy
            # of 'assume_zero' to calculate them as zero.
            dins = [k for k, v in comp.items(iotype='in', deriv_ignore=_is_false)] #comp.list_inputs()
            douts = [k for k, v in comp.items(iotype='out', deriv_ignore=_is_false)]#comp.list_outputs()
            comp.provideJ(dins, douts, check_only=True)
            dins, douts = list_deriv_vars(comp)
            # if inputs are vartrees and we have full vt connections inside, add
            # leaf nodes to our list
            for i,din in enumerate(dins[:]):
                obj = getattr(comp, din)
                if has_interface(obj, IVariableTree):
                    dins.extend([n for n,v in vt_flattener(din, obj)])
            for i,dout in enumerate(douts[:]):
                obj = getattr(comp, dout)
                if has_interface(obj, IVariableTree):
                    douts.extend([n for n,v in vt_flattener(dout, obj)])
        else:
            dins, douts = list_deriv_vars(comp)
            for name in chain(dins, douts):
                if not comp.contains(name):
                    raise RuntimeError("'%s' reports '%s' as a deriv var, but it doesn't exist." %
                                        (comp.get_pathname(), name))
        if len(dins) == 0 or len(douts) == 0:
            if hasattr(comp, 'provideJ'):
                raise RuntimeError("'%s' defines provideJ but doesn't provide input or output deriv vars" % comp.get_pathname())
            else:
                continue  # we'll finite difference this comp
        missing = []
        for name in vnames:
            if name not in dins and name not in douts:
                nname = name.split('[', 1)[0]
                if nname not in dins and nname not in douts and is_differentiable_var(nname.split('.',1)[0], comp):
                    missing.append(nname)
        if missing:
            if comp.missing_deriv_policy == 'error':
                raise RuntimeError("'%s' doesn't provide analytical derivatives %s"
                                   % (comp.get_pathname(), missing))
            elif comp.missing_deriv_policy == 'assume_zero':
                # remove the vars with zero derivatives
                comps[cname] = [n for n in vnames if n not in missing]
                remove.extend(['.'.join([cname, m]) for m in missing])

    return remove

def mod_for_derivs(graph, inputs, outputs, wflow, full_fd=False):
    """Adds needed nodes and connections to the given graph
    for use in derivative calculations.
    """
    #print "mod_for_derivs for %s" % wflow._parent.get_pathname()
    indct = {}
    inames = []
    onames = []

    scope = wflow.scope

    # We want our top level graph metadata to be stored in the copy, but not in the
    # parent, so make our own copy of the metadata dict.
    graph.graph = {}

    graph.graph['inputs'] = list(inputs)
    graph.graph['mapped_inputs'] = list(inputs)
    graph.graph['outputs'] = list(outputs)
    graph.graph['mapped_outputs'] = list(outputs)

    relevant = set()
    
    # add nodes for input parameters
    for i, varnames in enumerate(inputs):
        iname = '@in%d' % i
        inames.append(iname)
        graph.add_node(iname, var=True, iotype='in', valid=True)
        for varname in flatten_list_of_iters(varnames):
            base = graph.base_var(varname)
            relevant.add(base) # keep basevars around
            subvars = graph._all_child_vars(base)
            # does base have any full basevar connections?
            fulls = set(graph.successors(base)) - set(subvars)
            if varname not in graph: # should only happen for a subvar
                graph.add_node(varname, basevar=base,
                               iotype='in', valid=True)

            graph.add_edge(iname, varname, conn=True) 

            if varname in subvars:
                # make sure this subvar is connected to its base in the direction we need
                graph.add_edge(varname, base)
            elif fulls:
                # we have a full basevar connection, so we can get a subvar deriv
                # varname is a subvar, so need to connect to basevar
                tail = varname[len(base):]
                if '.' in base: # this is a component var
                    if varname != base:
                        graph.add_edge(varname, base)
                    for dest in fulls:
                        sub = dest+tail
                        if not is_comp_node(graph, dest):
                            dbase = graph.base_var(dest)
                            if sub not in graph:
                                graph.add_node(sub, basevar=dbase, iotype='in', valid=True)
                                graph.add_edge(sub, dbase)
                            graph.add_edge(iname, sub)
                else: # it's a boundary var
                    graph.add_edge(base, varname)
                    for dest in fulls:
                        sub = dest+tail
                        dbase = graph.base_var(dest)
                        if sub not in graph:
                            graph.add_node(sub, basevar=dbase, iotype='in', valid=True)
                            graph.add_edge(sub, dbase)
                        graph.add_edge(varname, sub, conn=True)

            indct[varname] = iname

    # add nodes for desired outputs
    for i, varnames in enumerate(outputs):
        oname = '@out%d' % i
        onames.append(oname)
        graph.add_node(oname, var=True, iotype='out', valid=False)
        for varname in flatten_list_of_iters(varnames):
            if varname not in graph:
                graph.add_node(varname, basevar=graph.base_var(varname),
                               iotype='out', valid=False)
            graph.connect(None, varname, oname,
                          check=False, invalidate=False)

    rep_drivers, xtra_ins, xtra_outs = \
                   get_subdriver_graph(graph, inputs, outputs, wflow, full_fd)

    inames += list(xtra_ins)
    onames += list(xtra_outs)

    _remove_ignored_derivs(graph)

    _explode_vartrees(graph, scope)

    edges = _get_inner_edges(graph, inames, onames)

    edict = graph.edge
    conns = [(u,v) for u,v in edges if 'conn' in edict[u][v]]
    relevant.update([u for u,v in edges])
    relevant.update([v for u,v in edges])
    comps = partition_names_by_comp([u for u,v in conns])
    partition_names_by_comp([v for u,v in conns], compmap=comps)

    if full_fd == False:

        remove = _check_for_missing_derivs(scope, comps)

        if remove:
            remove = set(remove)

            # remove edges associated with missing derivs
            for u,v in graph.list_connections():
                if u in remove or v in remove:
                    graph.remove_edge(u, v)
            edges = _get_inner_edges(graph, inames, onames)
            relevant = set([u for u,v in edges])
            relevant.update([v for u,v in edges])
            conns = [(u,v) for u,v in edges if 'conn' in edict[u][v]]
            comps = partition_names_by_comp([u for u,v in conns])
            partition_names_by_comp([v for u,v in conns], compmap=comps)

    graph = graph.subgraph(relevant)

    # if we have destinations connected to subvars of a basevar
    # that's a destination of a parameter, then we have to
    # create a new edge from a new subvar of the parameter to
    # the original destination
    for vname, iname in indct.items():
        if is_basevar_node(graph, vname):
            for sub in graph.subvars(vname):
                for src, dest in graph._var_connections(sub,
                                                        direction='out'):
                    newsub = sub.replace(vname, iname, 1)
                    graph.add_subvar(newsub)
                    graph.add_edge(newsub, dest, conn=True)
                    graph.remove_edge(src, dest)

    to_remove = set()
    for src, dest in conns:
        if src.startswith('@in'):
            # move edges from input boundary nodes if they're
            # connected to an @in node
            base_dest = graph.base_var(dest)
            if is_boundary_node(graph, base_dest):
                if base_dest == dest: # dest is a basevar
                    newsrc = src
                else: # dest is a subvar
                    if graph.out_degree(base_dest) > 0:
                        for s in graph.successors(base_dest):
                            if graph.base_var(s) != base_dest:
                                newdest = dest.replace(base_dest, s, 1)
                                if newdest not in graph:
                                    graph.add_subvar(newdest)
                                graph.add_edge(src, newdest, conn=1)
                        newsrc = src
                    else:
                        newsrc = dest.replace(base_dest, graph.base_var(src), 1)
                    if newsrc not in graph:
                        graph.add_subvar(newsrc)
                for s, d in graph.edges_iter(dest):
                    graph.add_edge(newsrc, d, attr_dict=graph.edge[s][d])
                    to_remove.add((s,d))
                to_remove.add((src, dest))
            continue
        elif dest.startswith('@out') and not is_input_node(graph, src):
            # move edges from output boundary nodes if they're
            # connected to an @out node
            base_src = graph.base_var(src)
            if is_boundary_node(graph, base_src):
                for pred in graph.predecessors_iter(base_src):
                    if not base_src == graph.base_var(pred): # it's not one of our subvars
                        break
                else:
                    continue  # FIXME: not sure what to really do here.
                if base_src == src: # src is a basevar
                    newsrc = pred
                else: # src is a subvar
                    newsrc = src.replace(base_src, graph.base_var(pred), 1)
                    if newsrc not in graph:
                        graph.add_subvar(newsrc)
                graph.add_edge(newsrc, dest, conn=True)
                to_remove.add((src, dest))
            continue

        if is_input_node(graph, src):

            if is_basevar_node(graph, src):
                subs = graph._all_child_vars(src, direction='in')
                if not subs:
                    preds = graph.predecessors(src)
                    if preds:
                        newsrc = preds[0]
                        graph.add_edge(newsrc, dest, attr_dict=graph.edge[src][dest])
                        to_remove.add((src, dest))
                    continue
            else:
                subs = [src]

            # if we have an input source basevar that has multiple inputs (subvars)
            # then we have to create fake subvars at the destination to store
            # derivative related metadata
            for sub in subs:
                preds = graph.predecessors(sub)
                newsrc = None
                for p in preds:
                    if graph.base_var(sub) != p:
                        newsrc = p
                        break
                if newsrc is None:
                    continue
                new_target = sub.replace(src, dest, 1)
                if new_target not in graph:
                    graph.add_subvar(new_target)

                if dest in graph.edge[src]:
                    graph.add_edge(newsrc, new_target,
                                                  attr_dict=graph.edge[src][dest])
                else:
                    graph.add_edge(newsrc, new_target)

            to_remove.add((src, dest))

        else:
            base = graph.base_var(src)
            if is_boundary_node(graph, base):
                to_remove.add((src, dest))

    # get rid of any left over boundary connections
    for s, d in graph.list_connections():
        sbase = graph.base_var(s)
        dbase = graph.base_var(d)
        if is_boundary_node(graph, sbase) or is_boundary_node(graph, dbase):
            to_remove.add((s,d))

    graph.remove_edges_from(to_remove)

    return graph

def _explode_vartrees(graph, scope):
    # if full vartrees are connected, create subvar nodes for all of their
    # internal variables
    for edge in graph.list_connections():
        src, dest = edge
        srcnames = []
        destnames = []

        if '@' not in src and '[' not in src:

            if '~' in src:
                obj = scope.get(from_PA_var(src))
            else:
                obj = scope.get(src)
            if has_interface(obj, IVariableTree):
                srcnames = sorted([n for n,v in obj.items(recurse=True) if not has_interface(v, IVariableTree)])
                srcnames = ['.'.join([src, n]) for n in srcnames]

        if '@' not in dest and '[' not in dest:

            if '~' in dest:
                obj = scope.get(from_PA_var(dest))
            else:
                obj = scope.get(dest)

            if has_interface(obj, IVariableTree):
                destnames = sorted([n for n,v in obj.items(recurse=True) if not has_interface(v, IVariableTree)])
                destnames = ['.'.join([dest, n]) for n in destnames]

        if '@' not in src and '@' not in dest and (srcnames or destnames):
            _replace_full_vtree_conn(graph, src, srcnames,
                                            dest, destnames, scope)

    return graph

def _replace_full_vtree_conn(graph, src, srcnames, dest, destnames, scope):
    fail = False
    if len(srcnames) != len(destnames):
        fail = True
    else:
        for s, d in zip(srcnames, destnames):
            if s.split('.')[-1] != d.split('.')[-1]:
                fail = True
                break

    if fail:
        snames = [n.split('.',1)[1] for n in srcnames]
        dnames = [n.split('.',1)[1] for n in destnames]
        msg = "connected full vartrees '%s' and '%s' have non-matching leaf nodes" % (src,dest)
        missing_srcs = set(dnames).difference(snames)
        missing_dests = set(snames).difference(dnames)
        if missing_srcs:
            msg += ", variables %s are missing from %s" % (list(missing_srcs), src)
        if missing_dests:
            msg += ", variables %s are missing from %s" % (list(missing_dests), dest)
        raise ValueError(msg)

    graph.disconnect(src, dest)

    for s, d in zip(srcnames, destnames):
        graph.connect(scope, s, d, check=False,
                      invalidate=False)



def get_missing_derivs(obj, recurse=True):
    """Return a list of missing derivatives found in the given object.

    """
    if not has_interface(obj, IComponent):
        raise RuntimeError("Given object is not a Component")

    vt_flattener = flatteners[VariableTree]

    def _get_missing_derivs(comp, missing, finite_diffs, recurse):

        cins = comp.list_inputs()
        couts = comp.list_outputs()

        for i,cin in enumerate(cins[:]):
            obj = comp.get(cin)
            meta = comp.get_metadata(cin, 'framework_var')
            if has_interface(obj, IVariableTree):
                cins.extend([n for n,v in vt_flattener(cin, obj)])

        for i,cout in enumerate(couts[:]):
                    obj = comp.get(cout)
                    if has_interface(obj, IVariableTree) :
                        couts.extend([n for n,v in vt_flattener(cout, obj)])


        if has_interface(comp, IAssembly):
            # Assemblies need to call into provideJ so that we can determine
            # what derivatives are available.
            comp.provideJ(cins, couts, check_only=True)
            dins, douts = list_deriv_vars(comp)
            # if inputs are vartrees and we have full vt connections inside, add
            # leaf nodes to our list
            for i,din in enumerate(dins[:]):
                obj = comp.get(din)
                if has_interface(obj, IVariableTree):
                    dins.extend([n for n,v in vt_flattener(din, obj)])
            for i,dout in enumerate(douts[:]):
                obj = comp.get(dout)
                if has_interface(obj, IVariableTree):
                    douts.extend([n for n,v in vt_flattener(dout, obj)])

            if recurse:
                for cname in comp.list_containers():
                    ccomp = getattr(comp, cname)
                    if has_interface(ccomp, IComponent):
                        _get_missing_derivs(ccomp, missing, finite_diffs, recurse)

        else:
            dins, douts = list_deriv_vars(comp)

            for name in chain(dins, douts):
                if not comp.contains(name):
                    raise RuntimeError("'%s' reports '%s' as a deriv var, but it doesn't exist." %
                                        (comp.get_pathname(), name))

            for i,din in enumerate(dins[:]):
                obj = comp.get(din)
                if has_interface(obj, IVariableTree):
                    dins.extend([n for n,v in vt_flattener(din, obj)])
            for i,dout in enumerate(douts[:]):
                obj = comp.get(dout)
                if has_interface(obj, IVariableTree):
                    douts.extend([n for n,v in vt_flattener(dout, obj)])

        if (len(dins) == 0 or len(douts) == 0) and comp.parent:
            if hasattr(comp, 'provideJ'):
                raise RuntimeError("'%s' defines provideJ but doesn't provide input or output deriv vars" % comp.get_pathname())
            else:
                finite_diffs.append(comp.get_pathname())
                return

        for name in chain(cins, couts):
            obj = comp.get(name)
            if has_interface(obj, IVariableTree): #can never be a whole vartree, only the children
                continue

            base_name, _, index = name.partition("[")
            if index:
                if base_name in dins or base_name in douts :
                    continue


            if name not in dins and name not in douts and is_differentiable_var(name, comp):
                missing.append('.'.join([comp.get_pathname(), name]))



    missing = []
    finite_diffs = []

    _get_missing_derivs(obj, missing, finite_diffs, recurse)

    return missing, finite_diffs

