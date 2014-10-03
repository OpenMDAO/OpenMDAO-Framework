import sys
from collections import deque
from itertools import chain
from ordereddict import OrderedDict
from functools import cmp_to_key

import networkx as nx
from networkx.algorithms.dag import is_directed_acyclic_graph
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IVariableTree, \
                                     IImplicitComponent, ISolver, \
                                     IAssembly, IComponent
from openmdao.main.exceptions import NoFlatError
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.array_helpers import is_differentiable_var, is_differentiable_val, flattened_size
from openmdao.main.case import flatteners
from openmdao.main.vartree import VariableTree
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
    return [x for x in seq if x not in seen and not sadd(x)]


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
#   fake    used to designate subvar nodes that are essentially metadata placeholders and
#           are not really part of the dataflow
#
# EDGES:
#   conn    means that the edge is a connection that was specified
#           by calling connect()
#   fake    edge is artificial and doesn't represent a real data connection


# NODE selectors

def is_input_node(graph, node):
    if graph.node[node].get('iotype') == 'in':
        return True

def is_input_base_node(graph, node):
    return graph.node[node].get('iotype') == 'in' and \
                'basevar' not in graph.node[node]

def is_output_node(graph, node):
    if graph.node[node].get('iotype') == 'out':
        return True

def is_output_base_node(graph, node):
    return graph.node[node].get('iotype') == 'out' and \
                'basevar' not in graph.node[node]

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
    return 'var' in graph.node.get(node, '')

def is_basevar_node(graph, node):
    """Returns True if this node represents an
    actual input or output variable.
    """
    return 'var' in graph.node.get(node, '') and \
             'basevar' not in graph.node.get(node, '')

def is_subvar_node(graph, node):
    """Returns True if this node represents some
    subdivision of an input or output variable,
    e.g., an array index -   comp_1.x[2]
    """
    return 'basevar' in graph.node.get(node, '')

def is_fake_node(graph, node):
    return 'fake' in graph.node.get(node, '')

def is_pseudo_node(graph, node):
    return 'pseudo' in graph.node.get(node, '')

def is_objective_node(graph, node):
    return graph.node[node].get('pseudo') == 'objective'

def is_param_node(graph, node):
    return 'param' in graph.node.get(node, '')

def is_non_driver_pseudo_node(graph, node):
    pseudo = graph.node[node].get('pseudo')
    return pseudo == 'units' or pseudo == 'multi_var_expr'

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False

def is_drv_connection(graph, src, dest, driver=True):
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

        base = parts[0]
        if base in self and 'var' in self.node[base]:
            return base

        return '.'.join(parts[:2])

    # def sever_edges(self, edges):
    #     """Temporarily remove the specified edges but save
    #     them and their metadata for later restoration.
    #     """
    #     # Note: This will NOT call config_changed(), and if
    #     # component_graph() has not been called since the last
    #     # config_changed, it WILL create a temporary new
    #     # component graph which will be overwritten by the original
    #     # one when unsever_edges() is called.
    #     if self._severed_edges:
    #         raise RuntimeError("only one set of severed edges is permitted")

    #     # save old stuff to restore later
    #     self._saved_comp_graph = self._component_graph
    #     self._saved_loops = self._loops

    #     self._severed_edges = list(edges)

    #     self._allow_config_changed = False
    #     try:
    #         for u,v in edges:
    #             self.disconnect(u, v)
    #     finally:
    #         self._allow_config_changed = True

    # def unsever_edges(self, scope):
    #     """Restore previously severed edges."""
    #     if not self._severed_edges:
    #         return

    #     self._allow_config_changed = False
    #     try:
    #         for u,v in self._severed_edges:
    #             self.connect(scope, u, v)
    #     finally:
    #         self._allow_config_changed = True

    #     self._severed_edges = []

    #     self._loops = self._saved_loops
    #     self._component_graph = self._saved_comp_graph

    def config_changed(self):
        if self._allow_config_changed:
            self._component_graph = None
            self._loops = None
            self._saved_loops = None
            self._saved_comp_graph = None
            self._chvars = {}
            self._bndryins = None
            self._bndryouts = None
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
        old_states = set([n for n in old_outs if self.node[self.base_var(n)]['iotype'] == 'state'])
        old_resids = set([n for n in old_outs if self.node[self.base_var(n)]['iotype'] == 'residual'])

        # remove states from old_ins
        old_ins -= old_states

        # removes states and residuals from old_outs
        old_outs -= old_states
        old_outs -= old_resids

        new_ins  = set(['.'.join((cname, n)) for n in child.list_inputs()])
        new_outs = set(['.'.join((cname, n)) for n in child.list_outputs()])

        if has_interface(child, IImplicitComponent):
            new_states = set(['.'.join((cname, n)) for n in child.list_states()])
            new_resids = set(['.'.join((cname, n)) for n in child.list_residuals()])
        else:
            new_states = set()
            new_resids = set()

        if adding:
            added_ins = new_ins - old_ins
            added_outs = new_outs - old_outs
            added_states = new_states - old_states
            added_resids = new_resids - old_resids

            # add new inputs/outputs/states/residuals to the graph
            self.add_nodes_from(added_ins,    var=True, iotype='in')
            self.add_nodes_from(added_outs,   var=True, iotype='out')
            self.add_nodes_from(added_states, var=True, iotype='state')
            self.add_nodes_from(added_resids, var=True, iotype='residual')

            # add edges from the variables to their parent component
            self.add_edges_from([(v,cname) for v in chain(added_ins, added_states)])
            self.add_edges_from([(cname,v) for v in chain(added_outs, added_states,
                                                          added_resids)])

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

        kwargs['comp'] = True
        self.add_node(cname, **kwargs)

        inputs  = ['.'.join((cname, v)) for v in obj.list_inputs()]
        outputs = ['.'.join((cname, v)) for v in obj.list_outputs()]

        self.add_nodes_from(inputs, var=True, iotype='in')
        self.add_nodes_from(outputs, var=True, iotype='out')

        self.add_edges_from([(v, cname) for v in inputs])
        self.add_edges_from([(cname, v) for v in outputs])

        if has_interface(obj, IImplicitComponent):
            states = ['.'.join((cname, v)) for v in obj.list_states()]
            resids = ['.'.join((cname, v)) for v in obj.list_residuals()]
            self.add_nodes_from(states, var=True, iotype='state')
            self.add_nodes_from(resids, var=True, iotype='residual')
            self.add_edges_from([(cname, v) for v in chain(states, resids)])
            self.add_edges_from([(v, cname) for v in states])

        self._update_graph_metadata(obj, cname,
                                    obj.list_inputs()+obj.list_outputs())

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
        kwargs['boundary'] = True
        self.add_node(name, **kwargs)

        self._update_graph_metadata(obj, '', (name,))

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.' or 'C1['.
        """
        nodes = self.find_prefixed_nodes([name])
        self.disconnect(name)
        if nodes:
            self.remove_nodes_from(nodes)

    def check_connect(self, srcpath, destpath):
        """Raise an exception if the specified destination is already
        connected.
        """
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
                check=True):
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
            if v not in self:
                raise RuntimeError("Can't find variable '%s' in graph." % v)
            elif not is_var_node(self, v):
                raise RuntimeError("'%s' is not a variable node" % v)

        # # path is a list of tuples of the form (var, basevar)
        # path = [(base_src, base_src)]

        # if srcpath != base_src:  # srcpath is a subvar

        #     path.append((srcpath, base_src))

        # if destpath != base_dest:  # destpath is a subvar
        #     path.append((destpath, base_dest))

        # path.append((base_dest, base_dest))

        # if check:
        #     self.check_connect(srcpath, destpath)

        # for i in range(len(path)):
        #     dest, base = path[i]
        #     if dest not in self:  # create a new subvar if it's not already there
        #         self.add_node(dest, basevar=base, **self.node[base])
        #     if i > 0:
        #         src = path[i-1][0]
        #         try:
        #             self[src][dest]
        #         except KeyError:
        #             self.add_edge(src, dest)

        # # mark the actual connection edge to distinguish it
        # # from other edges (for list_connections, etc.)
        # self.edge[srcpath][destpath]['conn'] = True

        # # create expression objects to handle setting of
        # # array indces, etc.
        # sexpr = ConnectedExprEvaluator(srcpath, scope=scope, getter='get_attr')
        # dexpr = ConnectedExprEvaluator(destpath, scope=scope, getter='get_attr', is_dest=True)

        # self.edge[srcpath][destpath]['sexpr'] = sexpr
        # self.edge[srcpath][destpath]['dexpr'] = dexpr
        added_nodes = []
        added_edges = []
        if srcpath == base_src:
            pass  # will create connection edge later
        else:  # srcpath is a subvar
            if srcpath not in self:
                self.add_node(srcpath, basevar=base_src, **self.node[base_src])
                added_nodes.append(srcpath)
            if self.node[base_src].get('boundary'):
                iotypes = ('in', 'state')
            else:
                iotypes = ('out', 'state', 'residual')
            if self.node[base_src]['iotype'] in iotypes:
                self.add_edge(base_src, srcpath)
                added_edges.append((base_src, srcpath))

        if destpath == base_dest:
            pass
        else:  # destpath is a subvar
            if destpath not in self:
                self.add_node(destpath, basevar=base_dest, **self.node[base_dest])
                added_nodes.append(destpath)
            if self.node[base_dest].get('boundary'):
                iotypes = ('out', 'state', 'residual')
            else:
                iotypes = ('in', 'state')
            if self.node[base_dest]['iotype'] in iotypes:
                self.add_edge(destpath, base_dest)
                added_edges.append((destpath, base_dest))

        if check:
            try:
                self.check_connect(srcpath, destpath)
            except Exception:
                e = sys.exc_info()
                self.remove_edges_from(added_edges)
                self.remove_nodes_from(added_nodes)
                raise e[0], e[1], e[2]

        # create expression objects to handle setting of
        # array indces, etc.
        sexpr = ConnectedExprEvaluator(srcpath, scope=scope, getter='get_attr')
        dexpr = ConnectedExprEvaluator(destpath, scope=scope, getter='get_attr', is_dest=True)

        self.add_edge(srcpath, destpath, conn=True, sexpr=sexpr, dexpr=dexpr)

    def add_subvar(self, subvar):
        """ Adds a subvar node to the graph, properly connecting
        it to its basevar.
        """
        base = self.base_var(subvar)
        if base not in self:
            raise RuntimeError("can't find basevar '%s' in graph" % base)
        elif subvar in self:
            # adding something that's already there
            return subvar

        self.add_node(subvar, basevar=base, **self.node[base])
        if is_boundary_node(self, base):
            u, v = base, subvar
        else: # it's a var of a child component
            u, v = subvar, base

        if is_input_node(self, base):
            self.add_edge(u, v)
        else:
            self.add_edge(v, u)

        return subvar

    def disconnect(self, srcpath, destpath=None):

        if destpath is None:
            if is_comp_node(self, srcpath):
                edges = self.edges(self.successors(srcpath))
                edges.extend(self.in_edges_iter(self.predecessors(srcpath)))

            elif is_subvar_node(self, srcpath):
                # for a single subvar, add all of its downstream
                # edges to the removal list. The
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

    def list_connections(self, show_passthrough=True):
        conns = self._conns.get(show_passthrough)
        if conns is None:
            conns = list_data_connections(self)

            if show_passthrough is False:
                conns = [(u,v) for u,v in conns
                           if not ('.' in u or '.' in v)]

            self._conns[show_passthrough] = conns
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

    def find_prefixed_nodes(self, nodes, data=False):
        """Returns a list of nodes including the given nodes and
        any node that is prefixed with the name of any of those
        nodes. This is useful for retrieving all variable and
        subvar nodes associated with a component node.
        """

        full = []
        for node in nodes:
            if node in self:
                full.extend(self._all_child_vars(node))
                full.append(node)

        if data:
            sn = self.node
            return [(n, sn[n]) for n in full]
        else:
            return full

    def full_subgraph(self, nodes):
        """Returns the subgraph specified by the given nodes and any
        variable or subvar nodes belonging to those nodes.
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

    def get_boundary_inputs(self):
        """Returns inputs that are on the component boundary.
        """
        if self._bndryins is None:
            self._bndryins = [n for n in self.nodes_iter()
                                if is_boundary_node(self, n) and
                                   is_input_base_node(self, n)]
        return self._bndryins[:]

    def get_boundary_outputs(self):
        """Returns outputs that are on the component boundary.
        """
        if self._bndryouts is None:
            self._bndryouts = [n for n in self.nodes_iter()
                                if is_boundary_node(self, n) and
                                   is_output_base_node(self, n)]
        return self._bndryouts[:]

    def list_inputs(self, cname, connected=None):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.
        """
        if cname in self:
            if connected:
                return [n for n in self.pred[cname] if self.in_degree(n)]
            elif connected is False:
                return [n for n in self.pred[cname]
                                                if self.in_degree(n)==0]
            else:
                return self.pred[cname].keys()
        else:
            return []

    def list_input_outputs(self, cname):
        """Return a list of names of input or state nodes that are used
        as outputs.
        """
        return [n for n in self.pred[cname]
                             if self.out_degree(n)>1]

    def list_outputs(self, cname, connected=None):
        """Return a list of names of output, state or residual nodes for a component.
        If connected is True, return only connected outputs.
        """
        if connected:
            return [n for n in self.succ[cname]
                                            if self.out_degree(n)>0]
        elif connected is False:
            outs = self.succ[cname].keys()
            return [n for n in outs if not self._var_connections(n, 'out')]
        else:
            return self.succ[cname].keys()

    def component_graph(self):
        """Return a subgraph containing only Components
        and PseudoComponents and edges between them.
        """
        if self._component_graph is None:
            compset = set(all_comps(self))

            g = nx.DiGraph()

            for comp in compset:
                g.add_node(comp, self.node[comp].copy())

            for src, dest in self.list_connections():
                destcomp = dest.split('.', 1)[0]
                srccomp =  src.split('.', 1)[0]

                if srccomp in compset and destcomp in compset:
                    g.add_edge(srccomp, destcomp)

            self._component_graph = g

        return self._component_graph

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
        visited = set()
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

    # def update_boundary_outputs(self, scope):
    #     """Update destination vars on our boundary."""
    #     for out in self.get_boundary_outputs():
    #         self.update_destvar(scope, out)

    def update_destvar(self, scope, vname):
        """Update the value of the given variable in the
        given scope using upstream variables.
        """
        tup = self._dstvars.get(vname)
        if tup is None:
            sexprs = []
            dexprs = []
            for u,v,data in self.in_edges_iter(vname, data=True):
                if 'conn' in data:
                    dexprs.append(data['dexpr'])
                    sexprs.append(data['sexpr'])
                else:
                    for uu,vv,ddata in self.in_edges_iter(u, data=True):
                        if 'conn' in ddata:
                            dexprs.append(ddata['dexpr'])
                            sexprs.append(ddata['sexpr'])
            self._dstvars[vname] = (sexprs, dexprs)
        else:
            sexprs, dexprs = tup

        try:
            for sexpr, dexpr in zip(sexprs, dexprs):
                dexpr.set(sexpr.evaluate(scope=scope), scope=scope)
        except Exception as err:
            raise err.__class__("cannot set '%s' from '%s': %s" %
                                 (dexpr.text, sexpr.text, str(err)))

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

    def prune_unconnected_vars(self):
        """Remove unconnected variable nodes"""
        conns = self.list_connections()
        conns.extend([(u,v) for u,v in list_driver_connections(self)])
        convars = set([u for u,v in conns])
        convars.update([v for u,v in conns])
        convars.update([self.base_var(v) for v in convars])
        to_remove = [v for v in self.nodes_iter()
                         if v not in convars and is_var_node(self,v)]
        self.remove_nodes_from(to_remove)

    # The following group of methods are overridden so we can
    # call config_changed when the graph structure is modified
    # in any way.

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


def find_related_pseudos(depgraph, nodes):
    """Return a set of pseudocomponent nodes not driver related and are
    attached to the given set of component nodes.
    """

    pseudos = set()
    compgraph = depgraph.component_graph()

    for node in nodes:
        for upcomp in compgraph.predecessors_iter(node):
            if is_non_driver_pseudo_node(compgraph, upcomp):
                pseudos.add(upcomp)
        for dwncomp in compgraph.successors_iter(node):
            if is_non_driver_pseudo_node(compgraph, dwncomp):
                # FIXME: normally successor pseudocomps are ignored, but
                # if they connect to a boundary variable on the Assembly,
                # they'll never get evaluated unless they're in a workflow somewhere.
                # It may be better to leave them out of the workflow and just
                # add something to Assembly to have it evaluate them when updating
                # its boundary vars
                for dnode in depgraph.successors_iter(dwncomp+'.out0'):
                    if is_boundary_node(depgraph, dnode):
                        pseudos.add(dwncomp)
                        break

    return list(pseudos)

def find_all_connecting(graph, start, end):
    """Return the set of all nodes along all paths
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

                # All this stuff here is so that we can exclude irrelevant
                # solvers (i.e., they don't show up between our inputs
                # and outputs.)

                dg_exp = comp.workflow.derivative_graph(inputs=inputs,
                                                        outputs=outputs,
                                                        group_nondif=False,
                                                        add_implicit=False)

                dg_base = comp.workflow.derivative_graph(inputs=None,
                                                         outputs=None,
                                                         group_nondif=False,
                                                         add_implicit=True)

                subcomps = set([key.partition('.')[0] for key in dg_base.node])
                combocomps = set([key.partition('.')[0] for key in dg_exp.node])

                if len(subcomps.intersection(combocomps)) < 1:
                    continue

                dg = comp.workflow.derivative_graph(inputs=inputs,
                                                    outputs=outputs,
                                                    group_nondif=False,
                                                    add_implicit=True)

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

            if not hasattr(drv, 'list_param_targets'):
                continue

            # The parameters of other drivers can propagate to our expressions
            # via input-input connections. These are relevant, so save them.
            sub_params = drv.list_param_targets()
            pa_name = pa_list[-1].name
            sub_param_inputs = [to_PA_var(v, pa_name) for v in sub_params]
            xtra_outputs.update(sub_param_inputs)

            # Our parameter inputs are outputs to the outer drivers, so reverse the
            # connection direction here.
            for param in sub_param_inputs:
                graph.add_edge(pa_name, param)
                graph.node[param]['iotype'] = 'out'

                # Also gotta reverse basevar connectionss if we are sub
                if is_subvar_node(graph, param):
                    base_param = graph.base_var(param)
                    graph.add_edge(pa_name, base_param)
                    graph.add_edge(base_param, param)
                    graph.node[base_param]['iotype'] = 'out'

        for pa in pa_list:
            pa.clean_graph(startgraph, graph, using)

    # return the list of names of subdrivers that were
    # replaced with PAs, along with any subsolver states/resids
    return [d.name for d in fd_drivers], xtra_inputs, xtra_outputs

def _remove_ignored_derivs(graph):
    to_remove = [n for n, data in graph.nodes_iter(data=True) if data.get('deriv_ignore')]
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
                remove.extend(['.'.join((cname, m)) for m in missing])

    return remove

def get_missing_derivs(obj, recurse=True):
    """Return a list of missing derivatives found in the given
    object.
    """
    if not has_interface(obj, IComponent):
        raise RuntimeError("Given object is not a Component")

    vt_flattener = flatteners[VariableTree]

    def _get_missing_derivs(comp, missing, finite_diffs, recurse):

        cins = comp.list_inputs()
        couts = comp.list_outputs()

        for i,cin in enumerate(cins[:]):
            obj = comp.get(cin)
            #meta = comp.get_metadata(cin, 'framework_var')
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
                if base_name in dins or base_name in douts:
                    continue


            if name not in dins and name not in douts and is_differentiable_var(name, comp):
                missing.append('.'.join((comp.get_pathname(), name)))

    missing = []
    finite_diffs = []

    _get_missing_derivs(obj, missing, finite_diffs, recurse)

    return missing, finite_diffs

def break_cycles(graph):
    """Breaks up a cyclic graph and returns a list of severed
    edges. The severed edges list
    is a list of tuples of the form [(u,v,metadata), ...]
    """
    severed_edges = []

    if hasattr(graph, 'list_connections'):
        conns = set(graph.list_connections())
    else:
        conns = graph.edges()

    while not is_directed_acyclic_graph(graph):
        strong = strongly_connected_components(graph)
        if not strong or len(strong[0]) == 1:
            return []

        # look at only one component at a time
        strong = strong[0]

        # Break one edge of the loop.
        # For now, just break the first edge.
        # TODO: smarter ways to choose edge to break.
        for i in range(1,len(strong)):
            u,v = strong[i-1], strong[i]
            if (u,v) in conns:
                meta = graph[u][v].copy()
                graph.remove_edge(u, v)
                severed_edges.append((u,v,meta))
                break

    return severed_edges

def get_edge_boundary(g, nodes):
    """Returns a tuple of the form (in_edges, out_edges),
    where in_edges and out_edges are boundary
    edges between the nodes and the rest of the full graph.
    """
    others = set(g.nodes_iter()).difference(nodes)
    out_edges = nx.edge_boundary(g, nodes)
    in_edges = nx.edge_boundary(g, others)

    return in_edges, out_edges

def get_node_boundary(g, nodes):
    """Returns a tuple of the form (in_nodes, out_nodes),
    where in_nodes and out_nodes are boundary
    nodes between the given nodes and the rest of the full graph.
    """
    ins, outs = get_edge_boundary(g, nodes)

    return set([u for u,v in ins]), set([v for u,v in outs])

def collapse_nodes(g, collapsed_name, nodes, remove=True):
    """Collapse the given set of nodes into a single
    node with the specified name.
    """
    in_edges, out_edges = \
                  get_edge_boundary(g, nodes)

    # create new connections to collapsed node
    for u,v in in_edges:
        if u != collapsed_name:
            g.add_edge(u, collapsed_name)
            # create our own copy of edge metadata
            g[u][collapsed_name] = g[u][v].copy()

    for u,v in out_edges:
        if v != collapsed_name:
            g.add_edge(collapsed_name, v)
            # create our own copy of edge metadata
            g[collapsed_name][v] = g[u][v].copy()

    if remove:
        g.remove_nodes_from(nodes)

    return in_edges, out_edges

def collapse_driver(g, driver, excludes=()):
    """For the given driver object, collapse the
    driver's iteration set nodes into a single driver
    system node.
    """
    nodes = driver.get_full_nodeset()
    nodes.remove(driver.name)
    nodes = [n for n in nodes
                if n not in excludes]

    return collapse_nodes(g, driver.name, nodes)

def internal_nodes(g, comps):
    """Returns a set of nodes containing the given component
    nodes, plus any variable nodes between them.
    """
    nodes = set(comps)
    for comp1 in comps:
        for comp2 in comps:
            if comp1 != comp2:
                outs1 = set([v for u,v in g.edges_iter(comp1)])
                ins2 = set([u for u,v in g.in_edges_iter(comp2)])
                nodes.update(outs1.intersection(ins2))

    return nodes

def transitive_closure(g):
    """Return a set of edge tuples where the
    existence of a tuple (u,v) means that v depends
    on u, either directly or indirectly.  Note that this will
    be slow for large dense graphs.
    """
    edges = set()
    dfs = nx.dfs_edges

    for node in g.nodes_iter():
        edges.update([(node, v) for u,v in dfs(g, node)])

    return edges

# this could be optimized a bit for speed, but we only use it
# for small graphs, so performance isn't an issue
def gsort(deps, names):
    """Return a sorted version of the given names
    iterator, based on dependency specified by the
    given set of dependencies.
    """

    def gorder(n1, n2):
        if (n1,n2) in deps:
            return -1
        elif (n2,n1) in deps:
            return 1
        return 0

    # get all names that actually have a graph dependency
    depnames = set([u for u,v in deps])
    depnames.update([v for u,v in deps])

    ordered = [n for n in names if n in depnames]

    # get the sorted list of names with dependencies. Note that this
    # is a stable sort, so original order of the list will be
    # preserved unless there's a dependency violation.
    sortlist = sorted(ordered, key=cmp_to_key(gorder))

    # reverse the list so we can pop from it below
    rev = sortlist[::-1]

    # now take the names without dependencies and insert them in the
    # proper location in the list.  Since they have no dependencies,
    # they can be inserted anywhere in the list without messing up
    # the sort

    final = [None]*len(names)
    for i,n in enumerate(names):
        if n in depnames:
            final[i] = rev.pop()
        else:
            final[i] = n

    return final

def list_data_connections(graph):
    """Return all edges that are data connections"""
    return [(u,v) for u,v,data in graph.edges_iter(data=True)
                      if data.get('conn')]

def list_driver_connections(graph, driver=True):
    """Return all edges that are driver connections, i.e.,
    (params, objectives,constraints).  If driver contains the
    name of a specific driver, then only return connections for
    that driver.
    """
    if driver is True:
        return [(u,v) for u,v,data in graph.edges_iter(data=True)
                            if data.get('drv_conn')]
    else:
        return [(u,v) for u,v,data in graph.edges_iter(data=True)
                            if data.get('drv_conn')==driver]

def _add_collapsed_node(g, src, dests):
    """Adds new collapsed node which collapses a source and any
    destinations of that source, and adds any new edges due to
    collapsing the nodes.

    Does NOT remove the old nodes.
    """
    dests = list(dests)
    if '@' in src: # src is a driver node
        meta = g.node[dests[0]]
        newname = (dests[0], tuple(dests))
        src = src.split('@')[0]
    else:
        meta = g.node[src]

        for i, dest in enumerate(dests):
            if '@' in dest: # dest is a driver node
                newdests = dests[:]
                newdests[i] = src
                newname = (src, tuple(newdests))
                dests[i] = dest.split('@')[0]
                break
            if 'boundary' in g.node[dest]:
                meta['boundary'] = True
        else:
            newname = (src, tuple(dests))

    # if src is an input, we need to put src on the dest side so it
    # will receive scatters
    if g.node[src].get('iotype') in ('in', 'state'):
        if src not in dests:
            dests.append(src)
            newname = (src, tuple(dests))

    g.add_node(newname, meta.copy())

    # cname = src.split('.', 1)[0]
    # if is_comp_node(g, cname):
    #     ## edge goes the other way if src is actually an input
    #     #if g.node[src].get('iotype') == 'in':
    #         #g.add_edge(newname, cname)
    #     #else:
    #     g.add_edge(cname, newname)

    # for dest in dests:
    #     cname = dest.split('.', 1)[0]
    #     if is_comp_node(g, cname):
    #         g.add_edge(newname, cname)

    # now wire up the new node
    collapse_nodes(g, newname, [newname[0]]+list(newname[1]), remove=False)

    # and make sure its source, dests are components
    for p in g.predecessors(newname):
        cname = p.split('.', 1)[0]
        g.remove_edge(p, newname)
        if g.node[cname].get('comp'):
            g.add_edge(cname, newname)

    for s in g.successors(newname):
        cname = s.split('.', 1)[0]
        g.remove_edge(newname, s)
        if g.node[cname].get('comp'):
             g.add_edge(newname, cname)

def all_comps(g):
    """Returns a list of all component and PseudoComponent
    nodes.
    """
    return [n for n in g.nodes_iter() if is_comp_node(g, n)]

def collapse_connections(orig_graph):
    """Returns a new graph with each variable
    connection collapsed into a single node.  Any connections
    where inputs are sources are rerouted to their true
    source, if there is one.
    """
    src2dests = {}
    dest2src = {}

    g = nx.DiGraph(orig_graph)
    #g = orig_graph.subgraph(orig_graph.nodes_iter())

    conns = list_data_connections(g)
    drvconns = list_driver_connections(g)

    # mark all connected vars for removal since they're being collapsed
    to_remove = set([u for u,v in conns])
    to_remove.update([v for u,v in conns])
    to_remove.update([u for u,v in drvconns if not is_driver_node(g, u)])
    to_remove.update([v for u,v in drvconns if not is_driver_node(g, v)])

    # temporarily rename drivers in driver connections to include
    # the name of the connected var
    for i,(u,v) in enumerate(drvconns):
        if is_driver_node(g, u):
            drvconns[i] = ('%s@%s' % (u,v), v)
            dest2src[v] = drvconns[i][0]
        else:
            drvconns[i] = (u, '%s@%s' % (v,u))

    for u,v in drvconns:
        src2dests.setdefault(u, set()).add(v)

    for u,v in conns:
        src2dests.setdefault(u, set()).add(v)
        dest2src[v] = u

    # reroute inputs that are also sources
    while True:
        size = len(src2dests)
        for src, dests in src2dests.items():
            if src in dest2src:
                truesrc = dest2src[src]
                src2dests[truesrc].update(dests)
                for d in dests:
                    if d in dest2src:
                        dest2src[d] = truesrc
                del src2dests[src]

        # if no nodes re-routed, stop
        if len(src2dests) == size:
            break

    for src, dests in src2dests.items():
        _add_collapsed_node(g, src, dests)

    g.remove_nodes_from(to_remove)

    return g

def _find_in_meta(g, node, meta_name):
    vals = []
    if isinstance(node, basestring):
        v = g.node[node].get(meta_name)
        if v:
            vals.append(v)
        return vals

    for name in [node[0]]+list(node[1]):
        meta = g.node.get(name)
        if meta:
            v = meta.get(meta_name)
            if v:
                vals.append(v)
    return vals

def prune_reduced_graph(orig_g, g, keep):
    """Remove all unconnected vars that are not states."""
    to_remove = []

    for node, data in g.nodes_iter(data=True):
        if node in keep:
            continue

        # # don't prune states even if they're not connected
        # if 'state' in _find_in_meta(orig_g, node, 'iotype'):
        #     continue

        # don't prune components
        if data.get('comp'):
            continue

        indeg = g.in_degree(node)
        outdeg = g.out_degree(node)

        # keep all nodes between two other nodes
        if indeg > 0 and outdeg > 0:
            continue

        # keep boundary vars that only connect on one side
        if (indeg > 0 or outdeg > 0): # and _find_in_meta(orig_g, node, 'boundary'):
            if (node[0],) != node[1]:
                continue

        to_remove.append(node)

    g.remove_nodes_from(to_remove)

def vars2tuples(orig_g, g):
    """convert all var nodes to tuple form"""
    varmap = {}
    for node, data in orig_g.nodes_iter(data=True):
        if 'comp' not in data and node in g and isinstance(node, basestring):
            varmap[node] = (node, (node,))

    nx.relabel_nodes(g, varmap, copy=False)

def get_unconnected_vars(g):
    """Return a list of var nodes that
    are not between two (or more) component nodes.
    """
    ins = []
    outs = []
    for node, data in g.nodes_iter(data=True):
        if 'comp' not in data:
            if g.in_degree(node) == 0:
                ins.append(node)
            elif g.out_degree(node) == 0:
                outs.append(node)
    return ins, outs

def map_collapsed_nodes(g):
    """Return a dict of simple names to their collapsed node."""

    name2collapsed = {}
    for node in g.nodes_iter():
        if isinstance(node, basestring):
            name2collapsed[node] = node
        else:
            name2collapsed[node[0]] = node
            for n in node[1]:
                name2collapsed[n] = node

    return name2collapsed

def relevant_subgraph(g, srcs, dests, keep=()):
    """Return a subgraph of g that contains
    srcs and dests and all nodes connecting
    them.  Include any driver loops between them.
    """
    to_add = [s for s in srcs if s not in g]
    to_add.extend([d for d in dests if d not in g])

    if to_add:
        for node in to_add:
            base = g.base_var(node)
            if base == node:
                g.add_node(node, **g.node[base])
            else:
                g.add_node(node, basevar=base, **g.node[base])
            # connect it to its component
            if node in srcs:
                g.add_edge(node, node.split('.',1)[0])
            else:
                g.add_edge(node.split('.',1)[0], node)

    # create a 'fake' driver loop and grab
    # everything that's strongly connected to
    # that fake driver.
    g.add_node('@driver')
    for src in srcs:
        g.add_edge('@driver', src)
    for dest in dests:
        g.add_edge(dest, '@driver')

    for comps in strongly_connected_components(g):
        if '@driver' in comps:
            comps.remove('@driver')
            break

    # now remove the driver we added to g, which
    # also will remove all of the edges we added
    g.remove_node('@driver')

    comps = set(comps)

    # make sure we include srcs and dests even if they're
    # not connected
    comps.update(srcs)
    comps.update(dests)

    # keep any var we've been told to keep if its
    # parent component is relevant or if it's a component.
    for k in keep:
        if k.split('.', 1)[0] in comps:
            comps.add(k)
        elif k in g and g.node[k].get('comp'):
            comps.add(k)

    return g.subgraph(comps)

def simple_node_iter(nodes):
    """Return individual nodes from an iterator containing nodes and
    iterators of nodes.
    """
    if isinstance(nodes, basestring):
        nodes = (nodes,)

    allnodes = []
    for node in nodes:
        if isinstance(node, basestring):
            allnodes.append(node)
        else:
            allnodes.extend(simple_node_iter(node))
    return allnodes

def reduced2component(reduced):
    """Return a component graph based on
    the reduced graph (var edges collapsed)
    """
    cgraph = nx.DiGraph()
    for node, data in reduced.nodes_iter(data=True):
        if data.get('comp'):
            cgraph.add_node(node, **data)

    for node, data in reduced.nodes_iter(data=True):
        if not data.get('comp'):
            succ = reduced.successors(node)
            for p in reduced.predecessors(node):
                for s in succ:
                    if s in cgraph[p]: # edge exists
                        cgraph[p][s]['varconns'].append(node)
                    else:
                        cgraph.add_edge(p, s, varconns=[node])

    return cgraph

def get_reduced_subgraph(g, compnodes):
    """For a given set of component nodes in a
    reduced graph g, return the subgraph containing those
    compnodes and all variable nodes that are inputs or outputs
    to those nodes.
    """
    compset = set(compnodes)
    vnodes = set([])
    edges = g.edges()
    vnodes.update([u for u,v in edges if v in compset])
    vnodes.update([v for u,v in edges if u in compset])
    return g.subgraph(vnodes.union(compset))

def get_nondiff_groups(graph, scope):
    """Return a modified graph with connected
    nondifferentiable systems grouped together.
    """
    groups = []

    nondiff = [n for n,data in graph.nodes_iter(data=True)
                      if not data['system'].is_differentiable()]

    # If a connection is non-differentiable, so are its src and
    # target components.
    dgraph = scope._depgraph
    for edge in dgraph.list_connections():
        src, target = edge

        # passing non-diff stuff up and down and assy scope won't work anyway.
        #if src.startswith('@') or target.startswith('@') or '.' not in src:
        #    continue

        # Figure out if the src is differentiable
        try:
            flattened_size(src, scope.get(src), scope=scope)
        except NoFlatError:
            nondiff.append(src.split('.')[0])
            nondiff.append(target.split('.')[0])
            print "Non-differentiable connection: ", src, target

    # TODO: add pseudocomps to nondiff groups if they're
    # connected to nondiff systems on both sides

    # Groups any connected non-differentiable blocks. Each block is a
    # set of component names.
    sub = graph.subgraph(nondiff)

    # don't use to_undirected() to get an undirected graph from a directed graph,
    # as it does a deepcopy of all graph metadata. Instead, just use nx.Graph(digraph),
    # which creates an undirected graph using shallow copies of metadata.
    for inodes in nx.connected_components(nx.Graph(sub)):

        # Pull in any differentiable islands
        nodeset = set(inodes)
        for src in inodes:
            for targ in inodes:
                if src != targ:
                    nodeset.update(find_all_connecting(graph, src,
                                                       targ))
        groups.append(nodeset)

    return groups

def connect_subvars_to_comps(g):
    """Take all subvars and connect them directly to
    their parent component node rather than to their
    base var node.

    This should be called on a graph before edge
    collapsing.
    """
    # for node, data in g.nodes(data=True):
    #     if 'basevar' in data:
    #         base = data['basevar']
    #         if base in g:
    #             for u,v in g.edges(base):
    #                 if g.node[v].get('comp'):
    #                     g.add_edge(node, v)
    #             for u,v in g.in_edges(base):
    #                 if g.node[u].get('comp'):
    #                     g.add_edge(u, node)
    #             if base in g[node]:
    #                 g.remove_edge(node, base)
    #             if node in g[base]:
    #                 g.remove_edge(base, node)
    for node, data in g.nodes_iter(data=True):
        if 'comp' in data:
            comp = node
            for base_in in g.predecessors(node):
                for sub_in in g.predecessors(base_in):
                    if g.node[sub_in].get('basevar') == base_in:
                        g.add_edge(sub_in, comp)
                        g.remove_edge(sub_in, base_in)

            for base_out in g.successors(node):
                for sub_out in g.successors(base_out):
                    if g.node[sub_out].get('basevar') == base_out:
                        g.add_edge(comp, sub_out)
                        g.remove_edge(base_out, sub_out)


def simple_prune(g):
    """Remove all unconnected var nodes (except states).
    """
    for node, data in g.nodes(data=True):
        if 'comp' in data:
            for base_in in g.predecessors(node):
                if not (g.node[base_in].get('basevar') or g.node[base_in].get('iotype') == 'state'):
                    if g.in_degree(base_in) == 0 and g.out_degree(base_in) == 1:
                        g.remove_node(base_in)

            for base_out in g.successors(node):
                if not (g.node[base_out].get('basevar') or g.node[base_out].get('iotype') == 'state'):
                    if g.in_degree(base_out) == 1 and g.out_degree(base_out) == 0:
                        g.remove_node(base_out)

        elif 'boundary' in data:
            if g.degree(node) == 0:
                g.remove_node(node)


def prune_framework_vars(g):
    g.remove_nodes_from([n for n,data in g.nodes_iter(data=True)
                                    if 'framework_var' in data])

def fix_state_connections(scope, g):
    eval_only = set()
    for node, data in g.nodes_iter(data=True):
        if 'comp' in data:
            comp = getattr(scope, node, None)
            if comp and has_interface(comp, IImplicitComponent) and getattr(comp, 'eval_only'):
                    eval_only.add(node)

    for node, data in g.nodes_iter(data=True):
        if data.get('iotype') == 'state':
            cname = node.split('.', 1)[0]
            if cname in g and g.node[cname].get('comp'):
                if cname in eval_only: # tread states as inputs
                    if node in g[cname]:
                        g.remove_edge(cname, node)
                else:
                    if cname in g[node]:
                        g.remove_edge(node, cname)

