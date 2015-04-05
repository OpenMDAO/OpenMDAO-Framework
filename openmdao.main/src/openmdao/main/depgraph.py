import sys
from collections import deque
from itertools import chain

import networkx as nx
from networkx.algorithms.dag import is_directed_acyclic_graph
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IImplicitComponent
from openmdao.main.exceptions import NoFlatError
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.array_helpers import is_differentiable_val, flattened_size
from openmdao.main.vartree import VariableTree
from openmdao.util.graph import base_var, fix_single_tuple

# # to use as a quick check for exprs to avoid overhead of constructing an
# # ExprEvaluator
_exprchars = set('+-/*()&| %<>!')

# metadata to pull from component and place in graph
_metasrch = ['deriv_ignore', 'framework_var']

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

def is_non_driver_pseudo_node(graph, node):
    pseudo = graph.node[node].get('pseudo')
    return pseudo == 'units' or pseudo == 'multi_var_expr'

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False


class DGraphBase(nx.DiGraph):
    def __init__(self, *args, **kwargs):
        super(DGraphBase, self).__init__(*args, **kwargs)

    # The following group of methods are overridden so we can
    # call config_changed when the graph structure is modified
    # in any way.

    def add_node(self, n, attr_dict=None, **attr):
        super(DGraphBase, self).add_node(n, attr_dict=attr_dict,
                                         **attr)
        self.config_changed()

    def add_nodes_from(self, nodes, **attr):
        super(DGraphBase, self).add_nodes_from(nodes, **attr)
        self.config_changed()

    def remove_node(self, n):
        super(DGraphBase, self).remove_node(n)
        self.config_changed()

    def remove_nodes_from(self, nbunch):
        super(DGraphBase, self).remove_nodes_from(nbunch)
        self.config_changed()

    def add_edge(self, u, v, attr_dict=None, **attr):
        super(DGraphBase, self).add_edge(u, v,
                                         attr_dict=attr_dict, **attr)
        self.config_changed()

    def add_edges_from(self, ebunch, attr_dict=None, **attr):
        super(DGraphBase, self).add_edges_from(ebunch,
                                               attr_dict=attr_dict,
                                               **attr)
        self.config_changed()

    def remove_edge(self, u, v):
        super(DGraphBase, self).remove_edge(u, v)
        self.config_changed()

    def remove_edges_from(self, ebunch):
        super(DGraphBase, self).remove_edges_from(ebunch)
        self.config_changed()

    def in_degree(self, name):
        """The nx.DiGraph version returns an empty dict when it should
        return 0, so fix it here...
        """
        indeg = super(DGraphBase, self).in_degree(name)
        if isinstance(indeg, dict):
            return len(indeg)
        return indeg

    def out_degree(self, name):
        """The nx.DiGraph version returns an empty dict when it should
        return 0, so fix it here...
        """
        odeg = super(DGraphBase, self).out_degree(name)
        if isinstance(odeg, dict):
            return len(odeg)
        return odeg

    def degree(self, name):
        """The nx.DiGraph version returns an empty dict when it should
        return 0, so fix it here...
        """
        deg = super(DGraphBase, self).degree(name)
        if isinstance(deg, dict):
            return len(deg)
        return deg


class DependencyGraph(DGraphBase):
    def __init__(self, *args, **kwargs):
        super(DependencyGraph, self).__init__(*args, **kwargs)
        self._allow_config_changed = True
        self.config_changed()

    def config_changed(self):
        if self._allow_config_changed:
            self._component_graph = None
            self._chvars = {}
            self._bndryins = None
            self._bndryouts = None
            self._conns = {}

    def child_config_changed(self, child, adding=True, removing=True):
        """A child has changed its input lists and/or output lists,
        so we need to update the graph.
        """
        cname = child.name
        old_ins  = set(self.list_inputs(cname))
        old_outs = set(self.list_outputs(cname))
        old_states = set([n for n in old_outs if self.node[base_var(self, n)]['iotype'] == 'state'])
        old_resids = set([n for n in old_outs if self.node[base_var(self, n)]['iotype'] == 'residual'])

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
        dpbase = base_var(self, destpath)

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

        base_src  = base_var(self,srcpath)
        base_dest = base_var(self,destpath)

        for v in [base_src, base_dest]:
            if v not in self:
                raise RuntimeError("Can't find variable '%s' in graph." % v)
            elif not is_var_node(self, v):
                raise RuntimeError("'%s' is not a variable node" % v)

        added_nodes = []
        added_edges = []
        if srcpath == base_src:
            pass  # will create connection edge later
        else:  # srcpath is a subvar
            if srcpath not in self:
                self.add_subvar(srcpath)
                added_nodes.append(srcpath)
                #if self.in_degree(srcpath) == 0:
                    #self.add_edge(base_var(self, srcpath), srcpath)
                    ##self.remove_edge(srcpath, base_var(self, srcpath))

        if destpath == base_dest:
            pass
        else:  # destpath is a subvar
            if destpath not in self:
                self.add_subvar(destpath)
                added_nodes.append(destpath)

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
        sexpr = ConnectedExprEvaluator(srcpath, scope=scope, getter='get_attr_w_copy')
        dexpr = ConnectedExprEvaluator(destpath, scope=scope, getter='get_attr_w_copy', is_dest=True)

        self.add_edge(srcpath, destpath, conn=True, sexpr=sexpr, dexpr=dexpr)

    def add_subvar(self, subvar):
        """ Adds a subvar node to the graph, properly connecting
        it to its basevar.
        """
        base = base_var(self,subvar)
        if base not in self:
            raise NameError("can't find basevar '%s' in graph" % base)
        elif subvar in self:
            # adding something that's already there
            return subvar

        self.add_node(subvar, basevar=base, **self.node[base])
        if is_boundary_node(self, base):
            if is_input_node(self, base):
                self.add_edge(base, subvar)
            else:
                self.add_edge(subvar, base)
        else: # it's a var of a child component
            if is_input_node(self, base):
                self.add_edge(subvar, base)
            else:
                self.add_edge(base, subvar)

        return subvar

    def add_connected_subvar(self, subvar):
        """ Adds a subvar node to the graph, properly connecting
        it to its basevar and to anything that its basevar is connected to.
        """
        base = base_var(self,subvar)
        if base not in self:
            raise RuntimeError("can't find basevar '%s' in graph" % base)
        elif subvar in self:
            # adding something that's already there
            return subvar

        diff = subvar[len(base):]

        self.add_node(subvar, basevar=base, **self.node[base])
        if is_boundary_node(self, base):
            if is_input_node(self, base):
                for s in self.successors(base):
                    if s == subvar or base_var(self, s) == base:
                        continue
                    self.add_subvar(s+diff)
                    self.add_edge(subvar, s+diff, conn=True)
            else:
                for p in self.predecessors(base):
                    if p == subvar or base_var(self, p) == base:
                        continue
                    self.add_subvar(p+diff)
                    self.add_edge(p+diff, subvar, conn=True)
        else: # it's a var of a child component
            if is_input_node(self, base):
                for p in self.predecessors(base):
                    if p == subvar or base_var(self, p) == base:
                        continue
                    self.add_subvar(p+diff)
                    self.add_edge(p+diff, subvar, conn=True)
            else:
                for s in self.successors(base):
                    if s == subvar or base_var(self, s) == base:
                        continue
                    self.add_subvar(s+diff)
                    self.add_edge(subvar, s+diff, conn=True)

        return subvar

    def disconnect(self, srcpath, destpath=None):

        if destpath is None:
            if is_comp_node(self, srcpath):
                srcdot = srcpath + '.'
                edges = [(u,v) for u,v in self.edges_iter(self.successors(srcpath)) if u.startswith(srcdot)]
                edges.extend([(u,v) for u,v in self.in_edges_iter(self.predecessors(srcpath)) if v.startswith(srcdot)])
                edges.extend([(u,v) for u,v in self.edges_iter(srcpath) if not v.startswith(srcdot)])
                edges.extend([(u,v) for u,v in self.in_edges_iter(srcpath) if not u.startswith(srcdot)])

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

    def list_connections(self, drivers=True):
        conns = self._conns.get(drivers)
        if conns is None:
            conns = list_data_connections(self)
            if drivers:
                conns.extend(list_driver_connections(self))
            self._conns[drivers] = conns
        return conns[:]

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
        if reverse and isinstance(self, nx.DiGraph):
            neighbors = self.predecessors_iter
        else:
            neighbors = self.neighbors_iter
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

    def prune_unconnected_vars(self):
        """Remove unconnected variable nodes"""
        conns = self.list_connections()
        convars = set([u for u,v in conns])
        convars.update([v for u,v in conns])
        convars.update([base_var(self,v) for v in convars])
        to_remove = [v for v in self.nodes_iter()
                         if v not in convars and is_var_node(self,v)]
        self.remove_nodes_from(to_remove)

    def get_pruned(self):
        """Return a copy of the graph with all unconnected
        var nodes (except states) removed.
        """
        g = self.subgraph(self.nodes_iter())

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

        return g

    def add_param(self, drvname, param):

        if drvname:
            param = fix_single_tuple(param)
            if isinstance(param, tuple):
                self.add_edge(drvname, self.add_subvar(param[0]),
                              drv_conn=drvname)

                # now connect the first member of the param group as a
                # source for all of the other members
                for i,p in enumerate(param[1:]):
                    if self.has_edge(param[0], p):
                        self[param[0]][p]['drv_conn'] = drvname
                    else:
                        self.add_edge(param[0], self.add_subvar(p),
                                      conn=True, drv_conn_ext=drvname)
            else:
                self.add_edge(drvname, self.add_subvar(param),
                              drv_conn=drvname)

    def add_driver_input(self, drvname, vname):
        # use this to add driver connections from objectives
        # and constraints to a driver
        if drvname:
            self.add_edge(self.add_subvar(vname), drvname,
                          drv_conn=drvname)

    def _fix_state_connections(self, scope):
        eval_only = set()
        for node, data in self.nodes_iter(data=True):
            if 'comp' in data:
                comp = getattr(scope, node, None)
                if comp and has_interface(comp, IImplicitComponent) \
                     and getattr(comp, 'eval_only'):
                        eval_only.add(node)

        for node, data in self.nodes_iter(data=True):
            if data.get('iotype') == 'state':
                cname = node.split('.', 1)[0]
                if cname in self and self.node[cname].get('comp'):
                    if cname in eval_only: # treat states as inputs
                        if node in self[cname]:
                            self.remove_edge(cname, node)
                    else:
                        if cname in self[node]:
                            self.remove_edge(node, cname)
        return self

    def _connect_subvars_to_comps(self):
        """Take all subvars and connect them directly to
        their parent component node rather than to their
        base var node.
        """
        self._add_boundary_comps()

        for node, data in self.nodes_iter(data=True):
            if 'comp' in data:
                comp = node
                for base_in in self.predecessors(node):
                    for sub_in in self.predecessors(base_in):
                        if self.node[sub_in].get('basevar') == base_in:
                            self.add_edge(sub_in, comp)
                            self.remove_edge(sub_in, base_in)

                for base_out in self.successors(node):
                    for sub_out in self.successors(base_out):
                        if self.node[sub_out].get('basevar') == base_out:
                            self.add_edge(comp, sub_out)
                            self.remove_edge(base_out, sub_out)
                            #for subsucc in self.successors(sub_out):
                            #    sub_comp = subsucc.split('.', 1)[0]
                            #    if sub_comp in self and not self.has_edge(base_out, sub_comp):
                            #        self.add_edge(base_out, sub_comp, nodata=True)

        # get rid of fake boundary comps
        self.remove_nodes_from(['#in', '#out'])

    def relevant_subgraph(self, srcs, dests, keep=()):
        """Return a subgraph of g that contains
        srcs and dests and all nodes connecting
        them.  Include any driver loops between them.
        """
        to_add = [s for s in srcs if s not in self]
        to_add.extend([d for d in dests if d not in self])

        for node in to_add:
            base = base_var(self, node)
            if base == node:
                self.add_node(node, **self.node[base])
            else:
                self.add_node(node, basevar=base, **self.node[base])
            # connect it to its component
            if '.' in node: # it's a component var. connect to its component
                if node in srcs:
                    self.add_edge(node, node.split('.',1)[0])
                else:
                    self.add_edge(node.split('.',1)[0], node)
            elif base in self:
                if node in srcs:
                    for s in self.successors(base):
                        if base_var(self, s) == base:
                            continue
                        dest = s+node[len(base):]
                        if dest not in self:
                            self.add_node(dest, basevar=s, **self.node[s])
                            self.add_edge(dest, s)
                        self.add_edge(node, dest, conn=True)
                        self.add_edge(base, node)
                else:
                    for p in self.predecessors(base):
                        if base_var(self, p) == base:
                            continue
                        src = p+node[len(base):]
                        if src not in self:
                            self.add_node(src, basevar=p, **self.node[p])
                            self.add_edge(p, src)
                        self.add_edge(src, node, conn=True)
                        self.add_edge(node, base)

        # create a 'fake' driver loop and grab
        # everything that's strongly connected to
        # that fake driver.
        self.add_node('@driver')
        for src in srcs:
            self.add_edge('@driver', src)
        for dest in dests:
            self.add_edge(dest, '@driver')
        for u,v in self.list_connections():
            self.add_edge('@driver', u)
            self.add_edge(v, '@driver')
        for k in keep:
            self.add_edge('@driver', k)
            self.add_edge(k, '@driver')

        for comps in strongly_connected_components(self):
            if '@driver' in comps:
                comps.remove('@driver')
                break

        # now remove the driver we added to g, which
        # also will remove all of the edges we added
        self.remove_node('@driver')

        comps = set(comps)

        # make sure we include srcs and dests even if they're
        # not connected
        comps.update(srcs)
        comps.update(dests)

        return self.subgraph(comps)

    def collapse_connections(self):
        """Returns a new graph with each variable
        connection collapsed into a single node.  Any connections
        where inputs are sources are rerouted to their true
        source, if there is one.
        """
        src2dests = {}
        dest2src = {}

        g = CollapsedGraph(self)

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
            elif is_driver_node(g, v):
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

        # clean up any driver connections to params that have already been
        # included in a param group node
        dparams = {}
        for src, dests, in src2dests.items():
            if '@' in src:
                drv, var = src.split('@',1)
                dparams.setdefault(drv, set()).update([d for d in dests if d != var])

        for src, dests, in src2dests.items():
            if '@' in src:
                drv, var = src.split('@',1)
                if var in dparams[drv]:
                    del src2dests[src]

        for src, dests in src2dests.items():
            _add_collapsed_node(g, src, dests)

        g.remove_nodes_from(to_remove)

        return g

    def _add_boundary_comps(self):
        """Add fake boundary components and connect them to boundary
        basevars.
        """
        self.add_node('#in', comp=True)
        self.add_node('#out', comp=True)

        for node, data in self.nodes_iter(data=True):
            if 'boundary' in data:# and 'basevar' not in data:
                if data['iotype'] == 'in':
                    self.add_edge('#in', node)
                elif data['iotype'] == 'out':
                    self.add_edge(node, '#out')

        return self

    def _explode_vartrees(self, scope):
        """Given a depgraph, take all connected variable nodes corresponding
        to VariableTrees and replace them with a variable node for each
        variable in the VariableTree.
        """
        vtvars = {}
        for n in self:
            if '[' not in n: # we want vartrees only
                if isinstance(scope.get(n), VariableTree):
                    vtvars[n] = None

        conns = [(u,v) for u,v in self.list_connections(drivers=False) if u in vtvars]

        depgraph = self.subgraph(self.nodes_iter())

        # explode all vt nodes first
        for vt in vtvars:
            obj = scope.get(vt)
            vtvars[vt] = ['.'.join((vt, n.split('.',1)[1]))
                                     for n in obj.list_all_vars()]
            vtvars[vt].sort()
            for sub in vtvars[vt]:
                if sub not in depgraph:
                    depgraph.add_subvar(sub)

        vtconns = {}
        for u,v in conns:
            varlist = [vtvars[u], vtvars[v]]

            if len(varlist[0]) != len(varlist[1]):
                scope.raise_exception("connected vartrees '%s' and '%s' do not have the same variable list" %
                                      (u, v))

            for src, dest in zip(varlist[0], varlist[1]):
                if src.split('.')[-1] != dest.split('.')[-1]:
                    scope.raise_exception("variables '%s' and '%s' in vartree connection '%s' -> '%s' do not match" %
                                          (src, dest, u, v))

            vtconns[(u,v)] = varlist

        for (u,v), varlist in vtconns.items():
            ucomp = udestcomp = vcomp = None

            # see if there's a u component
            comp = u.split('.', 1)[0]
            if comp in depgraph and depgraph.node[comp].get('comp'):
                if comp in depgraph.predecessors(u):
                    ucomp = comp
                elif comp in depgraph.successors(u):  # handle input as output case
                    udestcomp = comp

            # see if there's a v component
            comp = v.split('.', 1)[0]
            if comp in depgraph and depgraph.node[comp].get('comp'):
                if comp in depgraph.successors(v):
                    vcomp = comp

            for src, dest in zip(varlist[0], varlist[1]):
                depgraph.add_edge(src, dest, conn=True)
                if ucomp:
                    depgraph.add_edge(ucomp, src)
                if udestcomp:
                    depgraph.add_edge(src, udestcomp)
                if vcomp:
                    depgraph.add_edge(dest, vcomp)

        return depgraph

    def _remove_vartrees(self, scope):
        """Remove all vartree nodes."""
        vtnodes = [n for n in self if scope.contains(n)
                     and isinstance(scope.get(n), VariableTree)]

        for vt in vtnodes:
            succ = self.successors(vt)
            pred = self.predecessors(vt)

            if '.' in vt:
                # connect subs to parent comp
                cname = vt.split('.', 1)[0]
                if cname in succ:
                    for p in pred:
                        if p != cname and base_var(self, p) == vt:
                            self.add_edge(p, cname)
                elif cname in pred:
                    for s in succ:
                        if s != cname and base_var(self, s) == vt:
                            self.add_edge(cname, s)

        self.remove_nodes_from(vtnodes)


class CollapsedGraph(DGraphBase):
    def __init__(self, *args, **kwargs):
        super(CollapsedGraph, self).__init__(*args, **kwargs)
        self._name2collapsed = {}

    def full_subgraph(self, compnodes):
        """For a given set of component nodes in a
        collapsed graph g, return the subgraph containing those
        compnodes and all variable nodes that are inputs or outputs
        to those nodes.
        """
        compset = set(compnodes)
        vnodes = set()
        edges = self.edges()
        vnodes.update([u for u,v in edges if v in compset])
        vnodes.update([v for u,v in edges if u in compset])
        return self.subgraph(vnodes.union(compset))

    def component_graph(self):
        """Return a component graph based on
        the reduced graph (var edges collapsed)
        """
        cgraph = nx.DiGraph()
        for node, data in self.nodes_iter(data=True):
            if data.get('comp'):
                cgraph.add_node(node, **data)

        for node, data in self.nodes_iter(data=True):
            if not data.get('comp'):
                succ = self.successors(node)
                for p in self.predecessors(node):
                    for s in succ:
                        if s in cgraph[p]: # edge exists
                            cgraph[p][s]['varconns'].append(node)
                        else:
                            cgraph.add_edge(p, s, varconns=[node])

        return cgraph

    def internal_nodes(self, comps, shared=False):
        """Returns a set of nodes containing the given component
        nodes, plus any variable nodes between them that are not
        connected to any external nodes. If shared is True, then
        variable nodes connected to the outside will be included.
        """
        nodes = set(comps)
        ins = set()
        outs = set()

        for comp in comps:
            if comp in self:
                outs.update(self.successors_iter(comp))
                ins.update(self.predecessors_iter(comp))
                # handle inputs that are also srcs
                for n in self.predecessors_iter(comp):
                    if self.out_degree(n) > 1:
                        outs.add(n)

        inner = outs.intersection(ins)

        if not shared:
            to_remove = set()
            for var in inner:
                if set(self.successors(var)).difference(nodes):
                    to_remove.add(var)
                if set(self.predecessors(var)).difference(nodes):
                    to_remove.add(var)

            inner = inner - to_remove

        nodes.update(inner)

        return nodes

    def collapsed(self, name):
        return self._name2collapsed[name]

    def map_collapsed_nodes(self):
        """Return a dict of simple names to their collapsed node."""
        name2collapsed = {}
        for node in self.nodes_iter():
            if isinstance(node, basestring):
                name2collapsed[node] = node
            else:
                name2collapsed[node[0]] = node
                for n in node[1]:
                    name2collapsed[n] = node

        self._name2collapsed = name2collapsed

        return name2collapsed

    def collapse_subdrivers(self, names, subdrivers):
        """collapse subdriver iteration sets into single nodes in a
        reduced graph.
        """
        itercomps = {}
        itercomps['#parent'] = set(names)

        for comp in subdrivers:
            cnodes = set([c.name for c in comp.iteration_set()])
            itercomps[comp.name] = cnodes

        to_remove = []
        for child_drv in subdrivers:
            excludes = set()
            for name, comps in itercomps.items():
                if name != child_drv.name:
                    for cname in comps:
                        if cname in itercomps[child_drv.name]:
                            excludes.add(cname)

            self.collapse_driver(child_drv, excludes)

            # locate any bidirectional edges to/from collapsed driver.
            # If the driver isn't the only input to a var, remove the
            # driver input to the var.
            dname = child_drv.name
            succ = self.successors(dname)
            for p in self.predecessors(dname):
                if p in succ:
                    if self.in_degree(p) > 1:
                        to_remove.append((dname, p))

        self.remove_edges_from(to_remove)

        # now remove any comps that are shared by subdrivers but are not found
        # in our workflow
        to_remove = set()
        for name, comps in itercomps.items():
            if name != '#parent':
                for comp in comps:
                    if comp not in itercomps['#parent']:
                        to_remove.add(comp)

        self.remove_nodes_from(to_remove)

    def collapse_driver(self, driver, excludes=()):
        """For the given driver object, collapse the
        driver's iteration set nodes into a single driver
        system node in a reduced graph.
        """
        excludes = set(excludes)
        excludes.add(driver.name)
        names = driver.get_full_nodeset()
        nodes = [n for n in self.internal_nodes(names)
                    if n not in excludes]

        collapse_nodes(self, driver.name, nodes)
        self.node[driver.name]['comp'] = True

    def prune(self, keep):
        """Remove all unconnected vars that are not in 'keep'."""
        to_remove = []

        for node, data in self.nodes_iter(data=True):
            if node in keep:
                continue

            # don't prune components
            if data.get('comp'):
                continue

            indeg = self.in_degree(node)
            outdeg = self.out_degree(node)

            # keep all nodes between two other nodes
            if indeg > 0 and outdeg > 0:
                continue

            # keep boundary vars that only connect on one side
            if (indeg > 0 or outdeg > 0):
                if (node[0],) != node[1]:
                    continue

            to_remove.append(node)

        self.remove_nodes_from(to_remove)

        return self

    def fix_duplicate_dests(self):
        """If a dest has multiple sources, there will be multiple
        varnodes for it in the reduced graph, so consolidate
        those.
        """
        dests2node = {}
        drivers = []
        for node, data in self.nodes_iter(data=True):
            if 'comp' not in data:
                if isinstance(node, tuple):
                    dests2node.setdefault(node[1], set()).add(node)
                else:
                    pass  # disconnected variable
            elif is_driver_node(self, node):
                drivers.append(node)

        for drv in drivers:
            for s in self.successors(drv):
                if isinstance(s, tuple) and len(dests2node[s[1]]) > 1 and s[0] == s[1][0] and s in self:
                    self.remove_node(s)

    def vars2tuples(self, orig_g):
        """convert all var nodes to tuple form"""
        varmap = {}
        for node, data in orig_g.nodes_iter(data=True):
            if 'comp' not in data and node in self and isinstance(node, basestring):
                varmap[node] = (node, (node,))

        nx.relabel_nodes(self, varmap, copy=False)

    def fix_dangling_vars(self, scope):
        """If there are any dangling var nodes left in the
        collapsed graph g, connect them to a var comp.
        """
        to_add = set()
        for node, data in self.nodes(data=True):
            if 'comp' not in data and 'iotype' in data:
                base = node[0].split('[', 1)[0]
                bvar = scope.name2collapsed.get(base)
                if self.in_degree(node) == 0:
                    if base in self and 'comp' in self.node[base]:
                        newname = base
                    elif bvar in self:
                        self.add_node(base, comp='invar')
                        newname = base
                    else:
                        if data['iotype'] == 'in':
                            newname = node[0]
                        else:
                            newname = '@'+node[0]
                        self.add_node(newname, comp='invar')

                    if not self.has_edge(node, newname) and (node, newname) not in to_add:
                        to_add.add((newname, node))

                if self.out_degree(node) == 0:
                    if base in self and 'comp' in self.node[base]:
                        newname = base
                    else:
                        if data['iotype'] == 'out':
                            newname = node[0]
                        else:
                            newname = '@'+node[0]
                        self.add_node(newname, comp='outvar')

                    if not self.has_edge(newname, node) and (newname, node) not in to_add:
                        to_add.add((node, newname))

        if to_add:
            self.add_edges_from(to_add)

    # def _connect_srcs_to_comps(self):
    #     for node, data in self.nodes_iter(data=True):
    #         if 'comp' not in data:
    #             comp = node[0].split('[', 1)[0].split('.', 1)[0]
    #             if comp in self and not self.has_edge(comp, node):
    #                 self.add_edge(comp, node)

    def config_changed(self):
        pass

    def _get_duped_varnodes(self):
        """Return any varnodes that share the same source. (this is a no-no)"""
        srcmap = {}
        allsrcs = set()
        for node, data in self.nodes_iter(data=True):
            if 'comp' not in data:
                if isinstance(node, tuple):
                    allsrcs.add(node[0])

        for node, data in self.nodes_iter(data=True):
            if 'comp' not in data:
                if isinstance(node, tuple):
                    srcmap.setdefault(node[0], set()).add(node)
                    # check dests for any sources.  this can happen with
                    # parameters
                    for dest in node[1]:
                        if dest in allsrcs:
                            srcmap.setdefault(dest, set()).add(node)

        for src, nodes in srcmap.items():
            if len(nodes) < 2:
                del srcmap[src]

        return srcmap

    def _consolidate_srcs(self):
        """ consolidate any varnodes with a common src."""
        d = self._get_duped_varnodes()

        newnodes = []
        for src, nodes in d.items():
            alldests = set()
            for n in nodes:
                alldests.update(n[1])
            if src in alldests:
                alldests.remove(src)
                alldests = [src]+sorted(alldests)
            else:
                alldests = sorted(alldests)
            newname = (src, tuple(alldests))
            newnodes.append((newname, merge_metadata(self, nodes), nodes))

        for newname, meta, nodes in newnodes:
            self.add_node(newname, **meta)
            collapse_nodes(self, newname, nodes)

        return self

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

def break_cycles(graph):
    """Breaks up a cyclic graph and returns a list of severed
    edges. The severed edges list
    is a list of tuples of the form [(u,v,metadata), ...]
    """
    severed_edges = []

    if hasattr(graph, 'list_connections'):
        conns = set(graph.list_connections(drivers=False))
    else:
        conns = graph.edges()

    while not is_directed_acyclic_graph(graph):
        strong = list(strongly_connected_components(graph))
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

def collapse_nodes(g, collapsed_name, nodes, remove=True, add=False):
    """Collapse the given set of nodes into a single
    node with the specified name. The node for the collapsed
    name must be preexisting unless add is True.
    """
    if add:
        if isinstance(nodes, basestring):
            newnode = nodes
        else:
            newnode = tuple(sorted(nodes))
        g.add_node(newnode, comp=True)

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
    system node. (this only works on component graphs)
    """
    nodes = driver.get_full_nodeset()
    nodes.remove(driver.name)
    nodes = [n for n in nodes
                if n not in excludes]

    return collapse_nodes(g, driver.name, nodes)

def gsort(g, names):
    """Return a sorted version of the given names
    iterator, based on dependency specified by the
    given directed graph.
    """
    if len(names) < 2:
        return names

    empty = set()
    nset = set(names)
    final = list(names)
    ups = {}

    for name in names:
        downs = [v for u,v in nx.dfs_edges(g, name) if v in nset]
        for d in downs:
            if d not in ups.get(name, empty):  # handle cycles
                ups.setdefault(d, set()).add(name)

    i = 0
    while True:
        tmp = final[:i]
        tset = set(tmp)
        unames = [f for f in final[i+1:] if f in ups.get(final[i],empty) and
                     f not in tset]
        if unames:
            tmp.extend(unames)
            tmp.extend([f for f in final[i:] if f not in unames])
        else:
            tmp.extend(final[i:])
            i += 1

        if i == len(names):
            break
        final = tmp

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
        src, vsrc = src.split('@', 1)
        meta = g.node[vsrc]
        newname = (vsrc, tuple(dests))
        drvsrc = src
    else:
        meta = g.node[src]
        drvsrc = None

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

    # now wire up the new node
    collapse_nodes(g, newname, set([newname[0]]+list(newname[1])), remove=False)

    to_add = []

    # and make sure its source, dests are components
    for p in g.predecessors(newname):
        cname = p.split('.', 1)[0]
        if cname not in g:
            continue
        pmeta = g[p][newname]
        cmeta = g[cname][newname] if g.has_edge(cname,newname) else {}
        g.remove_edge(p, newname)
        if g.node[cname].get('comp'):
            if p == cname or p in g[cname]:
                if 'drv_conn' in cmeta:
                    to_add.append((cname, newname, {'drv_conn':cmeta['drv_conn']}))
                elif 'drv_conn' in pmeta:
                    to_add.append((cname, newname, {'drv_conn':pmeta['drv_conn']}))
                elif drvsrc == p:
                    to_add.append((cname, newname, {'drv_conn':drvsrc}))
                elif not g.has_edge(cname, newname):
                    to_add.append((cname, newname, {}))

    for s in g.successors(newname):
        if isinstance(s, tuple):
            continue
        cname = s.split('.', 1)[0]
        pmeta = g[newname][s]
        cmeta = g[newname][cname] if g.has_edge(newname, cname) else {}
        g.remove_edge(newname, s)
        if g.node.get(cname,{}).get('comp'):
            if s == cname or cname in g[s]:
                if 'drv_conn' in cmeta:
                    to_add.append((newname, cname, {'drv_conn': cmeta['drv_conn']}))
                elif 'drv_conn' in pmeta:
                    to_add.append((newname, cname, {'drv_conn': pmeta['drv_conn']}))
                elif not g.has_edge(newname, cname):
                    to_add.append((newname, cname, {}))

    for u,v, meta in to_add:
        g.add_edge(u, v, **meta)


def all_comps(g):
    """Returns a list of all component and PseudoComponent
    nodes.
    """
    return [n for n in g.nodes_iter() if is_comp_node(g, n)]

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

def get_nondiff_groups(graph, cgraph, scope):
    """Return a modified graph with connected
    nondifferentiable systems grouped together.
    """
    groups = []

    nondiff = set([n for n,data in cgraph.nodes_iter(data=True)
                    if 'system' in data
                        and not data['system'].is_differentiable()])

    # If a connection is non-differentiable, so are its src and
    # target components.
    data = graph.node
    for src, target in graph.edges_iter():
        if 'comp' in data[src]:
            comp, var = src, target
        elif 'comp' in data[target]:
            var, comp = src, target
        else:
            raise RuntimeError("malformed graph in get_nondiff_groups()")

        # Ignore deriv-ignore
        if data[var].get('deriv_ignore') == True:
            continue

        # Can't include the containing assembly as a nondiff block.
        if data[var].get('boundary'):
            continue

        # Input-input connections don't matter
        if data[var].get('iotype') == 'in':
            continue

        # Figure out if the src is differentiable
        try:
            flattened_size(var[0], scope.get(var[0]), scope=scope)
        except NoFlatError:
            nondiff.add(comp)

    # TODO: add pseudocomps to nondiff groups if they're
    # connected to nondiff systems on both sides

    # Groups any connected non-differentiable blocks. Each block is a
    # set of component names.
    sub = cgraph.subgraph(nondiff)

    # don't use to_undirected() to get an undirected graph from a directed graph,
    # as it does a deepcopy of all graph metadata. Instead, just use nx.Graph(digraph),
    # which creates an undirected graph using shallow copies of metadata.
    for inodes in nx.connected_components(nx.Graph(sub)):

        # Pull in any differentiable islands
        nodeset = set(inodes)
        for src in inodes:
            for targ in inodes:
                if src != targ:
                    nodeset.update(find_all_connecting(cgraph, src,
                                                       targ))
        groups.append(nodeset)

    return groups

def merge_metadata(g, nodes):
    """Return a combined metadata dict for the given set of
    nodes.
    """
    meta = {}
    for n in nodes:
        m = g.node[n]
        meta.update(m)
        io = m.get('iotype')
        if io == 'state':
            meta['state'] = True
        elif io == 'residual':
            meta['residual'] = True
    return meta
