from collections import deque
import pprint

import networkx as nx

from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver
from openmdao.main.expreval import ExprEvaluator

# # to use as a quick check for exprs to avoid overhead of constructing an
# # ExprEvaluator
_exprchars = set('+-/*()&| %<>!')

_missing = object()

def _is_expr(node):
    """Returns True if node is an expression that is not a simple
    variable reference, or a reference to a single array entry or
    vartree attribute.
    """
    return len(_exprchars.intersection(node)) > 0

def base_var(graph, node):
    """Returns the name of the variable node that is the 'base' for
    the given node name.  For example, for the node A.b[4], the
    base variable is A.b.  For the node d.x.y, the base variable
    is d if d is a boundary variable node, or d.x otherwise.
    """
    if node in graph:
        base = graph.node[node].get('basevar')
        if base:
            return base

    parts = node.split('[', 1)[0].split('.')
    # for external connections, we don't do the error checking at
    # this level so ambiguity in actual base varname doesn't
    # matter.  So just return the full name and be done with it.
    if parts[0] == 'parent':
        return node

    base = parts[0]
    if base in graph and 'var' in graph.node[base]:
        return base

    return '.'.join(parts[:2])

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
#
# EDGES:
#   conn    means that the edge is a connection that was specified
#           by calling connect()


# NODE selectors

def is_input_node(graph, node):
    if graph.node[node].get('iotype') == 'in':
        return True
    base = graph.node[node].get('basevar')
    return base in graph and graph.node[base].get('iotype') == 'in'

def is_input_base_node(graph, node):
    return graph.node[node].get('iotype') == 'in'

def is_output_node(graph, node):
    if graph.node[node].get('iotype') == 'out':
        return True
    base = graph.node[node].get('basevar')
    return base in graph and graph.node[base].get('iotype') == 'out'

def is_output_base_node(graph, node):
    return graph.node[node].get('iotype') == 'out'

def is_boundary_node(graph, node):
    return 'boundary' in graph.node[node]

def is_comp_node(graph, node):
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

def is_connected_src_node(graph, node):
    """Return True if this node is part of a connection,
    i.e., there is at least one successor edge that
    satisfies 'is_connection'.
    """
    for u,v in graph.edges_iter(node):
        if is_connection(graph, u, v):
            return True

def is_connected_dest_node(graph, node):
    """Return True if this node is part of a connection,
    i.e., there is at least one predecessor edge that
    satisfies 'is_connection'.
    """
    for u,v in graph.in_edges_iter(node):
        if is_connection(graph, u, v):
            return True

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

def is_external_node(graph, node):
    return node.startswith('parent.')

def is_nested_node(graph, node):
    """Returns True if the given node refers to an attribute that
    is nested within the child of a Component in our scope, or
    within a boundary variable in our scope.  For
    example, if a Component 'comp1' is within our scope,
    a variable node referring to 'comp1.child.x' would be a
    nested node while a 'comp1.y' node would not.  If we had a boundary
    var called 'b', then 'b.x' would be a nested node.
    """
    base = base_var(graph, node)
    return '.' in node[len(base):]

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False

# predicate factories

def all_preds(*args):
    """Returns a function that returns True if all preds passed to it
    return True.  If called with no predicates, always returns False.
    """
    def _all_preds(graph, node):
        for pred in args:
            if not pred(graph, node):
                return False
        return False
    return _all_preds

def any_preds(*args):
    """Returns a function that returns True if any preds passed to it
    return True.  If called with no predicates, always returns False.
    """
    def _any_preds(graph, node):
        for pred in args:
            if pred(graph, node):
                return True
        return False
    return _any_preds


class DependencyGraph(nx.DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()
        self._severed_edges = {}
        self._saved_comp_graph = None
        self._saved_loops = None
        self.config_changed()

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
        
        for u,v in edges:
            self.disconnect(u, v, config_change=False)

    def unsever_edges(self, scope):
        """Restore previously severed edges."""
        if not self._severed_edges:
            return

        for u,v in self._severed_edges:
            self.connect(scope, u, v, config_change=False)
            
        self._severed_edges = []

        self._loops = self._saved_loops
        self._component_graph = self._saved_comp_graph

    def config_changed(self):
        self._component_graph = None
        self._loops = None
        self._saved_loops = None
        self._saved_comp_graph = None

    def child_config_changed(self, child):
        """A child has changed its input or output lists, so
        we need to update the graph.
        """
        cname = child.name
        old_ins  = set(self.list_inputs(cname))
        old_outs = set(self.list_outputs(cname))

        new_ins  = set(['.'.join([cname,n]) for n in child.list_inputs()])
        new_outs = set(['.'.join([cname,n]) for n in child.list_outputs()])

        added_ins = new_ins - old_ins
        added_outs = new_outs - old_outs

        rem_ins = old_ins - new_ins
        rem_outs = old_outs - new_outs

        # for new inputs/outputs, just add them to graph
        self.add_nodes_from(added_ins, var=True, iotype='in', valid=True)
        self.add_nodes_from(added_outs, var=True, iotype='out', valid=False)

        # add edges from the variables to their parent component
        self.add_edges_from([(v,cname) for v in added_ins])
        self.add_edges_from([(cname,v) for v in added_outs])

        if added_outs:
            self.node[cname]['valid'] = False

        # for removed inputs/outputs, may need to remove connections
        # and subvars
        for n in rem_ins:
            self.remove(n)
        for n in rem_outs:
            self.remove(n)

        # update the iograph for the component
        #self.node[cname]['iograph'] = child.io_graph()

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
        if 'comp' not in kwargs:
            kwargs['comp'] = True

        inputs  = ['.'.join([cname, v]) for v in obj.list_inputs()]
        outputs = ['.'.join([cname, v]) for v in obj.list_outputs()]
        #iograph = obj.io_graph()
        #kwargs['iograph'] = iograph
        #if iograph is None:
        kwargs['valid'] = False
        #else:
        #    kwargs['valid'] = 'partial' # have to handle comps with partial
                                        # invalidation in a special way

        self.add_node(cname, **kwargs)
        self.add_nodes_from(inputs, var=True, iotype='in', valid=True)
        self.add_nodes_from(outputs, var=True, iotype='out', valid=False)

        self.add_edges_from([(v, cname) for v in inputs])
        self.add_edges_from([(cname, v) for v in outputs])

        self.config_changed()

    def add_boundary_var(self, name, **kwargs):
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
        self.config_changed()

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.' or 'C1['.
        """
        nodes = self.find_prefixed_nodes([name])
        self.disconnect(name)  # updates validity
        if nodes:
            self.remove_nodes_from(nodes)
        self.config_changed()

    def check_connect(self, srcpath, destpath):
        if is_external_node(self, destpath):  # error will be caught at parent level
            return

        dpbase = base_var(self, destpath)

        dest_iotype = self.node[dpbase].get('iotype')
        if dest_iotype == 'out' and not is_boundary_node(self, dpbase) and not dpbase == srcpath:
            raise RuntimeError("'%s' must be an input variable" % destpath)

        connected = False
        conns = self._var_connections(dpbase, 'in')
        if conns:
            if destpath == dpbase:
                connected = True
            elif destpath in [v for u,v in conns]:
                connected = True
            else:
                for u, v in conns:
                    if is_basevar_node(self, v):
                        connected = True
                        break
                    if destpath != dpbase and _sub_or_super(v, destpath):
                        connected = True
                        break

        if connected:
            raise RuntimeError("'%s' is already connected to '%s'" %
                                  (conns[0][1], conns[0][0]))

    def connect(self, scope, srcpath, destpath, config_change=True):
        """Create a connection between srcpath and destpath,
        and create any necessary additional connections to base
        variable nodes.  For example, connecting A.b[3] to
        B.c.x will create an edge between A.b[3] and B.c.x, and
        will also add an edge from base variable A.b to A.b[3],
        and another edge from B.c.x to base variable B.c.
        """

        base_src  = base_var(self, srcpath)
        base_dest = base_var(self, destpath)

        for v in [base_src, base_dest]:
            if not v.startswith('parent.') and v not in self:
                raise RuntimeError("Can't find variable '%s' in graph." % v)

        if base_src in self:
            src_validity = self.node[base_src]['valid']
        else:
            src_validity = False
            
        path = [(base_src, base_src, src_validity)]

        if srcpath != base_src:
            path.append((srcpath, base_src, src_validity))

        if destpath != base_dest:
            path.append((destpath, base_dest, True))

        path.append((base_dest, base_dest, True))

        self.check_connect(srcpath, destpath)

        for i in range(len(path)):
            var, base, valid = path[i]
            if var not in self:
                self.add_node(var, basevar=base, valid=valid) # subvar
            else:
                self.node[var]['valid'] = valid
            if i > 0:
                self.add_edge(path[i-1][0], var)

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True

        # create expression objects to handle setting of 
        # array indces, etc.
        self.edge[srcpath][destpath]['sexpr'] = ExprEvaluator(srcpath)
        self.edge[srcpath][destpath]['dexpr'] = ExprEvaluator(destpath)

        self.invalidate_deps(scope, [destpath])
        
        if config_change:
            self.config_changed()

    def disconnect(self, srcpath, destpath=None, config_change=True):

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
                if is_subvar_node(self, node):
                    if self.in_degree(node) < 1 or self.out_degree(node) < 1:
                        self.remove_node(node)

        if config_change:
            self.config_changed()

    def get_interior_connections(self, comps=None):
        """ Returns all interior connections between the given comps.
        """
        if comps is None:
            pred = any_preds(is_comp_node, is_pseudo_node)
            compset = set([n for n in self.nodes_iter() if pred(self, n)])
        else:
            compset = set(comps)
        return [(u,v) for u, v in self.edges_iter()
                   if is_connection(self, u, v) and
                      u.split('.', 1)[0] in compset and
                      v.split('.', 1)[0] in compset]

    def get_directional_interior_edges(self, comp1, comp2):
        """ Behaves like get_ineterior_edges, except that it only
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
        if is_comp_node(self, path) or is_pseudo_node(self, path):
            return self._comp_connections(path, direction)
        else:
            return self._var_connections(path, direction)

    def list_connections(self, show_passthrough=True,
                               show_external=False):
        conns = [(u,v) for u,v in self.edges_iter()
                          if is_connection(self, u, v)]

        if show_passthrough is False:
            conns = [(u,v) for u,v in conns if not ('.' in u or '.' in v)]

        if show_external is False:
            conns = [(u,v) for u,v in conns
                          if not (u.startswith('parent.')
                               or v.startswith('parent.'))]

        return conns

    def get_sources(self, name):
        """Return the node that's actually a source for the
        named node (as opposed to just a connected subvar).
        This should only be called for destination nodes (inputs
        or output boundary vars).
        """
        srcs = []
        for u,v in self.in_edges_iter(name):
            if is_connection(self, u, v):
                srcs.append(u)
            else:
                for uu,vv in self.in_edges_iter(u):
                    if is_connection(self, uu, vv):
                        srcs.append(uu)
        return srcs

    def _check_source(self, path, src):
        preds = self.predecessors(path)
        for pred in preds:
            if src == pred:
                return
            if pred.startswith(path):  # subvar
                for p in self.predecessors(pred):
                    if src == p:
                        return
        if len(preds) > 0:
            raise RuntimeError(
                "'%s' is connected to source '%s' and cannot be "
                "set by source '%s'" %
                (path, preds[0], src))

    def _all_vars(self, node, direction=None):
        """Return a list of nodes containing all nodes that are one
        or two connections out from the starting node that are prefixed
        by the starting node.  This captures all var and subvar
        nodes associated with the starting node.
        """
        bunch = []
        if direction != 'in':
            succ = self.successors_iter(node)
            bunch.extend(succ)
            for s in succ:
                bunch.extend(self.successors_iter(s))
        if direction != 'out':
            pred = self.predecessors_iter(node)
            bunch.extend(pred)
            for p in pred:
                bunch.extend(self.predecessors_iter(p))
        ndot = node+'.'
        nbrack = node+'['
        return [n for n in bunch if n.startswith(ndot)
                                 or n.startswith(nbrack)]

    def all_comps(self):
        """Returns a list of all component and PseudoComponent
        nodes.
        """
        pred = any_preds(is_comp_node, is_pseudo_node)
        return [n for n in self.nodes_iter() if pred(self, n)]

    def find_prefixed_nodes(self, nodes, data=False):
        """Returns a list of nodes including the given nodes and
        any node that is prefixed with the name of any of those
        nodes. This is useful for retrieving all variable and
        subvar nodes associated with a component node.
        """

        full = []
        for node in nodes:
            full.extend(self._all_vars(node))
            full.append(node)

        if data:
            sn = self.node
            return [(n, sn[n]) for n in full]
        else:
            return full

    def full_subgraph(self, nodes):
        """Returns the subgraph specified by the given nodes and
        any variable or expr nodes corresponding to those nodes.
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

    def invalidate_deps(self, scope, vnames):#, force=False):
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
        stack = [(n, self.successors_iter(n), ndata[n]['valid']) 
                    for n in vnames]

        visited = set()
        while(stack):
            src, neighbors, valid = stack.pop()
            if is_comp_node(self, src):
                ndata[src]['valid'] = False
            else:
                visited.add(src)
                if valid is False:
                    continue
                if self.in_degree(src): # don't invalidate unconnected inputs
                    ndata[src]['valid'] = False
                    
                if is_boundary_node(self, src) and is_output_base_node(self, src):
                    outset.add(src)

            for node in neighbors:
                if is_comp_node(self, node):
                    outs = getattr(scope, node).invalidate_deps([src.split('.',1)[1]])
                    if outs is None:
                        stack.append((node, self.successors_iter(node), 
                                     False))
                    else: # partial invalidation
                        outs = ['.'.join([node,n]) for n in outs]
                        stack.extend([(n, self.successors_iter(n),
                                             ndata[n]['valid'])
                                        for n in outs]) 
                elif node not in visited:
                    nvalid = ndata[node]['valid']
                    if nvalid:
                        stack.append((node, self.successors_iter(node), 
                                        nvalid))
                # else: # a component node with partial invalidation
                #     stack.extend([(n,iograph.successors_iter(n),
                #                                  ndata[n]['valid']) 
                #                     for n in iograph.nodes()
                #                       if iograph.out_degree(n)>0 and
                #                          self.in_degree(n)>0])

        return outset

    def get_boundary_inputs(self, connected=False):
        """Returns inputs that are on the component boundary.
        If connected is True, return a list of those nodes
        that are connected externally.
        """
        ins = []
        for node in self.nodes_iter():
            if is_boundary_node(self, node) and \
               is_input_base_node(self, node):
                    if connected:
                        if self.in_degree(node) > 0:
                            ins.append(node)
                    else:
                        ins.append(node)
        return ins

    def get_boundary_outputs(self, connected=False):
        """Returns outputs that are on the component boundary.
        If connected is True, return a list of those nodes
        that are connected externally.
        """
        outs = []
        for node in self.nodes_iter():
            if is_boundary_node(self, node) and \
               is_output_base_node(self, node):
                    if connected:
                        if self.out_degree(node) > 0:
                            outs.append(node)
                    else:
                        outs.append(node)
        return outs

    def list_inputs(self, cname, connected=False, invalid=False):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)
        if connected:
            lst = [n for n in self.pred[cname]
                                            if self.in_degree(n)>0]
        else:
            lst = self.pred[cname].keys()
            
        if invalid:
            return [n for n in lst if self.node[n]['valid'] is False]
        
        return lst

    def list_input_outputs(self, cname):
        """Return a list of names of input nodes that are used
        as outputs. This can happen if an input is part of a
        constraint or an objective.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)
        return [n for n in self.pred[cname]
                             if self.out_degree(n)>1]

    def list_outputs(self, cname, connected=False):
        """Return a list of names of output nodes for a component.
        If connected is True, return only connected outputs.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)
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
            if is_external_node(self, n):
                for p in self.predecessors_iter(n):
                    if self.has_edge(p, n) and '.' in p:
                        conns.append((p, n))
                for s in self.successors_iter(n):
                    if self.has_edge(n, s) and '.' in s:
                        conns.append((n, s))

        return conns

    def component_graph(self):
        """Return a subgraph containing only Components
        and PseudoComponents and edges between them.
        """
        if self._component_graph is not None:
            return self._component_graph

        compset = set(self.all_comps())

        g = nx.DiGraph()
        for comp in compset:
            g.add_node(comp, self.node[comp].copy())

        for src, dest in self.list_connections():
            destcomp = dest.split('.', 1)[0]
            srccomp  =  src.split('.', 1)[0]
            if srccomp in compset and destcomp in compset:
                g.add_edge(srccomp, destcomp)

        self._component_graph = g
        return g

    # def io_graph(self, cname):
    #     """Return a graph showing connections between boundary
    #     inputs and boundary outputs only, for use by parent
    #     graphs.
    #     """
    #     iograph = nx.DiGraph()
    #     inputs = set()
    #     outputs = set()
    #     for node in self.nodes_iter():
    #         if is_boundary_node(self, node):
    #             if is_input_base_node(self, node):
    #                 inputs.add(node)
    #             elif is_output_base_node(self, node):
    #                 outputs.add(node)

    #     edges = []
    #     for inp in inputs:
    #         for u,v in nx.dfs_edges(self, source=inp):
    #             if v in outputs:
    #                 edges.append((inp, v))

    #     if cname:
    #         inputs = ['.'.join([cname,n]) for n in inputs]
    #         outputs = ['.'.join([cname,n]) for n in outputs]
    #         edges = [('.'.join([cname,u]),'.'.join([cname,v]))
    #                       for u,v in edges]

    #     iograph.add_nodes_from(inputs)
    #     iograph.add_nodes_from(outputs)
    #     iograph.add_edges_from(edges)

    #     return iograph

    def get_loops(self):
        if self._loops is None:
            self._loops = [s for s in
                nx.strongly_connected_components(self.component_graph())
                if len(s)>1]
        return self._loops

    def find_nodes(self, nodes, predicate, stop_predicate=None, reverse=False):
        """Returns an iterator over the nearest successor nodes that satisfy the predicate.
        For example, if node is a base variable node and predicate is True for base variable
        nodes, then the nearest base variable successors would be returned in the iterator.
        Note that depending on iotype, the behavior may be surprising.  For example, if node is
        an input base variable of a component, then the iterator will return all of the output
        base variable nodes of the component because they are the next base variable successors,
        but if node is an output variable, then any input base variables connected to that node
        will be returned.

        nodes: str or iter of str
            The group of starting nodes, or just a single node

        predicate: boolean function of the form  f(graph, node)
            Should return True for nodes that should be included
            in the iterator

        stop_predicate: boolean function of the form f(graph, node) (optional)
            Should return True for nodes that should stop the traversal
            of that branch.  Traversal on a branch will stop when the first
            node satisfying 'predicate' is found OR if stop_predicate is True.

        reverse: bool (optional)
            if True, find previous nodes instead of successor nodes.

        If predicate is True AND stop_predicate is True, the node will still
        be added to the iterator and traversal on the branch will stop.
        """
        if isinstance(nodes, basestring):
            nodes = [nodes]

        if reverse:
            neighbors = self.predecessors_iter
        else:
            neighbors = self.successors_iter

        visited=set()
        for start in nodes:
            if start in visited:
                continue
            visited.add(start)
            stack = [(start, neighbors(start))]
            while stack:
                parent, children = stack[-1]
                try:
                    child = next(children)
                    if child not in visited:
                        visited.add(child)
                        if predicate(self, child):
                            yield child
                        elif not (stop_predicate is not None and stop_predicate(self, child)):
                            stack.append((child, neighbors(child)))
                except StopIteration:
                    stack.pop()

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

    def comp_iter(self, nodes, reverse=False, include_pseudo=True):
        """Given a group of nodes, return an iterator
        over all component nodes that are nearest in one
        direction.
        """
        if include_pseudo:
            return self.find_nodes(nodes, any_preds(is_comp_node, is_pseudo_node),
                                   reverse=reverse)
        else:
            return self.find_nodes(nodes, is_comp_node, reverse=reverse)
                
    def child_run_finished(self, childname):
        """Called by a child when it completes its run() function."""
        data = self.node
        data[childname]['valid'] = True

        for var in self._all_vars(childname):
            data[var]['valid'] = True

    def get_invalid_out_edges(self):
        """Return a list of edges connected to invalid boundary
        outputs.
        """
        edges = []
        for u,v,data in self.edges_iter(data=True):
            if 'conn' in data and is_boundary_node(self,v) \
                                and self.node[v]['valid'] is False:
                edges.append((u,v,data))
        return edges

    def update_destvar(self, scope, vname):
        """Update the value of the given variable in the 
        given scope using upstream variables.
        """
        valid_set = set([vname])
        for u,v,data in self.in_edges_iter(vname, data=True):
            if 'conn' in data:
                try:
                    data['dexpr'].set(data['sexpr'].evaluate(scope=scope), 
                                      src=u, scope=scope)
                except Exception as err:
                    raise err.__class__("cannot set '%s' from '%s': %s" %
                                         (v, u, str(err)))
                valid_set.add(v)
            else:
                for uu,vv,ddata in self.in_edges_iter(u, data=True):
                    if 'conn' in ddata:
                        try:
                            ddata['dexpr'].set(ddata['sexpr'].evaluate(scope=scope), 
                                               src=uu, scope=scope)
                        except Exception as err:
                            raise err.__class__("cannot set '%s' from '%s': %s" %
                                                 (v, u, str(err)))
                        valid_set.add(vv)

        for node in valid_set:
            self.node[node]['valid'] = True


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
    fwdset = set()
    backset = set()
    tmpset = set([end])
    while tmpset:
        node = tmpset.pop()
        if node in backset:
            continue
        backset.add(node)
        tmpset.update(graph.pred[node].keys())

    tmpset = set([start])
    while tmpset:
        node = tmpset.pop()
        if node in fwdset:
            continue
        fwdset.add(node)
        tmpset.update(graph.succ[node].keys())

    return fwdset.intersection(backset)

def get_meta_dict(graph, metaname):
    dct = {}
    for n, data in graph.nodes(data=True):
        try:
            dct[n] = data[metaname]
        except KeyError:
            pass
    return dct

def dumpmeta(graph, metaname):
    pprint.pprint(get_meta_dict(graph, metaname))

