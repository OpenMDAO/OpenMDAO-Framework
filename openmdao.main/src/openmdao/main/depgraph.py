from collections import deque
from itertools import chain
from ordereddict import OrderedDict
import pprint

import networkx as nx

from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IVariableTree, \
                                     IImplicitComponent, ISolver
from openmdao.main.expreval import ExprEvaluator
from openmdao.util.nameutil import partition_names_by_comp

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
        elif 'var' in graph.node[node]:
            return node

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
    base = base_var(graph, node)
    return '.' in node[len(base):]

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False


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

        added_ins = new_ins - old_ins
        added_outs = new_outs - old_outs
        added_states = new_states - old_states
        added_resids = new_resids - old_resids

        rem_ins = old_ins - new_ins
        rem_outs = old_outs - new_outs
        rem_states = old_states - new_states
        rem_resids = old_resids - new_resids

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
            self.add_nodes_from(resids, var=True, iotype='residual', valid=True)

            self.add_edges_from([(cname, v) for v in chain(states, resids)])
            self.add_edges_from([(v, cname) for v in states])


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
        """Raise an exception if the specified destination is already
        connected.
        """
        if is_extern_node(self, destpath):  # error will be caught at parent level
            return

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

    def connect(self, scope, srcpath, destpath, 
                config_change=True, check=True, invalidate=True):
        """Create a connection between srcpath and destpath,
        and create any necessary additional subvars with connections to base
        variable nodes.  For example, connecting A.b[3] to
        B.c.x will create an edge between A.b[3] and B.c.x, and
        will also add an edge from base variable A.b to A.b[3],
        and another edge from B.c.x to base variable B.c.
        """

        base_src  = base_var(self, srcpath)
        base_dest = base_var(self, destpath)

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
                self.add_edge(src, dest)

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True

        # create expression objects to handle setting of 
        # array indces, etc.
        self.edge[srcpath][destpath]['sexpr'] = ExprEvaluator(srcpath, getter='get_attr')
        self.edge[srcpath][destpath]['dexpr'] = ExprEvaluator(destpath, getter='get_attr')

        if invalidate:
            self.invalidate_deps(scope, [srcpath])
        
        if config_change:
            self.config_changed()

    def add_subvar(self, subvar):
        """ Adds a subvar node for a model input. This node is used to
        represent parameters that are array slices, mainly for metadata
        storage and for defining edge iterators, but not for workflow
        execution. Subvars created using this function will be labeled
        as 'fake' in their metadata.
        """
        base = base_var(self, subvar)
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
                if is_subvar_node(self, node) and not is_fake_node(self, node):
                    if self.in_degree(node) < 1 or self.out_degree(node) < 1:
                        self.remove_node(node)

        if config_change:
            self.config_changed()

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
                for p in self.predecessors(pred):
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
        bunch = []
        ndot = node+'.'
        nbrack = node+'['
        
        if direction != 'in':
            succ = self.successors(node)
            bunch.extend(succ)
            for s in succ:
                bunch.extend(self.successors_iter(s))
                
        if direction != 'out':
            pred = self.predecessors(node)
            bunch.extend(pred)
            for p in pred:
                bunch.extend(self.predecessors_iter(p))
                # it's possible for some input basevars to have subvars thar are
                # sucessors instead of predecessors
                for succ in self.successors_iter(p):
                    if succ not in bunch:
                        bunch.append(succ)
                
        return [n for n in bunch if n.startswith(ndot)
                                 or n.startswith(nbrack)]

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
            
            oldvalid = ndata[src]['valid']
            if oldvalid is True:
                if self.in_degree(src) or src.startswith('parent.'): # don't invalidate unconnected inputs
                    ndata[src]['valid'] = False
                if is_boundary_node(self, src) and is_output_base_node(self, src):
                    outset.add(src)

            parsources = self.get_sources(src)
            for node in neighbors:
                if is_comp_node(self, node):
                    if ndata[node]['valid'] or ndata[node].get('invalidation')=='partial':
                        outs = getattr(scope, node).invalidate_deps(['.'.join(['parent', n]) 
                                                                      for n in parsources])
                        if outs is None:
                            stack.append((node, self.successors_iter(node), True))
                        else: # partial invalidation
                            stack.append((node, ['.'.join([node,n]) for n in outs], False))
                else:
                    stack.append((node, self.successors_iter(node), True))

        return outset

    def get_boundary_inputs(self, connected=False):
        """Returns inputs that are on the component boundary.
        If connected is True, return a list of only those nodes
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
        If connected is True, return a list of only those nodes
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

    def get_extern_srcs(self):
        """Returns sources from external to our parent
        component that are connected to our boundary inputs.
        """
        return [n for n in self.nodes_iter()
                    if is_extern_src(self, n)]

    def get_extern_dests(self):
        """Returns destinations that are external to our parent
        component that are connected to our boundary outputs.
        """
        return [n for n in self.nodes_iter()
                    if is_extern_dest(self, n)]

    def list_inputs(self, cname, connected=False, invalid=False):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.  If
        invalid is True, return only invalid inputs.
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
        """Return a list of names of input or state nodes that are used
        as outputs.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)

        return [n for n in self.pred[cname]
                             if self.out_degree(n)>1]

    def list_outputs(self, cname, connected=False):
        """Return a list of names of output, state or residual nodes for a component.
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
            if is_extern_node(self, n):
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
        valid_set = set(self.find_prefixed_nodes([vname]))
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

def _dfs_connections(G, source, reverse=False):
    """Produce connections in a depth-first-search starting at source."""
    # Slightly modified version of the networkx function dfs_edges

    if reverse:
        neighbors = G.predecessors_iter
    else:
        neighbors = G.successors_iter

    visited=set()

    stack = [(source, neighbors(source))]
    while stack:
        parent, children = stack[-1]
        try:
            child = next(children)
            if reverse:
                tup = (child, parent)
            else:
                tup = (parent, child)
            if tup not in visited:
                if 'conn' in G.edge[tup[0]][tup[1]]:
                    yield tup
                visited.add(tup)
                stack.append((child, neighbors(child)))
        except StopIteration:
            stack.pop()

def _get_inner_edges(G, srcs, dests):
    """Return a set of connection edges between the
    given sources and destinations.

    srcs: iter of (str or tuple of str)
        Starting var or subvar nodes

    dests: iter of str
        Ending var or subvar nodes

    """
    fwdset = set()
    backset = set()
    for node in dests:
        backset.update(_dfs_connections(G, node, reverse=True))

    for node in srcs:
        fwdset.update(_dfs_connections(G, node))

    return fwdset.intersection(backset)

def get_solver_edges(wflow, graph, graphcomps, scope, inputs, outputs):
    """Return edges coming from solvers that are in the
    specified workflow or part of the graph between specified
    derivative inputs and outputs.
    """
    #from openmdao.main.pseudoassembly import from_PA_var # prevent recursive import
    
    # add edges from any nested solvers
    edges = set()
    comps = set(wflow)
    comps.update([getattr(scope,n) for n in graphcomps if n is not None])

    for comp in comps:  #._parent.iteration_set():
        if has_interface(comp, ISolver):
            g = comp.workflow.derivative_graph(inputs=inputs,outputs=outputs,
                                                             group_nondif=False)
            for u, v, data in g.edges_iter(data=True):
                if u.startswith('@') or v.startswith('@'):
                    continue
                if 'conn' in data:
                    #edges.add((from_PA_var(u), from_PA_var(v)))
                    edges.add((u, v))
            
    return edges

def mark_nonsolver_driver_comps(wflow, graph, graphcomps, scope):
    """Mark all components as non-differentiable that are in the
    itersets of any non-solver drivers in the specified workflow or
    in the graph.
    """
    comps = set(wflow)
    comps.update([getattr(scope,n) for n in graphcomps if n is not None])
    nondiff_comps = set()

    for comp in comps:
        if has_interface(comp, IDriver) and not has_interface(comp, ISolver):
            nondiff_comps.add(comp.name)
            nondiff_comps.update(comp.iteration_set())

    for comp in nondiff_comps:
        if comp in graph:
            graph.node[comp] = graph.node[comp].copy() # don't pollute top level graph with nondiff markers
            graph.node[comp]['non-differentiable'] = True

def mod_for_derivs(graph, inputs, outputs, wflow):
    """Adds needed nodes and connections to the given graph
    for use in derivative calculations.
    """
    indct = {}
    inames = []
    onames = []

    scope = wflow.scope

    # add nodes for input parameters
    for i, varnames in enumerate(inputs):
        iname = '@in%d' % i
        inames.append(iname)
        graph.add_node(iname, var=True, iotype='in', valid=True)
        for varname in flatten_list_of_iters(varnames):
            if varname not in graph:  # must be a subvar
                graph.add_node(varname, basevar=base_var(graph, varname), 
                               iotype='in', valid=True)
            graph.connect(None, iname, varname,
                          check=False, invalidate=False)
            indct[varname] = iname

    # add nodes for desired outputs
    for i, varnames in enumerate(outputs):
        oname = '@out%d' % i
        onames.append(oname)
        graph.add_node(oname, var=True, iotype='out', valid=False)
        for varname in flatten_list_of_iters(varnames):
            if varname not in graph:
                graph.add_node(varname, basevar=base_var(graph, varname), 
                               iotype='out', valid=False)
            graph.connect(None, varname, oname, 
                          check=False, invalidate=False)

    edges = _get_inner_edges(graph, 
                             ['@in%d' % i for i in range(len(inputs))],
                             ['@out%d' % i for i in range(len(outputs))])
    
    comps = partition_names_by_comp([e[0] for e in edges])
    partition_names_by_comp([e[1] for e in edges], compmap=comps)
    
    slv_edges = get_solver_edges(wflow, graph, comps.keys(), scope, inputs, outputs)
    edges.update(slv_edges)

    mark_nonsolver_driver_comps(wflow, graph, comps.keys(), scope)

    # get comps for any new edges due to sub-solvers
    partition_names_by_comp([e[0] for e in slv_edges], compmap=comps)
    partition_names_by_comp([e[1] for e in slv_edges], compmap=comps)

    full = [k for k in comps.keys() if k]
    if None in comps:
        full.extend([v.split('[')[0] for v in comps[None] if v != '@fake'])
        
    subgraph = graph.full_subgraph(full)
    
    to_remove = [n for n in graph.nodes_iter() if n not in subgraph]
            
    graph.remove_nodes_from(to_remove)

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
    for src, dest in edges:
        if src == '@fake' or dest == '@fake':
            continue
        if src.startswith('@in'):
            # move edges from input boundary nodes if they're
            # connected to an @in node
            base_dest = base_var(graph, dest)
            if is_boundary_node(graph, base_dest):
                if base_dest == dest: # dest is a basevar
                    newsrc = src
                else: # dest is a subvar
                    if graph.out_degree(base_dest) > 0:
                        for s in graph.successors(base_dest):
                            if base_var(graph, s) != base_dest:
                                newdest = dest.replace(base_dest, s, 1)
                                if newdest not in graph:
                                    graph.add_subvar(newdest)
                                graph.add_edge(src, newdest, conn=1)
                        newsrc = src
                    else:
                        newsrc = dest.replace(base_dest, base_var(graph, src), 1)
                    if newsrc not in graph:
                        graph.add_subvar(newsrc)
                for s, d in graph.edges(dest):
                    graph.add_edge(newsrc, d, attr_dict=graph.edge[s][d])
                    to_remove.add((s,d))
                to_remove.add((src, dest))
            continue
        elif dest.startswith('@out') and not is_input_node(graph, src):
            # move edges from output boundary nodes if they're
            # connected to an @out node
            base_src = base_var(graph, src)
            if is_boundary_node(graph, base_src):
                for pred in graph.predecessors(base_src):
                    if not base_src == base_var(graph, pred): # it's not one of our subvars
                        break
                else:
                    continue  # FIXME: not sure what to really do here.
                if base_src == src: # src is a basevar
                    newsrc = pred
                else: # src is a subvar
                    newsrc = src.replace(base_src, base_var(graph, pred), 1)
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
                    if base_var(graph, sub) != p:
                        newsrc = p
                        break
                if newsrc is None:
                    continue
                graph.add_edge(newsrc, sub.replace(src, dest, 1), 
                               attr_dict=graph.edge[src][dest])

            to_remove.add((src, dest))

        else:
            base = base_var(graph, src)
            if is_boundary_node(graph, base):
                to_remove.add((src, dest))
    
    # get rid of any left over boundary connections
    for s, d in graph.list_connections():
        sbase = base_var(graph, s)
        dbase = base_var(graph, d)
        if is_boundary_node(graph, sbase) or is_boundary_node(graph, dbase):
            to_remove.add((s,d))

    graph.remove_edges_from(to_remove)

    # if full vartrees are connected, create subvar nodes for all of their
    # internal variables
    visited = set()
    for src, dest in graph.list_connections():
        srcnames = []
        destnames = []
        if '@' not in src and '[' not in src and src not in visited:
            visited.add(src)
            obj = scope.get(src)
            if has_interface(obj, IVariableTree):
                srcnames = sorted([n for n,v in obj.items(recurse=True) if not has_interface(v, IVariableTree)])
                srcnames = ['.'.join([src, n]) for n in srcnames]
        if '@' not in dest and '[' not in dest and dest not in visited:
            visited.add(dest)
            obj = scope.get(dest)
            if has_interface(obj, IVariableTree):
                destnames = sorted([n for n,v in obj.items(recurse=True) if not has_interface(v, IVariableTree)])
                destnames = ['.'.join([dest, n]) for n in destnames]
        if '@' not in src and '@' not in dest and (srcnames or destnames):
            _replace_full_vtree_conn(graph, src, srcnames, 
                                            dest, destnames)
    
    # disconnected boundary vars that are explicitly specified as inputs
    # or outputs need to be added back so that bounds data can be kept 
    # for them
    for inp in flatten_list_of_iters(inputs):
        if inp not in graph:
            if '@fake' not in graph:
                graph.add_node('@fake')
            graph.add_node(inp)
            #graph.add_edge(inp, '@fake', conn=True)
            graph.add_edge('@fake', inp, conn=True)

    for out in flatten_list_of_iters(outputs):
        if out not in graph:
            if '@fake' not in graph:
                graph.add_node('@fake')
            graph.add_node(out)
            #graph.add_edge('@fake', out, conn=True)
            graph.add_edge(out, '@fake', conn=True)

    # also add @fake connections for sub-solver related nodes that
    # aren't connected in the final graph
    for u,v in slv_edges:
        if u == '@fake' or v == '@fake':
            graph.add_edge(u, v, conn=True)

    # We want our top level graph metadata to be stored in the copy, but not in the
    # parent, so make our own copy of the metadata dict.
    graph.graph = {}
    
    graph.graph['inputs'] = inputs[:]
    graph.graph['outputs'] = outputs[:]
                
    return graph

def _replace_full_vtree_conn(graph, src, srcnames, dest, destnames):
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
        graph.connect(None, s, d, check=False, 
                      invalidate=False)

        
# utility/debugging functions

def edges_to_dict(edges, dct=None):
    """Take an iterator of edges and return an ordered dict
    of sources mapped to lists of destinations.
    """
    if dct is None:
        dct = OrderedDict()
    for u, v in edges:
        dct.setdefault(u, []).append(v)
    return dct


def edge_dict_to_comp_list(graph, edges, implicit_edges=None):
    """Converts inner edge dict into an ordered dict whose keys are component
    names, and whose values are lists of relevant (in the graph) inputs and
    outputs.
    """
    comps = OrderedDict()
    basevars = set()
    for src, targets in edges.iteritems():
        
        if src == '@fake':
            continue
        
        if not isinstance(targets, list):
            targets = [targets]
            
        numfakes = 0
        for target in targets:
            if target.startswith('@fake'):
                numfakes += 1
            if  not target.startswith('@'):
                comp, _, var = target.partition('.')
                if var:
                    if comp not in comps:
                        comps[comp] = {'inputs': [],
                                       'outputs': [],
                                       'residuals': [],
                                       'states': []}
                    
                    basevar = base_var(graph, target)
                    if basevar not in basevars:
                        comps[comp]['inputs'].append(var)
                        
                        if target == basevar:
                            basevars.add(target)
                            
        if len(targets) == numfakes:
            continue
        
        if not src.startswith('@'):
            comp, _, var = src.partition('.')
            if var:
                if comp not in comps:
                    comps[comp] = {'inputs': [],
                                   'outputs': [],
                                   'residuals': [],
                                   'states': []}
                
                basevar = base_var(graph, src)
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
                
    return comps

def nodes_matching_all(graph, **kwargs):
    """Return an iterator over nodes matching all kwargs names and values. 
    For example, nodes_matching_all(G, valid=True, boundary=True) would
    return a list of all nodes that are marked as valid that
    are also boundary nodes.
    """
    for n,data in graph.node.iteritems():
        for arg,val in kwargs.items():
            if data.get(arg, _missing) != val:
                break
        else:
            yield n

def nodes_matching_some(graph, **kwargs):
    """Return an iterator over nodes matching at least one of 
    the kwargs names and values. For
    example, nodes_matching_some(G, valid=True, boundary=True) would
    return a list of all nodes that either are marked as valid or nodes
    that are boundary nodes, or nodes that are both.
    """
    for n,data in graph.node.iteritems():
        for arg,val in kwargs.items():
            if data.get(arg, _missing) == val:
                yield n
                break

def edges_matching_all(graph, **kwargs):
    """Return an iterator over edges matching all kwargs names and 
    values. For example, edges_matching_all(G, foo=True, bar=True) would
    return a list of all edges that are marked with True
    values of both foo and bar.
    """
    for u,v,d in graph.edges(data=True):
        for arg,val in kwargs.items():
            if d.get(arg, _missing) != val:
                break
        else:
            yield (u,v)

def edges_matching_some(graph, **kwargs):
    """Return an iterator over edges matching some kwargs names 
    and values. For example, edges_matching_some(G, foo=True, bar=True) 
    would return a list of all edges that are marked with True
    values of either foo or bar or both.
    """
    for u,v,d in graph.edges(data=True):
        for arg,val in kwargs.items():
            if d.get(arg, _missing) == val:
                yield (u,v)
                break

def get_valids(graph, val, prefix=None):
    """Returns all nodes with validity matching the
    given value.
    """
    if prefix:
        return [n for n in nodes_matching_all(graph, valid=val)
                    if n.startswith(prefix)]
    return sorted(nodes_matching_all(graph, valid=val))

def dump_valid(graph, filter=None, stream=None):
    dct = {}
    for node in graph.nodes_iter():
        if filter and not filter(node):
            continue
        dct[node] = graph.node[node]['valid']
    pprint.pprint(dct, stream=stream)


def flatten_list_of_iters(lst):
    """Returns a list of simple values, flattening
    any sub-lists or sub-tuples, or if the input is a 
    string it just returns that string.  NOTE: this only
    goes down one level.
    """
    if isinstance(lst, basestring):
        return [lst]
    else:
        ret = []
        for entry in lst:
            if isinstance(entry, basestring):
                ret.append(entry)
            else:
                ret.extend(entry)
        return ret

def bfs_find(G, source, pred):
    """Return the first target in BFS order that satisfies 
    pred(graph, target), or None if no target satisfying 
    pred is found.
    """
    for u, v in nx.bfs_edges(G, source):
        if pred(G, v):
            return True
    return None
