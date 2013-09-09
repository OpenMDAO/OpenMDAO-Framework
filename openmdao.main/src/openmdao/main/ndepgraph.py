import networkx as nx

from openmdao.util.nameutil import partition_names_by_comp
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IDriver, IComponent

# # to use as a quick check for exprs to avoid overhead of constructing an
# # ExprEvaluator
_exprchars = set('+-/*()&| %<>!')

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
    return graph.node[node].get('iotype') == 'in'

def is_output_node(graph, node):
    return graph.node[node].get('iotype') == 'out'

def is_boundary_node(graph, node):
    return '.' not in node and is_var_node(graph, node)

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
        self.config_changed()

    def config_changed(self):
        self._component_graph = None
        self._loops = None

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
        self.add_nodes_from(added_ins, var=True, iotype='in')
        self.add_nodes_from(added_outs, var=True, iotype='out')

        # add edges from the variables to their parent component
        self.add_edges_from([(v,cname) for v in added_ins])
        self.add_edges_from([(cname,v) for v in added_outs])

        # for removed inputs/outputs, may need to remove connections
        # and subvars
        for n in rem_ins:
            self.remove(n)
        for n in rem_outs:
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
        if 'comp' not in kwargs:
            kwargs['comp'] = True

        inputs  = ['.'.join([cname, v]) for v in obj.list_inputs()]
        outputs = ['.'.join([cname, v]) for v in obj.list_outputs()]

        self.add_node(cname, **kwargs)
        self.add_nodes_from(inputs, var=True, iotype='in')
        self.add_nodes_from(outputs, var=True, iotype='out')

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
        self.add_node(name, **kwargs)
        self.config_changed()

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.' or 'C1['.
        """
        nodes = self.find_prefixed_nodes([name])
        if nodes:
            self.remove_nodes_from(nodes)
            self.config_changed()

    def check_connect(self, srcpath, destpath):
        if is_external_node(self, destpath): # error will be caught at parent level
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

    def connect(self, srcpath, destpath):
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

        path = [base_src]
        bases = [base_src]

        if srcpath != base_src:
            path.append(srcpath)
            bases.append(base_src)

        if destpath != base_dest:
            path.append(destpath)
            bases.append(base_dest)

        path.append(base_dest)
        bases.append(base_dest)

        # check the connection first before we add any nodes or edges
        # so we don't have anything to clean up if there's a problem
        self.check_connect(srcpath, destpath)

        for i in range(len(path)):
            if path[i] not in self:
                self.add_node(path[i], basevar=bases[i])
            if i > 0:
                self.add_edge(path[i-1], path[i])

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True

        self.config_changed()

    def disconnect(self, srcpath, destpath=None):

        if destpath is None:
            if is_comp_node(self, srcpath):
                edges = self.edges(self.successors(srcpath))
                edges.extend(self.in_edges_iter(self.predecessors(srcpath)))

            elif is_subvar_node(self, srcpath):
                self.remove_node(srcpath)
                edges = []

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
                if is_subvar_node(self, node):
                    if self.in_degree(node) < 1 or self.out_degree(node) < 1:
                        self.remove_node(node)

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
        in_set = set(self._var_connections(self.list_inputs(comp2), 'in'))
        return in_set.intersection(
                    self._var_connections(self.list_outputs(comp1), 'out'))

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
            succ = self.successors(node)
            bunch.extend(succ)
            for s in succ:
                bunch.extend(self.successors(s))
        if direction != 'out':
            pred = self.predecessors(node)
            bunch.extend(pred)
            for p in pred:
                bunch.extend(self.predecessors(p))
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

    def invalidate_deps(self, scope, cname, srcvars, force=False):
        """Walk through all dependent nodes in the graph, invalidating all
        variables that depend on output sets for the given component names.

        scope: Component
            Scoping object containing this dependency graph.

        cname: str
            Name of starting node.

        srcvars: list of set of str or None
            Names of sources from each starting node. If None,
            all outputs from specified component are assumed
            to be invalidated.
            
        force: bool (optional)
            If True, force invalidation to continue even if a component in
            the dependency chain was already invalid.
        """
        
        if srcvars is None:
            srcvars = self.list_outputs(cname, connected=True)
        elif cname:
            srcvars = ['.'.join([cname,n]) for n in srcvars]

        stack = [(cname, srcvars)]
        outset = set()  # set of changed boundary outputs

        if not srcvars:
            return outset

        # Keep track of the comp/var we already invalidated, so we
        # don't keep doing them. This allows us to invalidate loops.
        invalidated = {}

        while(stack):
            srccomp, srcvars = stack.pop()
   
            if srcvars is None:
                srcvars = self.list_outputs(srccomp, connected=True)
           
            # KTM1 - input-input connections were excluded. Add them in by
            # adding the inputs to our check. The extra unconnected ones
            # shouldn't hurt the call into find_nodes.

            # FIXME: fix this to include ONLY inputs that are also outputs
            if srccomp: 
                srcvars += self.list_inputs(srccomp)
            
            if not srcvars:
                continue

            invalidated.setdefault(srccomp, set()).update(srcvars) 
            
            cmap = partition_names_by_comp(self.find_nodes(srcvars, 
                                                           is_basevar_node, 
                                                           is_comp_node))
            
            for dcomp, dests in cmap.items():
                if dests:
                    if dcomp in invalidated:
                        diff = set(dests) - invalidated[dcomp]
                        if diff:
                            invalidated[dcomp].update(diff)
                        else:
                            continue
                    if dcomp:
                        comp = getattr(scope, dcomp)
                        newouts = comp.invalidate_deps(varnames=dests,
                                                       force=force)
                        if newouts is None:
                            stack.append((dcomp, None))
                        elif newouts:
                            newouts = ['.'.join([dcomp,v]) for v in newouts]
                            stack.append((dcomp, newouts))
                    else: # boundary outputs
                        outset.update(dests)

        return outset

    def get_connected_inputs(self, nodes=None):
        """Returns inputs that are connected externally.
        If nodes is not None, return a list of those nodes
        that are connected.
        """
        if nodes is None:
            ins = []
            for node in self.nodes_iter():
                if is_external_node(self, node):
                    succs = self.succ.get(node)
                    if succs:
                        ins.extend(succs.keys())
            return ins
        else:
            return [n for n in nodes if self.in_degree(n) > 0]

    def get_connected_outputs(self, nodes=None):
        """Returns outputs that are connected externally.
        If nodes is not None, return a list of those nodes
        that are connected.
        """
        if nodes is None:
            outs = []
            for node in self.nodes_iter():
                if is_external_node(self, node):
                    preds = self.pred.get(node)
                    if preds:
                        outs.extend(preds.keys())
            return outs
        else:
            return [n for n in nodes if self.out_degree(n) > 0]

    def list_inputs(self, cname, connected=False):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)
        if connected:
            return [n for n in self.pred[cname]
                                            if self.in_degree(n)>0]
        else:
            return self.pred[cname].keys()

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
        variable connections to the specified variable.  If direction is None, both
        ins and outs are included. Other allowed values for direction are
        'in' and 'out'.
        """
        conns = []

        if direction != 'in':  # get 'out' connections
            for u,v in self.edges_iter(path):
                if is_connection(self, u, v):
                    conns.append((u,v))
                else:
                    for uu,vv in self.edges_iter(v):
                        if is_connection(self, uu, vv):
                            conns.append((uu,vv))

        if direction != 'out':  # get 'in' connections
            for u,v in self.in_edges_iter(path):
                if is_connection(self, u, v):
                    conns.append((u,v))
                else:
                    for uu,vv in self.in_edges_iter(u):
                        if is_connection(self, uu, vv):
                            conns.append((uu,vv))

        return conns

    def basevar_iter(self, nodes, reverse=False):
        """Given a group of nodes, return an iterator
        over all base variable nodes that are nearest in one
        direction.
        """
        return self.find_nodes(nodes, is_basevar_node, reverse=reverse)

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

