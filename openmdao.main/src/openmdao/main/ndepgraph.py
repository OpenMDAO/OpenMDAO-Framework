import itertools
import networkx as nx

from openmdao.util.nameutil import partition_names_by_comp

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


# Explanation of node/edge metadata dict entries
#
# NODES:
#   comp    means it's a Component node
#   pseudo  means it's a PseudoComponent
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
    return '.' not in node

def is_comp_node(graph, node):
    return 'comp' in graph.node.get(node, '')

def is_driver_node(graph, node):
    return 'driver' in graph.node.get(node, '')

def is_var_node(graph, node):
    return 'var' in graph.node.get(node, '')

def is_subvar_node(graph, node):
    return 'basevar' in graph.node.get(node, '')

def is_pseudo_node(graph, node):
    return 'pseudo' in graph.node.get(node, '')

def is_param_pseudo_node(graph, node):
    return graph.node[node].get('pseudo') == 'param'

def is_pseudo_unit_node(graph, node):
    return graph.node[node].get('pseudo') == 'unit'

def is_comp_or_pseudo_node(graph, node):
    return is_comp_node(graph, node) or is_pseudo_node(graph, node)

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

    def add_component(self, cname, inputs, outputs, **kwargs):
        """Create nodes in the graph for the component and all of
        its input and output variables.  inputs and outputs names
        are local to the component.  Any other named args that
        are passed will be placed in the metadata for the component
        node.
        """

        if 'comp' not in kwargs:
            kwargs['comp'] = True

        self.add_node(cname, **kwargs)

        inputs  = ['.'.join([cname, v]) for v in inputs]
        outputs = ['.'.join([cname, v]) for v in outputs]

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
        if destpath not in self or is_external_node(self, destpath):
            return
        
        inedges = self.in_edges((destpath,))
        dest_iotype = self.node[destpath].get('iotype')
        if dest_iotype == 'out' and not is_boundary_node(self, destpath):
            raise RuntimeError("'%s' must be an input variable" % destpath)

        if len(inedges) > 0:
            if not is_subvar_node(self, srcpath):
                raise RuntimeError("'%s' is already connected to '%s'" % 
                                    (destpath, inedges[0][0]))

        base = base_var(self, destpath)
        if base != destpath and base != srcpath:
            usdot = destpath + '.'
            usbracket = destpath + '['
            for u,v in self.in_edges_iter((base,)):
                if destpath.startswith(u+'.') or destpath.startswith(u+'[') \
                       or u.startswith(usdot) or u.startswith(usbracket):
                    raise RuntimeError("'%s' is already connected to '%s'" % (base, u))

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

        for i in range(len(path)):
            if path[i] not in self:
                self.add_node(path[i], basevar=bases[i])
            if i > 0:
                self.check_connect(path[i-1], path[i])
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

    # def is_connected(self, src, dest):
    #     return self.trans_closure().is_connected(src, dest)

    def get_interior_connections(self, comps=None):
        if comps is None:
            compset = set([n for n in self.nodes_iter() if
                            is_comp_or_pseudo_node(self, n)])
        else:
            compset = set(comps)
        return [(u,v) for u, v in self.edges_iter()
                   if is_connection(self, u, v) and
                      u.split('.', 1)[0] in compset and 
                      v.split('.', 1)[0] in compset]

    def connections_to(self, path, direction=None):
        if is_comp_or_pseudo_node(self, path):
            conns = []
            for vname in itertools.chain(self.list_inputs(path, 
                                                    connected=True),
                                         self.list_outputs(path, 
                                                    connected=True)):
                conns.extend(self.connections_to(vname, direction))
            return conns
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

    def get_source(self, name):
        preds = self.pred.get(name)
        if preds:
            if len(preds) == 1:
                src = preds.keys()[0]
                if is_comp_or_pseudo_node(self, src):
                    return None # src is a comp so this node is really an output
                return src
            else:
                raise RuntimeError("'%s' has multiple sources" % name)
        return None

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
        return [n for n in self.nodes_iter() 
                    if is_comp_or_pseudo_node(self, n)]

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

    def find_all_connecting(self, start, end):
        """Return the set of all component nodes along all paths 
        between start and end. The start and end nodes are included 
        in the set if they're connected.
        """
        graph = self.component_graph()

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

    def _comp_connections(self, cname, direction=None):
        conns = []
        if direction != 'in':
            for out in self.list_outputs(cname):
                conns.extend(self._var_connections(out, 'out'))
        if direction != 'out':
            for inp in self.list_inputs(cname):
                conns.extend(self._var_connections(inp, 'in'))
        return conns

    def _var_connections(self, path, direction=None):
        """Returns a list of tuples of the form (srcpath, destpath) for all
        variable connections to the specified variable.  If direction is None, both
        ins and outs are included. Other allowed values for direction are
        'in' and 'out'. 
        """
        conns = []

        if direction != 'in':  # get 'out' connections
            for u,v in self.edges_iter(path):
                if v.startswith(u):  # a subvar of u
                    conns.extend([(uu,vv) for uu,vv in self.edges_iter(v) 
                        if not is_comp_node(self, vv)])
                elif not is_comp_node(self, v):
                    conns.append((u, v))

        if direction != 'out':  # get 'in' connections
            for u,v in self.in_edges_iter(path):
                if u.startswith(v):  # a subvar of v
                    conns.extend([(uu,vv) for uu,vv in self.in_edges_iter(u) 
                        if not is_comp_node(self, uu)])
                elif not is_comp_node(self, u):
                    conns.append((u, v))

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
        
        # Keep track of the comp/var we already invalidated, so we
        # don't keep doing them. This allows us to invalidate loops.
        invalidated = set()

        while(stack):
            srccomp, srcvars = stack.pop()
            invalidated.add(srccomp)
            if srcvars is None:
                srcvars = self.list_outputs(srccomp, connected=True)

            cmap = partition_names_by_comp(self.basevar_iter(srcvars))
            for dcomp, dests in cmap.items():
                if dcomp in invalidated:
                    continue
                if dests:
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

    def get_connected_inputs(self):
        """Returns inputs that are connected externally."""
        ins = []
        for node in self.nodes_iter():
            if is_external_node(self, node):
                succs = self.succ.get(node)
                if succs:
                    ins.extend(succs.keys())
        return ins

    def get_connected_outputs(self):
        """Returns outputs that are connected externally."""
        outs = []
        for node in self.nodes_iter():
            if is_external_node(self, node):
                preds = self.pred.get(node)
                if preds:
                    outs.extend(preds.keys())
        return outs

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

        g = super(DependencyGraph, self).subgraph(compset)
        for src, dest in self.list_connections():
            destcomp = dest.split('.', 1)[0]
            srccomp  =  src.split('.', 1)[0]
            if srccomp in compset and destcomp in compset:
                g.add_edge(srccomp, destcomp)

        self._component_graph = g
        return g

    def trans_closure(self):
        if self._trans_closure is None:
            self._trans_closure = TransClosure(self)
        return self._trans_closure

    def basevar_iter(self, nodes):
        """Given a group of nodes, return an iterator
        over all base variable nodes until the next 
        component node.
        """
        visited=set()
        for start in nodes:
            if start in visited:
                continue
            visited.add(start)
            stack = [(start, iter(self[start]))]
            while stack:
                parent, children = stack[-1]
                try:
                    child = next(children)
                    if child not in visited:
                        if is_var_node(self, child):
                            yield child
                        elif is_comp_or_pseudo_node(self, child):
                            raise StopIteration()
                        visited.add(child)
                        stack.append((child, iter(self[child])))
                except StopIteration:
                    stack.pop()

    def get_loops(self):
        if self._loops is None:
            self._loops = [s for s in 
                nx.strongly_connected_components(self.component_graph()) 
                if len(s)>1]
        return self._loops


def find_related_pseudos(compgraph, nodes):
    """Return a set of pseudocomponent nodes that are between the
    nodes in the given list or between them and a boundary.
    """
    
    pseudos = set()
    
    for node in nodes:
        for upcomp in compgraph.predecessors_iter(node):
            if is_pseudo_node(compgraph, upcomp):
                pseudos.add(upcomp)
        for dwncomp in compgraph.successors_iter(node):
            if is_pseudo_node(compgraph, dwncomp):
                pseudos.add(dwncomp)

    return list(pseudos)

