import sys
import itertools
import networkx as nx

# # to use as a quick check for exprs to avoid overhead of constructing an
# # ExprEvaluator
_exprchars = set('+-/*()&| %<>!')

def _is_array_entry(node):
    return '[' in node

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
    parts = node.split('[', 1)[0].split('.')
    # for external connections, we don't do the error checking at
    # this level so ambiguity in actual base varname doesn't
    # matter.  So just return the full name and be done with it.
    if parts[0] == 'parent':
        return node

    for base in [parts[0], '.'.join(parts[:2])]:
        if base in graph and 'var' in graph.node[base]:
            return base
    else:
        raise KeyError("Can't find base variable for '%s' in graph" % 
                       node) 


# Explanation of node/edge metadata dict entries
#
# NODES:
#   comp    means it's a Component node
#   pseudo  means it's a PseudoComponent
#   var     means it's a simple Variable node (no indexing or attr access)
#   subvar  means it's some reference to a part of a Variable (like an array ref or attr access)
#
# EDGES:
#   conn    means that the edge is a connection that was specified
#           by calling connect()


# NODE selectors (pass as arg to the find_nodes function)

def is_input_node(graph, node):
    return graph.node[node].get('iotype') == 'in'

def is_output_node(graph, node):
    return graph.node[node].get('iotype') == 'out'

def is_boundary_node(graph, node):
    return '.' not in node

def is_comp_node(graph, node):
    return 'comp' in graph.node[node]

def is_driver_node(graph, node):
    return 'driver' in graph.node[node]

def is_var_node(graph, node):
    return 'var' in graph.node[node]

def is_subvar_node(graph, node):
    return 'subvar' in graph.node[node]

def is_attr_node(graph, node):
    base = base_var(graph, node)
    return '.' in node[len(base):]

def is_pseudo_node(graph, node):
    return 'pseudo' in graph.node[node]

def is_param_pseudo_node(graph, node):
    return graph.node[node].get('pseudo') == 'param'

def is_pseudo_unit_node(graph, node):
    return graph.node[node].get('pseudo') == 'unit'

def is_comp_or_pseudo_node(graph, node):
    return is_comp_node(graph, node) or is_pseudo_node(graph, node)

def is_external_node(graph, node):
    return node.startswith('parent.')

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False


class DiGraph(nx.DiGraph):
    """A version of networkx.DiGraph that provides a few
    extra 'finder' and 'visitor' methods that are based on 
    node/edge metadata.
    """

    def find_edges(self, predicate, nodes=None, data=False):
        """Returns all edges in the graph such that the predicate
        returns True.  The predicate must take args of the form:
        def pred(graph, u, v).
        """
        if data:
            return [(u, v, dat) 
                       for u, v, dat in self.edges(nodes, data=True)
                            if predicate(self, u, v)]
        else:
            return [(u, v) for u, v in self.edges(nodes)
                            if predicate(self, u, v)]

    def find_edges_iter(self, predicate, nodes=None, data=False):
        """Returns an iterator over all edges in the graph such that 
        the predicate returns True.  The predicate must take args of 
        the form: pred(graph, u, v).
        """
        if data:
            for u, v, dat in self.edges_iter(nodes, data=True):
                if predicate(self, u, v):
                    yield (u, v, dat)
        else:
            for u, v in self.edges_iter(nodes):
                if predicate(self, u, v):
                    yield (u, v)

    def find_nodes(self, predicate, data=False):
        """Returns all nodes in the graph such that the predicate
        returns True. The predicate must take args of the form:
        def pred(graph, node).
        """
        if data:
            return [(n, dat) for n, dat in self.nodes(data=True)
                            if predicate(self, n)]
        else:
            return [n for n in self.nodes() if predicate(self, n)]

    def find_nodes_iter(self, predicate, data=False):
        """Returns an iterator over all nodes in the graph such that 
        the predicate returns True. The predicate must take args of the 
        form: pred(graph, node).
        """
        if data:
            for n, dat in self.nodes(data=True):
                if predicate(self, n):
                    yield (n, dat)
        else:
            for n in self.nodes():
                if predicate(self, n):
                    yield n


class DependencyGraph(DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()
        self.config_changed()

    def config_changed(self):
        self._component_graph = None

    def add_component(self, cname, inputs, outputs, **kwargs):
        """Create nodes in the graph for the component and all of
        its input and output variables.  inputs and outputs names
        are local to the component.  Any other named args that
        are passed will be placed in the metadata for the component
        node.
        """

        self.config_changed()

        if 'comp' not in kwargs:
            kwargs['comp'] = True

        self.add_node(cname, **kwargs)

        inputs  = ['.'.join([cname, v]) for v in inputs]
        outputs = ['.'.join([cname, v]) for v in outputs]

        self.add_nodes_from(inputs, var=True, iotype='in')
        self.add_nodes_from(outputs, var=True, iotype='out')

        self.add_edges_from([(v, cname) for v in inputs])
        self.add_edges_from([(cname, v) for v in outputs])

    def add_boundary_var(self, name, **kwargs):
        """Add a boundary variable, i.e., one not associated
        with any component in the graph.
        """

        self.config_changed()

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

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.'.
        """
        self.config_changed()     
        self.remove_nodes_from(self.find_prefixed_nodes([name]))

    def check_connect(self, srcpath, destpath):
        if _is_expr(destpath):
            raise RuntimeError("Can't connect '%s' to '%s'. '%s' is not a valid destination." % 
                                   (srcpath, destpath, destpath))

        inedges = self.in_edges((destpath,))
        if len(inedges) > 0:
            raise RuntimeError("Can't connect '%s' to '%s'. '%s' is already connected to '%s'" % 
                              (srcpath, destpath, destpath, inedges[0][0]))

        if not destpath.startswith('parent.'):
            base = base_var(self, destpath)
            if base != destpath and base != srcpath:
                usdot = destpath + '.'
                usbracket = destpath + '['
                for u,v in self.in_edges((base,)):
                    if destpath.startswith(u+'.') or destpath.startswith(u+'[') \
                           or u.startswith(usdot) or u.startswith(usbracket):
                        raise RuntimeError("Can't connect '%s' to '%s'. '%s' is already connected to '%s'" % 
                                  (srcpath, destpath, base, u))

    def _connect_expr(self, srcpath, destpath):
        raise NotImplementedError("_connect_expr")

    def connect(self, srcpath, destpath):
        """Create a connection between srcpath and destpath,
        and create any necessary additional connections to base 
        variable nodes.  For example, connecting A.b[3] to
        B.c.x will create an edge between A.b[3] and B.c.x, and
        will also add an edge from base variable A.b to A.b[3],
        and another edge from B.c.x to base variable B.c.
        """

        self.config_changed()
        
        if _is_expr(srcpath):
            self._connect_expr(srcpath, destpath)
            return

        base_src  = base_var(self, srcpath)
        base_dest = base_var(self, destpath)

        for v in [base_src, base_dest]:
            if not v.startswith('parent.') and v not in self:
                raise RuntimeError("Can't find variable '%s' in graph." % v)

        path = [base_src]

        if srcpath != base_src:
            path.append(srcpath)

        if destpath != base_dest:
            path.append(destpath)

        path.append(base_dest)

        kwargs = { 'subvar': True }
        for i in range(len(path)):
            if path[i] not in self:
                self.add_node(path[i], **kwargs)
            if i > 0:
                self.check_connect(path[i-1], path[i])
                self.add_edge(path[i-1], path[i])

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True
        
    def disconnect(self, srcpath, destpath=None):

        self.config_changed()
        
        if destpath is None:
            if is_comp_node(self, srcpath):
                edges = self.edges(self.successors(srcpath))
                edges.extend(self.in_edges(self.predecessors(srcpath)))

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

    def get_interior_connections(self, comps=None):
        if comps is None:
            compset = set(self.find_nodes(is_comp_or_pseudo_node))
        else:
            compset = set(comps)
        return [(u,v) for u, v in self.find_edges(is_connection)
                   if u.split('.', 1)[0] in compset and 
                      v.split('.', 1)[0] in compset]

    def list_connections(self, show_passthrough=True, show_external=False):
        conns = []
        for src, dest in self.find_edges(is_connection):
            if src.startswith('parent.') or dest.startswith('parent.'):
                if show_external:
                    conns.append((src, dest))
            elif '.' not in base_var(self, src) or '.' not in base_var(self, dest):
                if show_passthrough:
                    conns.append((src, dest))
            else:
                conns.append((src, dest))
        return conns

    def get_sources(self, name):
        return [u for u,v in self.in_edges((name,))]

    def get_source(self, name):
        preds = self.pred.get(name)
        if preds:
            if len(preds) == 1:
                return preds.keys()[0]
            else:
                raise RuntimeError("'%s' has multiple sources" % name)
        return None

    def find_prefixed_nodes(self, nodes, data=False):
        """Returns a list of nodes including the given nodes and
        any node that is prefixed with the name of any of those
        nodes. This is useful for retrieving all variable and
        subvar nodes associated with a component node.
        """
        nodeset = set(nodes)
        if data:
            return [(n,dat) for n, dat in self.nodes_iter(data=True) 
                        if n.split('.', 1)[0] in nodeset]
        else:
            return [n for n in self.nodes_iter() 
                        if n.split('.', 1)[0] in nodeset]

    def full_subgraph(self, nodes):
        """Returns the subgraph specified by the given nodes and
        any variable or expr nodes corresponding to those nodes.
        """
        return self.subgraph(self.find_prefixed_nodes(nodes))

    def component_graph(self):
        """Return a subgraph containing only Components
        and PseudoComponents and edges between them. 
        """
        if self._component_graph:
            return self._component_graph

        compset = set(self.find_nodes_iter(is_comp_or_pseudo_node))
        g = super(DependencyGraph, self).subgraph(compset)
        for src, dest in self.list_connections():
            destcomp = dest.split('.', 1)[0]
            srccomp  =  src.split('.', 1)[0]
            if srccomp in compset and destcomp in compset:
                g.add_edge(srccomp, destcomp)

        self._component_graph = g
        return g

    def get_connected_inputs(self):
        ins = []
        for node in self.find_nodes_iter(is_external_node):
            succs = self.succ.get(node)
            if succs:
                ins.extend(succs.keys())
        return ins

    def get_connected_outputs(self):
        outs = []
        for node in self.find_nodes_iter(is_external_node):
            preds = self.pred.get(node)
            if preds:
                outs.extend(preds.keys())
        return outs

    def find_all_connecting(self, start, end):
        """Return the set of all nodes along all paths between 
        start and end.  The start and end nodes are included
        in the set if they're connected.
        """
        
        if start == end:
            return set()
        graph = self._graph
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

    def _var_connections(self, path, direction=None):
        """Returns a list of tuples of the form (srcpath, destpath) for all
        connections to the specified variable.  If direction is None, both
        ins and outs are included. Other allowed values for direction are
        'in' and 'out'.
        """
        conns = []
            
        if direction != 'in':  # get 'out' connections
            conns.extend(self.edges(path))

        if direction != 'out':  # get 'in' connections
            conns.extend(self.in_edges(path))

        return conns

    def invalidate_deps(self, scope, cname, varset, force=False):
        """Walk through all dependent nodes in the graph, invalidating all
        variables that depend on output sets for the given component names.
        
        scope: Component
            Scoping object containing this dependency graph.
            
        cname: str
            Name of starting node.
            
        varset: iter of str
            Iter of names of outputs from starting node.
            
        force: bool (optional)
            If True, force invalidation to continue even if a component in
            the dependency chain was already invalid.
        """

        if cname:
            stack = ['.'.join([cname, v]) for v in varset]
        else:
            stack = list(varset)

        outset = set()  # set of changed boundary outputs
        
        # Keep track of the comp/var we already invalidated, so we
        # don't keep doing them. This allows us to invalidate loops.
        invalidated = set()
        
        while(stack):
            src, varset = stack.pop()
            invalidated.append(src)
            for dest, link in self.out_links(src):
                if dest == '@bout':
                    bouts = link.get_dests(varset)
                    outset.update(bouts)
                    scope.set_valid(bouts, False)
                else:
                    dests = link.get_dests(varset)
                    if dests:
                        comp = getattr(scope, dest)
                        outs = comp.invalidate_deps(varnames=dests, force=force)
                        if (outs is None) or outs:
                            if dest not in invalidated:
                                stack.append((dest, outs))
        return outset

    def list_autopassthroughs(self):
        return []

    def var_edges(self, name=None):
        pass

    def var_in_edges(self, name=None):
        pass

    def connections_to(self, path):
        pass

    def dump(self, stream=sys.stdout):
        pass


if __name__ == '__main__':
    class Dummy(object):
        pass
        


