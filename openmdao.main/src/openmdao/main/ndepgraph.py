import sys
import networkx as nx
from networkx import all_neighbors

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
    for base in [parts[0], '.'.join(parts[:2])]:
        if base in graph and 'var' in graph.node[base]:
            return base
    else:
        raise KeyError("Can't find base variable for '%s' in graph" % 
                       node) 


# NODE selectors (pass as arg to the find_nodes function)

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

def is_dangling(graph, node):
    """Returns True if node should be removed because it has 
    insufficient connections, i.e., some nodes can only 
    exist as internal connecting nodes between 'real' variable
    nodes.
    """
    # dangling subvar node
    if 'subvar' in graph.node[node]:
        return  graph.degree(node) < 2

# EDGE selectors

def is_connection(graph, src, dest):
    try:
        return 'conn' in graph.edge[src][dest]
    except KeyError:
        return False


class DiGraph(nx.DiGraph):
    """A slightly fancier version of networkx.DiGraph that provides a few
    extra 'finder' and 'visitor' methods that are based on 
    node/edge metadata.
    """

    def find_edges(self, predicate, nodes=None, data=False):
        """Returns all edges in the graph such that the predicate
        returns True.  The predicate must take args of the form:
        def pred(graph, u, v).
        """
        if data:
            return [(u, v, dat) for u, v, dat in self.edges(nodes, 
                                                            data=True)
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


# Explanation of node/edge metadata dict entries
#
#   comp    means it's a Component node
#   pseudo  means it's a PseudoComponent
#   var     means it's a simple Variable node (no indexing or attr access)
#   subvar  means it's some reference to a part of a Variable (like and array ref or attr access)
#   conn    means that the edge is a connection that was specified
#           by calling connect()

class DependencyGraph(DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()

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

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name plus a dot, e.g., for name C1, remove all
        nodes prefixed by 'C1.'.
        """
        self.remove_nodes_from(self.find_prefixed_nodes([name]))

    def _check_connect(self, srcpath, destpath):
        if _is_expr(destpath):
            raise RuntimeError("Can't connect '%s' to '%s'. '%s' is not a valid destination." % 
                                   (srcpath, destpath, destpath))

        inedges = self.in_edges((destpath,))
        if len(inedges) > 0:
            raise RuntimeError("Can't connect '%s' to '%s'. '%s' is already connected to '%s'" % 
                              (srcpath, destpath, destpath, inedges[0][0]))

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

        if _is_expr(srcpath):
            self._connect_expr(srcpath, destpath)
            return

        base_src  = base_var(self, srcpath)
        base_dest = base_var(self, destpath)

        for v in [base_src, base_dest]:
            if v not in self:
                raise RuntimeError("Can't find variable '%s' in graph." % v)

        path = [base_src]

        if srcpath != base_src:
            path.append(srcpath)

        if destpath != base_dest:
            path.append(destpath)

        path.append(base_dest)

        kwargs = { 'subvar': True }
        for i in range(1, len(path)):
            if path[i] not in self:
                self.add_node(path[i], **kwargs)
            self._check_connect(path[i-1], path[i])
            self.add_edge(path[i-1], path[i])

        # mark the actual connection edge to distinguish it
        # from other edges (for list_connections, etc.)
        self.edge[srcpath][destpath]['conn'] = True
        
    def disconnect(self, srcpath, destpath=None):
        if destpath is None:
            if 'comp' in self.node[srcpath]:
                # outputs
                out_edges = self.edges(self.successors(srcpath))

                # need to clean up dangling array ref nodes
                self.remove_nodes_from([v for u,v in out_edges 
                                           if len(v) > len(u) 
                                               and v[len(u)] == '['])
                self.remove_edges_from(out_edges)

                # inputs
                in_edges = self.in_edges(self.predecessors(srcpath))

                # need to clean up dangling array ref nodes
                self.remove_nodes_from([u for u,v in in_edges 
                                           if len(u) > len(v) 
                                               and u[len(v)] == '['])
                self.remove_edges_from(in_edges)
        else:
            self.remove_edge(srcpath, destpath)

    def get_interior_connections(self, comps):
        compset = set(comps)
        return [(u,v) for u, v in self.find_edges(is_connection)
                   if u.split('.', 1)[0] in compset and 
                      v.split('.', 1)[0] in compset]

    def list_connections(self, show_passthrough=True):
        return self.find_edges(is_connection)

    def get_sources(self, name):
        return [u for u,v in self.in_edges((name,))]

    def find_prefixed_nodes(self, nodes):
        """Returns a list of nodes including the given nodes and
        any node that is prefixed with the name of any of those
        nodes. This is useful for retrieving all variable and
        subvar nodes associated with a component node.
        """
        nodeset = set(nodes)
        return [n for n in self.nodes_iter() 
                        if n.split('.', 1)[0] in nodeset]

    def full_subgraph(self, nodes):
        """Returns the subgraph specified by the given nodes and
        any variable or expr nodes corresponding to those nodes.
        """
        return self.subgraph(self.find_prefixed_nodes(nodes))

    def invalidate_deps(self, scope, cnames, varsets, force=False):
        pass

    def list_autopassthroughs(self):
        pass

    def var_edges(self, name=None):
        pass

    def var_in_edges(self, name=None):
        pass

    def get_connected_inputs(self):
        pass

    def get_connected_outputs(self):
        pass

    def connections_to(self, path):
        pass

    def dump(self, stream=sys.stdout):
        pass

    def find_all_connecting(self, start, end):
        pass




if __name__ == '__main__':
    class Dummy(object):
        pass
        


