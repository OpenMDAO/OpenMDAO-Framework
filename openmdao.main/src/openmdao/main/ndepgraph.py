import sys
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
    #TODO: make this more robust
    return len(_exprchars.intersection(node)) > 0

# NODE selectors

def is_comp_node(graph, node, data):
    return 'comp' in data

def is_driver_node(graph, node, data):
    return 'driver' in data

def is_var_node(graph, node, data):
    return 'var' in data

def is_subvar_node(graph, node, data):
    return 'subvar' in data

def is_pseudo_node(graph, node, data):
    return 'pseudo' in data

def is_param_pseudo_node(graph, node, data):
    return data.get('pseudo') == 'param'

def is_dangling(graph, node, data):
    """Returns True if node should be removed because it has insufficient connections"""
    # dangling subvar node
    if 'subvar' in data:
        return  graph.degree(node) < 2


# EDGE selectors

def is_connection(graph, u, v, data):
    return 'conn' in data


# Explanation of node/edge metadata dict entries
#
#   comp = True means it's a Component node
#   pseudo = True means it's a PseudoComponent
#   var = True means it's a simple Variable node (no indexing or attr access)
#   subvar = True means it's some reference to a part of a Variable (like and array ref or attr access)
#   

class DependencyGraph(nx.DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()
        self.change_count = 0 # increment this whenever graph changes

    def _is_vtree_attr(self, node):
        if node in self:
            return 'vtree' in self.node[node]
        return '.' in node[len(self._get_base_var(node)):]

    def _get_base_var(self, node):
        """Returns the name of the variable node that is the 'base' for 
        the given node name.  For example, for the node A.b[4], the
        base variable is A.b.  For the node d.x.y, the base variable
        is d if d is a boundary variable node, or d.x otherwise.
        """
        parts = node.split('[', 1)[0].split('.')
        if len(parts) == 1:
            return parts[0]
        base = parts[0]
        if base in self and 'var' in self.node[base]:
            return base
        return '.'.join(parts[:2])

    def add_component(self, cname, inputs, outputs, **kwargs):
        """Create nodes in the graph for the component and all of
        its input and output variables.  inputs and outputs names
        are local to the component.  Any other named args that
        are passed will be placed in the metadata for the component
        node.
        """
        self.change_count += 1

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
        """Add a boundary variable using this."""

        if _is_expr(name): # it's an expression, so we need to create a PseudoComponent
            raise RuntimeError("can't add expression '%s'. as a Variable node." % name)

        if '.' in name:
            raise RuntimeError("'%s' is not a valid boundary variable name." % name)
        else:
            kwargs['boundary'] = True

        if _is_array_entry(name) or self._is_vtree_attr(name):
            raise RuntimeError("subvars like '%s' can only be added as part of a connection" % name)
        else:
            kwargs['var'] = True
            
        if 'iotype' not in kwargs:
            raise RuntimeError("variable '%s' can't be added because its iotype was not specified." % name)

        self.add_node(name, **kwargs)

    def remove(self, name):
        """Remove the named node and all nodes prefixed by the
        given name.
        """
        self.change_count += 1

        name_dot = name + '.'
        neighbors = self.successors(name)
        neighbors.extend(self.predecessors(name))
        to_remove = [n for n in neighbors if n.startswith(name_dot)]
        to_remove.append(name)
        self.remove_nodes_from(to_remove)
        self.remove_nodes_from(self.find_nodes(is_dangling))

    def _check_connect(self, srcpath, destpath):
        if _is_expr(destpath):
            raise RuntimeError("Can't connect '%s' to '%s'. '%s' is not a valid destination." % 
                                   (srcpath, destpath, destpath))

        if destpath in self:
            inedges = self.in_edges(destpath)
            if len(inedges) > 0:
                raise RuntimeError("Can't connect '%s' to '%s'. '%s' is already connected to '%s'" % 
                                  (srcpath, destpath, destpath, inedges[0]))

    def _connect_expr(self, srcpath, destpath):
        raise NotImplementedError("_connect_expr")

    def connect(self, srcpath, destpath):

        if _is_expr(srcpath):
            self._connect_expr(srcpath, destpath)
            return

        true_src = self._get_base_var(srcpath)
        true_dest = self._get_base_var(destpath)

        for v in [true_src, true_dest]:
            if v not in self:
                raise RuntimeError("Can't find '%s' in grpah." % v)

        path = [true_src]

        if srcpath != true_src:
            path.append(srcpath)

        if destpath != true_dest:
            path.append(destpath)

        path.append(true_dest)

        kwargs = { 'subvar': True }
        for i in range(len(path)-1):
            if path[i] not in self:
                self.add_node(path[i], **kwargs)
            self._check_connect(path[i], path[i+1])
            self.add_edge(path[i], path[i+1])

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

    def find_edges(self, predicate, nodes=None, data=False):
        """Returns all edges in the graph such that the predicate
        returns True.  The predicate must take args of the form:
        def pred(graph, u, v, data) where data is the data dict for that
        edge.
        """
        if data:
            return [(u,v,dat) for u, v, dat in self.edges(nodes, data=True)
                            if predicate(self, u, v, dat)]
        else:
            return [(u,v) for u, v, dat in self.edges(nodes, data=True)
                            if predicate(self, u, v, dat)]

    def find_nodes(self, predicate, data=False):
        """Returns all nodes in the graph such that the predicate
        returns True. The predicate must take args of the form:
        def pred(graph, node, data) where data is the data dict for that
        node.
        """
        if data:
            return [(n,dat) for n, dat in self.nodes(data=True)
                            if predicate(self, n, dat)]
        else:
            return [n for n, dat in self.nodes(data=True)
                            if predicate(self, n, dat)]

    def list_connections(self, show_passthrough=True):
        return self.find_edges(is_connection)

    def get_sources(self, name):
        return [u for u,v in self.in_edges(name)]

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

    def check_connect(self, srcpath, destpath):
        pass

    def get_interior_edges(self, comps):
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
        


