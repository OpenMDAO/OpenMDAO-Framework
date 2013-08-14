import sys
from array import array
import itertools
import networkx as nx
from networkx import dfs_edges

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

class TransClosure(object):
    def __init__(self, graph):
        # self._idxmap = {}
        # self._tcmap = {}
        # for src, dest in graph.edges():
        #     self.add_edge(src, dest)

        nodes = graph.nodes()
        initer = [0]*len(nodes)
        self._idxmap = dict([(n, i) for i, n in enumerate(nodes)])
        self._tcmap = dict([(n, array('b', initer)) for n in nodes])
        self._calc_closure(graph)

    def _calc_closure(self, graph):
        # this is O(n*(n+m)), but our graphs should be
        # relatively sparse, so typically it should be closer
        # to n^2 than n^3

        # this assumes NO partial invalidation of component outputs,
        # i.e., if ANY inputs are invalidated, then ALL outputs
        # are invalidated
        tcmap = self._tcmap
        idxmap = self._idxmap
        for src in graph.nodes():
            for nsrc, ndest in dfs_edges(graph, src):
                tcmap[src][idxmap[ndest]] = 1
    
    def __getitem__(self, src):
        """Return all nodes that depend on src."""

        arr = self._tcmap[src]
        idxmap = self._idxmap
        return [n for n, i in idxmap.items() if arr[i]] 

    def is_connected(self, src, dest):
        """Returns True if dest is downstream of src."""
        return self._tcmap[src][self._idxmap[dest]] == 1

    # def add_node(self, node):
    #     if node not in self._tcmap:
    #         for name, arr in self._tcmap.items():
    #             arr.append(0)
    #         self._idxmap[node] = len(self._idxmap)
    #         self._tcmap[node] = array('b', [0]*len(self._idxmap))

    # def add_edge(self, src, dest):
    #     if src not in self._tcmap:
    #         self.add_node(src)
    #     if dest not in self._tcmap:
    #         self.add_node(dest)

    #     # we need to ad dest's dependents to src, as well as to
    #     # any node that has src as a dependent
    #     sarr = self._tcmap[src]
    #     darr = self._tcmap[dest]

    #     idxmap = self._idxmap
    #     sarr[idxmap[dest]] = 1

    #     for i in range(len(sarr)):
    #         if darr[i]:
    #             sarr[i] == 1

    #     # since we're pretty sparse, create an index set
    #     # containing only the filled entries
    #     idxset = set([i for i in darr if darr[i]])
    #     idxset.add(idxmap[dest])
    #     sidx = idxmap[src]
    #     for name, arr in self._tcmap.items():
    #         if arr[sidx]:
    #             for i in idxset:
    #                 arr[i] = 1


class DiGraph(nx.DiGraph):
    """A version of networkx.DiGraph that provides a few
    extra 'finder' and 'visitor' methods that are based on 
    node/edge metadata.
    """

    def find_edges(self, predicate, nodes=None, data=False):
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
        """Returns an iterator over all nodes in the graph such that 
        the predicate returns True. The predicate must take args of the 
        form: pred(graph, node).
        """
        if data:
            for n, dat in self.nodes_iter(data=True):
                if predicate(self, n):
                    yield (n, dat)
        else:
            for n in self.nodes_iter():
                if predicate(self, n):
                    yield n


class DependencyGraph(DiGraph):
    def __init__(self):
        super(DependencyGraph, self).__init__()
        self.config_changed()

    def config_changed(self):
        self._component_graph = None
        self._trans_closure = None

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
                for u,v in self.in_edges_iter((base,)):
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

    def get_interior_connections(self, comps=None):
        if comps is None:
            compset = set(self.find_nodes(is_comp_or_pseudo_node))
        else:
            compset = set(comps)
        return [(u,v) for u, v in self.find_edges(is_connection)
                   if u.split('.', 1)[0] in compset and 
                      v.split('.', 1)[0] in compset]

    def connections_to(self, path, direction=None):
        if is_comp_or_pseudo_node(self, path):
            conns = []
            for vname in itertools.chain(self.list_inputs(path, connected=True),
                                         self.list_outputs(path, connected=True)):
                conns.extend(self.connections_to(vname, direction))
            return conns
        else:
            return self._var_connections(path, direction)

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

    get_sources = DiGraph.predecessors

    def get_source(self, name):
        preds = self.pred.get(name)
        if preds:
            if len(preds) == 1:
                return preds.keys()[0]
            else:
                raise RuntimeError("'%s' has multiple sources" % name)
        return None

    def _all_vars(self, node, direction=None):
        """Return a list of nodes containing all nodes that are one
        or two connections out from the starting node that are prefixed
        by the starting node.  This captures all basevar and subvar 
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
        return [n for n in bunch if n.startswith(ndot)]

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

    def component_graph(self):
        """Return a subgraph containing only Components
        and PseudoComponents and edges between them. 
        """
        if self._component_graph is not None:
            return self._component_graph

        compset = set(self.find_nodes(is_comp_or_pseudo_node))
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

    def get_connected_inputs(self):
        ins = []
        for node in self.find_nodes(is_external_node):
            succs = self.succ.get(node)
            if succs:
                ins.extend(succs.keys())
        return ins

    def get_connected_outputs(self):
        outs = []
        for node in self.find_nodes(is_external_node):
            preds = self.pred.get(node)
            if preds:
                outs.extend(preds.keys())
        return outs

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

    def invalidate_deps(self, scope, cnames, varsets, force=False):
        """Walk through all dependent nodes in the graph, invalidating all
        variables that depend on output sets for the given component names.
        
        scope: Component
            Scoping object containing this dependency graph.
            
        cnames: list of str
            Names of starting nodes.
            
        varsets: list of sets of str
            Sets of names of outputs from each starting node.
            
        force: bool (optional)
            If True, force invalidation to continue even if a component in
            the dependency chain was already invalid.
        """

        stack = zip(cnames, varsets)
        outset = set()  # set of changed boundary outputs
        
        # Keep track of the comp/var we already invalidated, so we
        # don't keep doing them. This allows us to invalidate loops.
        invalidated = set()

        compgraph = self.component_graph()
        tc = self.trans_closure()
        
        while(stack):
            srccomp, varset = stack.pop()
            invalidated.append(srccomp)
            if srccomp:
                varset = ['.'.join([srccomp,v]) for v in varset]
                for dcomp in compgraph.successors(srccomp):
                    inputs = self.list_inputs(dcomp, connected=True)
                    dests = []
                    for v in varset:
                        dests.extend([i for i in inputs 
                                          if tc.is_connected(v, i)])
                    if dests:
                        idx = len(dcomp)
                        dests = [s[idx:] for s in dests]
                        comp = getattr(scope, dcomp)
                        outs = comp.invalidate_deps(varnames=dests, 
                                                    force=force)
                        if (outs is None) or outs:
                            if dcomp not in invalidated:
                                stack.append((dcomp, outs))

            for dest, link in self.out_links(srccomp):
                if dest == '@bout':
                    bouts = link.get_dests(varset)
                    outset.update(bouts)
                    scope.set_valid(bouts, False)
                # else:
                #     dests = link.get_dests(varset)
                #     if dests:
                #         comp = getattr(scope, dest)
                #         outs = comp.invalidate_deps(varnames=dests, 
                #                                     force=force)
                #         if (outs is None) or outs:
                #             if dest not in invalidated:
                #                 stack.append((dest, outs))
        return outset

    def list_inputs(self, cname, connected=False):
        """Return a list of names of input nodes to a component.
        If connected is True, return only connected inputs.
        """
        if not is_comp_node(self, cname):
            raise RuntimeError("'%s' is not a component node" % cname)
        if connected:
            return [n for n in self.pred[cname].keys() 
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
            return [n for n in self.succ[cname].keys() 
                                            if self.out_degree(n)>0]
        else:
            return self.succ[cname].keys()

    def list_autopassthroughs(self):
        """Returns a list of autopassthrough connections as (src, dest)
        tuples.
        """
        conns = []
        for n in self.find_nodes(is_external_node):
            for p in self.predecessors_iter(n):
                if self.has_edge(p, n) and '.' in p:
                    conns.append((p, n))
            for s in self.successors_iter(n):
                if self.has_edge(n, s) and '.' in s:
                    conns.append((n, s))

        return conns



if __name__ == '__main__':
    class Dummy(object):
        pass
        


