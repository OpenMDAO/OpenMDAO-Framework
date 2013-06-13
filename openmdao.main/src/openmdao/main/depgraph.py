""" Class definition for Dependencygraph, an object for interacting with an
Assembly's network graph."""

import sys

# pylint: disable-msg=E0611,F0401
import networkx as nx
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.printexpr import transform_expression
from openmdao.main.variable import is_legal_name

class AlreadyConnectedError(RuntimeError):
    """Special exception that is raised when trying to connect to an input
    that is already connected."""
    pass


def _split_expr(text):
    """Take an expression string and return varpath, expr"""
    if text.startswith('@') or is_legal_name(text):
        return text, text
    
    expr = ExprEvaluator(text)
    return expr.get_referenced_varpaths().pop(), text
    

def _cvt_names_to_graph(srcpath, destpath):
    """Translates model pathnames into pathnames in term of
    our 'fake' graph nodes @xin, @bin, @bout, and @xout.
    """
    srcvar, _ = _split_expr(srcpath)
    
    srccompname, _, srcvarname = srcvar.partition('.')
    destcompname, _, destvarname = destpath.partition('.')
    
    if srccompname == 'parent':  # external input
        srccompname = '@xin'
        srcvarname = srcpath
        if not destvarname:
            destcompname = '@bin'  # ext input to boundary var
            destvarname = destpath
    elif destcompname == 'parent':  # external output
        destcompname = '@xout'
        destvarname = destpath
        if not srcvarname:
            srccompname = '@bout'  # ext output from boundary var
            srcvarname = srcpath
    elif not srcvarname:  # internal connection from a boundary var
        srcvarname = srcpath
        srccompname = '@bin'
    elif not destvarname:  # internal connection to a boundary var
        destvarname = destpath
        destcompname = '@bout'
    elif srcvar != srcpath:
        srcvarname = transform_expression(srcpath, 
                     { '.'.join([srccompname, srcvarname]): srcvarname })
        
    return (srccompname, srcvarname, destcompname, destvarname)

#fake nodes for boundary and passthrough connections
_fakes = ['@xin', '@xout', '@bin', '@bout']

# to use as a quick check for exprs to avoid overhead of constructing an
# ExprEvaluator
_exprset = set('+-/*[]()&| %<>!')

class DependencyGraph(object):
    """
    A dependency graph for Components. Each edge contains a _Link object,
    which maps all connected inputs and outputs between the two Components.
    Graph nodes starting with '@' are abstract nodes that represent boundary
    connections.
    
    @xin is external to our input boundary

    @bin is our input boundary

    @bout is our output boundary

    @xout is external to our output boundary
    """

    def __init__(self):
        self._graph = nx.DiGraph()
        self._graph.add_nodes_from(_fakes)
        self._allsrcs = {}
        
    def __contains__(self, compname):
        """Return True if this graph contains the given component."""
        return compname in self._graph
    
    def __eq__(self, other):
        if isinstance(other, DependencyGraph):
            if self._graph.nodes() == other._graph.nodes():
                if self._graph.edges() == other._graph.edges():
                    return True
        return False
    
    def __ne__(self, other):
        return not self.__eq__(other)
            
    def copy_graph(self):
        """Return a copy of the graph without the 'fake' boundary edges."""
        graph = self._graph.copy()
        graph.remove_nodes_from(_fakes)
        return graph
    
    def get_source(self, destpath):
        """Return the source variable path for the given destination path."""
        return self._allsrcs.get(destpath)

    def add(self, name):
        """Add the name of a Component to the graph."""
        self._graph.add_node(name)

    def remove(self, name):
        """Remove the name of a Component from the graph. It is not
        an error if the component is not found in the graph.
        """
        self.disconnect(name)
        self._graph.remove_node(name)
                                    
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
        
        # Keep track of the comps we already invalidated, so we
        # don't keep doing them. This allows us to invalidate loops.
        invalidated = [] 
        
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

    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for u, v, data in self._graph.edges(data=True):
            link = data['link']
            # leave out external connections to boundary
            if v == '@bin' or u == '@bout': 
                continue
            elif u == '@bin':
                if show_passthrough:
                    for dest, src in link._dests.items():
                        vpath, expr = _split_expr(src)
                        if '.' not in vpath:
                            conns.append((src, '.'.join([v, dest])))
            elif v == '@bout':
                if show_passthrough:
                    for dest, src in link._dests_ext.items():
                        if '.' not in dest:
                            conns.append((src, dest))
            else:
                for dest, src in link._dests_ext.items():
                    conns.append((src, '.'.join([v, dest])))
        return conns
    
    def list_autopassthroughs(self):
        """Returns a list of autopassthrough connections as (src, dest)
        tuples."""
        conns = []
        #namesub = re.compile('(([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*)')
        inlink = self.get_link('@xin', '@bin')
        if inlink:
            conns.extend([(v, u) 
                           for u, v in inlink._dests.items() if '.' in u and '.' in v])
        outlink = self.get_link('@bout', '@xout')
        if outlink:
            conns.extend([(v, u) 
                           for u, v in outlink._dests.items() if '.' in u and '.' in v])
        return conns
    
    def get_link(self, srcname, destname):
        """Return the link between the two specified nodes.  If there is no 
        connection, then None is returned.
        """
        try:
            return self._graph[srcname][destname]['link']
        except KeyError:
            return None

    def in_links(self, cname):
        """Return a list of the form [(compname, link), (compname2, link2)...]
        containing each incoming link to the given component and the name
        of the connected component.
        """
        return [(u, data['link']) \
                for u, v, data in self._graph.in_edges(cname, data=True)]
    
    def out_links(self, cname):
        """Return a list of the form [(compname, link), (compname2, link2)...]
        containing each outgoing link from the given component and the name
        of the connected component.
        """
        return [(v, data['link']) \
                for u, v, data in self._graph.edges(cname, data=True)]

    def var_edges(self, name=None):
        """Return a list of outgoing edges connecting variables."""
        edges = []
        for u, v, data in self._graph.edges(name, data=True):
            for dest, src in data['link']._dests_ext.items():
                edges.append((src, '.'.join([v, dest])))
        return edges
    
    def var_in_edges(self, name=None):
        """Return a list of incoming edges connecting variables."""
        edges = []
        for u, v, data in self._graph.in_edges(name, data=True):
            for dest, src in data['link']._dests_ext.items():
                edges.append((src, '.'.join([v, dest])))
        return edges
    
    def get_connected_inputs(self):
        try:
            return self._graph['@xin']['@bin']['link']._dests.keys()
        except KeyError:
            return []
    
    def get_connected_outputs(self):
        try:
            return self._graph['@bout']['@xout']['link']._srcs.keys()
        except KeyError:
            return []
    
    def check_connect(self, srcpath, destpath):
        """ Raise exception if destpath is already connected """
        
        msg = "'%s' is already connected to source '%s'"
        if destpath in self._allsrcs:
            raise AlreadyConnectedError(msg % \
                                        (destpath, self._allsrcs[destpath]))
        
        dpdot = destpath+'.'
        for dst, src in self._allsrcs.items():
            if destpath.startswith(dst+'.') or dst.startswith(dpdot):
                raise AlreadyConnectedError(msg % (dst, src))
                
    def get_interior_edges(self, comps):
        """ Returns the set of all output edges that are interior to the set
        of components supplied. For example, you may want the set of all 
        outputs in a driver's workflow that are connected to other comps in
        that workflow and, hence, need derivatives.
        
        comps: list of str
            List of component names
        """
        
        out_set = set(self.var_edges(comps))
        in_set = set(self.var_in_edges(comps))
        
        return in_set.intersection(out_set)
        
    def connect(self, srcpath, destpath):
        """Add an edge to our Component graph from 
        *srccompname* to *destcompname*. 
        """
        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if srccompname == '@xin' and destcompname != '@bin':
            # this is an auto-passthrough input so we need 2 links
            if '@bin' not in graph['@xin']:
                link = _Link('@xin', '@bin')
                graph.add_edge('@xin', '@bin', link=link)
            else:
                link = graph['@xin']['@bin']['link']
            link.connect(srcvarname, '.'.join([destcompname, destvarname]))
            if destcompname not in graph['@bin']:
                link = _Link('@bin', destcompname)
                graph.add_edge('@bin', destcompname, link=link)
            else:
                link = graph['@bin'][destcompname]['link']
            link.connect('.'.join([destcompname, destvarname]), destvarname)
        elif destcompname == '@xout' and srccompname != '@bout':
            # this is an auto-passthrough output so we need 2 links
            if '@xout' not in graph['@bout']:
                link = _Link('@bout', '@xout')
                graph.add_edge('@bout', '@xout', link=link)
            else:
                link = graph['@bout']['@xout']['link']
            link.connect('.'.join([srccompname, srcvarname]), destvarname)
            if srccompname not in graph or '@bout' not in graph[srccompname]:
                link = _Link(srccompname, '@bout')
                graph.add_edge(srccompname, '@bout', link=link)
            else:
                link = graph[srccompname]['@bout']['link']
            link.connect(srcvarname, '.'.join([srccompname, srcvarname]))
        else:
            try:
                link = graph[srccompname][destcompname]['link']
            except KeyError:
                link = _Link(srccompname, destcompname)
                graph.add_edge(srccompname, destcompname, link=link)
            
            link.connect(srcvarname, destvarname)
                    
        self._allsrcs[destpath] = srcpath
        
    def _comp_connections(self, cname):
        """Returns a list of tuples of the form (srcpath, destpath) for all
        connections to and from the specified component.
        """
        conns = self.var_edges(cname)
        conns.extend(self.var_in_edges(cname))
        return conns
    
    def _var_connections(self, path):
        """Returns a list of tuples of the form (srcpath, destpath) for all
        connections to and from the specified variable.
        """
        conns = []
        vpath, expr = _split_expr(path)
        cname, _, vname = vpath.partition('.')
            
        if not vname:  # a boundary variable
            for name in ['@bin', '@bout']:
                for u, v in self.var_edges(name):
                    if u.split('.', 1)[1] == path:
                        conns.append((u, v))
                for u, v in self.var_in_edges(name):
                    if v.split('.', 1)[1] == path:
                        conns.append((u, v))
        else:
            for u, v in self.var_edges(cname):
                if u == path:
                    conns.append((u, v))
            for u, v in self.var_in_edges(cname):
                if v == path:
                    conns.append((u, v))
        return conns
    
    def connections_to(self, path):
        """Returns a list of tuples of the form (srcpath, destpath) for
        all connections between the variable or component specified
        by *path*.
        """
        if path in self._graph:
            return self._comp_connections(path)
        else:
            return self._var_connections(path)

    def disconnect(self, srcpath, destpath=None):
        """Disconnect the given variables."""
        if destpath is None:
            for src, dest in self.connections_to(srcpath):
                self.disconnect(src, dest)
            return

        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if srccompname == '@xin' and destcompname != '@bin':
            # this is an auto-passthrough input, so there are two connections
            # that must be removed (@xin to @bin and @bin to some internal
            # component)
            link = graph['@xin']['@bin']['link']
            link.disconnect(srcvarname, '.'.join([destcompname, destvarname]))
            if len(link) == 0:
                self._graph.remove_edge('@xin', '@bin')
            link = graph['@bin'][destcompname]['link']
            link.disconnect('.'.join([destcompname, destvarname]), destvarname)
            if len(link) == 0:
                self._graph.remove_edge('@bin', destcompname)
        elif destcompname == '@xout' and srccompname != '@bout':
            # this is an auto-passthrough output, so there are two
            # connections that must be removed (@bout to @xout and some
            # internal component to @bout)
            if graph['@bout'].get('@xout'):
                link = graph['@bout']['@xout']['link']
                link.disconnect('.'.join([srccompname, srcvarname]), 
                                destvarname)
                if len(link) == 0:
                    self._graph.remove_edge('@bout', '@xout')
            if graph[srccompname].get('@bout'):
                link = graph[srccompname]['@bout']['link']
                link.disconnect(srcvarname, 
                                '.'.join([srccompname, srcvarname]))
                if len(link) == 0:
                    self._graph.remove_edge(srccompname, '@bout')
        else:
            link = self.get_link(srccompname, destcompname)
            if link:
                link.disconnect(srcvarname, destvarname)
                if len(link) == 0:
                    self._graph.remove_edge(srccompname, destcompname)
        
        try:
            del self._allsrcs[destpath]
        except KeyError:
            pass
        dpdot = destpath+'.'
        for d in [k for k in self._allsrcs if k.startswith(dpdot)]:
            del self._allsrcs[d]

    def dump(self, stream=sys.stdout):
        """Prints out a simple sorted text representation of the graph."""
        tupdict = {}
        tuplst = []
        for u, v, data in self._graph.edges(data=True):
            tupdict[(u, v)] = data['link']
            tuplst.append((u, v))
            
        tuplst.sort()
        for tup in tuplst:
            link = tupdict[tup]
            stream.write('%s -> %s\n' % tup)
            for src, dests in link._srcs.items():
                stream.write('   %s : %s\n' % (src, dests))

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
            tmpset.update(graph.predecessors(node))
        
        tmpset = set([start])
        while tmpset:
            node = tmpset.pop()
            if node in fwdset:
                continue
            fwdset.add(node)
            tmpset.update(graph.successors(node))
        
        return fwdset.intersection(backset)

class _Link(object):
    """A Class for keeping track of all connections between two Components."""
    def __init__(self, srcnode, destnode):
        self._srcs = {}
        self._dests = {}
        # dict of dest names to src expressions translated into parent context
        self._dests_ext = {} 
        self.srcnode = srcnode
        self.destnode = destnode

    def __len__(self):
        return len(self._srcs)

    #def _translate_down(self, text):
        #if is_legal_name(text):
            #compname, _, varname = text.partition('.')
        #else:
            #expr = ExprEvaluator(text)
            #varpath = expr.get_referenced_varpaths().pop()
            #compname, _, varname = varpath.partition('.')
            #if varname:
                #text = transform_expression(text, { varpath: varname })
        #return compname, varname, text
    
    def _translate_up(self, text, node):
        """Upscoping"""
        if is_legal_name(text):
            return '.'.join([node, text])

        expr = ExprEvaluator(text)
        varpath = expr.get_referenced_varpaths().pop()
        return transform_expression(text, { varpath: '.'.join([node, varpath]) })
    
    def connect(self, src, dest):
        """Add connection between src and dest."""
        srcvar, srcexpr = _split_expr(src)
        self._srcs.setdefault(srcvar, []).append(dest)
        self._dests[dest] = srcexpr
        self._dests_ext[dest] = self._translate_up(srcexpr, self.srcnode)
        
    def disconnect(self, src, dest):
        """Remove connection between src and dest."""
        if dest in self._dests:
            srcvar, srcexpr = _split_expr(src)
            if self._dests[dest] == srcexpr:
                del self._dests[dest]
                del self._dests_ext[dest]
                dests = self._srcs[srcvar]
                dests.remove(dest)
                if len(dests) == 0:
                    del self._srcs[srcvar]
    
    def get_dests(self, srcs=None):
        """Return the list of destination vars that match the given source
        vars. If srcs is None, return a list of all dest vars. Ignore any src
        vars that are not part of this link.
        """
        if srcs is None:
            return self._dests.keys()
        else:
            empty = []
            dests = []
            for name in srcs:
                dests.extend(self._srcs.get(name, empty))
            return dests
    
    def get_srcs(self, dests=None):
        """Return the list of source vars that match the given dest vars. If
        dests is None, return a list of all src vars. Ignore any dest vars
        that are not part of this link.
        """
        if dests is None:
            return self._srcs.keys()
        else:
            srcs = []
            for name in dests:
                src = self._dests.get(name)
                if src is not None:
                    srcs.append(src)
            return srcs


