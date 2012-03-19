""" Class definition for Assembly """

import sys
import StringIO

import networkx as nx
from networkx.algorithms.dag import topological_sort_recursive,is_directed_acyclic_graph
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.printexpr import eliminate_expr_ws

class AlreadyConnectedError(RuntimeError):
    pass

def _cvt_names_to_graph(srcpath, destpath):
    """Translates model pathnames into pathnames in term of
    our 'fake' graph nodes @xin, @bin, @bout, and @xout.
    """
    srccompname, _, srcvarname = srcpath.partition('.')
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
        srcvarname = srccompname
        srccompname = '@bin'
    elif not destvarname:  # internal connection to a boundary var
        destvarname = destcompname
        destcompname = '@bout'
        
    return (srccompname, srcvarname, destcompname, destvarname)

#fake nodes for boundary and passthrough connections
_fakes = ['@xin', '@xout', '@bin', '@bout']

_exprset = set('+-/*[]()&| %<>!') # to use as a quick check for exprs to avoid overhead of constructing an ExprEvaluator

class DependencyGraph(object):
    """
    A dependency graph for Components.  Each edge contains a _Link object, which 
    maps all connected inputs and outputs between the two Components.  Graph
    nodes starting with '@' are abstract nodes that represent boundary 
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
        graph = self._graph.copy()
        graph.remove_nodes_from(_fakes)
        return graph
    
    def get_source(self, destpath):
        return self._allsrcs.get(destpath)
        #cname, _, vname = destpath.partition('.')
        #if vname: # internal dest
            #for srccomp, link in self.in_links(cname):
                #src = link._dests.get(vname)
                #if src:
                    #if srccomp[0] == '@':
                        #return src
                    #else:
                        #return '.'.join([srccomp,src])
        #else: # boundary dest
            #try:
                #dests = self._graph['@xin']['@bin']['link']._dests
            #except KeyError:
                #dests = {}
            #srclst = dests.get(destpath)
            #if srclst:
                #return srclst
            #for u,v,data in self._graph.in_edges('@bout', data=True):
                #srclst = data['link']._dests.get(destpath)
                #if srclst:
                    #return ['.'.join([u, src]) for src in srclst]
        #return None

    def add(self, name):
        """Add the name of a Component to the graph."""
        self._graph.add_node(name)

    def remove(self, name):
        """Remove the name of a Component from the graph. It is not
        an error if the component is not found in the graph.
        """
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
        graph = self._graph
        stack = zip(cnames, varsets)
        outset = set()  # set of changed boundary outputs
        while(stack):
            src, varset = stack.pop()
            for dest,link in self.out_links(src):
                if varset is None:
                    srcvars = set(link._srcs.keys())
                else:
                    srcvars = varset
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
                            stack.append((dest, outs))
        return outset

    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for u,v,data in self._graph.edges(data=True):
            link = data['link']
            if v == '@bin' or u == '@bout': # leave out external connections to boundary
                continue
            elif u == '@bin':
                if show_passthrough:
                    for dest,srclst in link._dests.items():
                        for src in srclst:
                            if '.' not in src:
                                conns.append((src, '.'.join([v,dest])))
            elif v == '@bout':
                if show_passthrough:
                    for dest,srclst in link._dests.items():
                        for src in srclst:
                            if '.' not in dest:
                                conns.append(('.'.join([u,src]), dest))
            else:
                for dest,srclst in link._dests.items():
                    for src in srclst:
                        conns.append(('.'.join([u,src]), '.'.join([v,dest])))
        return conns
    
    def in_map(self, cname, varset):
        """Yield a tuple of lists of the form (srccompname, srclist, destlist) for each link,
        where all dests in destlist are found in varset.  If no dests are found in varset,
        a tuple will not be returned at all for that link.
        """
        for u,v,data in self._graph.in_edges(cname, data=True):
            srcs = []
            dests = []
            link = data['link']
            matchset = varset.intersection(link._dests.keys())
            for dest in matchset:
                dests.append(dest)
                srcs.append(link._dests[dest])
            if not dests or not srcs:
                continue
            yield (u, srcs, dests)
            
    def get_link(self, srcname, destname):
        """Return the link between the two specified nodes.  If there is no 
        connection then None is returned.
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
        return [(u,data['link']) for u,v,data in self._graph.in_edges(cname, data=True)]
    
    def out_links(self, cname):
        """Return a list of the form [(compname, link), (compname2, link2)...]
        containing each outgoing link from the given component and the name
        of the connected component.
        """
        return [(v,data['link']) for u,v,data in self._graph.edges(cname, data=True)]

    def var_edges(self, name=None):
        """Return a list of outgoing edges connecting variables."""
        if name is None:
            names = self._graph.nodes()
        else:
            names = [name]
        edges = []
        for name in names:
            for u,v,data in self._graph.edges(name, data=True):
                for dest, srclst in data['link']._dests.items():
                    for src in srclst:
                        edges.extend([('.'.join([u,src]), '.'.join([v,dest]))])
        return edges
    
    def var_in_edges(self, name=None):
        """Return a list of incoming edges connecting variables."""
        if name is None:
            names = self._graph.nodes()
        else:
            names = [name]
        edges = []
        for name in names:
            for u,v,data in self._graph.in_edges(name, data=True):
                for dest, srclst in data['link']._dests.items():
                    for src in srclst:
                        edges.extend([('.'.join([u,src]), '.'.join([v,dest]))])
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
    
    def connect(self, srcpath, destpath, scope, expr=None):
        """Add an edge to our Component graph from 
        *srccompname* to *destcompname*.  If expr is defined, it's
        the actual ExprEvaluator object for the source.
        """
        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if not (expr and self._allsrcs.get(destpath) == expr.text):
            if not _exprset.intersection(srcpath):
                dpdot = destpath+'.'
                for dst,src in self._allsrcs.items():
                    if destpath.startswith(dst+'.') or dst.startswith(dpdot) or destpath==dst:
                        raise AlreadyConnectedError("%s is already connected to source %s" %
                                                    (dst, src))
        #oldsrc = self.get_source('.'.join([destcompname,destvarname]))
        #if oldsrc:
            #raise AlreadyConnectedError("%s is already connected to source %s" %
                                        #(destpath, oldsrc))
                
        if srccompname == '@xin' and destcompname != '@bin':
            # this is an auto-passthrough input so we need 2 links
            if '@bin' not in graph['@xin']:
                link = _Link('@xin', '@bin')
                graph.add_edge('@xin', '@bin', link=link)
            else:
                link = graph['@xin']['@bin']['link']
            link.connect(srcvarname, '.'.join([destcompname,destvarname]))
            if destcompname not in graph['@bin']:
                link = _Link('@bin', destcompname)
                graph.add_edge('@bin', destcompname, link=link)
            else:
                link = graph['@bin'][destcompname]['link']
            link.connect('.'.join([destcompname,destvarname]), destvarname)
        elif destcompname == '@xout' and srccompname != '@bout':
            # this is an auto-passthrough output so we need 2 links
            if '@xout' not in graph['@bout']:
                link = _Link('@bout', '@xout')
                graph.add_edge('@bout', '@xout', link=link)
            else:
                link = graph['@bout']['@xout']['link']
            link.connect('.'.join([srccompname,srcvarname]), destvarname)
            if srccompname not in graph or '@bout' not in graph[srccompname]:
                link = _Link(srccompname, '@bout')
                graph.add_edge(srccompname, '@bout', link=link)
            else:
                link = graph[srccompname]['@bout']['link']
            link.connect(srcvarname,'.'.join([srccompname,srcvarname]))
        else:
            try:
                link = graph[srccompname][destcompname]['link']
            except KeyError:
                link=_Link(srccompname, destcompname)
                graph.add_edge(srccompname, destcompname, link=link)
            
            if is_directed_acyclic_graph(graph):
                link.connect(srcvarname, destvarname)
            else:   # cycle found
                # do a little extra work here to give more info to the user in the error message
                strongly_connected = strongly_connected_components(graph)
                if len(link) == 0:
                    graph.remove_edge(srccompname, destcompname)
                for strcon in strongly_connected:
                    if len(strcon) > 1:
                        raise RuntimeError(
                            'circular dependency (%s) would be created by connecting %s to %s' %
                                     (str(strcon), 
                                      '.'.join([srccompname,srcvarname]), 
                                      '.'.join([destcompname,destvarname])))
        if expr is not None:
            self._allsrcs[destpath] = expr.text
        else:
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
        cname, _, vname = path.partition('.')
        if not vname:  # a boundary variable
            for name in ['@bin', '@bout']:
                for u,v in self.var_edges(name):
                    if u.split('.',1)[1] == path:
                        conns.append((u, v))
                for u,v in self.var_in_edges(name):
                    if v.split('.',1)[1] == path:
                        conns.append((u, v))
        else:
            for u,v in self.var_edges(cname):
                if u == path:
                    conns.append((u, v))
            for u,v in self.var_in_edges(cname):
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
            for src,dest in self.connections_to(srcpath):
                self.disconnect(src, dest)
            return

        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if srccompname == '@xin' and destcompname != '@bin':
            # this is an auto-passthrough input, so there are two connections
            # that must be removed (@xin to @bin and @bin to some internal component)
            link = graph['@xin']['@bin']['link']
            link.disconnect(srcvarname, '.'.join([destcompname,destvarname]))
            if len(link) == 0:
                self._graph.remove_edge('@xin', '@bin')
            link = graph['@bin'][destcompname]['link']
            link.disconnect('.'.join([destcompname,destvarname]), destvarname)
            if len(link) == 0:
                self._graph.remove_edge('@bin', destcompname)
        elif destcompname == '@xout' and srccompname != '@bout':
            # this is an auto-passthrough output, so there are two connections
            # that must be removed (@bout to @xout and some internal component to @bout)
            if graph['@bout'].get('@xout'):
                link = graph['@bout']['@xout']['link']
                link.disconnect('.'.join([srccompname,srcvarname]), destvarname)
                if len(link) == 0:
                    self._graph.remove_edge('@bout', '@xout')
            if graph[srccompname].get('@bout'):
                link = graph[srccompname]['@bout']['link']
                link.disconnect(srcvarname,'.'.join([srccompname,srcvarname]))
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
        for u,v,data in self._graph.edges(data=True):
            tupdict[(u,v)] = data['link']
            tuplst.append((u,v))
            
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
    def __init__(self, srccomp, destcomp):
        self._srcs = {}
        self._dests = {}
        self._srccomp = srccomp
        self._destcomp = destcomp

    def __len__(self):
        return len(self._srcs)

    def connect(self, src, dest):
        if src not in self._srcs:
            self._srcs[src] = []
        if dest not in self._dests:
            self._dests[dest] = []
        self._srcs[src].append(dest)
        self._dests[dest].append(src)
        
    def disconnect(self, src, dest):
        if dest in self._dests:
            del self._dests[dest]
            dests = self._srcs[src]
            dests.remove(dest)
            if len(dests) == 0:
                del self._srcs[src]
    
    def get_dests(self, srcs=None):
        """Return the list of destination vars that match the given source vars.
        If srcs is None, return a list of all dest vars.  Ignore any src vars
        that are not part of this link.
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
        """Return the list of source vars that match the given dest vars.
        If dests is None, return a list of all src vars.  Ignore any dest vars
        that are not part of this link.
        """
        if dests is None:
            return self._srcs.keys()
        else:
            srcs = []
            for name in dests:
                srclst = self._dests.get(name, ())
                srcs.extend(srclst)
            return srcs


