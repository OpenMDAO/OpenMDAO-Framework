""" Class definition for Assembly """


import networkx as nx
from networkx.algorithms.traversal import topological_sort_recursive, \
                                          strongly_connected_components

def _cvt_names_to_graph(srcpath, destpath):
    """Translates model pathnames into pathnames in term of
    our 'fake' graph nodes @exin, @bin, @bout, and @exout.
    """
    srccompname, _, srcvarname = srcpath.partition('.')
    destcompname, _, destvarname = destpath.partition('.')
    
    if srccompname == 'parent':  # external input
        srccompname = '@exin'
        srcvarname = srcpath
        if not destvarname:
            destcompname = '@bin'  # ext input to boundary var
            destvarname = destpath
    elif destcompname == 'parent':  # external output
        destcompname = '@exout'
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

#fake nodes for boundary  and passthrough connections
_fakes = ['@exin', '@exout', '@bin', '@bout']

class DependencyGraph(object):
    """
    A dependency graph for Components.  Each edge contains a _Link object, which 
    maps all connected inputs and outputs between the two Components.  Graph
    nodes starting with '@' are abstract nodes that represent boundary connections.
    """

    def __init__(self):
        self._graph = nx.DiGraph()
        self._graph.add_nodes_from(_fakes) 
        
    def __contains__(self, compname):
        """Return True if this graph contains the given component."""
        return compname in self._graph
    
    def __len__(self):
        return len(self._graph) - len(_fakes)
        
    def subgraph(self, nodelist):
        return self._graph.subgraph(nodelist)
    
    def copy_graph(self):
        graph = self._graph.copy()
        graph.remove_nodes_from(_fakes)
        return graph
    
    def get_source(self, destvar):
        exin = self._graph['@exin']
        if '@bin' in exin:
            dests = exin['@bin']['link']._dests
            src = dests.get(destvar)
            if src: return src
        
        for u,v,data in self._graph.in_edges('@bout', data=True):
            src = data['link']._dests.get(destvar)
            if src:
                return '.'.join([u, src])
            
        return None

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
            
        force: bool, optional
            If True, force invalidation to continue even if a component in
            the dependency chain was already invalid.
        """
        graph = self._graph
        stack = []
        for cname, varset in zip(cnames, varsets):
            stack.append((cname, varset))
        outset = set()  # set of changed boundary outputs
        while(stack):
            src, varset = stack.pop()
            for dest,link in self.out_links(src):
                if varset is None:
                    srcvars = set(link._srcs.keys())
                else:
                    srcvars = varset
                if dest == '@bout':
                    outset.update(link.get_dests(varset))
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
        if show_passthrough:
            nbunch = None
        else:
            nbunch = [g for g in self._graph.nodes() if g[0] != '@']
        for u,v,data in self._graph.edges(nbunch, data=True):
            link = data['link']
            if u == '@bin':
                conns.extend([(src, '.'.join([v,dest])) for dest,src in link._dests.items()])
            elif v == '@bout':
                conns.extend([('.'.join([u,src]), dest) for dest,src in link._dests.items()])
            else:
                conns.extend([('.'.join([u,src]), '.'.join([v,dest])) for dest,src in link._dests.items()])
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
            
    def get_mapping(self, srcname, destname):
        """Return a tuple of the form (srcs, dests) where srcs and dests
        are dicts that map sources to destinations and destinations to
        sources respectively.  If there is no connection between the
        specified source and destination, (None,None) is returned.
        """
        try:
            link = self._graph[srcname][destname]['link']
        except KeyError:
            return (None,None)
        return (link._srcs, link._dests)

    def in_links(self, cname):
        """Return a list of the form [(compname,link), (compname2,link2)...]
        containing each incoming link to the given component and the name
        of the connected component.
        """
        return [(u,data['link']) for u,v,data in self._graph.in_edges(cname, data=True)]
    
    def out_links(self, cname):
        """Return a list of the form [(compname,link), (compname2,link2)...]
        containing each outgoing link from the given component and the name
        of the connected component.
        """
        return [(v,data['link']) for u,v,data in self._graph.edges(cname, data=True)]

    def var_edges(self, name=None):
        """Return a list of outgoing edges connecting variables, with 'fake'
        node names converted to real names.
        """
        if name is None:
            names = self._graph.nodes()
        else:
            names = [name]
        edges = []
        for name in names:
            for u,v,data in self._graph.edges(name, data=True):
                lst = [('.'.join([u,src]), '.'.join([v,dest])) 
                                    for dest,src in data['link']._dests.items()]
                for u,v in lst:
                    if u.startswith('@'):
                        u = u.split('.', 1)[1]
                    if v.startswith('@'):
                        v = v.split('.', 1)[1]
                edges.append((u,v))
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
                lst = [('.'.join([u,src]), '.'.join([v,dest])) 
                                     for dest,src in data['link']._dests.items()]
                for u,v in lst:
                    if u.startswith('@'):
                        u = u.split('.', 1)[1]
                    if v.startswith('@'):
                        v = v.split('.', 1)[1]
                edges.append((u,v))
        return edges
    
    def get_connected_inputs(self):
        try:
            link = self._graph['@exin']['@bin']['link']
        except KeyError:
            return []
        
        return link._dests.keys()
    
    def get_connected_outputs(self):
        try:
            link = self._graph['@bout']['@exout']['link']
        except KeyError:
            return []
        return link._srcs.keys()
    
    def connect(self, srcpath, destpath):
        """Add an edge to our Component graph from 
        *srccompname* to *destcompname*.
        """
        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if srccompname == '@exin' and destcompname != '@bin':
            # this is an auto-passthrough input so we need 2 links
            if '@bin' not in graph['@exin']:
                link = _Link('@exin', '@bin')
                graph.add_edge('@exin', '@bin', link=link)
            else:
                link = graph['@exin']['@bin']['link']
            link.connect(srcvarname, '.'.join([destcompname,destvarname]))
            if destcompname not in graph['@bin']:
                link = _Link('@bin', destcompname)
                graph.add_edge('@bin', destcompname, link=link)
            else:
                link = graph['@bin'][destcompname]['link']
            link.connect('.'.join([destcompname,destvarname]), destvarname)
        elif destcompname == '@exout' and srccompname != '@bout':
            # this is an auto-passthrough output so we need 2 links
            if '@exout' not in graph['@bout']:
                link = _Link('@bout', '@exout')
                graph.add_edge('@bout', '@exout', link=link)
            else:
                link = graph['@bout']['@exout']['link']
            link.connect('.'.join([srccompname,srcvarname]), destvarname)
            if '@bout' not in graph[srccompname]:
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
            
            if topological_sort_recursive(graph):
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

    def disconnect(self, srcpath, destpath):
        """Disconnect the given variables."""
        graph = self._graph
        srccompname, srcvarname, destcompname, destvarname = \
                           _cvt_names_to_graph(srcpath, destpath)
        
        if srccompname == '@exin' and destcompname != '@bin':
            link = graph['@exin']['@bin']['link']
            link.disconnect(srcvarname, '.'.join([destcompname,destvarname]))
            if len(link) == 0:
                self._graph.remove_edge('@exin', '@bin')
            link = graph['@bin'][destcompname]['link']
            link.disconnect('.'.join([destcompname,destvarname]), destvarname)
            if len(link) == 0:
                self._graph.remove_edge('@bin', destcompname)
        elif destcompname == '@exout' and srccompname != '@bout':
            link = graph['@bout']['@exout']['link']
            link.disconnect('.'.join([srccompname,srcvarname]), destvarname)
            if len(link) == 0:
                self._graph.remove_edge('@bout', '@exout')
            link = graph[srccompname]['@bout']['link']
            link.disconnect(srcvarname,'.'.join([srccompname,srcvarname]))
            if len(link) == 0:
                self._graph.remove_edge(srccompname, '@bout')
        else:
            link = self._graph[srccompname][destcompname]['link']
            link.disconnect(srcvarname, destvarname)
            if len(link) == 0:
                self._graph.remove_edge(srccompname, destcompname)

    #def push_data(self, srccompname, scope):
        #for destcompname, link in self.out_links(srccompname):
            #link.push(scope, srccompname, destcompname)


def dump_graph(g):
    """Prints out a simple text representation of the graph."""
    for u,v,data in g.edges(data=True):
        print '%s  -->  %s' % (u,v)
        for src,dests in data['link']._srcs.items():
            print '    %s : %s' % (src, dests)

            
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
        if dest in self._dests:
            raise RuntimeError("%s is already connected" % dest)
        if src not in self._srcs:
            self._srcs[src] = []
        self._srcs[src].append(dest)
        self._dests[dest] = src
        
    def disconnect(self, src, dest):
        del self._dests[dest]
        dests = self._srcs[src]
        dests.remove(dest)
        if len(dests) == 0:
            del self._srcs[src]
    
    def invalidate(self, destcomp, varlist=None):
        if varlist is None:
            destcomp.set_valid(self._dests.keys(), False)
        else:
            destcomp.set_valid(varlist, False)

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
                src = self._dests.get(name)
                if src:
                    srcs.append(src)
            return srcs

    #def push(self, scope, srccompname, destcompname):
        #"""Push the values of all sources to their corresponding destinations
        #for this link.
        #"""
        ## TODO: change to use multiset calls
        #srccomp = getattr(scope, srccompname)
        #destcomp = getattr(scope, destcompname)
        
        #for src,dests in self._srcs.items():
            #for dest in dests:
                #try:
                    #srcval = srccomp.get_wrapped_attr(src)
                #except Exception, err:
                    #scope.raise_exception(
                        #"error retrieving value for %s from '%s'" %
                        #(src,srccompname), type(err))
                #try:
                    #srcname = '.'.join([srccompname,src])
                    #destcomp.set(dest, srcval, src=srcname)
                #except Exception, exc:
                    #dname = '.'.join([destcompname,dest])
                    #scope.raise_exception("cannot set '%s' from '%s': %s" % 
                                          #(dname, srcname, exc), type(exc))
        