
import networkx as nx

def create_labeled_graph(parent_graph):
    graph = nx.LabeledDiGraph()
    graph .add_nodes_from([(name, set()) for name in parent_graph.nodes_iter()])
    graph.add_edges_from(parent_graph.edges_iter())
    graph.add_edges_from(parent_graph.in_edges_iter())
    return graph


class DriverForest(object):
    """Inserting drivers into this object will result in the drivers
    being arranged in a forrest structure indicating which drivers are 
    nested within others.  
    """
    def __init__(self, drivers):
        self.trees = []
        for drv in drivers:
            self.insert(drv)

    def insert(self, driver):
        if len(self.trees) == 0:
            self.trees.append(DriverTree(driver))
        else:
            dtree = DriverTree(driver)
            for i,tree in enumerate(self.trees):
                val = tree.insert(dtree)
                if val > 0:
                    return
                elif val < 0:
                    dtree.insert(tree)
                    self.trees[i] = dtree
                    return
            self.trees.append(dtree)
            
    def drivers_iter(self):
        for tree in self.trees:
            for drv in tree.drivers_iter():
                yield drv

    def collapse_graph(self, parent_graph):
        """Take the given graph, and collapse driver loops into single driver
        nodes while maintaining all of the edges to nodes outside of the loop.
        If the given graph contains multiple driver loops, they must be nested
        or an exception will be raised.
        
        Returns a modified copy of the graph.
        """
        graph = create_labeled_graph(parent_graph)
        if len(self.trees) > 1:
            raise RuntimeError('no single root node in DriverForest')
        
        for tree in self.trees:
            tree.collapse_graph(graph)
            
        return graph
    
    def locate(self, driver):
        """Return the DriverTree corresponding to the given driver"""
        for tree in self.trees:
            dtree = tree.locate(driver)
            if dtree: return dtree
        return None
                    
class DriverTree(object):
    def __init__(self, data):
        self.data = data
        self.children = []

    def locate(self, driver):
        if self.data is driver:
            return self
        for child in self.children:
            tree = child.locate(driver)
            if tree: return tree
        return None
    
    def insert(self, dtree):
        """If the given driver is nested within one of the drivers in
        this tree, insert it in the tree in the appropriate place.
        The heuristic used to determine nesting is
        that a driver is nested within another if its set of iteration 
        components is a strict subset of the other driver's set of iteration
        components.
        
        Returns 1 if the driver was inserted in the tree, or -1 if this
        driver is nested within the new driver, or 0 if neither driver is
        nested within the other.
        """
        newset = dtree.data.simple_iteration_set()
        newlen = len(newset)
        if newlen == 0: return 0
        myset = self.data.simple_iteration_set()
        mylen = len(myset)
        if mylen == 0: return 0
        interset = newset.intersection(myset)
        interlen = len(interset)
        
        if interlen == newlen:
            if newlen < mylen:  # new driver's iteration set is a strict subset of ours
                for i,child in enumerate(self.children):
                    val = child.insert(dtree)
                    if val > 0:
                        return 1
                    elif val < 0:
                        dtree.insert(child)
                        self.children[i] = dtree
                        return 1
                self.children.append(dtree)
                return 1 
            elif newlen == mylen:
                raise RuntimeError(('Drivers %s and %s iterate over the same '+
                                    'set of components (%s), so their order cannot be '+
                                    'determined') % (self.data.get_pathname(),
                                                     dtree.data.get_pathname(),
                                                     sorted(list(myset))))
        elif interlen == mylen:  # our iter set is a strict subset of the new driver
            return -1
        elif interlen > 0:  # iteration sets overlap
                raise RuntimeError(('Drivers %s and %s have overlap (%s) in their '+
                                    'iteration sets, so their order cannot be determined')
                                   % (self.data.get_pathname(),
                                      dtree.data.get_pathname(),
                                      sorted(list(interset))))
        
        return 0
        
    def collapse_graph(self, graph):
        """Take the given graph, and collapse driver loops into single driver
        nodes while maintaining all of the edges to nodes outside of the loop.
        
        Modifies the graph in place, keeping pre-collapsed subgraphs nodes 
        as node data for collapsed nodes.
        """
        for child in self.children:
            child.collapse_graph(graph)            
        self._collapse(graph)

    def _collapse(self, graph):
        name = self.data.name
        to_add = []
        itergraph = self.data._get_simple_iteration_subgraph()
        nodes = itergraph.nodes()
        added_nodes = []
        
        # we may have already collapsed away some of the nodes in our iteration subgraph,
        # so add any collapsed nodes to our node list so we don't miss any edges
        if len(self.children) > 0:
            for n,data in graph.nodes_iter(data=True):
                if len(data) > 0 and n not in itergraph:
                    added_nodes.append(n)
        
        all_nodes = nodes+added_nodes
        for u,v in graph.edges_iter(nbunch=all_nodes): # outgoing edges
            if v not in all_nodes:
                to_add.append((name, v)) # add output edge to collapsed loop
                
        for u,v in graph.in_edges_iter(nbunch=all_nodes):
            if u not in all_nodes:
                to_add.append((u, name)) # add input edge to collapsed loop

        newset = set()
        for node, data in graph.nodes_iter(data=True):
            newset = newset.union(data)
        newset.update(nodes)
        newset.remove(name)
        
        to_remove = [n for n in all_nodes if n in graph]
        graph.remove_nodes_from(to_remove)
        graph.add_node(name, data=newset)
        graph.add_edges_from(to_add)
        return nodes, to_add
        
        
    def drivers_iter(self):
        yield self.data
        for child in self.children:
            for drv in child.drivers_iter():
                yield drv
            