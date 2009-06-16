#public symbols
__all__ = ["Driver"]

__version__ = "0.1"


from zope.interface import implements
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.interfaces import IDriver, IComponent, IAssembly
from openmdao.main.component import Component, STATE_WAITING, STATE_IDLE
from openmdao.main import Assembly, RefVariable, RefVariableArray
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.drivertree import DriverForest, create_labeled_graph

class Driver(Assembly):
    """ A Driver iterates over a collection of Components until some condition
    is met. """
    
    implements(IDriver)

    def __init__(self, name, parent=None, doc=None):
        super(Driver, self).__init__(name, parent, doc=doc)
        self._ref_graph = { None: None, INPUT: None, OUTPUT: None }
        self._ref_comps = { None: None, INPUT: None, OUTPUT: None }
        self.force_graph_regen()
    
    def force_graph_regen(self):
        self._iteration_comps = None
        self._simple_iteration_subgraph = None
        self._simple_iteration_set = None    
        self._driver_tree = None
        
    def _pre_execute (self):
        """Call base class _pre_execute after determining if we have any invalid
        ref variables, which will cause us to have to regenerate our ref dependency graph.
        """
        exec_needed = False
        refs = [v for v in self.get_inputs() if isinstance(v,RefVariable) or
                                                isinstance(v,RefVariableArray)]
        invalid_refs = [v for v in refs if v.valid is False]
        if len(invalid_refs) > 0:
            exec_needed = True
            # force regeneration of _ref_graph, _ref_comps, _iteration_comps
            self._ref_graph = { None: None, INPUT: None, OUTPUT: None } 
            self._ref_comps = { None: None, INPUT: None, OUTPUT: None }
            self.force_graph_regen()
        super(Driver, self)._pre_execute()
        
        # force execution of the driver if any of its RefVariables reference
        # invalid Variables
        for rv in refs:
            if rv.refs_invalid():
                exec_needed = True
                break
        
        self._execute_needed |= exec_needed
                
    def execute(self):
        """ Iterate over a collection of Components until some condition
        is met. If you don't want to structure your driver to use pre_iteration,
        post_iteration, etc., just override this function. As a result, none
        of the <start/pre/post/continue>_iteration() functions will be called.
        """
        self.state = STATE_WAITING
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()
        self.state = STATE_IDLE

    def start_iteration(self):
        """Called just prior to the beginning of an iteration loop. This can 
        be overridden by inherited classes. It can be used to perform any 
        necessary pre-iteration initialization.
        """
        self._continue = True

    def continue_iteration(self):
        """Return False to stop iterating."""
        return self._continue
    
    def pre_iteration(self):
        """Called prior to each iteration."""
        pass
        
    def run_iteration(self):
        """Runs the full set of components, in dataflow order, 
        that are part of our iteration loop, including any nested drivers.
        """
        if self.parent:
            drivers = self.parent.drivers
            if len(drivers) > 1:
                dtree = self._get_driver_tree()  # determine driver nesting hierarchy
                if len(dtree.children) > 0:  # we have nested drivers
                    graph = self.parent.get_component_graph().copy()
                    for drv in dtree.drivers_iter():
                        graph.add_edges_from(drv.get_ref_graph().edges_iter())
                    strongs = strongly_connected_components(graph)
                    for strong in strongs:
                        if self.name in strong:
                            subgraph = create_labeled_graph(graph.subgraph(nbunch=strong))
                            for nested in dtree.children: # collapse immediate children
                                nested.collapse_graph(subgraph)
                            subgraph.remove_edges_from(
                                self.get_ref_graph(iostatus=INPUT).edges_iter())
                            sorted = nx.topological_sort(subgraph)
                            for comp in sorted:
                                if comp != self.name:
                                    getattr(self.parent, comp).run()
                else:  # no nested drivers
                    self._run_simple_iteration()
            else:  # single driver case
                self._run_simple_iteration()
        else:
            self.raise_exception('Driver cannot run referenced components without a parent',
                                 RuntimeError)

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration
            
    def get_referenced_comps(self, iostatus=None):
        """Return a set of names of Components that we reference based on the 
        contents of our RefVariables and RefVariableArrays.  If iostatus is
        supplied, return only component names that are referenced by ref
        variables with matching iostatus.
        """
        if self._ref_comps[iostatus] is None:
            comps = set()
        else:
            return self._ref_comps[iostatus]
        
        if iostatus is None:
            for refin in [v for v in self._pub.values() if (isinstance(v,RefVariable) or 
                                                            isinstance(v,RefVariableArray))]:
                comps.update(refin.get_referenced_compnames())
        else:
            for refin in [v for v in self._pub.values() if (isinstance(v,RefVariable) or 
                                                            isinstance(v,RefVariableArray))
                          and v.iostatus==iostatus]:
                comps.update(refin.get_referenced_compnames())
                
        self._ref_comps[iostatus] = comps
        return comps
        
    def get_ref_graph(self, iostatus=None):
        """Returns the dependency graph for this Driver based on
        RefVariables and RefVariableArrays.
        """
        if self._ref_graph[iostatus] is not None:
            return self._ref_graph[iostatus]
        
        self._ref_graph[iostatus] = nx.DiGraph()
        name = self.name
        
        if iostatus is OUTPUT or iostatus is None:
            self._ref_graph[iostatus].add_edges_from([(name,rv) 
                                  for rv in self.get_referenced_comps(iostatus=OUTPUT)])
            
        if iostatus is INPUT or iostatus is None:
            self._ref_graph[iostatus].add_edges_from([(rv, name) 
                                  for rv in self.get_referenced_comps(iostatus=INPUT)])
        return self._ref_graph[iostatus]
    
    def _get_simple_iteration_subgraph(self):
        """Return a graph of our iteration loop (ourself plus all components we
        iterate over). This does not include nested drivers, unless they are
        explicitly referenced by us through a ReferenceVariable or they are
        explicitly connected to another component in our set of iteration
        components via a non-ReferenceVariable connection.
        """
        if self._simple_iteration_subgraph is None:
            graph = self.parent.get_component_graph().copy()
            # add all of our RefVariable edges and find any strongly connected
            # components (SCCs) that are created as a result
            graph.add_edges_from(self.get_ref_graph().edges_iter())
            strcons = strongly_connected_components(graph)
            # No cycles are allowed other than the one we possibly just
            # created, so our cycle must be the first SCC in the list. If there
            # is no cycle, then this driver may not be in the first SCC since
            # they are sorted by size and they will all have size 1.
            if len(strcons[0]) > 1:
                self._simple_iteration_subgraph = graph.subgraph(nbunch=strcons[0])
            else:
                # no cycle, so just return a graph with the driver and any components
                # it references
                self._simple_iteration_subgraph = graph.subgraph(nbunch=[self.name]+
                                                                 list(self.get_referenced_comps()))
            self._simple_iteration_set = None
        
        return self._simple_iteration_subgraph
    
    def simple_iteration_set(self):
        """Return the set of components iterated over by this driver, not including
        other drivers that may be nested within this driver.
        """
        if self._simple_iteration_set is None:
            iterset = set(self._get_simple_iteration_subgraph().nodes_iter())
            iterset.remove(self.name)
            self._simple_iteration_set = iterset
        return self._simple_iteration_set
        
    def _get_driver_tree(self):
        """Returns the DriverTree object corresponding to this Driver from the 
        DriverTree hierarchy in the parent Assembly."""
        if not self._driver_tree:
            self._driver_tree = DriverForest(self.parent.drivers).locate(self)
        return self._driver_tree    
            
    def _run_simple_iteration(self):
        """There are no nested drivers. Just run our subgraph with our 
        input edges removed.
        """
        graph = self._get_simple_iteration_subgraph()
        graph.remove_edges_from(self.get_ref_graph(iostatus=INPUT).edges_iter())
        itercomps = nx.topological_sort(graph)
        for comp in itercomps:
            if comp != self.name:
                getattr(self.parent, comp).run()
        