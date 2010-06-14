#public symbols
__all__ = ["Driver"]




from enthought.traits.api import implements, List, Instance
from enthought.traits.trait_base import not_none
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.interfaces import IDriver
from openmdao.main.exceptions import RunStopped
from openmdao.main.component import Component
from openmdao.main.workflow import Workflow
from openmdao.main.expression import Expression, ExpressionList

    
class Driver(Component):
    """ A Driver iterates over a workflow of Components until some condition
    is met. """
    
    implements(IDriver)
    
    workflow = Instance(Workflow, allow_none=True)
    
    def __init__(self, doc=None):
        super(Driver, self).__init__(doc=doc)
        self.workflow = None
        self._iter = None
        self._workflow_set = False
        
    def _get_workflow(self):
        if self.workflow:
            return self.workflow
        else:
            return self.parent.workflow

    def is_valid(self):
        """Return False if any Component in our workflow(s) is invalid,
        or if any of our public variables is invalid, or if any public
        variable referenced by any of our Expressions is invalid.
        """
        if super(Driver, self).is_valid() is False:
            return False
        
        # driver is invalid if any of its Expressions reference
        # invalid Variables or if the Expression itself is invalid
        for name in self.get_expr_names(iotype='in'):
            if not self.get_valid(name):
                return False
            rv = getattr(self, name)
            if isinstance(rv, list):
                for entry in rv:
                    if not entry.refs_valid():
                        return False
            else:
                if not rv.refs_valid():
                    return False
                    
        # force execution if any component in the workflow is invalid
        for comp in self._get_workflow().contents():
            if not comp.is_valid():
                return False

        return True

    def _pre_execute (self):
        """Call base class *_pre_execute* after determining if we have any invalid
        ref variables, which will cause us to have to regenerate our ref dependency graph.
        """
        if not self.is_valid():
            self._call_execute = True
        super(Driver, self)._pre_execute()
        
        if self._call_execute:
            if self in self._get_workflow().contents():
                self.raise_exception("Driver '%s' is a member of it's own workflow!" %
                                     self.name, RuntimeError)


    def remove_from_workflow(self, component):
        """Remove the specified component from our workflow."""
        if self.workflow:
            self.workflow.remove(component)

    def iteration_set(self):
        """Return a set of all Components in our workflow, and 
        recursively in any workflow in any driver in our workflow.
        """
        allcomps = set()
        for child in self._get_workflow().contents():
            allcomps.add(child)
            if isinstance(child, Driver):
                allcomps.update(child.iteration_set())
        return allcomps
        
    def execute(self):
        """ Iterate over a workflow of Components until some condition
        is met. If you don't want to structure your driver to use *pre_iteration*,
        *post_iteration*, etc., just override this function. As a result, none
        of the <start/pre/post/continue>_iteration() functions will be called.
        """
        self._iter = None
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()

    def step(self):
        """Similar to the 'execute' function, but this one only 
        executes a single Component from the workflow each time
        it's called.
        """
        if self._iter is None:
            self.start_iteration()
            self._iter = self._step()
        try:
            self._iter.next()
        except StopIteration:
            self._iter = None
            raise
        raise RunStopped('Step complete')
        
    def _step(self):
        while self.continue_iteration():
            self.pre_iteration()
            for junk in self._step_workflow():
                yield
            self.post_iteration()
        self._iter = None
        raise StopIteration()
    
    def _step_workflow(self):
        while True:
            try:
                self._get_workflow().step()
            except RunStopped:
                pass
            yield

            
    def stop(self):
        self._stop = True
        self._get_workflow().stop()

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
        """Runs the workflow of components."""
        wf = self._get_workflow()
        if len(wf) == 0:
            self._logger.warning("'%s': workflow is empty!" % self.get_pathname())
        wf.run()

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration

    #def get_referenced_comps(self, iotype=None):
        #"""Return a set of names of Components that we reference based on the 
        #contents of our Expressions and ExpressionLists.  If iotype is
        #supplied, return only component names that are referenced by ref
        #variables with matching iotype.
        #"""
        #if self._expr_comps[iotype] is None:
            #comps = set()
        #else:
            #return self._expr_comps[iotype]
    
        #for name in self.get_expr_names(iotype):
            #obj = getattr(self, name)
            #if isinstance(obj, list):
                #for entry in obj:
                    #comps.update(entry.get_referenced_compnames())
            #else:
                #comps.update(obj.get_referenced_compnames())
                
        #self._expr_comps[iotype] = comps
        #return comps
        
    #def get_expr_graph(self, iotype=None):
        #"""Return the dependency graph for this Driver based on
        #Expressions and ExpressionLists.
        #"""
        #if self._expr_graph[iotype] is not None:
            #return self._expr_graph[iotype]
        
        #self._expr_graph[iotype] = nx.DiGraph()
        #name = self.name
        
        #if iotype == 'out' or iotype is None:
            #self._expr_graph[iotype].add_edges_from([(name,rv) 
                                  #for rv in self.get_referenced_comps(iotype='out')])
            
        #if iotype == 'in' or iotype is None:
            #self._expr_graph[iotype].add_edges_from([(rv, name) 
                                  #for rv in self.get_referenced_comps(iotype='in')])
        #return self._expr_graph[iotype]
    