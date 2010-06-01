#public symbols
__all__ = ["Driver"]




from enthought.traits.api import implements, List, Instance
from enthought.traits.trait_base import not_none
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.interfaces import IDriver
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
        #self._expr_graph = { None: None, 'in': None, 'out': None }
        #self._expr_comps = { None: None, 'in': None, 'out': None }
        self.workflow = None
    
    def _pre_execute (self):
        """Call base class *_pre_execute* after determining if we have any invalid
        ref variables, which will cause us to have to regenerate our ref dependency graph.
        """
        if self._call_execute:
            super(Driver, self)._pre_execute()
            return
        
        exprnames = self.get_expr_names(iotype='in')
        
        if not all(self.get_valids(exprnames)):
            self._call_execute = True
            # force regeneration of _expr_graph, _expr_comps, _iteration_comps
            #self._expr_graph = { None: None, 'in': None, 'out': None } 
            #self._expr_comps = { None: None, 'in': None, 'out': None }
            
        super(Driver, self)._pre_execute()
        
        if not self._call_execute:
            # force execution of the driver if any of its Expressions reference
            # invalid Variables
            for name in exprnames:
                rv = getattr(self, name)
                if isinstance(rv, list):
                    for entry in rv:
                        if not entry.refs_valid():
                            self._call_execute = True
                            return
                else:
                    if not rv.refs_valid():
                        self._call_execute = True
                        return
                
    def execute(self):
        """ Iterate over a workflow of Components until some condition
        is met. If you don't want to structure your driver to use *pre_iteration*,
        *post_iteration*, etc., just override this function. As a result, none
        of the <start/pre/post/continue>_iteration() functions will be called.
        """
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()

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
        if self.parent:
            self.workflow = self.parent.workflow
            self.workflow.run()
        else:
            self.raise_exception('Driver cannot run referenced components without a parent',
                                 RuntimeError)

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
    