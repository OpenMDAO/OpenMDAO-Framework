""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

import networkx as nx
from networkx.algorithms.components import strongly_connected_components

try:
    from numpy import ndarray
except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray
    
    
from openmdao.main.array_helpers import flattened_value
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.ndepgraph import edge_dict_to_comp_list
from openmdao.main.pseudoassembly import from_PA_var, to_PA_var
from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.vartree import VariableTree

__all__ = ['CyclicWorkflow']

# SequentialWorkflow gives us the add and remove methods.
class CyclicWorkflow(SequentialWorkflow):
    """A CyclicWorkflow consists of a collection of Components that contains
    loops in the graph.
    """   
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        
        super(CyclicWorkflow, self).__init__(parent, scope, members)
        self.config_changed()
        
    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(CyclicWorkflow, self).config_changed()
        self._workflow_graph = None
        self._topsort = None
        self._severed_edges = []
        self._mapped_severed_edges = []
        
    def check_config(self):
        """Any checks we need. For now, drivers are not allowed. You can get
        around this by placing them in an assembly."""         
        
        super(CyclicWorkflow, self).check_config()
        
        for comp in self.get_components():
            if has_interface(comp, IDriver):
                msg = 'Subdriver not supported in a cyclicflow. Please ' \
                      'place it in a subassembly.'
                self.scope.raise_exception(msg, RuntimeError)
               
    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the workflow by name. """
        
        super(CyclicWorkflow, self).add(compnames, index, check)
        self.config_changed()

    def remove(self, compname):
        """Remove a component from this Workflow by name."""
        
        super(CyclicWorkflow, self).remove(compname)
        self.config_changed()
        
    def __iter__(self):
        """Iterate through the nodes in some proper order."""
        
        # resolve all of the components up front so if there's a problem it'll
        # fail early and not waste time running components
        scope = self.scope
        return [getattr(scope, n) for n in self._get_topsort()].__iter__()

    def _get_topsort(self):
        """ Return a sorted list of components in the workflow.
        """
        
        if self._topsort is None:
            graph = nx.DiGraph(self._get_collapsed_graph())
            
            cyclic = True
            self._severed_edges = []
            while cyclic:
                
                try:
                    self._topsort = nx.topological_sort(graph)
                    cyclic = False
                    
                except nx.NetworkXUnfeasible:
                    strong = strongly_connected_components(graph)
                    
                    # We may have multiple loops. We only deal with one at
                    # a time because multiple loops create some non-unique
                    # paths.
                    strong = strong[0]
                    
                    # Break one edge of the loop.
                    # For now, just break the first edge.
                    # TODO: smarter ways to choose edge to break.
                    graph.remove_edge(strong[-1], strong[0])
                    
                    # Keep a list of the edges we break, so that a solver
                    # can use them as its independents/dependents.
                    depgraph = self.scope._depgraph
                    edge_set = set(depgraph.get_directional_interior_edges(strong[-1], 
                                                                            strong[0]))
                    
                    self._severed_edges += list(edge_set)
                
        return self._topsort
    
    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it. This graph can be cyclic."""
        
        # Cached
        if self._workflow_graph is None:
       
            contents = self.get_components()
           
            # get the parent assembly's component graph
            scope = self.scope
            compgraph = scope._depgraph.component_graph()
            graph = compgraph.subgraph([c.name for c in contents])
           
            # add any dependencies due to ExprEvaluators
            for comp in contents:
                graph.add_edges_from([tup for tup in comp.get_expr_depends()])
                
            self._workflow_graph = graph
       
        return self._workflow_graph
    
    def derivative_graph(self, inputs=None, outputs=None, fd=False):
        """Returns the local graph that we use for derivatives. For cyclic flows,
        we need to sever edges and use them as inputs/outputs.
        """
    
        if self._derivative_graph is None:
            
            # Cyclic flows need to be severed before derivatives are calculated.
            self._get_topsort()
            
            inputs = []
            outputs = []
            for src, target in self._severed_edges:
                inputs.append(target)
                outputs.append(src)
                
            super(CyclicWorkflow, self).derivative_graph(inputs, outputs, fd, 
                                                         self._severed_edges)
            
            # Finally, we need to map any of our edges if they are in a
            # pseudo-assy
            dgraph = self._derivative_graph
            comps = edge_dict_to_comp_list(dgraph, self.edge_list())
            pa_keys = [name for name in comps if '~' in name]
            
            if len(pa_keys) == 0:
                self._mapped_severed_edges = self._severed_edges
            else:
                self._mapped_severed_edges = []
                for src, target in self._severed_edges:
                    
                    compname, _, varname = src.partition('.')
                    for pa_key in pa_keys:
                        pseudo = dgraph.node[pa_key]['pa_object']
                        if src in pseudo.outputs:
                            src = to_PA_var(src, pseudo.name)
                            break
                
                    compname, _, varname = target.partition('.')
                    for pa_key in pa_keys:
                        pseudo = dgraph.node[pa_key]['pa_object']
                        if target in pseudo.inputs:
                            target = to_PA_var(target, pseudo.name)
                            break
                            
                    self._mapped_severed_edges.append((src, target))
                
            
        return self._derivative_graph

    def set_new_state(self, dv):
        """Adds a vector of new values to the current model state at the
        input edges.

        dv: ndarray (nEdge, 1)
            Array of values to add to the model inputs.
        """
        for src, targets in self._edges.iteritems():
            
            if '@in' in src:
                continue
            
            i1, i2 = self.get_bounds(src)
            
            if isinstance(targets, str):
                targets = [targets]
                
            for target in targets:
                
                if '@out' in target:
                    for sev_src, sev_target in self._mapped_severed_edges:
                        if sev_src == src:
                            target = sev_target
                            break
                
                target = from_PA_var(target)
                old_val = self.scope.get(target)
    
                if isinstance(old_val, float):
                    new_val = old_val + float(dv[i1:i2])
                elif isinstance(old_val, ndarray):
                    shape = old_val.shape
                    if len(shape) > 1:
                        new_val = old_val.flatten() + dv[i1:i2]
                        new_val = new_val.reshape(shape)
                    else:
                        new_val = old_val + dv[i1:i2]
                elif isinstance(old_val, VariableTree):
                    new_val = old_val.copy()
                    self._update(target, new_val, dv[i1:i2])
                else:
                    msg = "Variable %s is of type %s." % (target, type(old_val)) + \
                          " This type is not supported by the MDA Solver."
                    self.scope.raise_exception(msg, RuntimeError)
    
                # Poke new value into the input end of the edge.
                self.scope.set(target, new_val, force=True)
    
                # Prevent OpenMDAO from stomping on our poked input.
                self.scope.set_valid([target.split('[',1)[0]], True)
    
                # Array inputs aren't triggering invalidation
                self.scope._input_updated(target.split('[')[0])
                    
                #(An alternative way to prevent the stomping. This is more
                #concise, but setting an output and allowing OpenMDAO to pull it
                #felt hackish.)
                #self.scope.set(src, new_val, force=True)

    def calculate_residuals(self):
        """Calculate and return the vector of residuals based on the current
        state of the system in our workflow."""
        
        for src, targets in self._edges.iteritems():
            
            if '@in' in src:
                # This residual will always be zero
                continue
            
            i1, i2 = self.get_bounds(src)
            src_val = self.scope.get(from_PA_var(src))
            src_val = flattened_value(src, src_val).reshape(-1, 1)
                
            if isinstance(targets, str):
                targets = [targets]
                
            for target in targets:
                
                if '@out' in target:
                    for sev_src, sev_target in self._mapped_severed_edges:
                        if sev_src == src:
                            target = sev_target
                            break
                
                target_val = self.scope.get(from_PA_var(target))
                target_val = flattened_value(target, target_val).reshape(-1, 1)
            
                self.res[i1:i2] = src_val - target_val

        return self.res
