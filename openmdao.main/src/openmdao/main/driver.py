#public symbols
__all__ = ["Driver"]


from enthought.traits.api import implements, List, Instance

from openmdao.main.interfaces import ICaseRecorder, IDriver, IComponent, \
                                     obj_has_interface 
from openmdao.main.exceptions import RunStopped
from openmdao.main.component import Component
from openmdao.main.workflow import Workflow
from openmdao.main.dataflow import Dataflow

    
class Driver(Component):
    """ A Driver iterates over a workflow of Components until some condition
    is met. """
    
    implements(IDriver)

    recorder = Instance(ICaseRecorder, desc='Case recorder for iteration data', 
                        required=False)

    workflow = Instance(Workflow, allow_none=True)
    
    def __init__(self, doc=None):
        self._workflows = []
        self._iter = None
        super(Driver, self).__init__(doc=doc)
        self.add_workflow('workflow', Dataflow(self))
        
    def add_workflow(self, name, wf):
        """Add a new Workflow with the given name to this Driver"""
        if isinstance(wf, Workflow):
            setattr(self, name, wf)
            self._workflows.append(wf)
        else:
            self.raise_exception("'%s' is not a Workflow" % name,
                                 TypeError)
            
    def remove_workflow(self, name):
        wf = getattr(self, name, None)
        try:
            self._workflows.remove(wf)
        except:
            self.raise_exception("'%s' is not a member of the Workflow list" % name,
                                 NameError)
        
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
        for wf in self._workflows:
            for comp in wf.contents():
                if not comp.is_valid():
                    return False

        return True

    def config_changed(self):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed.
        """
        super(Driver, self).config_changed()
        try:
            wfs = self._workflows
        except:
            pass  # early on, self._workflows may not exist yet
        else:
            for wf in wfs:
                wf.config_changed()

    def _pre_execute (self):
        """Call base class *_pre_execute* after determining if we have any invalid
        ref variables, which will cause us to have to regenerate our ref dependency graph.
        """
        if not self.is_valid():
            self._call_execute = True
        super(Driver, self)._pre_execute()

    def remove_from_workflow(self, component):
        """Remove the specified component from our workflow(s).
        """
        for wf in self._workflows:
            wf.remove(component)

    def iteration_set(self):
        """Return a set of all Components in our workflow(s), and 
        recursively in any workflow in any Driver in our workflow(s).
        """
        allcomps = set()
        for wf in self._workflows:
            for child in wf.contents():
                allcomps.add(child)
                if isinstance(child, Driver):
                    allcomps.update(child.iteration_set())
        return allcomps
        
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by any Expression or ExpressionList 
        traits in this Driver, ignoring any dependencies on components that are
        inside of this Driver's iteration set.
        """
        iternames = set([c.name for c in self.iteration_set()])
        conn_list = super(Driver, self).get_expr_depends()
        new_list = []
        for src, dest in conn_list:
            if src not in iternames and dest not in iternames:
                new_list.append((src, dest))
        return new_list

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
                self.workflow.step()
            except RunStopped:
                pass
            yield

    def stop(self):
        self._stop = True
        self.workflow.stop()

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
        """Runs workflow"""
        wf = self.workflow
        if len(wf) == 0:
            self._logger.warning("'%s': workflow is empty!" % self.get_pathname())
        wf.run()

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration
