""" Driver class definition """

#public symbols
__all__ = ["Driver"]


# pylint: disable-msg=E0611,F0401
from enthought.traits.api import implements, List, Instance

from openmdao.main.interfaces import ICaseRecorder, IDriver, IComponent, ICaseIterator, \
                                     IHasEvents, obj_has_interface
from openmdao.main.exceptions import RunStopped
from openmdao.main.component import Component
from openmdao.main.workflow import Workflow
from openmdao.main.dataflow import Dataflow
from openmdao.main.hasevents import HasEvents
from openmdao.util.decorators import add_delegate
from openmdao.main.mp_support import is_instance
from openmdao.main.rbac import rbac

@add_delegate(HasEvents)
class Driver(Component):
    """ A Driver iterates over a workflow of Components until some condition
    is met. """
    
    implements(IDriver, IHasEvents)

    recorder = Instance(ICaseRecorder, desc='Case recorder for iteration data.', 
                        required=False)

    workflow = Instance(Workflow, allow_none=True)
    
    def __init__(self, doc=None):
        self._iter = None
        super(Driver, self).__init__(doc=doc)
        self.workflow = Dataflow(self)
        
    def _workflow_changed(self, oldwf, newwf):
        if newwf is not None:
            newwf._parent = self

    def is_valid(self):
        """Return False if any Component in our workflow(s) is invalid,
        or if any of our variables is invalid.
        """
        if super(Driver, self).is_valid() is False:
            return False

        # force execution if any component in the workflow is invalid
        for comp in self.workflow.get_components():
            if not comp.is_valid():
                return False
        return True

    def check_config (self):
        """Verify that our workflow is able to resolve all of its components."""
        # workflow will raise an exception if it can't resolve a Component
        super(Driver, self).check_config()
        try:
            comps = self.workflow.get_components()
        except AttributeError as err:
            self.raise_exception("Component in workflow failed to resolve: %s" % str(err),
                                 AttributeError)
            
        if hasattr(self, 'check_gradients'):
            self.check_gradients()
        if hasattr(self, 'check_hessians'):
            self.check_hessians()

    def iteration_set(self):
        """Return a set of all Components in our workflow(s), and 
        recursively in any workflow in any Driver in our workflow(s).
        """
        allcomps = set()
        for child in self.workflow.get_components():
            allcomps.add(child)
            if is_instance(child, Driver):
                allcomps.update(child.iteration_set())
        return allcomps
        
    @rbac(('owner', 'user'))
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name,
        dest_comp_name) for each dependency introduced by any ExprEvaluators
        in this Driver, ignoring any dependencies on components that are
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
        """Called prior to each iteration.  This is where iteration events are set."""
        self.set_events()
        
    def run_iteration(self):
        """Runs workflow."""
        wf = self.workflow
        if len(wf) == 0:
            self._logger.warning("'%s': workflow is empty!" % self.get_pathname())
        wf.run(self.ffd_order)
        
    def calc_derivatives(self, first=False, second=False):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""
        self.workflow.calc_derivatives(first, second)
        
    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """ Check derivatives for all components in this workflow."""
        self.workflow.check_derivatives(order, driver_inputs, driver_outputs)
        
    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False  # by default, stop after one iteration

    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed or dependencies may have
        changed.
        """
        super(Driver, self).config_changed(update_parent)
        if self.workflow is not None:
            self.workflow.config_changed()
