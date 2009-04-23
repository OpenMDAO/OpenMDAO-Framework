#public symbols
__all__ = ["Driver"]

__version__ = "0.1"


from zope.interface import implements
import networkx as nx

from openmdao.main.interfaces import IDriver, IComponent, IAssembly
from openmdao.main.component import Component, STATE_WAITING, STATE_IDLE
from openmdao.main import Assembly


class Driver(Assembly):
    """ A Driver iterates over a collection of Components until some condition
    is met. """
    
    implements(IDriver)

    def __init__(self, name, parent=None, doc=None):
        super(Driver, self).__init__(name, parent, doc=doc)
        
    def execute(self):
        """ Iterate over a collection of Components until some condition
        is met. """
        self.state = STATE_WAITING
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()
        self.state = STATE_IDLE

    def step(self):
        """Execute a single step"""
        return self.parent.workflow.step()
        
    def stop(self):
        """ Stop the Model by stopping the Workflow. """
        self._stop = True
        self.parent.workflow.stop()
    
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
        """Run a single iteration over a group of Components."""
        if self.parent:
            self.parent.workflow.run()

    def post_iteration(self):
        """Called after each iteration."""
        self._continue = False
    
    def _execute_if_needed(self):
        """Overridden from the Component version to force Drivers to 
        execute even if they have no invalid outputs.
        """
        if self.parent and IAssembly.providedBy(self.parent):
            self.parent.update_inputs(self)
        if __debug__: self._logger.debug('executing %s' % self.get_pathname())
        self.execute()
        
