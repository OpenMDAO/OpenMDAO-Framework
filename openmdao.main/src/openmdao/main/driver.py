#public symbols
__all__ = ["Driver"]

__version__ = "0.1"


from zope.interface import implements

from openmdao.main.interfaces import IDriver, IComponent
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
        while True:
            self.run_iteration()
            if not self.continue_iteration():
                break
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
        be overridden by inherited classes."""
        pass
    
    def continue_iteration(self):
        """Return False if the iteration should end. This should be overridden
        by inherited classes."""
        return False
    
    def run_iteration(self):
        """Run a single iteration over a group of Components."""
        if self.parent:
            self.parent.workflow.run()

    def _execute_if_needed(self):
        """Override the Component version to force Drivers to execute even if
        they have no invalid outputs.
        """
        if IComponent.providedBy(self.parent):
            self.parent.update_inputs(self)
        if __debug__: self._logger.debug('executing %s' % self.get_pathname())
        self.execute()
            