#public symbols
__all__ = ["Driver"]

__version__ = "0.1"


from zope.interface import implements

from openmdao.main.interfaces import IDriver
from openmdao.main.component import Component, STATE_WAITING


class Driver(Component):
    """ A driver is used to run a workflow in an assembly. """
    
    implements(IDriver)

    def __init__(self, name, parent=None, doc=None):
        super(Driver, self).__init__(name, parent, doc=doc)
        
    def execute(self):
        """ Run the assembly by invoking run() on the workflow. """
        self.state = STATE_WAITING
        status = self.parent.workflow.run()
        #self.state = STATE_RUNNING
        return status

    def step(self):
        """Execute a single step."""
        return self.parent.workflow.step()
        
    def stop(self):
        """ Stop the assembly by stopping the workflow. """
        self._stop = True
        self.parent.workflow.stop()
        
