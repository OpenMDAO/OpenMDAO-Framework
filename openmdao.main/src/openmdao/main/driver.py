#public symbols
__all__ = ["Driver"]

__version__ = "0.1"


from zope.interface import implements

from openmdao.main.interfaces import IDriver
from openmdao.main.component import Component, STATE_WAITING
from openmdao.main import Assembly


class Driver(Assembly):
    """ A driver is used to run a Workflow in a Model. """
    
    implements(IDriver)

    def __init__(self, name, parent=None, doc=None):
        super(Driver, self).__init__(name, parent, doc=doc)
        
    def execute(self):
        """ Run the Model by invoking run() on the Workflow. """
        self.state = STATE_WAITING
        status = self.parent.workflow.run()
        #self.state = STATE_RUNNING
        return status

    def step(self):
        """Execute a single step"""
        return self.parent.workflow.step()
        
    def stop(self):
        """ Stop the Model by stopping the Workflow. """
        self._stop = True
        self.parent.workflow.stop()
        
