#public symbols
__all__ = ["Model"]

__version__ = "0.1"

import os

from openmdao.main import Assembly
from openmdao.main.interfaces import IDriver, IWorkflow


class Model(Assembly):
    """A container for Components, a Driver, and a Workflow. It manages
    connections between Components."""
   
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Model, self).__init__(name, parent, doc=doc,
                                       directory=directory)

        self.add_socket('driver', IDriver, doc='A Driver object is required here',
                        required=True)
        self.add_socket('workflow', IWorkflow, doc='A Workflow object is required here',
                        required=True)
        
        # by default, fill the driver and workflow sockets with a simple one-pass
        # driver and a sequential workflow
        self.driver = self.create('openmdao.main.Driver', 'driver')
        self.workflow = self.create('openmdao.main.Workflow','workflow')
   
    def execute(self):
        """run this model"""
        return self.driver.run()    

    def step(self):
        """Execute a single step."""
        self.driver.step()

    def stop(self):
        """ Stop by telling the driver to stop. """
        self._stop = True
        self.driver.stop()

    def remove_child(self, name):
        """Remove the named object from our workflow."""
        self.workflow.remove_node(getattr(self, name))           
        super(Model,self).remove_child(name)

