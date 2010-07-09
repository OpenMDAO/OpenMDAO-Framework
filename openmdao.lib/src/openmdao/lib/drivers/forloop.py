from enthought.traits.api import Instance
from openmdao.main.api import Driver, Expression, Workflow, Dataflow
from openmdao.main.exceptions import RunStopped

class ForLoop(Driver):
    """A driver with an initialization workflow and a boolean termination function 
    in addition to the usual iterative workflow.
    """

    init_workflow = Instance(Workflow, allow_none=True,
                             desc="Initialization workflow. This workflow will be executed once"
                                  " each time this Driver is executed.")
    
    def __init__(self, doc=None):
        super(ForLoop, self).__init__(doc)
        self.add_workflow('init_workflow', Dataflow(self))
        
    def start_iteration(self):
        if self.init_workflow is not None:
            self.init_workflow.run()
