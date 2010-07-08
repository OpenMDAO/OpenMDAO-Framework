import ast

from enthought.traits.api import Instance, Either, Function, Method
from openmdao.main.api import Driver, Expression, Workflow
from openmdao.main.exceptions import RunStopped


class ForLoop(Driver):
    """A driver with an initialization workflow and a boolean termination function 
    in addition to the usual iterative workflow.
    """

    init_workflow = Instance(Workflow, allow_none=True,
                             desc="Initialization workflow. This workflow will be executed once"
                                  " before the iterative workflow is executed.")
    
    continuation_funct_body = Str('return False', iotype='in', 
                                 desc='The body of a function that is called before each iteration'
                                 ' to determine if iteration should continue. The function should'
                                 ' return True if iteration should continue and False if not.' 
                                 ' The function takes a single arg which is a reference to the'
                                 ' ForLoop object.')

    def __init__(self, doc=None):
        super(Iterate, self).__init__(doc)
        self.current_iteration = 0
        self._continue_funct = None
    
    def _str_continue_funct_wrapper(self):
        pass
            
    def _continuation_funct_changed(self, oldstr, newstr):
        funct_parts = ["def continue_iter(self):"]
        for line in newstr.split('\n'):
            funct_parts.append('    %s' % line)
        functstr = '\n'.join(funct_parts)
        self._continue_funct = compile(functstr, '<string>', 'exec')
    
    def start_iteration(self):
        if init_workflow is not None:
            init_workflow.run()

    def execute(self):
        """Run the initialization workflow, then iterate over self.workflow
        until the termination function returns False.
        """
        self.start_iteration()
        while self.continue_iteration():
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()
