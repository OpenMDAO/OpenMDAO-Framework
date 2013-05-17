""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.seqentialflow import SequentialWorkflow

__all__ = ['CyclicFlow']

# SequentialWorkflow gives us the add and remove methods.
class CyclicFlow(SequentialWorkflow):
    """A CyclicFlow consists of a collection of Components that contains
    loops in the graph.
    """   
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        
        super(CyclicFlow, self).__init__(parent, scope, members)   
        
    def check_config(self):
        """Any checks we need. For now, drivers are not allowed. You can get
        around this by placing them in an assembly."""         
        
        for comp in self.get_components():
            if has_interface(comp, IDriver):
                self.scope.raise_exception('circular dependency found between'
                                           ' the following: %s'
                                           % str(strcon[0]), RuntimeError)                