""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

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