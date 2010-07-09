import ast
import types
import math

from enthought.traits.api import Instance, Either, Function, Method, TraitType
from openmdao.main.api import Driver, Expression, Workflow, Dataflow
from openmdao.main.exceptions import RunStopped

def _find_funct_def(s):
    """Walk the ast to find a function definition in the given string.
    Return the name of the function, or None if the function is not found.
    """
    node = ast.parse(s)
    for n in ast.iter_child_nodes(node):
        if isinstance(n, ast.FunctionDef):
            return n.name
    return None
    
class _StrOrFunct(TraitType):
    """A trait that can either be a function or a string defining a function"""

    def validate(self, obj, name, value):
        if isinstance(value, basestring):
            if not _find_funct_def(value):
                raise TraitError("couldn't find a function definition in %s" % name)
        elif not (isinstance(value, types.MethodType) or isinstance(value, types.FunctionType)):
            raise TraitError("%s must be a function or a string defining a function" % name)
        return value
    

class ForLoop(Driver):
    """A driver with an initialization workflow and a boolean termination function 
    in addition to the usual iterative workflow.
    """

    init_workflow = Instance(Workflow, allow_none=True,
                             desc="Initialization workflow. This workflow will be executed once"
                                  " before the iterative workflow is executed.")
    
    continuation_funct = _StrOrFunct(iotype='in', 
                                 desc='A function or the string definition of a function that '
                                 ' will be called before each iteration'
                                 ' to determine if iteration should continue. The function should'
                                 ' return True if iteration should continue and False if not.' 
                                 ' A reference to the ForLoop object will be passed to the function.')

    def __init__(self, doc=None):
        super(ForLoop, self).__init__(doc)
        self.init_workflow = Dataflow(self)
        self.current_iteration = 0
        self._funct_container = { 'math': math }
        self._fname = None
    
    def _continuation_funct_changed(self, oldval, newval):
        if isinstance(newval, basestring):
            self._funct_container = { 'math': math }
            fname = _find_funct_def(newval)
            exec newval in self._funct_container  # create the function object
            self._continue_funct = lambda obj: self._funct_container[fname](obj)
        else:
            self._continue_funct = newval
    
    def start_iteration(self):
        if self.init_workflow is not None:
            self.init_workflow.run()
            
    def continue_iteration(self):
        return self._continue_funct(self)
