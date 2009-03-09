#public symbols
#__all__ = []

__version__ = "0.1"

from zope.interface import implements

from openmdao.main.interfaces import ICaseIterator
from openmdao.main import ExprEvaluator

class Case(object):
    """Contains all information necessary to specify an input 'case', i.e., a
    list of name,index,value tuples for all inputs to the case, and all outputs
    collected after running the case, an indicator of the exit status of the
    case, and a string containing error messages associated  with the running of
    the case. The value entry of output triples should be set to None prior to
    executing the case.

    """
    def __init__(self, inputs=None, outputs=None):
        """If inputs or outputs are supplied to the constructor, each must be an
        iterator that returns (name,index,value) tuples. 
        
        """
        self.inputs = inputs or [] # a list of name,index,value tuples 
        self.outputs = outputs or [] # a list of name,index,value tuples 
                                     # Values for each output will be filled 
                                     # in after the case completes
        self.status = None
        self.msg = None




class FileCaseIterator(object):
    """An iterator that returns Case objects from a file having the simple
    format below, where a blank line indicates a separation between two cases.
    Whitespace outside of quotes is ignored.  Outputs are indicated
    by the lack of an assignment.
    
    TODO: allow multi-line values (strings, arrays, etc.) on right hand side
    
    TODO: allow array indexing for inputs, outputs, or RHS values
    
    .. parsed-literal::
    
       # Example of an input file
    
       someinput = value1
       blah = value2
       foo = 'abcdef'
       someoutput
       output2

       someinput = value3
       blah = value4
    
    """
    
    implements(ICaseIterator)
    
    def __init__(self, scope, fname):
        self.f = open(fname, 'r')
        self.scope = scope
        self.line_number = 0
    
    def __iter__(self):
        return self._next_case
        
    def _next_case(self):
        inputs = []
        outputs = []
        for line in self.f:
            self.line_number += 1
            line = line.strip()
            if line.startswith('#'):  # comment line
                continue
            if line == '':  # blank line
                if len(inputs) > 0:
                    newcase = Case(inputs, outputs)
                    inputs = []
                    outputs = []
                    yield newcase
                else: # extra blank line. ignore
                    pass
            else:
                parts = line.split('=')
                if len(parts) > 1:        # it's an input assignment
                    inputs.append((parts[0], None, parts[1]))
                else:
                    outputs.append(parts[0], None, None)
                    

                
