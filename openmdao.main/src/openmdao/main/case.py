
from uuid import uuid1
import re

import ordereddict

from openmdao.main.expreval import ExprEvaluator

class _Missing(object):
    pass

# regex to check for simple names
_namecheck_rgx = re.compile(
    '([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')

class Case(object):
    """Contains all information necessary to specify an input *case*, i.e., a
    list of names for all inputs to the case and their values. The case names
    may contain indexing into containers, attribute access, and/or function
    calls, as long as the full expression is valid as the left hand side of an
    assignment. Outputs to be collected may also be added to the Case, and
    they can be more general expressions, i.e., they do not have to refer to a
    single variable. After the Case is executed, it will contain an indicator
    of the exit status of the case, a string containing error messages
    associated with the running of the case (if any), and a unique case
    identifier. 

    """
    def __init__(self, inputs=None, outputs=None, max_retries=None,
                 retries=None, ident=None, msg=''):
        """If inputs are supplied to the constructor, it must be an
        iterator that returns (name,value) tuples, where name is allowed
        to contain array notation and/or function calls. outputs must be
        an interator that returns strings containing names or expressions.
        
        """
        self._exprs = None
        self._outputs = None
        self._inputs = ordereddict.OrderedDict()

        self.max_retries = max_retries  # times to retry after error(s)
        self.retries = retries          # times case was retried
        self.msg = msg                  # If non-null, error message.
                                        # Implies outputs are invalid. 
        self.ident = ident if ident is not None else uuid1()

        if inputs: 
            self.add_inputs(inputs)
        if outputs:
            self.add_outputs(outputs)

    def __str__(self):
        if self._outputs:
            outs = self._outputs.items()
        else:
            outs = []
        return 'Case %s:\n' \
               '    inputs: %s\n' \
               '    outputs: %s\n' \
               '    max_retries: %s, retries: %s\n' \
               '    msg: %s' % \
               (str(self.ident), self._inputs.items(), outs,
                self.max_retries, self.retries, self.msg)
    
    #def __eq__(self,other): 
        #if self._inputs == other._inputs and self._outputs == other._outputs: 
            #return True
        #return False
    
    def __getitem__(self, name):
        val = self._inputs.get(name, _Missing)
        if val is not _Missing:
            return val
        if self._outputs:
            return self._outputs[name]
        raise KeyError("'%s' not found" % name)
    
    def __setitem__(self, name, value):
        if self._outputs and name in self._outputs:
            self._outputs[name] = value
        elif name in self._inputs:
            self._inputs[name] = value
        else:
            raise KeyError("'%s' not found" % name)
    
    def __contains__(self, name):
        return name in self._inputs or (self._outputs and name in self._outputs)
    
    def items(self, iotype=None):
        if iotype == 'in':
            return self._inputs.items()
        elif iotype == 'out':
            if self._outputs:
                return self._outputs.items()
            else:
                return []
        else:
            lst = self._inputs.items()
            if self._outputs:
                lst.extend(self._outputs.items())
            return lst

    def apply_inputs(self, scope):
        """Set all of the inputs in this case to their specified values in
        the given scope.
        """
        if self.retries is None:
            self.retries = 0
        else:
            self.retries += 1
        for name,value in self._inputs.items():
            if self._exprs:
                expr = self._exprs.get(name)
                if expr:
                    expr.set(value, scope)
            else:
                expr = None
            if expr is None:
                scope.set(name, value)

    def update_outputs(self, scope, msg=None):
        """Update the value of all outputs of interest, using the given scope.
        If msg is not None, save it as part of the case.
        """
        if msg:
            self.msg = msg
        if self._outputs is not None:
            # TODO: make this smart enough to do a multiget on a component
            #       instead of multiple individual gets
            for name in self._outputs.keys():
                if self._exprs:
                    expr = self._exprs.get(name)
                else:
                    expr = None
                if expr:
                    self._outputs[name] = expr.evaluate(scope)
                else:
                    self._outputs[name] = scope.get(name)

    def add_input(self, name, value):
        """Adds an input and its value to this case.
        
        name: str
            name of the input to be added
            
        value: 
            value that the input will be assigned to
        """
        self._register_expr(name)
        self._inputs[name] = value
        
    def add_inputs(self, inp_iter):
        """Adds multiple inputs to this case.
        
        inp_iter: iterator returning (name,value)
            iterator of input names and values
        """
        for name, value in inp_iter:
            self.add_input(name, value)

    def add_output(self, name, value=_Missing):
        """Adds an output to this case.
        
        name: str
            name of output to be added
        """
        self._register_expr(name)
        if self._outputs is None:
            self._outputs = { name: value }
        else:
            self._outputs[name] = value
        
    def add_outputs(self, outputs):
        """Adds outputs to this case.
        
        outputs: iterator returning names or tuples of the form (name,value)
            outputs to be added
        """
        for entry in outputs:
            if isinstance(entry, basestring):
                self.add_output(entry)
            else: # assume it's a tuple of the form (name, value)
                self.add_output(entry[0], entry[1])
        
    def _register_expr(self, s):
        """If the given string contains an expression, create an ExprEvaluator and
        store it in self._exprs
        """
        if _namecheck_rgx.match(s) is None:
            expr =  ExprEvaluator(s)
            if self._exprs is None:
                self._exprs = { s: expr }
            else:
                self._exprs[s] = expr

                