
from uuid import uuid1
import re

#import ordereddict

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
                 retries=None, desc=None, parent_id=None, msg=None):
        """If inputs are supplied to the constructor, it must be an
        iterator that returns (name,value) tuples, where name is allowed
        to contain array notation and/or function calls. outputs must be
        an interator that returns strings containing names or expressions.
        
        """
        self._exprs = None
        self._outputs = None
        self._inputs = {}

        self.max_retries = max_retries  # times to retry after error(s)
        self.retries = retries          # times case was retried
        self.msg = msg                  # If non-null, error message.
                                        # Implies outputs are invalid. 
        self.desc = desc   # optional description
        self.ident = str(uuid1())  # unique identifier
        self.parent_id = parent_id  # identifier of parent case, if any

        if inputs: 
            self.add_inputs(inputs)
        if outputs:
            self.add_outputs(outputs)

    def __str__(self):
        if self._outputs:
            outs = self._outputs.items()
        else:
            outs = []
        return 'Case %s: (parent_id %s)\n' \
               '    description: %s\n' \
               '    inputs: %s\n' \
               '    outputs: %s\n' \
               '    max_retries: %s, retries: %s\n' \
               '    msg: %s' % \
               (self.ident, self.parent_id, 
                self.desc,
                self._inputs.items(), 
                outs,
                self.max_retries, self.retries, 
                self.msg)
    
    def __eq__(self,other): 
        if self is other:
            return True
        try:
            if len(self) != len(other):
                return False
            for selftup, othertup in zip(self.items(), other.items()):
                if selftup[0] != othertup[0] or selftup[1] != othertup[1]:
                    return False
        except:
            return False
        return True
    
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
    
    def __len__(self):
        if self._outputs is None:
            return len(self._inputs)
        else:
            return len(self._inputs) + len(self._outputs)
    
    def items(self, iotype=None):
        """Return a list of (name,value) tuples for variables/expressions in this Case.
        
        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned
            If None (the default), inputs and outputs are returned
        """
        if iotype is None:
            lst = self._inputs.items()
            if self._outputs:
                lst.extend(self._outputs.items())
            return lst
        elif iotype == 'in':
            return self._inputs.items()
        elif iotype == 'out':
            if self._outputs:
                return self._outputs.items()
            else:
                return []
        else:
            raise NameError("invalid iotype arg (%s). Must be 'in','out',or None" % str(iotype))
        
    def keys(self, iotype=None):
        """Return a list of name/expression strings for this Case.
        
        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned
            If None (the default), inputs and outputs are returned
        """
        return [k for k,v in self.items(iotype)]
        
    def values(self, iotype=None):
        """Return a list of values for this Case.
        
        iotype: str or None
            If 'in', only inputs are returned.
            If 'out', only outputs are returned
            If None (the default), inputs and outputs are returned
        """
        return [v for k,v in self.items(iotype)]

    def apply_inputs(self, scope):
        """Take the values of all of the inputs in this case and apply them
        to the specified scope.
        """
        if self._exprs:
            for name,value in self._inputs.items():
                expr = self._exprs.get(name)
                if expr:
                    expr.set(value, scope)
                else:
                    scope.set(name, value)
        else:
            for name,value in self._inputs.items():
                scope.set(name, value)

    def update_outputs(self, scope, msg=None):
        """Update the value of all outputs in this Case, using the given scope.
        """
        self.msg = msg
        if self._outputs is not None:
            if self._exprs:
                for name in self._outputs.keys():
                    expr = self._exprs.get(name)
                    self._outputs[name] = expr.evaluate(scope) if expr else scope.get(name)
            else:
                for name in self._outputs.keys():
                    self._outputs[name] = scope.get(name)

    def add_input(self, name, value):
        """Adds an input and its value to this case.
        
        name: str
            Name of the input to be added. May contain an expression as long
            as it is valid when placed on the left hand side of an assignment.
            
        value: 
            Value that the input will be assigned to.
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
        match = _namecheck_rgx.match(s)
        if match is None or match.group() != s:
            expr =  ExprEvaluator(s)
            if self._exprs is None:
                self._exprs = { s: expr }
            else:
                self._exprs[s] = expr

