
import ordereddict
from numpy import float32, float64, int32, int64

from openmdao.main.expreval import ExprEvaluator

import operator

class Constraint(object):
    def __init__(self, lhs, rhs, relation=operator.gt):
        self.lhs = None
        self.rhs = None
        self.relation = relation

class HasConstraints(object): 
    """This class provides an implementation of the IHasConstraints interface"""

    def __init__(self, parent):
        self._constraints = ordereddict.OrderedDict()
        self._parent = parent

    def add_constraint(self, expr_str, equality=False):
        """Adds a constraint to the driver. 
        
        expr_str : string
            string expression that is evaluated to calculate the constraint value
            
        equality : bool, optional
            if True, this is an equality constraint
        """
        parameter = _Parameter()
        parameter.expreval = ExprEvaluator(name, self._parent.parent)
        
        try:
            metadata = self._parent.parent.get_metadata(name.split('[')[0])
        except AttributeError:
            self._parent.raise_exception("Can't add parameter '%s' because it doesn't exist." % name,
                                         AttributeError)
        try:
            val = parameter.expreval.evaluate()
        except:
            self._parent.raise_exception("Can't add parameter because I can't evaluate '%s'" % name,
                                         AttributeError)
        if not isinstance(val,(float,float32,float64,int,int32,int64)):
            self._parent.raise_exception("The value of parameter '%s' must be of type float or int, but its type is '%s'." %
                                         (name,type(val).__name__), ValueError)
        
        meta_low = metadata.get('low') # this will be None if 'low' isn't there
        if low is None:
            parameter.low = meta_low
        else:  # low is not None
            if meta_low is not None and low < meta_low:
                self._parent.raise_exception("Trying to add parameter '%s', " 
                                             "but the lower limit supplied (%s) exceeds the " 
                                             "built-in lower limit (%s)." % 
                                             (name, low, meta_low), ValueError)
            parameter.low = low

        meta_high = metadata.get('high') # this will be None if 'high' isn't there
        if high is None:
            parameter.high = meta_high
        else:  # high is not None
            if meta_high is not None and high > meta_high:
                self._parent.raise_exception("Trying to add parameter '%s', " 
                                             "but the upper limit supplied (%s) exceeds the " 
                                             "built-in upper limit (%s)." % 
                                             (name, high, meta_high), ValueError)
            parameter.high = high
            
        values = metadata.get('values')
        if values is not None and len(values)>0:
            pass    # assume it's an Enum, so no need to set high or low
        else:
            if parameter.low is None:
                self._parent.raise_exception("Trying to add parameter '%s', "
                                             "but no lower limit was found and no " 
                                             "'low' argument was given. One or the "
                                             "other must be specified." % name,ValueError)
            if parameter.high is None: 
                self._parent.raise_exception("Trying to add parameter '%s', "
                                             "but no upper limit was found and no " 
                                             "'high' argument was given. One or the "
                                             "other must be specified." % name,ValueError)
        self._constraints[name] = parameter
            
    def remove_parameter(self, name):
        """Removes the parameter with the given name."""
        try:
            del self._constraints[name]
        except KeyError:
            self._parent.raise_exception("Trying to remove parameter '%s' "
                                         "that is not in this driver." % name,
                                         AttributeError)
    def list_constraints(self):
        """Returns an alphabetized list of parameter names."""
        return sorted(self._constraints.keys())
    
    def clear_constraints(self):
        """Removes all parameters."""
        self._constraints = ordereddict.OrderedDict()
        
    def get_constraints(self):
        """Returns an ordered dict of parameter objects."""
        return self._constraints

    def eval_constraints(self): 
        """Pushes the values in the X input array into the corresponding public 
        variables in the model.
        
        X: iterator
            iterator of input values with an order defined to match the order of parameters returned 
            by the list_parameter method. X must support the len() function.
        """
        if len(X) != len(self._constraints):
            raise ValueError("number of input values (%s) != number of parameters (%s)" % 
                             (len(X),len(self._constraints)))

        for x, param in zip(X, self._constraints.values()): 
            param.expreval.set(x)
