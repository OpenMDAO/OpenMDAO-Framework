
import operator
import ordereddict

from openmdao.main.expreval import ExprEvaluator

_ops = {
    '>': operator.gt,
    '<': operator.lt,
    '>=': operator.ge,
    '<=': operator.le,
    '==': operator.eq,
    '=': operator.eq,
    }

class _Constraint(object):
    def __init__(self, lhs, relation='>', rhs='0'):
        self.lhs = lhs
        self.relation = relation
        self.rhs = rhs
        #self.parent = None
        #self.children = []
        
    def evaluate(self):
        """Returns a tuple of the form (lhs, rhs, relation, is_violated)"""
        lhs = self.lhs.evaluate()
        rhs = self.rhs.evaluate()
        return (lhs, rhs, self.relation, _ops[self.relation](lhs,rhs))
        
    def violated(self):
        """Returns True if the constraint is voilated, False otherwise."""
        return self.evaluate()[3]


class HasConstraints(object): 
    """This class provides an implementation of the IHasConstraints interface"""

    def __init__(self, parent):
        self._constraints = ordereddict.OrderedDict()
        self._parent = parent

    def add_constraint(self, expr_string):
        """Adds a constraint to the driver"""
        for relation in ['>=','<=','==','>','<','=']:
            parts = expr_string.split(relation)
            if len(parts) > 1:
                lhs = parts[0]
                rel = relation
                rhs = parts[1]
                constraint = _Constraint(lhs, relation=rel, rhs=rhs)
                break
        else:
            constraint = _Constraint(expr_string)
        self._constraints[expr_string] = constraint

    def remove_constraint(self, expr_string):
        """Removes the constraint with the given name."""
        try:
            del self._constraints[expr_string]
        except KeyError:
            self._parent.raise_exception("Trying to remove constraint '%s' "
                                         "that is not in this driver." % expr_string,
                                         AttributeError)
    def clear_constraints(self):
        """Removes all constraints."""
        self._constraints = ordereddict.OrderedDict()
        
    def get_constraints(self):
        """Returns an ordered dict of constraint objects."""
        return self._constraints

    def eval_constraints(self): 
        """Returns a list of constraint values"""
        return [c.evaluate() for c in self._constraints.values()]
