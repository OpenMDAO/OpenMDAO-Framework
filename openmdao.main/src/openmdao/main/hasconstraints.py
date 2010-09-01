
import operator
import ordereddict

from openmdao.main.expreval import ExprEvaluator

_ops = {
    '>': operator.gt,
    '<': operator.lt,
    '>=': operator.ge,
    '<=': operator.le,
    '=': operator.eq,
    }

def _check_expr(expr):
    try:
        # force checking for existence of vars referenced in expression
        expr.refs_valid()  
    except (AttributeError, RuntimeError), err:
        msg = "Invalid expression '%s': %s" % (str(expr), err)
        raise ValueError( msg )

class Constraint(object):
    def __init__(self, lhs, relation='>', rhs='0', scope=None):
        self.lhs = ExprEvaluator(lhs, scope=scope)
        _check_expr(self.lhs)
        self.relation = relation
        self.rhs = ExprEvaluator(rhs, scope=scope)
        _check_expr(self.rhs)
        
    def evaluate(self):
        """Returns a tuple of the form (lhs, rhs, relation, is_violated)."""
        lhs = self.lhs.evaluate()
        rhs = self.rhs.evaluate()
        return (lhs, rhs, self.relation, _ops[self.relation](lhs,rhs))
        

def _parse_constraint(expr_string):
    for relation in ['>=','<=','>','<','=']:
        parts = expr_string.split(relation)
        if len(parts) > 1:
            return (parts[0].strip(), relation, parts[1].strip())
    else:
        if len(expr_string.split('==')) > 1:
            raise ValueError("'==' is not a valid relation in a constraint.  Use '=' instead.")
        return (expr_string, '>', '0')
    
def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

class _HasConstraintsBase(object):
    def __init__(self, parent):
        self._parent = parent
        self._constraints = ordereddict.OrderedDict()
    
    def remove_constraint(self, expr_string):
        """Removes the constraint with the given string."""
        try:
            del self._constraints[_remove_spaces(expr_string)]
        except KeyError:
            self._parent.raise_exception("Constraint '%s' was not found. Remove failed." % 
                                         expr_string, AttributeError)
    def clear_constraints(self):
        """Removes all constraints."""
        self._constraints = ordereddict.OrderedDict()
        
class HasEqConstraints(_HasConstraintsBase):
    def add_constraint(self, expr_string):
        """Adds a constraint to the driver"""
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self._parent.raise_exception(str(err), type(err))
        if rel=='=':
            self.add_eq_constraint(lhs, rhs)
        else:
            self._parent.raise_exception("add_ineq_constraint", NotImplemented)

    def add_eq_constraint(self, lhs, rhs):
        ident = _remove_spaces('='.join([lhs,rhs]))
        self._constraints[ident] = Constraint(lhs,'=',rhs, scope=self._parent)
        
    def get_eq_constraints(self):
        """Returns an ordered dict of constraint objects."""
        return self._constraints

    def eval_eq_constraints(self): 
        """Returns a list of tuples of the 
        form (lhs, rhs, relation, is_violated)
        """
        return [c.evaluate() for c in self._constraints.values()]

    
class HasIneqConstraints(_HasConstraintsBase):
    def add_constraint(self, expr_string):
        """Adds a constraint to the driver"""
        lhs, rel, rhs = _parse_constraint(expr_string)
        self.add_ineq_constraint(lhs, rel, rhs)

    def add_ineq_constraint(self, lhs, rel, rhs):
        if rel=='==' or rel=='=':
            self._parent.raise_exception("add_eq_constraint", NotImplemented)

        ident = _remove_spaces(rel.join([lhs,rhs]))
        self._constraints[ident] = Constraint(lhs,rel,rhs, scope=self._parent)
        
    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._constraints

    def eval_ineq_constraints(self): 
        """Returns a list of constraint values"""
        return [c.evaluate() for c in self._constraints.values()]
    

class HasConstraints(object):
    """Add this class as a delegate if your Driver supports both equality
    and inequality constraints.
    """
    def __init__(self, parent):
        self._parent = parent
        self._eq = HasEqConstraints(parent)
        self._ineq = HasIneqConstraints(parent)

    def add_constraint(self, expr_string):
        """Adds a constraint to the driver."""
        lhs, rel, rhs = _parse_constraint(expr_string)
        if rel=='==' or rel=='=':
            self._eq.add_eq_constraint(lhs, rhs)
        else:
            self._ineq.add_ineq_constraint(lhs, rel, rhs)

    def remove_constraint(self, expr_string):
        """Removes the constraint with the given string."""
        ident = _remove_spaces(expr_string)
        if ident in self._eq._constraints:
            self._eq.remove_constraint(expr_string)
        else:
            self._ineq.remove_constraint(expr_string)
        
    def clear_constraints(self):
        """Removes all constraints."""
        self._eq.clear_constraints()
        self._ineq.clear_constraints()
        
    def add_ineq_constraint(self, lhs, relation, rhs):
        self._ineq.add_ineq_constraint(lhs, relation, rhs)
    
    def add_eq_constraint(self, lhs, rhs):
        self._eq.add_eq_constraint(lhs, rhs)

    def get_eq_constraints(self):
        """Returns an ordered dict of equality constraint objects."""
        return self._eq.get_constraints()

    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._ineq.get_constraints()

    def eval_eq_constraints(self): 
        """Returns a list of tuples of the form (lhs, rhs, relation,
        is_violated) from evalution of equality constraints.
        """
        return self._eq.eval_eq_constraints()
    
    def eval_ineq_constraints(self): 
        """Returns a list of tuples of the form (lhs, rhs, relation,
        is_violated) from evalution of inequality constraints.
        """
        return self._ineq.eval_ineq_constraints()
