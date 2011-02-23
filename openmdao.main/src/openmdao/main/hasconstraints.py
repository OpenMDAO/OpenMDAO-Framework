"""
  Functions in the HasConstraints, HasEqConstraints, and HasIneqConstraints
  interfaces.
"""

# pylint: disable-msg=E0611,F0401
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
    """ force checking for existence of vars referenced in expression """
    if not expr.check_resolve():
        msg = "Invalid expression '%s'" % str(expr)
        raise ValueError( msg )

class Constraint(object):
    """ Object that stores info for a single constraint. """
    
    def __init__(self, lhs, comparator, rhs, scaler, adder, scope=None):
        self.lhs = ExprEvaluator(lhs, scope=scope)
        if not self.lhs.check_resolve():
            raise ValueError("Constraint '%s' has an invalid left-hand-side." \
                              % ' '.join([lhs, comparator, rhs]))
        self.comparator = comparator
        self.rhs = ExprEvaluator(rhs, scope=scope)
        if not self.rhs.check_resolve():
            raise ValueError("Constraint '%s' has an invalid right-hand-side." \
                              % ' '.join([lhs, comparator, rhs]))
        
        if not isinstance(scaler, float):
            raise ValueError("Scaler parameter should be a float")
        self.scaler = scaler
        
        if scaler <= 0.0:
            raise ValueError("Scaler parameter should be a float > 0")
        
        if not isinstance(adder, float):
            raise ValueError("Adder parameter should be a float")
        self.adder = adder
        
    def evaluate(self):
        """Returns a tuple of the form (lhs, rhs, comparator, is_violated)."""
        
        lhs = (self.lhs.evaluate() + self.adder)*self.scaler
        rhs = (self.rhs.evaluate() + self.adder)*self.scaler
        return (lhs, rhs, self.comparator, not _ops[self.comparator](lhs, rhs))
        

def _parse_constraint(expr_string):
    """ Parses the constraint expression string and returns the lhs string, 
    the rhs string, and comparator"""
    for comparator in ['>=', '<=', '>', '<', '=']:
        parts = expr_string.split(comparator)
        if len(parts) > 1:
            return (parts[0].strip(), comparator, parts[1].strip())
    else:
        msg = "Constraints require an explicit comparator (=, <, >, <=, or >=)"
        raise ValueError( msg )
    
def _remove_spaces(s):
    """ whitespace removal """
    return s.translate(None, ' \n\t\r')

class _HasConstraintsBase(object):
    _do_not_promote = ['get_expr_depends']
    
    def __init__(self, parent):
        self._parent = parent
        self._constraints = ordereddict.OrderedDict()
    
    def remove_constraint(self, expr_string):
        """Removes the constraint with the given string."""
        try:
            del self._constraints[_remove_spaces(expr_string)]
        except KeyError:
            msg = "Constraint '%s' was not found. Remove failed." % expr_string
            self._parent.raise_exception(msg, AttributeError)
            
    def clear_constraints(self):
        """Removes all constraints."""
        self._constraints = ordereddict.OrderedDict()
        
    def list_constraints(self):
        """Return a list of strings containing constraint expressions."""
        return self._constraints.keys()
    
    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a constraint.
        """
        conn_list = []
        pname = self._parent.name
        for name, constraint in self._constraints.items():
            for cname in constraint.lhs.get_referenced_compnames():
                conn_list.append((cname, pname))
            for cname in constraint.rhs.get_referenced_compnames():
                conn_list.append((cname, pname))
        return conn_list
    
        
class HasEqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports equality
    constraints but does not support inequality constraints.
    """
    
    def add_constraint(self, expr_string, scaler=1.0, adder=0.0):
        """Adds a constraint in the form of a boolean expression string
        to the driver.
        
        
        *Parameters:*
        
        expr_string: str
            Expression string containing the constraint.
        
        scaler: float (optional)
            Multiplicative scale factor applied to both sides of the
            constraint's boolean expression. It should be a positive nonzero
            value. Default is unity (1.0).
            
        adder: float (optional)
            Additive scale factor applied to both sides of the constraint's
            boolean expression. Default is no additive shift (0.0).
        """
        
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self._parent.raise_exception(str(err), type(err))
        if rel == '=':
            self.add_eq_constraint(lhs, rhs, scaler, adder)
        else:
            msg = "Inequality constraints are not supported on this driver"
            self._parent.raise_exception(msg, ValueError)

    def add_eq_constraint(self, lhs, rhs, scaler, adder):
        """Adds an equality constraint as two strings, a left-hand side and
        a right-hand side.
        """
        if not isinstance(lhs, basestring):
            msg = "Constraint left-hand side (%s) is not a string" % lhs
            raise ValueError(msg)
        if not isinstance(rhs, basestring):
            msg = "Constraint right-hand-side (%s) is not a string" % rhs
            raise ValueError(msg)
        ident = _remove_spaces('='.join([lhs, rhs]))
        self._constraints[ident] = Constraint(lhs, '=', rhs, scaler, adder, \
                                              scope=self._parent)
        
    def get_eq_constraints(self):
        """Returns an ordered dict of constraint objects."""
        return self._constraints

    def eval_eq_constraints(self): 
        """Returns a list of tuples of the 
        form (lhs, rhs, comparator, is_violated).
        """
        return [c.evaluate() for c in self._constraints.values()]

    
class HasIneqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports inequality
    constraints but does not support equality constraints.
    """
    
    def add_constraint(self, expr_string, scaler=1.0, adder=0.0):
        """Adds a constraint in the form of a boolean expression string
        to the driver.
        
        expr_string: str
            Expression string containing the constraint.
        
        scaler: float (optional)
            Multiplicative scale factor applied to both sides of the
            constraint's boolean expression. It should be a positive nonzero
            value. Default is unity (1.0).
            
        adder: float (optional)
            Additive scale factor applied to both sides of the constraint's
            boolean expression. Default is no additive shift (0.0).
        """
        
        lhs, rel, rhs = _parse_constraint(expr_string)
        self.add_ineq_constraint(lhs, rel, rhs, scaler, adder)

    def add_ineq_constraint(self, lhs, rel, rhs, scaler, adder):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right-hand side.
        """
        if rel == '==' or rel == '=':
            msg = "Equality constraints are not supported on this driver"
            self._parent.raise_exception(msg, ValueError)

        if not isinstance(lhs, basestring):
            msg = "Constraint left-hand-side (%s) is not a string" % lhs
            raise ValueError(msg)
        if not isinstance(rhs, basestring):
            msg = "Constraint right-hand-side (%s) is not a string" % rhs
            raise ValueError(msg)
        ident = _remove_spaces(rel.join([lhs, rhs]))
        self._constraints[ident] = Constraint(lhs, rel, rhs, scaler, adder, \
                                            scope=self._parent)
        
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
    
    _do_not_promote = ['get_expr_depends']
    
    def __init__(self, parent):
        self._parent = parent
        self._eq = HasEqConstraints(parent)
        self._ineq = HasIneqConstraints(parent)

    def add_constraint(self, expr_string, scaler=1.0, adder=0.0):
        """Adds a constraint in the form of a boolean expression string
        to the driver.
        
        expr_string: str
            Expression string containing the constraint.
        
        scaler: float (optional)
            Multiplicative scale factor applied to both sides of the
            constraint's boolean expression. It should be a positive nonzero
            value. Default is unity (1.0).
            
        adder: float (optional)
            Additive scale factor applied to both sides of the constraint's
            boolean expression. Default is no additive shift (0.0).
        """
        
        lhs, rel, rhs = _parse_constraint(expr_string)
        if rel == '==' or rel == '=':
            self._eq.add_eq_constraint(lhs, rhs, scaler, adder)
        else:
            self._ineq.add_ineq_constraint(lhs, rel, rhs, scaler, adder)

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
        
    def add_ineq_constraint(self, lhs, comparator, rhs, scaler, adder):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right hand side.
        """
        self._ineq.add_ineq_constraint(lhs, comparator, rhs, scaler, adder)
    
    def add_eq_constraint(self, lhs, rhs, scaler, adder):
        """Adds an equality constraint as two strings, a left-hand side and
        a right-hand side.
        """
        self._eq.add_eq_constraint(lhs, rhs, scaler, adder)

    def get_eq_constraints(self):
        """Returns an ordered dict of equality constraint objects."""
        return self._eq.get_eq_constraints()

    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._ineq.get_ineq_constraints()

    def eval_eq_constraints(self): 
        """Returns a list of tuples of the form (lhs, rhs, comparator,
        is_violated) from evalution of equality constraints.
        """
        return self._eq.eval_eq_constraints()
    
    def eval_ineq_constraints(self): 
        """Returns a list of tuples of the form (lhs, rhs, comparator,
        is_violated) from evalution of inequality constraints.
        """
        return self._ineq.eval_ineq_constraints()
    
    def list_constraints(self):
        """Return a list of strings containing constraint expressions."""
        lst = self._ineq.list_constraints()
        lst.extend(self._eq.list_constraints())
        return lst

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a constraint.
        """
        conn_list = self._eq.get_expr_depends()
        conn_list.extend(self._ineq.get_expr_depends())
        return conn_list
    
