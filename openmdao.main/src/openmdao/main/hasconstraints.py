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
        raise ValueError( "Invalid expression '%s'" % str(expr) )

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
        
    def evaluate(self, scope):
        """Returns a tuple of the form (lhs, rhs, comparator, is_violated)."""
        
        lhs = (self.lhs.evaluate(scope) + self.adder)*self.scaler
        rhs = (self.rhs.evaluate(scope) + self.adder)*self.scaler
        return (lhs, rhs, self.comparator, not _ops[self.comparator](lhs, rhs))
        
    def get_referenced_compnames(self):
        return self.lhs.get_referenced_compnames() + self.rhs.get_referenced_compnames()

    def __str__(self):
        return ' '.join([self.lhs, self.comparator, self.rhs])

def _parse_constraint(expr_string):
    """ Parses the constraint expression string and returns the lhs string, 
    the rhs string, and comparator
    """
    for comparator in ['==', '>=', '<=', '>', '<', '=']:
        parts = expr_string.split(comparator)
        if len(parts) > 1:
            if comparator == '==':  # check for == because otherwise they get a cryptic error msg
                break
            return (parts[0].strip(), comparator, parts[1].strip())

    msg = "Constraints require an explicit comparator (=, <, >, <=, or >=)"
    raise ValueError( msg )
    
def _remove_spaces(s):
    """ whitespace removal """
    return s.translate(None, ' \n\t\r')


def _get_scope(cnst, scope=None):
    if scope is None:
        try:
            return cnst._parent.get_expr_scope()
        except AttributeError:
            pass
    return scope

class _HasConstraintsBase(object):
    _do_not_promote = ['get_expr_depends','get_referenced_compnames',
                       'get_referenced_varpaths']
    
    def __init__(self, parent, allowed_types=None):
        self._parent = parent
        self._constraints = ordereddict.OrderedDict()
    
    def remove_constraint(self, key):
        """Removes the constraint with the given string."""
        try:
            del self._constraints[_remove_spaces(key)]
        except KeyError:
            msg = "Constraint '%s' was not found. Remove failed." % key
            self._parent.raise_exception(msg, AttributeError)
            
    def clear_constraints(self):
        """Removes all constraints."""
        self._constraints = ordereddict.OrderedDict()
        
    def list_constraints(self):
        """Return a list of strings containing constraint expressions."""
        return self._constraints.keys()
    
    def _check_add(self, cnststr):
        if cnststr in self._constraints:
            self._parent.raise_exception("'%s' is already a constraint" % cnststr)
    
    def get_expr_depends(self):
        """Returns a list of tuples of the form (comp_name, self_name)
        for each component name referenced by a constraint.
        """
        conn_list = []
        pname = self._parent.name
        for name, constraint in self._constraints.items():
            for cname in constraint.lhs.get_referenced_compnames():
                conn_list.append((cname, pname))
            for cname in constraint.rhs.get_referenced_compnames():
                conn_list.append((cname, pname))
        return conn_list
    
    def get_referenced_compnames(self):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set()
        for constraint in self._constraints.values():
            names.update(constraint.lhs.get_referenced_compnames())
            names.update(constraint.rhs.get_referenced_compnames())
        return names
    
    def get_referenced_varpaths(self):
        """Returns a set of variable names referenced by a
        constraint.
        """
        names = set()
        for constraint in self._constraints.values():
            names.update(constraint.lhs.get_referenced_varpaths())
            names.update(constraint.rhs.get_referenced_varpaths())
        return names
    
    
class HasEqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports equality
    constraints but does not support inequality constraints.
    """
    
    def add_constraint(self, expr_string, scaler=1.0, adder=0.0, name=None,
                       scope=None):
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
            
        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
            
        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        
        """
        
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self._parent.raise_exception(str(err), type(err))
        if rel == '=':
            self._add_eq_constraint(lhs, rhs, scaler, adder, name, scope)
        else:
            msg = "Inequality constraints are not supported on this driver"
            self._parent.raise_exception(msg, ValueError)

    def _add_eq_constraint(self, lhs, rhs, scaler, adder, name=None, scope=None):
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
        if ident in self._constraints: 
            self._parent.raise_exception('A constraint of the form "%s" already exists '
                                         'in the driver. Add failed.' % ident, ValueError)
        elif name is not None and name in self._constraints: 
            self._parent.raise_exception('A constraint named "%s" already exists '
                                         'in the driver. Add failed.' % name, ValueError)
            
        constraint = Constraint(lhs, '=', rhs, scaler, adder, scope=_get_scope(self,scope))
        if name is None:
            self._constraints[ident] = constraint
        else:
            self._constraints[name] = constraint

    def get_eq_constraints(self):
        """Returns an ordered dict of constraint objects."""
        return self._constraints

    def eval_eq_constraints(self, scope=None): 
        """Returns a list of tuples of the 
        form (lhs, rhs, comparator, is_violated).
        """
        return [c.evaluate(_get_scope(self,scope)) for c in self._constraints.values()]
    
    def allows_constraint_types(self, types):
        """Returns True if types is ['eq']."""
        return types == ['eq']

    
class HasIneqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports inequality
    constraints but does not support equality constraints.
    """
    
    def add_constraint(self, expr_string, scaler=1.0, adder=0.0, name=None,
                       scope=None):
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
        
        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
            
        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        
        """
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self._parent.raise_exception(str(err), type(err))
        self._add_ineq_constraint(lhs, rel, rhs, scaler, adder, name, scope)

    def _add_ineq_constraint(self, lhs, rel, rhs, scaler, adder, name=None,
                             scope=None):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right-hand side.
        """
        if rel == '=':
            msg = "Equality constraints are not supported on this driver"
            self._parent.raise_exception(msg, ValueError)

        if not isinstance(lhs, basestring):
            msg = "Constraint left-hand-side (%s) is not a string" % lhs
            raise ValueError(msg)
        if not isinstance(rhs, basestring):
            msg = "Constraint right-hand-side (%s) is not a string" % rhs
            raise ValueError(msg)
        ident = _remove_spaces(rel.join([lhs, rhs]))
        if ident in self._constraints: 
            self._parent.raise_exception('A constraint of the form "%s" already exists in '
                                         'the driver. Add failed.'%ident,ValueError)
        elif name is not None and name in self._constraints: 
            self._parent.raise_exception('A constraint named "%s" already exists '
                                         'in the driver. Add failed.' % name, ValueError)
            
        constraint = Constraint(lhs, rel, rhs, scaler, adder, scope=_get_scope(self,scope))
        if name is None:
            self._constraints[ident] = constraint
        else:
            self._constraints[name] = constraint
        
    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._constraints

    def eval_ineq_constraints(self, scope=None): 
        """Returns a list of constraint values"""
        return [c.evaluate(_get_scope(self,scope)) for c in self._constraints.values()]
    
    def allows_constraint_types(self, typ):
        """Returns True if types is ['ineq']."""
        return types == ['eq']


class HasConstraints(object):
    """Add this class as a delegate if your Driver supports both equality
    and inequality constraints.
    """
    
    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']
    
    def __init__(self, parent):
        self._parent = parent
        self._eq = HasEqConstraints(parent)
        self._ineq = HasIneqConstraints(parent)

    def add_constraint(self, expr_string, scaler=1.0, adder=0.0, name=None,
                       scope=None):
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
        
        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
            
        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        
        """
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self._parent.raise_exception(str(err), type(err))
        if rel == '=':
            self._eq._add_eq_constraint(lhs, rhs, scaler, adder, name, scope)
        else:
            self._ineq._add_ineq_constraint(lhs, rel, rhs, scaler, adder, name, scope)

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
        
    def _add_ineq_constraint(self, lhs, comparator, rhs, scaler, adder, name=None,
                             scope=None):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right hand side.
        """
        self._ineq._add_ineq_constraint(lhs, comparator, rhs, scaler, adder, name,
                                        scope)
    
    def _add_eq_constraint(self, lhs, rhs, scaler, adder, name=None, scope=None):
        """Adds an equality constraint as two strings, a left-hand side and
        a right-hand side.
        """
        self._eq._add_eq_constraint(lhs, rhs, scaler, adder, name, scope)

    def get_eq_constraints(self):
        """Returns an ordered dict of equality constraint objects."""
        return self._eq.get_eq_constraints()

    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._ineq.get_ineq_constraints()

    def eval_eq_constraints(self, scope=None): 
        """Returns a list of tuples of the form (lhs, rhs, comparator,
        is_violated) from evalution of equality constraints.
        """
        return self._eq.eval_eq_constraints(scope)
    
    def eval_ineq_constraints(self, scope=None): 
        """Returns a list of tuples of the form (lhs, rhs, comparator,
        is_violated) from evalution of inequality constraints.
        """
        return self._ineq.eval_ineq_constraints(scope)
    
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
    
    def get_referenced_compnames(self):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set(self._eq.get_referenced_compnames())
        names.update(self._ineq.get_referenced_compnames())
        return names
    
    def get_referenced_varpaths(self):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set(self._eq.get_referenced_varpaths())
        names.update(self._ineq.get_referenced_varpaths())
        return names
    
    def allows_constraint_types(self, types):
        """Returns True if this Driver supports constraints of the given types.
        
        types: list of str
            Types of constraints supported. Valid values are: ['eq', 'ineq']
        """
        for kind in types:
            if kind not in ['eq','ineq']:
                return False
        return True

