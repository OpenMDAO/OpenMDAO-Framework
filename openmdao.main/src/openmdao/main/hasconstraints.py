"""
  Functions in the HasConstraints, HasEqConstraints, and HasIneqConstraints
  interfaces.
"""

# pylint: disable=E0611,F0401
import operator
import ordereddict
import weakref

from numpy import ndarray

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent, \
                                     SimpleEQConPComp, \
                                     SimpleEQ0PComp, \
                                     _remove_spaces

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
        raise ValueError("Invalid expression '%s'" % str(expr))


def _parse_constraint(expr_string):
    """ Parses the constraint expression string and returns the lhs string,
    the rhs string, and comparator
    """
    for comparator in ['==', '>=', '<=', '>', '<', '=']:
        parts = expr_string.split(comparator)
        if len(parts) > 1:
            # check for == because otherwise they get a cryptic error msg
            if comparator == '==':
                break
            return (parts[0].strip(), comparator, parts[1].strip())

    msg = "Constraints require an explicit comparator (=, <, >, <=, or >=)"
    raise ValueError(msg)


def _get_scope(obj, scope=None):
    if scope is None:
        try:
            return obj.parent.get_expr_scope()
        except AttributeError:
            pass
    return scope


class Constraint(object):
    """ Object that stores info for a single constraint. """

    def __init__(self, lhs, comparator, rhs, scope):
        self.lhs = ExprEvaluator(lhs, scope=scope)
        unresolved_vars = self.lhs.get_unresolved()

        if unresolved_vars:
            msg = "Left hand side of constraint '{0}' has invalid variables {1}"
            expression = ' '.join((lhs, comparator, rhs))

            raise ExprEvaluator._invalid_expression_error(unresolved_vars,
                                                          expr=expression,
                                                          msg=msg)
        self.rhs = ExprEvaluator(rhs, scope=scope)
        unresolved_vars = self.rhs.get_unresolved()

        if unresolved_vars:
            msg = "Right hand side of constraint '{0}' has invalid variables {1}"
            expression = ' '.join((lhs, comparator, rhs))

            raise ExprEvaluator._invalid_expression_error(unresolved_vars,
                                                          expr=expression,
                                                          msg=msg)
        self.comparator = comparator
        self.pcomp_name = None
        self._size = None

    @property
    def size(self):
        """Total scalar items in this constraint."""
        if self._size is None:
            self._size = len(self.evaluate(self.lhs.scope))
        return self._size

    def activate(self, driver):
        """Make this constraint active by creating the appropriate
        connections in the dependency graph.
        """
        if self.pcomp_name is None:
            if self.comparator == '=':
                subtype = 'equality'
            else:
                subtype = 'inequality'
            
            # check for simple structure of equality constraint, 
            # either
            #     var1 = var2
            #  OR
            #     var1 - var2 = 0
            #  OR
            #     var1 = 0
            lrefs = list(self.lhs.refs())
            rrefs = list(self.rhs.refs())
        
            try:
                leftval = float(self.lhs.text)
            except ValueError:
                leftval = None

            try:
                rightval = float(self.rhs.text)
            except ValueError:
                rightval = None    

            pseudo_class = PseudoComponent

            if self.comparator == '=':
                if len(lrefs) == 2 and len(rrefs) == 0:
                    if rightval == 0. and \
                            _remove_spaces(self.lhs.text) == \
                                lrefs[0]+'-'+lrefs[1]:
                        pseudo_class = SimpleEQConPComp
                elif len(lrefs) == 0 and len(rrefs) == 2:
                    pass
                elif len(lrefs) == 1 and len(rrefs) == 1:
                    if lrefs[0] == self.lhs.text and \
                               rrefs[0] == self.rhs.text:
                        pseudo_class = SimpleEQConPComp
                elif len(lrefs) == 1 and len(rrefs) == 0 and rightval is not None:
                    pseudo_class = SimpleEQ0PComp

            pseudo = pseudo_class(self.lhs.scope, 
                                  self._combined_expr(),
                                  pseudo_type='constraint',
                                  subtype=subtype, 
                                  exprobject=self)

            self.pcomp_name = pseudo.name
            self.lhs.scope.add(pseudo.name, pseudo)
        getattr(self.lhs.scope, pseudo.name).make_connections(self.lhs.scope, driver)


    def deactivate(self):
        """Remove this constraint from the dependency graph and remove
        its pseudocomp from the scoping object.
        """
        if self.pcomp_name:
            scope = self.lhs.scope
            try:
                pcomp = getattr(scope, self.pcomp_name)
            except AttributeError:
                pass
            else:
                scope.remove(pcomp.name)
            finally:
                self.pcomp_name = None

    def _combined_expr(self):
        """Given a constraint object, take the lhs, operator, and
        rhs and combine them into a single expression by moving rhs
        terms over to the lhs.  For example,
        for the constraint 'C1.x < C2.y + 7', return the expression
        'C1.x - C2.y - 7'.  Depending on the direction of the operator,
        the sign of the expression may be flipped.  The final form of
        the constraint, when evaluated, will be considered to be satisfied
        if it evaluates to a value <= 0.
        """
        scope = self.lhs.scope

        if self.comparator.startswith('>'):
            first = self.rhs.text
            second = self.lhs.text
        else:
            first = self.lhs.text
            second = self.rhs.text

        first_zero = False
        try:
            f = float(first)
        except Exception:
            pass
        else:
            if f == 0:
                first_zero = True

        second_zero = False
        try:
            f = float(second)
        except Exception:
            pass
        else:
            if f == 0:
                second_zero = True

        if first_zero:
            newexpr = "-(%s)" % second
        elif second_zero:
            newexpr = first
        else:
            newexpr = '%s-(%s)' % (first, second)

        return ExprEvaluator(newexpr, scope)

    def copy(self):
        return Constraint(str(self.lhs), self.comparator, str(self.rhs),
                          scope=self.lhs.scope)

    def evaluate(self, scope):
        """Returns the value of the constraint as a sequence."""

        pcomp = getattr(scope, self.pcomp_name)
        val = pcomp.out0

        if isinstance(val, ndarray):
            return val.flatten()
        else:
            return [val]

    def evaluate_gradient(self, scope, stepsize=1.0e-6, wrt=None):
        """Returns the gradient of the constraint eq/ineq as a tuple of the
        form (lhs, rhs, comparator, is_violated)."""

        lhs = self.lhs.evaluate_gradient(scope=scope, stepsize=stepsize, wrt=wrt)
        if isinstance(self.rhs, float):
            rhs = 0.
        else:
            rhs = self.rhs.evaluate_gradient(scope=scope, stepsize=stepsize, wrt=wrt)

        return (lhs, rhs, self.comparator, not _ops[self.comparator](lhs, rhs))

    def get_referenced_compnames(self):
        """Returns a set of names of each component referenced by this
        constraint.
        """
        if isinstance(self.rhs, float):
            return self.lhs.get_referenced_compnames()
        else:
            return self.lhs.get_referenced_compnames().union(
                                            self.rhs.get_referenced_compnames())

    def get_referenced_varpaths(self, copy=True, refs=False):
        """Returns a set of names of each component referenced by this
        constraint.
        """
        if isinstance(self.rhs, float):
            return self.lhs.get_referenced_varpaths(copy=copy, refs=refs)
        else:
            return self.lhs.get_referenced_varpaths(copy=copy, refs=refs).union(
                    self.rhs.get_referenced_varpaths(copy=copy, refs=refs))

    def __str__(self):
        return ' '.join((str(self.lhs), self.comparator, str(self.rhs)))

    def __eq__(self, other):
        if not isinstance(other, Constraint):
            return False
        return (self.lhs, self.comparator, self.rhs) == \
               (other.lhs, other.comparator, other.rhs)


class _HasConstraintsBase(object):
    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']

    def __init__(self, parent, allowed_types=None):
        self._constraints = ordereddict.OrderedDict()
        self._parent = None if parent is None else weakref.ref(parent)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = self.parent
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        parent = state['_parent']
        self._parent = None if parent is None else weakref.ref(parent)

    @property
    def parent(self):
        """ The object we are a delegate of. """
        return None if self._parent is None else self._parent()

    def remove_constraint(self, key):
        """Removes the constraint with the given string."""
        key = _remove_spaces(key)
        cnst = self._constraints.get(key)
        if cnst:
            cnst.deactivate()
            del self._constraints[key]
        else:
            msg = "Constraint '%s' was not found. Remove failed." % key
            self.parent.raise_exception(msg, AttributeError)
        self.parent.config_changed()

    def get_references(self, name):
        """Return references to component `name` in
        preparation for subsequent :meth:`restore_references`
        call.

        name: string
            Name of component being referenced.
        """
        refs = ordereddict.OrderedDict()
        for cname, constraint in self._constraints.items():
            if name in constraint.get_referenced_compnames():
                refs[cname] = constraint
        return refs

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        to_remove = []
        for cname, constraint in self._constraints.items():
            if name in constraint.get_referenced_compnames():
                to_remove.append(cname)

        for cname in to_remove:
            self.remove_constraint(cname)

    def restore_references(self, refs):
        """Restore references to component `name` from `refs`.

        refs: object
            Value returned by :meth:`get_references`.

        Note: this is called from the replace() method, where
        the replacing object may be missing variables that were
        found in the target object, so no restore_references
        call should raise an exception when restoring a reference
        fails.
        """
        for name, constraint in refs.items():
            if name in self._constraints:
                self.remove_constraint(name)
            try:
                self.add_constraint(str(constraint), name,
                                    constraint.lhs.scope)
            except Exception as err:
                self.parent._logger.warning("Couldn't restore constraint '%s': %s"
                                            % (name, str(err)))

    def clear_constraints(self):
        """Removes all constraints."""
        for name in self._constraints:
            self.remove_constraint(name)

    def list_constraints(self):
        """Return a list of strings containing constraint expressions."""
        return self._constraints.keys()

    def copy_constraints(self):
        """Returns a copy of our constraints dict."""
        dct = ordereddict.OrderedDict()
        for key, val in self._constraints.items():
            dct[key] = val.copy()
        return dct

    def list_pseudocomps(self):
        """Returns a list of pseudocomponent names associated with our
        constraints.
        """
        return [c.pcomp_name for c in self._constraints.values()
                    if c.pcomp_name]

    def get_expr_depends(self):
        """Returns a list of tuples of the form (comp_name, self_name)
        for each component name referenced by a constraint. Note that this
        does NOT include pseudo-components.
        """
        conn_list = []
        pname = self.parent.name
        for constraint in self._constraints.values():
            conn_list.extend([(c, pname)
                              for c in constraint.get_referenced_compnames()])
        return conn_list

    def get_referenced_compnames(self):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set()
        for constraint in self._constraints.values():
            names.update(constraint.get_referenced_compnames())
        return names

    def get_referenced_varpaths(self, refs=False):
        """Returns a set of variable names referenced by a
        constraint.
        """
        names = set()
        for constraint in self._constraints.values():
            names.update(constraint.get_referenced_varpaths(copy=False, 
                                                            refs=refs))
        return names

    def mimic(self, target):
        """Tries to mimic the target object's constraints.  Target constraints
        that are incompatible with this object are ignored.
        """
        old = self._constraints
        self._constraints = ordereddict.OrderedDict()
        scope = _get_scope(target)

        for name, cnst in target.copy_constraints().items():
            try:
                self.add_existing_constraint(scope, cnst, name)
            except Exception:
                self._constraints = old
                raise

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from
        the target object is 'empty' or not.  If it's empty then it's not an
        error if the replacing object doesn't have this delegate.
        """
        return len(self._constraints)


class HasEqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports equality
    constraints but does not support inequality constraints.
    """

    def add_constraint(self, expr_string, name=None, scope=None):
        """Adds a constraint in the form of a boolean expression string
        to the driver.

        expr_string: str
            Expression string containing the constraint.

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.

        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        """
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self.parent.raise_exception(str(err), type(err))
        if rel == '=':
            self._add_eq_constraint(lhs, rhs, name, scope)
        else:
            msg = "Inequality constraints are not supported on this driver"
            self.parent.raise_exception(msg, ValueError)

    def _add_eq_constraint(self, lhs, rhs, name=None, scope=None):
        """Adds an equality constraint as two strings, a left-hand side and
        a right-hand side.
        """
        if not isinstance(lhs, basestring):
            msg = "Constraint left-hand side (%s) is not a string" % lhs
            raise ValueError(msg)
        if not isinstance(rhs, basestring):
            msg = "Constraint right-hand-side (%s) is not a string" % rhs
            raise ValueError(msg)
        ident = _remove_spaces('='.join((lhs, rhs)))
        if ident in self._constraints:
            self.parent.raise_exception('A constraint of the form "%s" already'
                                        ' exists in the driver. Add failed.'
                                        % ident, ValueError)
        elif name is not None and name in self._constraints:
            self.parent.raise_exception('A constraint named "%s" already exists'
                                        ' in the driver. Add failed.'
                                        % name, ValueError)

        constraint = Constraint(lhs, '=', rhs, scope=_get_scope(self, scope))
        constraint.activate(self.parent)

        name = ident if name is None else name
        self._constraints[name] = constraint

        self.parent.config_changed()

    def add_existing_constraint(self, scope, constraint, name=None):
        """Adds an existing Constraint object to the driver.

        scope: container object where constraint expression will
            be evaluated.

        constraint: Constraint object

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
        """
        if constraint.comparator == '=':
            constraint.activate(self.parent)
            self._constraints[name] = constraint
        else:
            self.parent.raise_exception("Inequality constraint '%s' is not"
                                        " supported on this driver"
                                        % constraint, ValueError)
        self.parent.config_changed()

    def get_eq_constraints(self):
        """Returns an ordered dict of constraint objects."""
        return self._constraints

    def total_eq_constraints(self):
        """Returns the total number of constraint values."""
        return sum([c.size for c in self._constraints.values()])

    def get_constraints(self):
        """Returns an ordered dict of constraint objects"""
        return self._constraints

    def eval_eq_constraints(self, scope=None):
        """Returns a list of constraint values."""
        scope = _get_scope(self, scope)
        result = []
        for constraint in self._constraints.values():
            result.extend(constraint.evaluate(scope))
        return result

    def list_eq_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return ["%s.out0" % c.pcomp_name for c in self._constraints.values()]

    def list_constraint_targets(self):
        return self.list_eq_constraint_targets()


class HasIneqConstraints(_HasConstraintsBase):
    """Add this class as a delegate if your Driver supports inequality
    constraints but does not support equality constraints.
    """

    def add_constraint(self, expr_string, name=None, scope=None):
        """Adds a constraint in the form of a boolean expression string
        to the driver.

        expr_string: str
            Expression string containing the constraint.

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.

        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        """
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self.parent.raise_exception(str(err), type(err))
        self._add_ineq_constraint(lhs, rel, rhs, name, scope)

    def _add_ineq_constraint(self, lhs, rel, rhs, name=None, scope=None):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right-hand side.
        """
        if rel == '=':
            msg = "Equality constraints are not supported on this driver"
            self.parent.raise_exception(msg, ValueError)

        if not isinstance(lhs, basestring):
            msg = "Constraint left-hand-side (%s) is not a string" % lhs
            raise ValueError(msg)
        if not isinstance(rhs, basestring):
            msg = "Constraint right-hand-side (%s) is not a string" % rhs
            raise ValueError(msg)
        ident = _remove_spaces(rel.join((lhs, rhs)))
        if ident in self._constraints:
            self.parent.raise_exception('A constraint of the form "%s" already'
                                        ' exists in the driver. Add failed.'
                                        % ident, ValueError)
        elif name is not None and name in self._constraints:
            self.parent.raise_exception('A constraint named "%s" already exists'
                                        ' in the driver. Add failed.'
                                        % name, ValueError)

        constraint = Constraint(lhs, rel, rhs, scope=_get_scope(self, scope))
        constraint.activate(self.parent)

        if name is None:
            self._constraints[ident] = constraint
        else:
            self._constraints[name] = constraint

        self.parent.config_changed()

    def add_existing_constraint(self, scope, constraint, name=None):
        """Adds an existing Constraint object to the driver.

        scope: container object where constraint expression will
            be evaluated.

        constraint: Constraint object

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
        """
        if constraint.comparator != '=':
            self._constraints[name] = constraint
            constraint.activate(self.parent)
        else:
            self.parent.raise_exception("Equality constraint '%s' is not"
                                        " supported on this driver"
                                        % constraint, ValueError)
        self.parent.config_changed()

    def get_ineq_constraints(self):
        """Returns an ordered dict of inequality constraint objects."""
        return self._constraints

    def total_ineq_constraints(self):
        """Returns the total number of inequality constraint values."""
        return sum([c.size for c in self._constraints.values()])

    def get_constraints(self):
        """Returns an ordered dict of constraint objects"""
        return self._constraints

    def eval_ineq_constraints(self, scope=None):
        """Returns a list of constraint values."""
        scope = _get_scope(self, scope)
        result = []
        for constraint in self._constraints.values():
            result.extend(constraint.evaluate(scope))
        return result

    def list_ineq_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return ["%s.out0" % c.pcomp_name for c in self._constraints.values()]

    def list_constraint_targets(self):
        return self.list_ineq_constraint_targets()


class HasConstraints(object):
    """Add this class as a delegate if your Driver supports both equality
    and inequality constraints.  For methods related to both equality and
    inequality constraints (such as :meth:`eval_constraints`), data for the
    equality constraints comes first.
    """

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']

    def __init__(self, parent):
        self._parent = None if parent is None else weakref.ref(parent)
        self._eq = HasEqConstraints(parent)
        self._ineq = HasIneqConstraints(parent)

    def __getstate__(self):
        state = self.__dict__.copy()
        state['_parent'] = self.parent
        return state

    def __setstate__(self, state):
        self.__dict__.update(state)
        parent = state['_parent']
        self._parent = None if parent is None else weakref.ref(parent)

    @property
    def parent(self):
        """ The object we are a delegate of. """
        return None if self._parent is None else self._parent()

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from
        the target object is 'empty' or not.  If it's empty then it's not an
        error if the replacing object doesn't have this delegate.
        """
        return self._eq._item_count() + self._ineq._item_count()

    def add_constraint(self, expr_string, name=None, scope=None):
        """Adds a constraint in the form of a boolean expression string
        to the driver.

        expr_string: str
            Expression string containing the constraint.

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.

        scope: object (optional)
            The object to be used as the scope when evaluating the expression.
        """
        try:
            lhs, rel, rhs = _parse_constraint(expr_string)
        except Exception as err:
            self.parent.raise_exception(str(err), type(err))
        if rel == '=':
            self._eq._add_eq_constraint(lhs, rhs, name, scope)
        else:
            self._ineq._add_ineq_constraint(lhs, rel, rhs, name, scope)

    def add_existing_constraint(self, scope, constraint, name=None):
        """Adds an existing Constraint object to the driver.

        scope: container object where constraint expression will
            be evaluated.

        constraint: Constraint object

        name: str (optional)
            Name to be used to refer to the constraint rather than its
            expression string.
        """
        if constraint.comparator == '=':
            self._eq.add_existing_constraint(scope, constraint, name)
        else:
            self._ineq.add_existing_constraint(scope, constraint, name)

    def remove_constraint(self, expr_string):
        """Removes the constraint with the given string."""
        ident = _remove_spaces(expr_string)
        if ident in self._eq._constraints:
            self._eq.remove_constraint(expr_string)
        else:
            self._ineq.remove_constraint(expr_string)

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        return (self._eq.get_references(name),
                self._ineq.get_references(name))

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        self._eq.remove_references(name)
        self._ineq.remove_references(name)

    def restore_references(self, refs):
        """Restore references to component `name` from `refs`.

        refs: dict
            References returned by :meth:`get_references`.
        """
        if isinstance(refs, tuple) and len(refs) == 2:
            self._eq.restore_references(refs[0])
            self._ineq.restore_references(refs[1])
        else:
            raise TypeError('refs should be tuple of ordereddict.OrderedDict,'
                            ' got %r' % refs)

    def clear_constraints(self):
        """Removes all constraints."""
        self._eq.clear_constraints()
        self._ineq.clear_constraints()

    def copy_constraints(self):
        dct = self._eq.copy_constraints()
        dct.update(self._ineq.copy_constraints())
        return dct

    def _add_ineq_constraint(self, lhs, comparator, rhs, scaler, adder,
                             name=None, scope=None):
        """Adds an inequality constraint as three strings; a left-hand side,
        a comparator ('<','>','<=', or '>='), and a right hand side.
        """
        self._ineq._add_ineq_constraint(lhs, comparator, rhs, scaler, adder,
                                        name, scope)

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

    def get_constraints(self):
        """Returns an ordered dict of constraint objects"""
        return ordereddict.OrderedDict(self._eq.get_eq_constraints().items() +
                                       self._ineq.get_ineq_constraints().items())

    def total_eq_constraints(self):
        """Returns the total number of equality constraint values."""
        return self._eq.total_eq_constraints()

    def total_ineq_constraints(self):
        """Returns the total number of inequality constraint values."""
        return self._ineq.total_ineq_constraints()

    def total_constraints(self):
        """Returns the total number of constraint values."""
        return self._eq.total_eq_constraints() + \
               self._ineq.total_ineq_constraints()

    def eval_eq_constraints(self, scope=None):
        """Returns a list of constraint values."""
        return self._eq.eval_eq_constraints(scope)

    def eval_ineq_constraints(self, scope=None):
        """Returns a list of constraint values."""
        return self._ineq.eval_ineq_constraints(scope)

    def eval_constraints(self, scope=None):
        """Returns a list of constraint values."""
        return self._eq.eval_eq_constraints(scope) + \
               self._ineq.eval_ineq_constraints(scope)

    def list_constraints(self):
        """Return a list of strings containing constraint expressions."""
        return self._eq.list_constraints() + self._ineq.list_constraints()

    def list_pseudocomps(self):
        """Returns a list of pseudocomponent names associated with our
        constraints.
        """
        return self._eq.list_pseudocomps() + self._ineq.list_pseudocomps()

    def list_eq_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return self._eq.list_eq_constraint_targets()

    def list_ineq_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return self._ineq.list_ineq_constraint_targets()

    def list_constraint_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return self._eq.list_eq_constraint_targets() + \
               self._ineq.list_ineq_constraint_targets()

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by a constraint.
        """
        return self._eq.get_expr_depends() + self._ineq.get_expr_depends()

    def get_referenced_compnames(self):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set(self._eq.get_referenced_compnames())
        names.update(self._ineq.get_referenced_compnames())
        return names

    def get_referenced_varpaths(self, refs=False):
        """Returns a set of names of each component referenced by a
        constraint.
        """
        names = set(self._eq.get_referenced_varpaths(refs=refs))
        names.update(self._ineq.get_referenced_varpaths(refs=refs))
        return names

    def mimic(self, target):
        """Tries to mimic the target object's constraints.  Target constraints
        that are incompatible with raise an exception.
        """
        self.clear_constraints()
        scope = _get_scope(self)
        for name, cnst in target.copy_constraints().items():
            self.add_existing_constraint(scope, cnst, name)
