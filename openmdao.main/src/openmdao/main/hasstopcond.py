from collections import OrderedDict

from openmdao.main.expreval import ExprEvaluator

def _check_expr(expr, scope):
    # force checking for existence of vars referenced in expression
    if not expr.check_resolve():
        msg = "Invalid expression '%s'" % str(expr)
        scope.raise_exception(msg, ValueError)

def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

class HasStopConditions(object):
    """A delegate that adds handling of stop conditions that are
    supplied as expression strings.
    """
    def __init__(self, parent):
        self._parent = parent
        self._stop_conditions = OrderedDict()

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from the
        target object is 'empty' or not.  If it's empty then it's not an error if the
        replacing object doesn't have this delegate.
        """
        return len(self._stop_conditions)

    def add_stop_condition(self, exprstr):
        ident = _remove_spaces(exprstr)
        expr = ExprEvaluator(exprstr, scope=self._parent.parent)
        _check_expr(expr, self._parent)
        self._stop_conditions[ident] = expr

    def remove_stop_condition(self, expr_string):
        """Removes the stop condition matching the given string."""
        try:
            del self._stop_conditions[_remove_spaces(expr_string)]
        except KeyError:
            self._parent.raise_exception("Stop condition '%s' was not found. Remove failed." %
                                         expr_string, AttributeError)

    def clear_stop_conditions(self):
        """Removes all stop conditions."""
        self._stop_conditions = OrderedDict()

    def get_stop_conditions(self):
        """Returns a list of stop condition strings."""
        return self._stop_conditions.keys()

    def eval_stop_conditions(self):
        """Returns a list of evaluated stop conditions."""
        return [c.evaluate() for c in self._stop_conditions.values()]

    def should_stop(self):
        """Return True if any of the stopping conditions evaluate to True."""
        for cond in self._stop_conditions.values():
            if cond.evaluate(): # and cond.refs_valid():
                return True
        return False

    def mimic(self, target):
        """Copy stop conditions from the target."""
        old_stop_cond = self._stop_conditions
        self.clear_stop_conditions()
        try:
            for exp in target.get_stop_conditions():
                self.add_stop_condition(exp)
        except Exception:
            self._stop_conditions = old_stop_cond
            raise
