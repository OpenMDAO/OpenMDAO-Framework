
import ordereddict

from openmdao.main.expreval import ExprEvaluator

def _check_expr(expr, scope):
    try:
        # force checking for existence of vars referenced in expression
        expr.refs_valid()  
    except (AttributeError, RuntimeError), err:
        msg = "Invalid expression '%s': %s" % (str(expr), err)
        scope.raise_exception(msg, ValueError)

def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

class HasStopConditions(object):
    """A delegate that adds handling of stop conditions that are
    supplied as expression strings.
    """
    def __init__(self, parent):
        self._parent = parent
        self._stop_conditions = ordereddict.OrderedDict()
    
    def add_stop_condition(self, exprstr):
        ident = _remove_spaces(exprstr)
        expr = ExprEvaluator(exprstr, scope=self._parent)
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
        self._stop_conditions = ordereddict.OrderedDict()
        
    def get_stop_conditions(self):
        """Returns an ordered dict of stop condition strings"""
        return self._stop_conditions.keys()

    def eval_stop_conditions(self): 
        """Returns a list of tuples of the 
        form (lhs, rhs, relation, is_violated)
        """
        return [c.evaluate() for c in self._stop_conditions.values()]
    
    def should_stop(self):
        """Return True if any of the stopping conditions evaluate to True."""
        return any(self.eval_stop_conditions())
