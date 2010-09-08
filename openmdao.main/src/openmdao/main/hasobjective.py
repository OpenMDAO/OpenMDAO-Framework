
import ordereddict

from openmdao.main.expreval import ExprEvaluator


def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')


class HasObjective(object): 
    """This class provides an implementation of the IHasObjective interface."""

    def __init__(self, parent):
        self._objective = ''
        self._parent = parent

    def add_objective(self, expr):
        """Sets the objective of this driver to be the specified expression.
        If there is a preexisting objective in this driver, it is replaced.
        
        expr: string
            String containing the objective expression.
         """
        expr = _remove_spaces(expr)
        expreval = ExprEvaluator(expr, self._parent)
        
        if not expreval.check_resolve():
            self._parent.raise_exception("Can't add objective because I can't evaluate '%s'." % expr, 
                                         ValueError)
        self._objective = expreval
            
    def list_objective(self):
        """Returns the expression string for the objective."""
        return str(self._objective)
    
    def get_objective(self):
        """Returns the objective object."""
        return self._objective
    
    def eval_objective(self):
        """Returns the value of the evaluated objective."""
        if self._objective is None:
            self._parent.raise_exception("No objective has been defined.")
        return self._objective.evaluate()


class HasObjectives(object): 
    """This class provides an implementation of the IHasObjectives interface."""

    def __init__(self, parent):
        self._objectives = ordereddict.OrderedDict()
        self._parent = parent

    def add_objectives(self, obj_iter):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """
        if isinstance(obj_iter, basestring):
            self._parent.raise_exception("add_objectives requires a list of expression strings.",
                                         ValueError)
        for expr in obj_iter:
            self._parent.add_objective(expr)

    def add_objective(self, expr):
        """Adds an objective to the driver. 
        
        expr: string
            String containing the objective expression.
            
         """
        expr = _remove_spaces(expr)
        if expr in self._objectives: 
            self._parent.raise_exception("Trying to add objective '%s' to driver, "
                                         "but it's already there" % expr,
                                         AttributeError)
        expreval = ExprEvaluator(expr, self._parent)
        
        if not expreval.check_resolve():
            self._parent.raise_exception("Can't add objective because I can't evaluate '%s'." % expr, 
                                         ValueError)
        self._objectives[expr] = expreval
            
    def remove_objective(self, expr):
        """Removes the specified objective expression. Spaces within
        the expression are ignored.
        """
        expr = _remove_spaces(expr)
        try:
            del self._objectives[expr]
        except KeyError:
            self._parent.raise_exception("Trying to remove objective '%s' "
                                         "that is not in this driver." % expr,
                                         AttributeError)

    def list_objectives(self):
        """Returns a list of objective expressions."""
        return self._objectives.keys()
    
    def clear_objectives(self):
        """Removes all objectives."""
        self._objectives = ordereddict.OrderedDict()
        
    def get_objectives(self):
        """Returns an ordered dict of objective objects."""
        return self._objectives

    def eval_objectives(self):
        """Returns the value of the evaluated objective."""
        return [obj.evaluate() for obj in self._objectives.values()]

    