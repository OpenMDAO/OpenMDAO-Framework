
import ordereddict

from openmdao.main.expreval import ExprEvaluator


def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')


class HasObjective(object): 
    """This class provides an implementation of the IHasObjective interface."""

    _do_not_promote = ['get_expr_depends']

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
        return self._objective.text
    
    def eval_objective(self):
        """Returns the value of the evaluated objective."""
        if not self._objective:
            self._parent.raise_exception("no objective specified", ValueError)
        return self._objective.evaluate()

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our objective.
        """
        if not self._objective:
            return []
        pname = self._parent.name
        return [(cname,pname) for cname in self._objective.get_referenced_compnames()]
    

class HasObjectives(object): 
    """This class provides an implementation of the IHasObjectives interface."""

    _do_not_promote = ['get_expr_depends']
    
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
        
    def eval_objectives(self):
        """Returns a list of values of the evaluated objectives."""
        return [obj.evaluate() for obj in self._objectives.values()]

    def get_expr_depends(self):
        """Returns a list of tuples of the form (src_comp_name, dest_comp_name)
        for each dependency introduced by our objectives.
        """
        pname = self._parent.name
        conn_list = []
        for obj in self._objectives.values():
            conn_list.extend([(cname,pname) for cname in obj.get_referenced_compnames()])
        return conn_list
    
