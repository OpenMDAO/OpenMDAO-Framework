
import ordereddict

from openmdao.main.expreval import ExprEvaluator


def _remove_spaces(s):
    return s.translate(None, ' \n\t\r')

class HasObjectives(object): 
    """This class provides an implementation of the IHasObjectives interface."""

    _do_not_promote = ['get_expr_depends','get_referenced_compnames',
                       'get_referenced_varpaths']
    
    def __init__(self, parent, max_objectives=0):
        self._objectives = ordereddict.OrderedDict()
        self._max_objectives = max_objectives
        self._parent = parent

    def add_objectives(self, obj_iter):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """
        if isinstance(obj_iter, basestring):
            self._parent.raise_exception("add_objectives requires an iterator of expression strings.",
                                         ValueError)
        for expr in obj_iter:
            self._parent.add_objective(expr)

    def add_objective(self, expr):
        """Adds an objective to the driver. 
        
        expr: string
            String containing the objective expression.
            
         """
        if self._max_objectives > 0 and len(self._objectives) >= self._max_objectives:
            self._parent.raise_exception("Can't add objective '%s'. Only %d objectives are allowed" % (expr,self._max_objectives),
                                         RuntimeError)
        expr = _remove_spaces(expr)
        if expr in self._objectives: 
            self._parent.raise_exception("Trying to add objective '%s' to driver, "
                                         "but it's already there" % expr,
                                         AttributeError)
        expreval = ExprEvaluator(expr, self._parent.parent)
        
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
    def get_objectives(self):
        """Returns an OrderedDict of objective expressions."""
        return self._objectives
    
    def clear_objectives(self):
        """Removes all objectives."""
        self._objectives = ordereddict.OrderedDict()
        
    def eval_objectives(self):
        """Returns a list of values of the evaluated objectives."""
        return [obj.evaluate(self._parent.parent) for obj in self._objectives.values()]

    def get_expr_depends(self):
        """Returns a list of tuples of the form (comp_name, parent_name)
        for each component referenced by our objectives.
        """
        pname = self._parent.name
        conn_list = []
        for obj in self._objectives.values():
            conn_list.extend([(cname,pname) for cname in obj.get_referenced_compnames()])
        return conn_list
    
    def get_referenced_compnames(self):
        """Returns the names of components referenced by the objectives."""
        lst = []
        for obj in self._objectives.values():
            lst.extend(obj.get_referenced_compnames())
        return lst

    def get_referenced_varpaths(self):
        """Returns the names of variables referenced by the objectives."""
        lst = []
        for obj in self._objectives.values():
            lst.extend(obj.get_referenced_varpaths())
        return lst
    
    def max_objectives(self):
        return self._max_objectives


class HasObjective(HasObjectives):
    def __init__(self, parent):
        super(HasObjective, self).__init__(parent, max_objectives=1)
        
    def eval_objective(self):
        """Returns a list of values of the evaluated objectives."""
        if len(self._objectives) > 0:
            return super(HasObjective, self).eval_objectives()[0]
        else:
            self._parent.raise_exception("no objective specified", Exception)


