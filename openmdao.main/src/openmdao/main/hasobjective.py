
import weakref
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
        self._max_objectives = max_objectives # max_objectives of 0 means unlimited objectives
        self._parent = parent

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from the
        target object is 'empty' or not.  If it's empty then it's not an error if the
        replacing object doesn't have this delegate.
        """
        return len(self._objectives)
    
    def add_objectives(self, obj_iter, scope=None):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """
        if isinstance(obj_iter, basestring):
            self._parent.raise_exception("add_objectives requires an iterator of expression strings.",
                                         ValueError)
        for expr in obj_iter:
            self._parent.add_objective(expr, scope=scope)
            
        self._parent._invalidate()

    def add_objective(self, expr, name=None, scope=None):
        """Adds an objective to the driver. 
        
        expr: string
            String containing the objective expression.
            
        name: string (optional)
            Name to be used to refer to the objective in place of the expression
            string.
            
        scope: object (optional)
            The object to be used as the scope when evaluating the expression.

         """
        if self._max_objectives > 0 and len(self._objectives) >= self._max_objectives:
            self._parent.raise_exception("Can't add objective '%s'. Only %d objectives are allowed" % (expr,self._max_objectives),
                                         RuntimeError)
        expr = _remove_spaces(expr)
        if expr in self._objectives: 
            self._parent.raise_exception("Trying to add objective "
                                         "'%s' to driver, but it's already there" % expr,
                                         AttributeError)
        if name is not None and name in self._objectives:
            self._parent.raise_exception("Trying to add objective "
                                         "'%s' to driver using name '%s', but name is already used" % (expr,name),
                                         AttributeError)
            
        expreval = ExprEvaluator(expr, self._get_scope(scope))
        
        if not expreval.check_resolve():
            self._parent.raise_exception("Can't add objective because I can't evaluate '%s'." % expr, 
                                         ValueError)
            
        if name is None:
            self._objectives[expr] = expreval
        else:
            self._objectives[name] = expreval
            
        self._parent._invalidate()
            
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
        self._parent._invalidate()

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        # Just returning everything for now.
        return self._objectives.copy()

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        for oname, obj in self._objectives.items():
            if name in obj.get_referenced_compnames():
                self.remove_objective(oname)

    def restore_references(self, refs, name):
        """Restore references to component `name` from `refs`.

        name: string
            Name of component being removed.

        refs: object
            Value returned by :meth:`get_references`.
        """
        # Not exactly safe here...
        if isinstance(refs, ordereddict.OrderedDict):
            self._objectives = refs
        else:
            raise TypeError('refs should be ordereddict.OrderedDict, got %r' 
                            % refs)

    def get_objectives(self):
        """Returns an OrderedDict of objective expressions."""
        return self._objectives
    
    def clear_objectives(self):
        """Removes all objectives."""
        self._objectives = ordereddict.OrderedDict()
        self._parent._invalidate()
        
    def eval_objectives(self):
        """Returns a list of values of the evaluated objectives."""
        return [obj.evaluate(self._get_scope()) for obj in self._objectives.values()]

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
            lst.extend(obj.get_referenced_varpaths(copy=False))
        return lst
    
    def _get_scope(self, scope=None):
        if scope is None:
            try:
                return self._parent.get_expr_scope()
            except AttributeError:
                pass
        return scope
    
    def mimic(self, target):
        """Copy what objectives we can from the target."""
        if self._max_objectives > 0 and len(target._objectives) > self._max_objectives:
            self._parent.raise_exception("This driver allows a maximum of %d objectives, but the driver being replaced has %d" %
                                         (self._max_objectives, len(target._objectives)),
                                         RuntimeError)
        old_obj = self._objectives
        self.clear_objectives()
        try:
            for name,obj in target._objectives.items():
                self.add_objective(obj.text, name=name, scope=obj.scope)
        except Exception:
            self._objectives = old_obj
            raise
        
class HasObjective(HasObjectives):
    def __init__(self, parent):
        super(HasObjective, self).__init__(parent, max_objectives=1)
        
    def eval_objective(self):
        """Returns a list of values of the evaluated objectives."""
        if len(self._objectives) > 0:
            return super(HasObjective, self).eval_objectives()[0]
        else:
            self._parent.raise_exception("no objective specified", Exception)


