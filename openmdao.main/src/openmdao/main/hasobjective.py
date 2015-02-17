
from collections import OrderedDict
import weakref

from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent, _remove_spaces
from openmdao.main.interfaces import IDriver


class Objective(ConnectedExprEvaluator):
    def __init__(self, *args, **kwargs):
        super(Objective, self).__init__(*args, **kwargs)
        self._pseudo = None
        unresolved_vars = self.get_unresolved()
        if unresolved_vars:
            msg = "Can't add objective '{0}' because of invalid variables {1}"
            raise ConnectedExprEvaluator._invalid_expression_error(unresolved_vars, self.text, msg)

        self._pseudo = PseudoComponent(self.scope, self, pseudo_type='objective')
        self.pcomp_name = self._pseudo.name

    def activate(self, driver):
        """Make this constraint active by creating the appropriate
        connections in the dependency graph.
        """
        self._pseudo.activate(self.scope, driver)

    def deactivate(self):
        """Remove this objective from the dependency graph and remove
        its pseudocomp from the scoping object.
        """
        if self._pseudo is not None:
            scope = self.scope
            try:
                getattr(scope, self._pseudo.name)
            except AttributeError:
                pass
            else:
                scope.remove(self._pseudo.name)

    def evaluate(self, scope=None):
        """Use the value in the u vector if it exists instead of pulling
        the value from scope.
        """
        if self.pcomp_name:
            scope = self._get_updated_scope(scope)
            try:
                system = getattr(scope, self.pcomp_name)._system
                vname = self.pcomp_name + '.out0'
                if scope._var_meta[vname].get('scalar'):
                    return system.vec['u'][scope.name2collapsed[vname]][0]
                else:
                    return system.vec['u'][scope.name2collapsed[vname]]
            except (KeyError, AttributeError):
                pass

        return super(Objective, self).evaluate(scope)



class HasObjectives(object):
    """This class provides an implementation of the IHasObjectives interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']

    def __init__(self, parent, max_objectives=0):
        self._objectives = OrderedDict()
        # max_objectives of 0 means unlimited objectives
        self._max_objectives = max_objectives
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

    def _item_count(self):
        """This is used by the replace function to determine if a delegate from
        the target object is 'empty' or not.  If it's empty then it's not an
        error if the replacing object doesn't have this delegate.
        """
        return len(self._objectives)

    def add_objectives(self, obj_iter, scope=None):
        """Takes an iterator of objective strings and creates
        objectives for them in the driver.
        """
        if isinstance(obj_iter, basestring):
            self.parent.raise_exception("add_objectives requires an iterator of"
                                        " expression strings.", ValueError)
        for expr in obj_iter:
            self.parent.add_objective(expr, scope=scope)

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
        if self._max_objectives > 0 and \
           len(self._objectives) >= self._max_objectives:
            self.parent.raise_exception("Can't add objective '%s'. Only %d"
                                        " objectives are allowed"
                                        % (expr, self._max_objectives),
                                        RuntimeError)
        expr = _remove_spaces(expr)
        if expr in self._objectives:
            self.parent.raise_exception("Trying to add objective '%s' to"
                                        " driver, but it's already there"
                                        % expr, AttributeError)
        if name is not None and name in self._objectives:
            self.parent.raise_exception("Trying to add objective '%s' to"
                                        " driver using name '%s', but name is"
                                        " already used" % (expr, name),
                                        AttributeError)

        scope = self._get_scope(scope)
        try:
            expreval = Objective(expr, scope)
        except Exception as err:
            self.parent.raise_exception(str(err), type(err))

        name = expr if name is None else name

        if IDriver.providedBy(self.parent):
            #expreval.activate(self.parent)
            self.parent.config_changed()

        self._objectives[name] = expreval

    def remove_objective(self, expr):
        """Removes the specified objective expression. Spaces within
        the expression are ignored.
        """
        expr = _remove_spaces(expr)
        obj = self._objectives.get(expr)
        if obj:
            obj.deactivate()
            del self._objectives[expr]
        else:
            self.parent.raise_exception("Trying to remove objective '%s' "
                                        "that is not in this driver." % expr,
                                        AttributeError)
        self.parent.config_changed()

    def name_changed(self, old, new):
        """Change any objectives that reference the old
        name of an object that has now been changed to a new name.

        old: string
            Original name of the object

        new: string
            New name of the object
        """
        for name, obj in self._objectives.items():
            orig = obj.text
            trans = obj.name_changed(old, new)
            if orig != trans and name == orig:  # expr changed and no alias
                del self._objectives[name]
                self._objectives[trans] = obj

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        refs = OrderedDict()
        for oname, obj in self._objectives.items():
            if name in obj.get_referenced_compnames():
                refs[oname] = obj
        return refs

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        to_remove = []
        for oname, obj in self._objectives.items():
            if name in obj.get_referenced_compnames():
                to_remove.append(oname)

        for oname in to_remove:
            self.remove_objective(oname)

    def restore_references(self, refs):
        """Restore references to component `name` from `refs`.

        refs: object
            Value returned by :meth:`get_references`.
        """

        # Old objective seems to get removed automatically so no need to
        # clear objectives and recreate them.
        for name, obj in refs.items():
            if not obj.check_resolve():
                self.parent._logger.warning("Couldn't restore objective"
                                            " '%s': %s were unresolved" %
                                            (name, obj.get_unresolved()))
            else:
                try:
                    self.add_objective(str(obj), name, obj.scope)
                except Exception as err:
                    self.parent._logger.warning("Couldn't restore objective"
                                                " '%s': %s" % (name, str(err)))

    def get_objectives(self):
        """Returns an OrderedDict of objective expressions."""
        return self._objectives

    def clear_objectives(self):
        """Removes all objectives."""
        for name in self._objectives.keys():
            self.remove_objective(name)

    def eval_objectives(self):
        """Returns a list of values of the evaluated objectives."""
        scope = self._get_scope()
        return [obj.evaluate(scope) for obj in self._objectives.values()]

    def eval_named_objective(self, name):
        """Returns the value of objective `name`."""
        return self._objectives[name].evaluate(self._get_scope())

    def list_pseudocomps(self):
        """Returns a list of pseudocomponent names associated with our
        parameters.
        """
        return [obj.pcomp_name for obj in self._objectives.values()
                      if obj.pcomp_name]

    def list_objective_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return ["%s.out0" % obj.pcomp_name for obj in self._objectives.values()]

    def get_expr_depends(self):
        """Returns a list of tuples of the form (comp_name, parent_name)
        for each component referenced by our objectives. Note that this does not
        include pseudo-components.
        """
        pname = self.parent.name
        conn_list = []
        for obj in self._objectives.values():
            conn_list.extend([(c, pname)
                              for c in obj.get_referenced_compnames()])
        return conn_list

    def get_referenced_compnames(self):
        """Returns the names of components referenced by the objectives."""
        lst = []
        for obj in self._objectives.values():
            lst.extend(obj.get_referenced_compnames())
        return lst

    def get_referenced_varpaths(self, refs=False):
        """Returns the names of variables referenced by the objectives."""
        lst = []
        for obj in self._objectives.values():
            lst.extend(obj.get_referenced_varpaths(copy=False, refs=refs))
        return lst

    def _get_scope(self, scope=None):
        if scope is None:
            try:
                return self.parent.get_expr_scope()
            except AttributeError:
                pass
        return scope

    def mimic(self, target):
        """Copy what objectives we can from the target."""
        if self._max_objectives > 0 and \
           len(target._objectives) > self._max_objectives:
            self.parent.raise_exception("This driver allows a maximum of %d"
                                        " objectives, but the driver being"
                                        " replaced has %d" %
                                        (self._max_objectives,
                                         len(target._objectives)),
                                        RuntimeError)
        self.clear_objectives()
        for name, obj in target._objectives.items():
            self.add_objective(obj.text, name=name, scope=obj.scope)


class HasObjective(HasObjectives):
    def __init__(self, parent):
        super(HasObjective, self).__init__(parent, max_objectives=1)

    def eval_objective(self):
        """Returns a list of values of the evaluated objectives."""
        if len(self._objectives) > 0:
            return super(HasObjective, self).eval_objectives()[0]
        else:
            self.parent.raise_exception("no objective specified", Exception)
