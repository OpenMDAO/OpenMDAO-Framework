import weakref
from collections import OrderedDict

from openmdao.main.vartree import VariableTree
from openmdao.main.datatypes.api import List, VarTree
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent, _remove_spaces
from openmdao.main.variable import make_legal_path


class Response(ConnectedExprEvaluator):

    def __init__(self, *args, **kwargs):
        super(Response, self).__init__(*args, **kwargs)
        self._pseudo = None
        self._pseudo = PseudoComponent(self.scope, self, pseudo_type='objective')
        self.pcomp_name = self._pseudo.name

    def activate(self, driver):
        """Make this response active by creating the appropriate
        connections in the dependency graph.
        """
        self._pseudo.activate(self.scope, driver)

    def deactivate(self):
        """Remove this response from the dependency graph and remove
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

        return super(Response, self).evaluate(scope)

class HasResponses(object):
    """This class provides an implementation of the IHasResponses interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']

    def __init__(self, parent):
        self._responses = OrderedDict()
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
        return len(self._responses)

    def add_responses(self, response_iter, scope=None):
        """Takes an iterator of response strings and creates
        responses for them in the driver.
        """
        if isinstance(response_iter, basestring):
            self.parent.raise_exception("add_responses requires an iterator of"
                                        " expression strings.", ValueError)
        for expr in response_iter:
            self.parent.add_response(expr, scope=scope)

    def add_response(self, expr, name=None, scope=None):
        """Adds a response to the driver.

        expr: string
            String containing the response expression.

        name: string (optional)
            Name to be used to refer to the response in place of the expression
            string.

        scope: object (optional)
            The object to be used as the scope when evaluating the expression.

        """
        expr = _remove_spaces(expr)
        if expr in self._responses:
            self.parent.raise_exception("Trying to add response '%s' to"
                                        " driver, but it's already there"
                                        % expr, AttributeError)
        if name is not None and name in self._responses:
            self.parent.raise_exception("Trying to add response '%s' to"
                                        " driver using name '%s', but name is"
                                        " already used" % (expr, name),
                                        AttributeError)

        scope = self._get_scope(scope)
        try:
            expreval = Response(expr, scope)
            unresolved_vars = expreval.get_unresolved()
        except AttributeError:
            unresolved_vars = [expr]
        if unresolved_vars:
            msg = "Can't add response '{0}' because of invalid variables {1}"
            error = ConnectedExprEvaluator._invalid_expression_error(unresolved_vars,
                                                                     expr, msg)
            self.parent.raise_exception(str(error), type(error))

        name = expr if name is None else name

        #expreval.activate(self.parent)

        self._responses[name] = expreval
        self.parent.config_changed()

    def remove_response(self, expr):
        """Removes the specified response expression. Spaces within
        the expression are ignored.
        """
        expr = _remove_spaces(expr)
        response = self._responses.get(expr)
        if response:
            response.deactivate()
            del self._responses[expr]
        else:
            self.parent.raise_exception("Trying to remove response '%s' "
                                        "that is not in this driver." % expr,
                                        AttributeError)
        self.parent.config_changed()

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        refs = OrderedDict()
        for rname, response in self._responses.items():
            if name in response.get_referenced_compnames():
                refs[rname] = response
        return refs

    def remove_references(self, name):
        """Remove references to component `name`.

        name: string
            Name of component being removed.
        """
        to_remove = []
        for rname, response in self._responses.items():
            if name in response.get_referenced_compnames():
                to_remove.append(rname)

        for rname in to_remove:
            self.remove_response(rname)

    def restore_references(self, refs):
        """Restore references to component `name` from `refs`.

        refs: object
            Value returned by :meth:`get_references`.
        """

        for name, response in refs.items():
            try:
                if response.check_resolve():
                    self.add_response(str(response), name, response.scope)
                else:
                    raise AttributeError("'%s' are unresolved." %
                                               response.get_unresolved())
            except Exception as err:
                self.parent._logger.warning("Couldn't restore response '%s':"
                                            " %s" % (name, err))

    def get_responses(self):
        """Returns an OrderedDict of response expressions."""
        return self._responses

    def clear_responses(self):
        """Removes all responses."""
        for name in self._responses.keys():
            self.remove_response(name)

    def eval_responses(self):
        """Returns a list of values of the evaluated responses."""
        return [r.evaluate() for r in self._responses.values()]

    def eval_response(self, name):
        """Returns the value of response `name`."""
        return self._responses[name].evaluate()

    def list_pseudocomps(self):
        """Returns a list of pseudocomponent names associated with our
        parameters.
        """
        return [response.pcomp_name for response in self._responses.values()
                                                 if response.pcomp_name]

    def list_response_targets(self):
        """Returns a list of outputs suitable for calc_gradient()."""
        return ["%s.out0" % response.pcomp_name for response
                                                 in self._responses.values()]

    def get_expr_depends(self):
        """Returns a list of tuples of the form (comp_name, parent_name)
        for each component referenced by our responses. Note that this does not
        include pseudo-components.
        """
        pname = self.parent.name
        conn_list = []
        for response in self._responses.values():
            conn_list.extend([(c, pname)
                              for c in response.get_referenced_compnames()])
        return conn_list

    def get_referenced_compnames(self):
        """Returns the names of components referenced by the responses."""
        lst = []
        for response in self._responses.values():
            lst.extend(response.get_referenced_compnames())
        return lst

    def get_referenced_varpaths(self):
        """Returns the names of variables referenced by the responses."""
        lst = []
        for response in self._responses.values():
            lst.extend(response.get_referenced_varpaths(copy=False))
        return lst

    def _get_scope(self, scope=None):
        if scope is None:
            try:
                return self.parent.get_expr_scope()
            except AttributeError:
                pass
        return scope

    def mimic(self, target):
        """Copy what responses we can from the target."""
        self.clear_responses()
        for name, response in target._responses.items():
            self.add_response(response.text, name=name, scope=response.scope)


class HasVarTreeResponses(HasResponses):
    """ Responses associated with a case driver which has VarTree outputs. """

    def add_response(self, expr, name=None, scope=None):
        """Adds a response to the driver."""
        super(HasVarTreeResponses, self).add_response(expr, name, scope)

        path = _remove_spaces(expr) if name is None else name
        path = make_legal_path(path)
        obj = self.parent
        names = ['case_outputs'] + path.split('.')
        for name in names[:-1]:
            if obj.get_trait(name):
                val = obj.get(name)
            else:
                val = VariableTree()
                obj.add_trait(name, VarTree(val, iotype='out'))
            obj = val

        name = names[-1]
        obj.add_trait(name, List(iotype='out'))

    def init_responses(self, length):
        """Initializes response storage in the driver."""
        nan = float('NaN')
        for path in self._responses:
            self.parent.case_outputs.set(make_legal_path(path),
                                         [nan] * length)

    def remove_response(self, expr):
        """Removes the specified response expression. Spaces within
        the expression are ignored.
        """
        super(HasVarTreeResponses, self).remove_response(expr)

        path = make_legal_path(_remove_spaces(expr))
        obj = self.parent
        names = ['case_outputs'] + path.split('.')
        for name in names[:-1]:
            obj = obj.get(name)

        name = names[-1]
        obj.remove_trait(name)
