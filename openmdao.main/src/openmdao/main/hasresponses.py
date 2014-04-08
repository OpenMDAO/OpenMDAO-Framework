import ordereddict

from numpy import array, ndarray

from openmdao.main.vartree import VariableTree
from openmdao.main.datatypes.api import Array, List, VarTree
from openmdao.main.expreval import ConnectedExprEvaluator
from openmdao.main.pseudocomp import PseudoComponent, _remove_spaces


class Response(ConnectedExprEvaluator):

    def __init__(self, *args, **kwargs):
        super(Response, self).__init__(*args, **kwargs)
        self.pcomp_name = None

    def activate(self):
        """Make this response active by creating the appropriate
        connections in the dependency graph.
        """
        if self.pcomp_name is None:
            pseudo = PseudoComponent(self.scope, self, pseudo_type='objective')
            self.pcomp_name = pseudo.name
            self.scope.add(pseudo.name, pseudo)
        getattr(self.scope, self.pcomp_name).make_connections(self.scope)

    def deactivate(self):
        """Remove this response from the dependency graph and remove
        its pseudocomp from the scoping object.
        """
        if self.pcomp_name:
            scope = self.scope
            try:
                getattr(scope, self.pcomp_name)
            except AttributeError:
                pass
            else:
                scope.remove(self.pcomp_name)

            self.pcomp_name = None


class HasResponses(object):
    """This class provides an implementation of the IHasResponses interface."""

    _do_not_promote = ['get_expr_depends', 'get_referenced_compnames',
                       'get_referenced_varpaths']

    def __init__(self, parent):
        self._responses = ordereddict.OrderedDict()
        self._parent = parent

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
            self._parent.raise_exception("add_responses requires an iterator of"
                                         " expression strings.", ValueError)
        for expr in response_iter:
            self._parent.add_response(expr, scope=scope)

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
            self._parent.raise_exception("Trying to add response '%s' to"
                                         " driver, but it's already there"
                                         % expr, AttributeError)
        if name is not None and name in self._responses:
            self._parent.raise_exception("Trying to add response '%s' to"
                                         " driver using name '%s', but name is"
                                         " already used" % (expr, name),
                                         AttributeError)


        scope = self._get_scope(scope)
        expreval = Response(expr, scope)
        unresolved_vars = expreval.get_unresolved()
        if unresolved_vars:
            msg = "Can't add response '{0}' because of invalid variables {1}"
            error = ConnectedExprEvaluator._invalid_expression_error(unresolved_vars,
                                                                     expreval.text, msg)
            self._parent.raise_exception(str(error), type(error))

        name = expr if name is None else name

        expreval.activate()

        self._responses[name] = expreval
        self._parent.config_changed()

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
            self._parent.raise_exception("Trying to remove response '%s' "
                                         "that is not in this driver." % expr,
                                         AttributeError)
        self._parent.config_changed()

    def get_references(self, name):
        """Return references to component `name` in preparation for subsequent
        :meth:`restore_references` call.

        name: string
            Name of component being removed.
        """
        refs = ordereddict.OrderedDict()
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

        # Old response seems to get removed automatically so no need to
        # clear responses and recreate them.
        for name, response in refs.items():
            try:
                self.add_response(str(response), name, response.scope)
            except Exception as err:
                self._parent._logger.warning("Couldn't restore response '%s':"
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
        scope = self._get_scope()
        responses = []
        for response in self._responses.values():
            pcomp = getattr(scope, response.pcomp_name)
            if not pcomp.is_valid():
                pcomp.update_outputs(['out0'])
            responses.append(pcomp.out0)
        return responses

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
        pname = self._parent.name
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
                return self._parent.get_expr_scope()
            except AttributeError:
                pass
        return scope

    def mimic(self, target):
        """Copy what responses we can from the target."""
        self.clear_responses()
        for name, response in target._responses.items():
            self.add_response(response.text, name=name, scope=response.scope)


class HasVarTreeResponses(HasResponses):

    def add_response(self, expr, name=None, scope=None):
        super(HasVarTreeResponses, self).add_response(expr, name, scope)

        name = expr if name is None else name
        value = self._responses[name].evaluate()

        obj = self._parent
        names = ['case_outputs'] + expr.split('.')
        for name in names[:-1]:
            if obj.get_trait(name):
                val = obj.get(name)
            else:
                val = VariableTree()
                obj.add_trait(name, VarTree(val, iotype='out'))
            obj = val
        name = names[-1]

        if isinstance(value, (float, int, bool, ndarray)):
            obj.add_trait(name, Array(iotype='out'))
        else:
            obj.add_trait(name, List(iotype='out'))

    def init_responses(self, length):
        nans = [float('NaN')] * length
        nones = [None] * length
        for path, expr in self._responses.items():
            if isinstance(expr.evaluate(), (float, int, bool, ndarray)):
                self._parent.set('case_outputs.'+path, array(nans), force=True)
            else:
                self._parent.set('case_outputs.'+path, list(nones), force=True)


