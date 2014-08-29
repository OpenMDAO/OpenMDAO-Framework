""" A workflow that allows the user to explicitly specify the execution
order. This workflow serves as the immediate base class for the two most
important workflows: Dataflow and CyclicWorkflow."""

from types import NoneType

from openmdao.main.vartree import VariableTree

from openmdao.main.workflow import Workflow
from openmdao.main.depgraph import find_related_pseudos
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.util.decorators import method_accepts
from openmdao.util.debug import strict_chk_config

from numpy import ndarray

_missing = object()

__all__ = ['SequentialWorkflow']


class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""

    def __init__(self, parent=None, members=None):
        """ Create an empty flow. """
        self._explicit_names = []  # names the user adds
        self._names = None   # names the user adds plus names required
                             # for params, objectives, and constraints
        super(SequentialWorkflow, self).__init__(parent, members)

        # Bookkeeping
        self._edges = None
        self._comp_edges = None
        self._derivative_graph = None
        self._J_cache = {}
        self._bounds_cache = {}
        self._shape_cache = {}
        self._width_cache = {}
        self._iternames = None
        self._initnames = None

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components(full=True))

    def __len__(self):
        return len(self.get_names(full=True))

    def __contains__(self, comp):
        return comp in self.get_names(full=True)

    def index(self, comp):
        """Return index number for a component in this workflow."""
        return self.get_names().index(comp)

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names

    def __ne__(self, other):
        return not self.__eq__(other)

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(SequentialWorkflow, self).config_changed()

        self._edges = None
        self._comp_edges = None
        self._derivative_graph = None
        self.res = None
        self._names = None
        self._J_cache = {}
        self._bounds_cache = {}
        self._shape_cache = {}
        self._width_cache = {}
        self._iternames = None
        self._initnames = None

    def check_config(self, strict=False):
        super(SequentialWorkflow, self).check_config(strict=strict)
        self.get_names()
        if self._initnames and self._iternames:
            msg = "The following components will execute EVERY iteration " \
                  "of this workflow (unnecessarily): %s" % list(self._initnames)
            if strict_chk_config(strict):
                self.parent.raise_exception(msg, RuntimeError)
            else:
                self.parent._logger.warning(msg)

    # def sever_edges(self, edges):
    #     """Temporarily remove the specified edges but save
    #     them and their metadata for later restoration.
    #     """
    #     if edges:
    #         params = self.parent.get_parameters()
    #         non_param_edges = [(src, targ) for (src, targ) in edges
    #                                        if targ not in params]
    #         self.scope._depgraph.sever_edges(non_param_edges)

    # def unsever_edges(self):
    #     self.scope._depgraph.unsever_edges(self.parent.get_expr_scope())

    def get_names(self, full=False):
        """Return a list of component names in this workflow.
        If full is True, include hidden pseudo-components in the list.
        """
        if self._names is None:
            comps = [getattr(self.scope, n) for n in self._explicit_names]
            drivers = [c for c in comps if has_interface(c, IDriver)]
            self._names = self._explicit_names[:]
            self._iternames = self.parent._get_required_compnames()

            if len(drivers) == len(comps):  # all comps are drivers or explicit set is empty
                iterset = set()
                for driver in drivers:
                    iterset.update([c.name for c in driver.iteration_set()])
                added = set([n for n in self._iternames if not n.startswith('_pseudo_')
                                 and n not in iterset]) - set(self._names)
                self._names.extend(added)

            self._fullnames = self._names[:]
            fullset = set(self.parent.list_pseudocomps())
            fullset.update(find_related_pseudos(self.scope._depgraph,
                                                self._names))
            self._fullnames.extend(fullset - set(self._names))

            self._initnames = set(self._fullnames) - self._iternames

            # drivers are always manually placed in the workflow, so
            # assume that they're supposed to be there and don't
            # warn the user
            self._initnames -= set([d.name for d in drivers])

        if full:
            return self._fullnames[:]
        else:
            return self._names[:]

    @method_accepts(TypeError,
                    compnames=(str, list, tuple),
                    index=(int, NoneType),
                    check=bool)
    def add(self, compnames, index=None, check=False):
        """
        add(self, compnames, index=None, check=False)
        Add new component(s) to the end of the workflow by name.
        """

        if isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            nodes = compnames

        try:
            iter(nodes)
        except TypeError:
            raise TypeError("Components must be added by name to a workflow.")

        # workflow deriv graph, etc. must be recalculated
        self.config_changed()

        for node in nodes:
            if isinstance(node, basestring):

                if check:
                    # check whether each node is valid and if not then
                    # construct a useful error message.
                    parent = self.parent
                    name = parent.parent.name
                    if not name:
                        name = "the top assembly."

                    # Components in subassys are never allowed.
                    if '.' in node:
                        msg = "Component '%s' is not" % node + \
                              " in the scope of %s" % name
                        raise AttributeError(msg)

                    # Does the component really exist?
                    try:
                        target = parent.parent.get(node)
                    except AttributeError:
                        msg = "Component '%s'" % node + \
                              " does not exist in %s" % name
                        raise AttributeError(msg)

                    # Don't add yourself to your own workflow
                    if target == parent:
                        msg = "You cannot add a driver to its own workflow"
                        raise AttributeError(msg)

                    # Check for circular dependency in driver workflow
                    if hasattr(target, 'iteration_set'):
                        iterset = target.iteration_set()
                        if parent in iterset:
                            msg = "Driver recursion loop detected"
                            raise AttributeError(msg)

                if index is None:
                    self._explicit_names.append(node)
                else:
                    self._explicit_names.insert(index, node)
                    index += 1
            else:
                msg = "Components must be added by name to a workflow."
                raise TypeError(msg)

    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            msg = "Components must be removed by name from a workflow."
            raise TypeError(msg)
        allnames = self.get_names(full=True)
        try:
            self._explicit_names.remove(compname)
        except ValueError:
            pass
        if compname in allnames:
            self.config_changed()

    def clear(self):
        """Remove all components from this workflow."""
        self._explicit_names = []
        self.config_changed()

    def _update(self, name, vtree, dv, i1=0):
        """ Update VariableTree `name` value `vtree` from `dv`. """
        for key in sorted(vtree.list_vars()):  # Force repeatable order.
            value = getattr(vtree, key)
            if isinstance(value, float):
                setattr(vtree, key, value + float(dv[i1]))
                i1 += 1
            elif isinstance(value, ndarray):
                shape = value.shape
                size = value.size
                i2 = i1 + size
                if len(shape) > 1:
                    value = value.flatten() + dv[i1:i2]
                    value = value.reshape(shape)
                else:
                    value = value + dv[i1:i2]
                setattr(vtree, key, value)
                i1 += size
            elif isinstance(value, VariableTree):
                i1 = self._update('.'.join((name, key)), value, dv, i1)
            else:
                msg = "Variable %s is of type %s." % (name, type(value)) + \
                      " This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)

        return i1

    def mimic(self, src):
        '''Mimic capability'''
        self.clear()
        par = self.parent.parent
        if par is not None:
            self._explicit_names = [n for n in src._explicit_names
                                            if hasattr(par, n)]
        else:
            self._explicit_names = src._explicit_names[:]

