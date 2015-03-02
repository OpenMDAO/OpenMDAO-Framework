""" A workflow that allows the user to explicitly specify the execution
order.
"""

# pylint: disable=E0611,F0401

from types import NoneType

from openmdao.main.depgraph import find_related_pseudos
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.workflow import Workflow
from openmdao.util.decorators import method_accepts
from openmdao.util.debug import strict_chk_config

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
        self._iternames = None
        self._initnames = None

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter([getattr(self.scope, n) for n in self.parent._ordering])

    def __len__(self):
        return len(self.parent._iter_set)

    def __contains__(self, comp):
        return comp in self.parent._iter_set

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names

    def __ne__(self, other):
        return not self.__eq__(other)

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(SequentialWorkflow, self).config_changed()

        self._names = None
        self._iternames = None
        self._initnames = None
        self._ordering = None

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
        try:
            self._explicit_names.remove(compname)
        except ValueError:
            pass
        self.config_changed()

    def clear(self):
        """Remove all components from this workflow."""
        self._explicit_names = []
        self.config_changed()

    def mimic(self, src):
        '''Mimic capability'''
        self.clear()
        par = self.parent.parent
        if par is not None:
            self._explicit_names = [n for n in src._explicit_names
                                            if hasattr(par, n)]
        else:
            self._explicit_names = src._explicit_names[:]
