""" Class definition for an Implicit Component. """

import numpy as np

# pylint: disable=E0611,F0401
from openmdao.main.array_helpers import flattened_value
from openmdao.main.component import Component
from openmdao.main.datatypes.api import Bool
from openmdao.main.interfaces import IImplicitComponent, IVariableTree, \
                                     implements
from openmdao.main.mp_support import has_interface
from openmdao.main.rbac import rbac


class ImplicitComponent(Component):
    """This is the base class for a component that represents an implicit
    function
    """

    implements(IImplicitComponent)

    eval_only = Bool(True, iotype='in', framework_var=True,
                     desc='Set to False if you define your own solver. '
                          'Otherwise, OpenMDAO must solve the implicit '
                          'equations for this component.')

    def __init__(self):
        super(ImplicitComponent, self).__init__()
        self._state_names = None
        self._resid_names = None
        self._shape_cache = {}

        # register callbacks for all of our 'state' traits
        for name, trait in self.class_traits().items():
            if trait.iotype == 'state':
                self._set_input_callback(name)

        # This flag is for testing. Set to True to run this as an explicit
        # component so you can test derivatives.
        self._run_explicit = False

    @rbac(('owner', 'user'))
    def list_states(self):
        """Return a list of names of state variables in alphabetical order.
        This specifies the order the state vector, so if you use a different
        ordering internally, override this function to return the states in
        the desired order.
        """

        if self._run_explicit == True:
            return []

        if self._state_names is None:
            self._state_names = \
                sorted([k for k, _ in self.items(iotype='state')])
        return self._state_names

    @rbac(('owner', 'user'))
    def list_residuals(self):
        """Return a list of names of residual variables in alphabetical
        order. This specifies the order the residual vector, so if you use
        a different order internally, override this function to return the
        residuals in the desired order.
        """

        if self._run_explicit == True:
            return []

        if self._resid_names is None:
            self._resid_names = \
                sorted([k for k, _ in self.items(iotype='residual')])
        return self._resid_names

    def evaluate(self):
        """run a single step to calculate the residual
        values for the given state var values.
        This must be overridden in derived classes.
        """
        raise NotImplementedError('%s.evaluate' % self.get_pathname())

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Reset internally cached values."""

        super(ImplicitComponent, self).config_changed(update_parent)
        self._state_names = None
        self._resid_names = None
        self._shape_cache = {}

    def check_config(self, strict=False):
        """
        Override this function to perform configuration checks specific to
        your class. Bad configurations should raise an exception.
        """
        super(ImplicitComponent, self).check_config(strict=strict)

        # TODO: add check that total width of states == total widtJh of
        # residuals

    def execute(self):
        """ Performs either an internal solver or a single evaluation.
        Do not override this function.
        """

        if self.eval_only:
            self.evaluate()
        else:
            self.solve()

    def get_residuals(self):
        """Return a vector of residual values."""

        resids = []
        if self._run_explicit == False:
            for name in self.list_residuals():
                resids.extend(flattened_value(name, getattr(self, name)))
        return np.array(resids)

    def get_state(self):
        """Return the current flattened state vector."""
        states = []
        for name in self.list_states():
            states.extend(flattened_value(name, getattr(self, name)))
        return np.array(states)

    def set_state(self, X):
        """Take the given state vector and set its values into the
        correct state variables.
        """
        unused = len(X)
        idx = 0

        for name in self.list_states():
            val = getattr(self, name)
            flatval = flattened_value(name, val)
            size = len(flatval)
            newval = X[idx:idx+size]
            unused -= size
            idx += size
            try:
                iter(val)
            except TypeError:
                if has_interface(val, IVariableTree):
                    msg = "VariableTree states are not supported yet."
                    raise RuntimeError(msg)
                else:
                    if len(newval) != 1:
                        self.raise_exception("Trying to set a scalar value '%s' with a ")
                    setattr(self, name, newval[0])
            else:
                setattr(self, name, newval.copy())

        if unused != 0:
            msg = "State vector size does not match flattened size of state variables."
            self.raise_exception(msg, ValueError)

