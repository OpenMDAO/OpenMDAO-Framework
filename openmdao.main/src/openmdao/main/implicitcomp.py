""" Class definition for an Implicit Component. """

from scipy.optimize import fsolve, check_grad
from scipy.sparse.linalg import gmres, LinearOperator
import numpy as np

from openmdao.main.array_helpers import flattened_value
from openmdao.main.component import Component
from openmdao.main.datatypes.api import Bool
from openmdao.main.derivatives import applyJ
from openmdao.main.interfaces import IImplicitComponent, IVariableTree, implements
from openmdao.main.mp_support import has_interface
from openmdao.main.rbac import rbac

class ImplicitComponent(Component):
    implements(IImplicitComponent)

    eval_only = Bool(False, iotype='in', framework_var=True,
                     desc='Set to True to have this comp perform a single '
                           'evaluate when execute() is called.')

    def __init__(self):
        super(ImplicitComponent, self).__init__()
        self._state_names = None
        self._resid_names = None

        # register callbacks for all of our 'state' traits
        for name, trait in self.class_traits().items():
            if trait.iotype == 'state':
                self._set_input_callback(name)

    @rbac(('owner', 'user'))
    def list_states(self):
        """Return a list of names of state variables in alphabetical order.
        This specifies the order the state vector, so if you use a different
        ordering internally, override this function to return the states in
        the desired order.
        """

        if self._state_names is None:
            self._state_names = sorted([k for k, v in self.items(iotype='state')])
        return self._state_names

    @rbac(('owner', 'user'))
    def list_residuals(self):
        """Return a list of names of residual variables in alphabetical
        order. This specifies the order the residual vector, so if you use
        a different order internally, override this function to return the
        residuals in the desired order.
        """

        if self._resid_names is None:
            self._resid_names = sorted([k for k, v in self.items(iotype='residual')])
        return self._resid_names

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Reset internally cached values."""

        super(ImplicitComponent, self).config_changed(update_parent)
        self._state_names = None
        self._resid_names = None

    def check_config(self):
        """
        Override this function to perform configuration checks specific to your class.
        Bad configurations should raise an exception.
        """
        pass  # TODO: add check that total width of states == total width of residuals

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
                    raise RuntimeError("VariableTree states are not supported yet.")
                else:
                    if len(newval) != 1:
                        self.raise_exception("Trying to set a scalar value '%s' with a ")
                    setattr(self, name, newval[0])
            else:
                setattr(self, name, flatval)

        if unused != 0:
            self.raise_exception("State vector size does not match flattened size of state variables.",
                                 ValueError)

    def solve(self):
        """Calculates the states that satisfy residuals using scipy.fsolve.
        You can override this function to provide your own internal solve."""

        x0 = self.get_state()

        # If our comp doesn't have derivatives, let the
        # internal solver calculate them however it does
        fprime = None
        if hasattr(self, 'provideJ'):
            fprime = self._jacobian_callback

        fsolve(self._solve_callback, x0, fprime=fprime)

    def _solve_callback(self, X):
        """This function is passed to the internal solver to set a new state,
        evaluate the residuals, and return them."""

        self.set_state(X)
        self.evaluate()

        return self.get_residuals()

    def _jacobian_callback(self, X):
        """This function is passed to the internal solver to return the
        jacobian of the states with respect to the residuals."""

        n_edge = 2*len(X)
        n_res = n_edge/2

        A = LinearOperator((n_edge, n_edge),
                           matvec=self._matvecFWD,
                           dtype=float)
        J = np.zeros((n_res, n_res))
        self._cache_J = self.provideJ()

        for irhs in np.arange(n_res):

            RHS = np.zeros((n_edge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=1.0e-9,
                             maxiter=100)

            J[:, irhs] = dx[n_res:]

        return J

    def calc_derivatives(self, first=False, second=False, savebase=False,
                         required_inputs=None, required_outputs=None):
        J = super(ImplicitComponent, self).calc_derivatives(first, second, savebase,
                                                            required_inputs, required_outputs)
        self._cache_J = J
        return J

    def _matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        state-to-residual Jacobian with an incoming vector arg.'''

        result = np.zeros(len(arg))

        comp_inputs = self.list_states()
        comp_outputs = self.list_residuals()
        inputs = {}
        outputs = {}

        idx = 0

        for varname in comp_inputs:
            val = getattr(self, varname)
            flatval = flattened_value(varname, val)
            size = len(flatval)

            i1, i2 = idx, idx + size
            inputs[varname] = arg[i1:i2].copy()

            idx += size

        for varname in comp_outputs:
            val = getattr(self, varname)
            flatval = flattened_value(varname, val)
            size = len(flatval)

            i1, i2 = idx, idx + size
            inputs[varname] = arg[i1:i2].copy()
            outputs[varname] = arg[i1:i2].copy()

            idx += size

        applyJ(self, inputs, outputs, [], J=self._cache_J)
        #print inputs, outputs

        idx = 0

        # Each state input adds an equation
        for varname in comp_inputs:
            val = getattr(self, varname)
            flatval = flattened_value(varname, val)
            size = len(flatval)

            i1, i2 = idx, idx + size
            result[i1:i2] = arg[i1:i2].copy()

            idx += size

        for varname in comp_outputs:
            val = getattr(self, varname)
            flatval = flattened_value(varname, val)
            size = len(flatval)

            i1, i2 = idx, idx + size
            result[i1:i2] = outputs[varname]

            idx += size

        #print arg, result
        return result
