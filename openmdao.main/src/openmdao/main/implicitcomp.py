""" Class definition for an Implicit Component. """

from scipy.optimize import fsolve
import numpy as np

from openmdao.main.datatypes.api import Bool
from openmdao.main.component import Component 
from openmdao.main.interfaces import IImplicitComponent, implements
from openmdao.main.rbac import rbac

class ImplicitComponent(Component):
    implements(IImplicitComponent)
    
    solve_internally = Bool(False, iotype='in', 
                            desc='Set to True to let this comp solve itself.')
    
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
        """Return a list of names of state variables."""

        if self._state_names is None:
            self._state_names = sorted([k for k, v in self.items(iotype='state')])
        return self._state_names

    @rbac(('owner', 'user'))
    def list_residuals(self):
        """Return a list of names of residual variables."""

        if self._resid_names is None:
            self._resid_names = sorted([k for k, v in self.items(iotype='residual')])
        return self._resid_names

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Reset internally cached values."""

        super(ImplicitComponent, self).config_changed(update_parent)
        self._state_names = None
        self._resid_names = None

    def execute(self):
        """ User should not override execute for an implicit component. """
        
        if self.solve_internally == True:
            self.solve()
        else:
            self.evaluate()
        
    def solve(self):
        """Calculates the states that satisfy residuals using scipy.fsolve.
        You can override this function to provide your own internal solve."""
        
        x0 = [self.x, self.y, self.z]
        sol = fsolve(self._function_callback, x0, fprime=self._jac)


    #note, these methods should be implemented in the ImplicitComp baseclass in a more general manner
    def _function_callback(self, X): 
        """This function is passed to the internal solver to set a new state, 
        evaluate the residuals, and return them.""" 

        self.x = X[0]
        self.y = X[1]
        self.z = X[2]

        self.evaluate()

        return np.array([self.r0, self.r1, self.r2])

