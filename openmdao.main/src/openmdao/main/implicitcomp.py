""" Class definition for an Implicit Component. """

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

        
