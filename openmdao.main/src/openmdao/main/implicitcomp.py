
from openmdao.main.component import Component 
from openmdao.main.interfaces import IImplicitComponent, implements
from openmdao.main.rbac import rbac

class ImplicitComponent(Component):
    implements(IImplicitComponent)

    def __init__(self):
        super(ImplicitComponent, self).__init__()
        self._state_names = None
        self._resid_names = None

    @rbac(('owner', 'user'))
    def list_states(self):
        """Return a list of names of state variables."""

        if self._state_names is None:
            self._state_names = [k for k, v in self.items(iotype='state')]
        return self._state_names

    @rbac(('owner', 'user'))
    def list_residuals(self):
        """Return a list of names of residual variables."""

        if self._resid_names is None:
            self._resid_names = [k for k, v in self.items(iotype='residual')]
        return self._resid_names

    @rbac(('owner', 'user'))
    def config_changed(self, update_parent=True):
        """Reset internally cached values."""

        super(ImplicitComponent, self).config_changed(update_parent)
        self._state_names = None
        self._resid_names = None
