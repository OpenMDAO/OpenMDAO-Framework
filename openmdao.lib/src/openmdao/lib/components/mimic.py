
from enthought.traits.api import Instance, ListStr

from openmdao.main.api import Component

class Mimic(Component):
    """
    A Component that encapsulates another Component and mimics its
    input/output interface.

    """
    
    model = Instance(Component, allow_none=True)
    mimic_includes = ListStr(iotype='in', 
                           desc='A list of names of variables to be included in the public interface.'
                                 ' The list may contain wildcards.')
    mimic_excludes = ListStr(iotype='in',
                           desc='A list of names of variables to be excluded from the public interface.'
                                 ' The list may contain wildcards.')

    def execute(self):
        """Default execution behavior is to set all inputs in the model, execute it, and
        update all outputs with the model's outputs.  Other classes can override this
        function to introduce other behaviors.
        """
        pass

    def _model_changed(self, old, new):
        pass
    
    def _include_vars_changed(self, old, new):
        pass
    
    def _exclude_vars_changed(self, old, new):
        pass
    