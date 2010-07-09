
from enthought.traits.api import Instance, ListStr
from enthought.traits.trait_base import not_none

from openmdao.main.api import Component
from openmdao.main.interfaces import IComponent, obj_has_interface
from openmdao.lib.components.mimic import Mimic

class SurrogateModel(Mimic):
    
    def __init__(self, *args, **kwargs):
        super(SurrogateModel, self).__init__(*args, **kwargs)

    def execute(self):
        """Default execution behavior is to set all inputs in the model, execute it, and
        update all outputs with the model's outputs.  Other classes can override this
        function to introduce other behaviors.
        """
        if self.model:
            self.update_model_inputs()
            self.model.run()
            self.update_outputs_from_model()

    def _model_changed(self, oldmodel, newmodel):
        """called whenever the model attribute is set."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested Mimics?  Is this a problem?
        # TODO: check for name collisions between Mimic class traits and traits from model
        if newmodel is not None and not obj_has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel),
                                 TypeError)

        if oldmodel is not None: # we have to clean up old traits
            for name in self._current_model_traits:
                if self.parent:
                    self.parent.disconnect('.'.join([self.name,name]))
                self.remove_trait(name)
        
        self._current_model_traits = set()
        
        if newmodel:
            traitdict = newmodel._traits_meta_filter(iotype=not_none)
            
            for name,trait in traitdict.items():
                if self._eligible(name):
                    self.add_trait(name, trait.trait_type)
                    self._current_model_traits.add(name)
                    setattr(self, name, getattr(newmodel, name))
