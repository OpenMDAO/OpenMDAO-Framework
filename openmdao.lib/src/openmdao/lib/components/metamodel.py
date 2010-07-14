
from enthought.traits.api import Instance, ListStr, Event
from enthought.traits.trait_base import not_none

from openmdao.main.api import Component
from openmdao.main.interfaces import IComponent, obj_has_interface
from openmdao.lib.components.mimic import Mimic

class MetaModel(Mimic):
    
    train_next = Event()  # when fired, the next execution will train the metamodel
    
    def __init__(self, *args, **kwargs):
        super(MetaModel, self).__init__(*args, **kwargs)
        self._train = False

    def _train_next_fired(self):
        self._train = True

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise
        """
        if self._train:
            if self.model:
                self.update_model_inputs()
                self.model.run()
                self.update_outputs_from_model()
                #save the case
            self._train = False
        else:
            # predict outputs     # how are the surrogates learning? No mention during training
            # copy outputs to boundary
            pass
            
    def update_model(self, oldmodel, newmodel):
        """called whenever the model attribute is set."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested Mimics?  Is this a problem?
        # TODO: check for name collisions between Mimic class traits and traits from model
        if newmodel is not None and not obj_has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel),
                                 TypeError)

        new_model_traitnames = set()
        if newmodel:
            traitdict = newmodel._traits_meta_filter(iotype=not_none)
            
            for name,trait in traitdict.items():
                if self._eligible(name):
                    if name not in self._current_model_traitnames:
                        self.add_trait(name, trait.trait_type)
                    new_model_traitnames.add(name)
                    setattr(self, name, getattr(newmodel, name))

        # remove any traits that aren't found in the new model
        for name in self._current_model_traitnames - new_model_traitnames:
            if self.parent:
                self.parent.disconnect('.'.join([self.name,name]))
            self.remove_trait(name)
        
        self._current_model_traitnames = new_model_traitnames
        

    def train(self):
        pass
