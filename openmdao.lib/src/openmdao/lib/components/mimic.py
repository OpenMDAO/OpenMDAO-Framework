
from enthought.traits.api import Instance, ListStr
from enthought.traits.trait_base import not_none

from openmdao.main.api import Component
from openmdao.main.interfaces import IComponent, obj_has_interface

_mimic_class_traitnames = set(['includes', 'excludes', 'model'])

class Mimic(Component):
    """
    A Component that encapsulates another Component and mimics its
    input/output interface.

    """
    
    model = Instance(Component, allow_none=True,
                     desc='Socket for the Component or Assembly being encapsulated')
    includes = ListStr(iotype='in', 
                           desc='A list of names of variables to be included in the public interface.')
    excludes = ListStr(iotype='in',
                           desc='A list of names of variables to be excluded from the public interface.')

    def __init__(self, *args, **kwargs):
        super(Mimic, self).__init__(*args, **kwargs)
        self._current_model_traitnames = set()
        
        # the following line will work for classes that inherit from Mimic
        # as long as they declare their traits in the class body and not in
        # the __init__ function.  If they need to dynamically create traits
        # during initialization they'll have to provide the value of 
        # _mimic_class_traitnames
        self._mimic_class_traitnames = set(self.traits(iotype=not_none).keys())

    def execute(self):
        """Default execution behavior is to set all inputs in the model, execute it, and
        update all outputs with the model's outputs.  Other classes can override this
        function to introduce other behaviors.
        """
        if self.model:
            self.update_model_inputs()
            self.model.run()
            self.update_outputs_from_model()

    def update_model_inputs(self):
        """Copy the values of the Mimic's inputs into the inputs of the model."""
        if self.model:
            for name in self.list_inputs_to_model():
                setattr(self.model, name, getattr(self, name))

    def update_outputs_from_model(self):
        """Copy output values from the model into the Mimic's outputs."""
        if self.model:
            for name in self.list_outputs_from_model():
                setattr(self, name, getattr(self.model, name))

    def list_inputs_to_model(self):
        """Return the list of names of public inputs that correspond 
        to model inputs.
        """
        return list(set(self.list_inputs())-self._mimic_class_traitnames)

    def list_outputs_from_model(self):
        """Return the list of names of public outputs that correspond
        to model outputs.
        """
        return list(set(self.list_outputs())-self._mimic_class_traitnames)

    def _model_changed(self, oldmodel, newmodel):
        self.update_model(oldmodel, newmodel)
        
    def update_model(self, oldmodel, newmodel):
        """called whenever the model attribute is set."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested Mimics?  Is this a problem?
        # TODO: check for name collisions between Mimic class traits and traits from model
        if newmodel is not None and not obj_has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel),
                                 TypeError)

        if oldmodel is not None: # we have to clean up old traits
            for name in self._current_model_traitnames:
                if self.parent:
                    self.parent.disconnect('.'.join([self.name,name]))
                self.remove_trait(name)
        
        self._current_model_traitnames = set()
        
        if newmodel:
            traitdict = newmodel._alltraits(iotype=not_none)
            for name,trait in traitdict.items():
                if self._eligible(name):
                    self.add_trait(name, trait.trait_type)
                    self._current_model_traitnames.add(name)
                    setattr(self, name, getattr(newmodel, name))
    
    def _includes_changed(self, old, new):
        if self.excludes and new is not None:
            self.__dict__['includes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self._model_changed(self.model, self.model)
    
    def _excludes_changed(self, old, new):
        if self.includes and new is not None:
            self.__dict__['excludes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self._model_changed(self.model, self.model)
    
    def _eligible(self, name):
        """Return True if the named model trait should become a trait in the Mimic."""
        # TODO: add wildcarding to includes and excludes
        if name in _mimic_class_traitnames:
            return False
        if self.includes and name not in self.includes:
            return False
        elif self.excludes and name in self.excludes:
            return False
        return True

