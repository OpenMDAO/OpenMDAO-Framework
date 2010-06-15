
from enthought.traits.api import Instance, ListStr
from enthought.traits.trait_base import not_none

from openmdao.main.api import Component
from openmdao.main.interfaces import IComponent

_mimic_class_traits = set(['mimic_includes', 'mimic_excludes', 'model'])

class Mimic(Component):
    """
    A Component that encapsulates another Component and mimics its
    input/output interface.

    """
    
    model = Instance(Component, allow_none=True,
                     desc='Socket for the Component or Assembly being encapsulated')
    mimic_includes = ListStr(iotype='in', 
                           desc='A list of names of variables to be included in the public interface.'
                                 ' The list may contain wildcards.')
    mimic_excludes = ListStr(iotype='in',
                           desc='A list of names of variables to be excluded from the public interface.'
                                 ' The list may contain wildcards.')

    def __init__(self, *args, **kwargs):
        super(Mimic, self).__init__(*args, **kwargs)
        self._current_model_traits = set()

    def execute(self):
        """Default execution behavior is to set all inputs in the model, execute it, and
        update all outputs with the model's outputs.  Other classes can override this
        function to introduce other behaviors.
        """
        pass

    def _model_changed(self, oldmodel, newmodel):
        if newmodel is not None and not self.obj_has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel),
                                 TypeError)

        if oldmodel is not None: # we have to clean up old traits
            for name in self._current_model_traits:
                self.remove_trait(name)
        
        self._current_model_traits = set()
        
        traitdict = newmodel._traits_meta_filter(iotype=not_none)
        
        for name,trait in traitdict.items():
            if self._eligible(name):
                self.add_trait(name, trait.trait_type)
                self._current_model_traits.add(name)
                setattr(self, name, getattr(newmodel, name))
    
    def _include_vars_changed(self, old, new):
        if self.mimic_excludes and new is not None:
            self.raise_exception("mimic_includes and mimic_excludes are mutually exclusive")
        self.__dict__['mimic_includes'] = old
    
    def _exclude_vars_changed(self, old, new):
        if self.mimic_includes and new is not None:
            self.raise_exception("mimic_includes and mimic_excludes are mutually exclusive")
        self.__dict__['mimic_excludes'] = old
    
    def _eligible(self, name):
        if name in _mimic_class_traits:
            return False
        if self.mimic_includes and name not in self.mimic_includes:
            return False
        elif self.mimic_excludes and name in self.mimic_excludes:
            return False
        return True