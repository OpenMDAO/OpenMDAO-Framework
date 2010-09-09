""" Metamodel provides basic Meta Modeling capability"""

# pylint: disable-msg=E0611,F0401
from numpy import array
from enthought.traits.api import Instance, ListStr, Event
from enthought.traits.trait_base import not_none

from openmdao.main.api import Component, Case
from openmdao.main.interfaces import IComponent, ISurrogate, ICaseRecorder
from openmdao.main.uncertain_distributions import UncertainDistribution, \
                                                  NormalDistribution

from openmdao.main.interfaces import obj_has_interface

class MetaModel(Component):
    """ A component that provides general Meta Modeling capability.
    
    See Appendix B for additional information on the :ref:`MetaModel` component."""
    
    # pylint: disable-msg=E1101
    model = Instance(Component, allow_none=True,
                     desc='Socket for the Component or Assembly being '
                          'encapsulated.')
    includes = ListStr(iotype='in', 
                           desc='A list of names of variables to be included '
                                'in the public interface.')
    excludes = ListStr(iotype='in',
                           desc='A list of names of variables to be excluded '
                                'from the public interface.')
    
    surrogate = Instance(ISurrogate, allow_none=True,
                         desc='An ISurrogate instance that is used as a '
                              'template for each output surrogate.')
    
    recorder = Instance(ICaseRecorder,
                        desc = 'Records training cases')

    # when fired, the next execution will train the metamodel
    train_next = Event()
    
    def __init__(self, *args, **kwargs):
        super(MetaModel, self).__init__(*args, **kwargs)
        self._current_model_traitnames = set()
        self._surrogate_info = {}
        self._surrogate_input_names = []
        self._training_input_history = []
        self._train = False
        self._new_train_data = False
        
        # the following line will work for classes that inherit from MetaModel
        # as long as they declare their traits in the class body and not in
        # the __init__ function.  If they need to dynamically create traits
        # during initialization they'll have to provide the value of 
        # _mm_class_traitnames
        self._mm_class_traitnames = set(self.traits(iotype=not_none).keys())

    def _train_next_fired(self):
        self._train = True
        self._new_train_data = True

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise, 
        predict outputs.
        """
        if self._train:
            if self.model:
                try:
                    inputs = self.update_model_inputs()
                    self._training_input_history.append(inputs)
                    #print '%s training with inputs: %s' % (self.get_pathname(), inputs)
                    self.model.run(force=True)
                    self.update_outputs_from_model()
                    case_outputs = []
                    for name, tup in self._surrogate_info.items():
                        surrogate, output_history = tup
                        case_outputs.append(('.'.join([self.name,name]), None, output_history[-1]))
                    # save the case, making sure to add out name to the local input name since
                    # this Case is scoped to our parent Assembly
                    case_inputs = [('.'.join([self.name,name]),None,val) for name,val in zip(self._surrogate_input_names, inputs)]
                    self.recorder.record(Case(inputs=case_inputs, outputs=case_outputs))
                except Exception as err:
                    self.raise_exception("training failed: %s" % str(err), type(err))
            else:
                self.raise_exception("MetaModel object must have a model!",
                                     RuntimeError)
            self._train = False
        else:
            #print '%s predicting' % self.get_pathname()
            if self._new_train_data: 
                for name,tup in self._surrogate_info.items(): 
                    surrogate, output_history = tup
                    surrogate.train(self._training_input_history, output_history) 
                self._new_train_data = False
                
            input_values = array([getattr(self, name) for name in self._surrogate_input_names])
            for name, tup in self._surrogate_info.items():
                surrogate = tup[0]
                predicted = surrogate.predict(input_values)
                # copy output to boudary
                setattr(self, name, predicted)
            
            
    def invalidate_deps(self, compname=None, varnames=None, notify_parent=False):
        if compname:  # we were called from our model, which expects to be in an Assembly
            return
        super(MetaModel, self).invalidate_deps(varnames=varnames, notify_parent=notify_parent)
        
    def exec_counts(self, compnames):
        # we force the run on our model, so it doesn't matter what we tell it the exec counts are
        return [0 for n in compnames]
    
    def _model_changed(self, oldmodel, newmodel):
        self.update_model(oldmodel, newmodel)
            
    def update_model(self, oldmodel, newmodel):
        """called whenever the model variable is set."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested MMs?  Is this a problem?
        # TODO: check for name collisions between MetaModel class traits and traits from model
        if newmodel is not None and not obj_has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel).__name__,
                                 TypeError)

        if not self.surrogate:
            self.raise_exception("surrogate must be set before the model or any includes/excludes of variables", RuntimeError)

        new_model_traitnames = set()
        self._surrogate_input_names = []
        self._taining_input_history = []
        self._surrogate_info = {}
        
        # remove traits promoted from the old model
        for name in self._current_model_traitnames:
            if self.parent:
                self.parent.disconnect('.'.join([self.name,name]))
            self.remove_trait(name)
            
        if newmodel:
            # query for inputs
            traitdict = newmodel._alltraits(iotype='in')
            for name,trait in traitdict.items():
                if self._eligible(name):
                    self._surrogate_input_names.append(name)
                self.add_trait(name, trait.trait_type)
                new_model_traitnames.add(name)
                setattr(self, name, getattr(newmodel, name))
                
            # now outputs
            traitdict = newmodel._alltraits(iotype='out')
            for name,trait in traitdict.items():
                if self._eligible(name):
                    self.add_trait(name, 
                                   Instance(UncertainDistribution, iotype='out', desc=trait.desc))
                    self._surrogate_info[name] = (self.surrogate.__class__(), []) # (surrogate,output_history)
                    new_model_traitnames.add(name)
                    setattr(self, name, NormalDistribution(getattr(newmodel, name)))
                    
            newmodel.parent = self
            newmodel.name = 'model'
        
        self._current_model_traitnames = new_model_traitnames

    def update_inputs(self, compname, varnames):
        if compname != 'model':
            self.raise_exception("cannot update inputs for child named '%s'" % compname)
        self.model.set_valids(varnames, True)
    
    def update_model_inputs(self):
        """Copy the values of the MetaModel's inputs into the inputs of the model.
        Returns the values of the inputs.
        """
        input_values = []
        for name in self._surrogate_input_names:
            inp = getattr(self, name)
            input_values.append(inp)
            setattr(self.model, name, inp)
        return input_values

    def update_outputs_from_model(self):
        """Copy output values from the model into the MetaModel's outputs, and
        if training, save the output associated with surrogate.
        """
        for name in self.list_outputs_from_model():
            out = getattr(self.model, name)
            setattr(self, name, self._surrogate_info[name][0].get_uncertain_value(out))
            if self._train:
                self._surrogate_info[name][1].append(out) # save to training output history

    def list_inputs_to_model(self):
        """Return the list of names of public inputs that correspond 
        to model inputs.
        """
        return self._surrogate_input_names

    def list_outputs_from_model(self):
        """Return the list of names of public outputs that correspond
        to model outputs.
        """
        return list(set(self.list_outputs())-self._mm_class_traitnames)

    def _includes_changed(self, old, new):
        if self.excludes and new is not None:
            self.__dict__['includes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self.update_model(self.model, self.model)
    
    def _excludes_changed(self, old, new):
        if self.includes and new is not None:
            self.__dict__['excludes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self.update_model(self.model, self.model)

    def _eligible(self, name):
        """Return True if the named trait is not excluded from the public interface based
        on the includes and excludes lists.
        """
        if name in self._mm_class_traitnames:
            return False
        if self.includes and name not in self.includes:
            return False
        elif self.excludes and name in self.excludes:
            return False
        return True
    
