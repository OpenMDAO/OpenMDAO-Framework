""" Metamodel provides basic Meta Modeling capability."""

# pylint: disable-msg=E0611,F0401
from enthought.traits.trait_base import not_none
from enthought.traits.has_traits import _clone_trait

from openmdao.main.api import Component, Case, Slot
from openmdao.lib.datatypes.api import Slot, ListStr, Event, \
     List, Str, Dict
from openmdao.main.interfaces import IComponent, ISurrogate, ICaseRecorder, \
     ICaseIterator
from openmdao.main.uncertain_distributions import UncertainDistribution, \
                                                  NormalDistribution
from openmdao.main.mp_support import has_interface

_missing = object()

class MetaModel(Component):
    
    # pylint: disable-msg=E1101
    model = Slot(IComponent, allow_none=True,
                   desc='Slot for the Component or Assembly being '
                   'encapsulated.')
    includes = ListStr(iotype='in', 
                       desc='A list of names of variables to be included '
                                'in the public interface.')
    excludes = ListStr(iotype='in',
                       desc='A list of names of variables to be excluded '
                       'from the public interface.')
    
    warm_start_data = Slot(ICaseIterator,iotype="in",
                              desc="CaseIterator containing cases to use as "
                              "initial training data. When this is set, all "
                              "previous training data is cleared, and replaced "
                              "with data from this CaseIterator")
    
    surrogate = Dict(key_trait=Str,
                     value_trait=Slot(ISurrogate),
                     allow_none=True,
                     desc='Dictionary that provides a mapping between variables and '
                          'surrogate models for each output. The "default" '
                          'key must be given. It is the default surrogate model for all '
                          'outputs. Any specific surrogate models can be '
                          'specifed by a key with the desired variable name.'
                    )
    surrogate_args = Dict(key_trait=Str,
                          allow_none=True,
                          desc='Dictionary that provides mapping between variables and '
                          'arguments that should be passed to the surrogate model. Keys should '
                          'match those in the surrogate dictionary. Values can be a list of ordered '
                          'arguments, a dictionary of named arguments, or a two-tuple of a list and a dictionary.')
    
    recorder = Slot(ICaseRecorder,
                        desc = 'Records training cases')

    # when fired, the next execution will train the metamodel
    train_next = Event()
    #when fired, the next execution will reset all training data
    reset_training_data = Event()
    
    def __init__(self, *args, **kwargs):
        super(MetaModel, self).__init__(*args, **kwargs)
        self._current_model_traitnames = set()
        self._surrogate_info = {}
        self._surrogate_input_names = []
        self._training_input_history = []
        self._const_inputs = {} # dict of constant training inputs indices and their values
        self._train = False
        self._new_train_data = False
        
        # the following line will work for classes that inherit from MetaModel
        # as long as they declare their traits in the class body and not in
        # the __init__ function.  If they need to create traits dynamically
        # during initialization they'll have to provide the value of 
        # _mm_class_traitnames
        self._mm_class_traitnames = set(self.traits(iotype=not_none).keys())

    def _train_next_fired(self):
        self._train = True
        self._new_train_data = True
    
    def _reset_training_data_fired(self):
        self._training_input_history = []
        self._const_inputs = {}
        self.update_model(self.model, self.model)
        
    def _warm_start_data_changed(self, oldval, newval): 
        self.reset_training_data = True
        
        #build list of inputs         
        for case in newval:
            if self.recorder: 
                self.recorder.record(case)
            inputs = []
            for inp_name in self._surrogate_input_names:
                var_name = '.'.join([self.name, inp_name])
                inp_val = case[var_name]
                if inp_val is not None: 
                    inputs.append(inp_val)
                else: 
                    self.raise_exception('The variable "%s" was not '
                                         'found as an input in one of the cases provided '
                                         'for warm_start_data.' % var_name, ValueError)
            #print "inputs", inputs
            self._training_input_history.append(inputs)                  
            
            for output_name in self.list_outputs_from_model():
                #grab value from case data
                var_name = '.'.join([self.name, output_name])
                try:
                    val = case.get_output(var_name)
                except KeyError:
                    self.raise_exception('The output "%s" was not found '
                                         'in one of the cases provided for '
                                         'warm_start_data' % var_name, ValueError) 
                else: # save to training output history   
                    self._surrogate_info[output_name][1].append(val)

        self._new_train_data = True        
        
    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise, 
        predict outputs.
        """
        
        if self._train:
            if self.model:
                try:
                    inputs = self.update_model_inputs()
                    
                    #print '%s training with inputs: %s' % (self.get_pathname(), inputs)
                    self.model.run(force=True)

                except Exception as err:
                    #self.raise_exception("training failed: %s" % str(err), type(err))
                    pass
                else: #if no exceptions are generated, save the data
                    self._training_input_history.append(inputs)
                    self.update_outputs_from_model()
                    case_outputs = []
                    
                    for name, tup in self._surrogate_info.items():
                        surrogate, output_history = tup
                        case_outputs.append(('.'.join([self.name,name]), output_history[-1]))
                    # save the case, making sure to add out name to the local input name since
                    # this Case is scoped to our parent Assembly
                    case_inputs = [('.'.join([self.name,name]),val) for name,val in zip(self._surrogate_input_names, inputs)]
                    if self.recorder: 
                        self.recorder.record(Case(inputs=case_inputs, outputs=case_outputs))
                    
            else:
                self.raise_exception("MetaModel object must have a model!",
                                     RuntimeError)
            self._train = False
        else:
            #print '%s predicting' % self.get_pathname()
            if self._new_train_data: 
                if len(self._training_input_history) < 2:
                    self.raise_exception("ERROR: need at least 2 training points!", 
                                         RuntimeError)
                    
                # figure out if we have any constant training inputs
                tcases = self._training_input_history
                in_hist = tcases[0][:]
                # start off assuming every input is constant
                idxlist = range(len(in_hist))
                self._const_inputs = dict(zip(idxlist, in_hist))
                for i in idxlist:
                    val = in_hist[i]
                    for case in range(1, len(tcases)):
                        if val != tcases[case][i]:
                            del self._const_inputs[i]
                            break
                  
                if len(self._const_inputs) == len(in_hist):
                    self.raise_exception("ERROR: all training inputs are constant.")
                elif len(self._const_inputs) > 0:
                    # some inputs are constant, so we have to remove them from the training set
                    training_input_history = []
                    for inputs in self._training_input_history:
                        training_input_history.append([val for i,val in enumerate(inputs) 
                                                       if i not in self._const_inputs])
                else:
                    training_input_history = self._training_input_history
                for name,tup in self._surrogate_info.items(): 
                    surrogate, output_history = tup  
                    surrogate.train(training_input_history, output_history)
                    
                #self._training_input_history = []
                self._new_train_data = False
                
            inputs = []
            for i,name in enumerate(self._surrogate_input_names):
                val = getattr(self, name)
                cval = self._const_inputs.get(i, _missing)
                if cval is _missing:
                    inputs.append(val)
                elif val != cval:
                    self.raise_exception("ERROR: training input '%s' was a constant value of (%s) but the value has changed to (%s)." %
                                         (name, cval, val), ValueError)
            for name, tup in self._surrogate_info.items():
                surrogate = tup[0]
                # copy output to boudary
                setattr(self, name, surrogate.predict(inputs))
            
    def _post_run (self):
        self._train = False
        super(MetaModel, self)._post_run()

    def invalidate_deps(self, compname=None, varnames=None, force=False):
        if compname:  # we were called from our model, which expects to be in an Assembly
            return
        super(MetaModel, self).invalidate_deps(varnames=varnames)
        
    def child_invalidated(self, childname, outs=None, force=False):
        pass

    def exec_counts(self, compnames):
        # we force the run on our model, so it doesn't matter what we tell it the exec counts are
        return [0 for n in compnames]
    
    def _model_changed(self, oldmodel, newmodel):
        self.update_model(oldmodel, newmodel)
        
    def update_model(self, oldmodel, newmodel):
        """called whenever the model variable is set or when includes/excludes change."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested MMs?  Is this a problem?
        # TODO: check for name collisions between MetaModel class traits and traits from model
        if newmodel is not None and not has_interface(newmodel, IComponent):
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
                if name not in self._mm_class_traitnames:
                    self.add_trait(name, _clone_trait(trait))
                    new_model_traitnames.add(name)
                    setattr(self, name, getattr(newmodel, name))
                
            # now outputs
            traitdict = newmodel._alltraits(iotype='out')
            for name,trait in traitdict.items():
                if self._eligible(name):
                    try: 
                        surrogate = self.surrogate[name]
                        args = self.surrogate_args.get(name,[])   
                    except KeyError: 
                        try: 
                            surrogate = self.surrogate['default']
                            args = self.surrogate_args.get('default',[])
                        except KeyError: 
                            self.raise_exception("No default surrogate model was" 
                            " specified. Either specify a default, or specify a "
                            "surrogate model for all outputs",ValueError)

                    if isinstance(args,dict): 
                        kwargs = args
                        args = []
                    elif isinstance(args,tuple): 
                        args = args[0]
                        kwargs = args[1]
                    else: 
                        kwargs = {}  
                    
                    trait_type = surrogate.get_uncertain_value(1.0).__class__()
                    self.add(name, Slot(trait_type, iotype='out', desc=trait.desc))
                    
                    self._surrogate_info[name] = (surrogate.__class__(*args,**kwargs), []) # (surrogate,output_history)
                    new_model_traitnames.add(name)
                    setattr(self, name, surrogate.get_uncertain_value(getattr(newmodel,name)))
                    
            newmodel.parent = self
            newmodel.name = 'model'
        
        self._current_model_traitnames = new_model_traitnames

    def update_inputs(self, compname, varnames):
        if compname != 'model':
            self.raise_exception("cannot update inputs for child named '%s'" % compname)
        self.model.set_valid(varnames, True)
    
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
    
