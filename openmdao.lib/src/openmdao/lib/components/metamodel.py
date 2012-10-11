""" Metamodel provides basic Meta Modeling capability."""

from copy import deepcopy

# pylint: disable-msg=E0611,F0401
from enthought.traits.trait_base import not_none
from enthought.traits.has_traits import _clone_trait

from openmdao.main.api import Component, Case
from openmdao.lib.datatypes.api import Slot, List, Str, Event, Dict, Bool
from openmdao.main.interfaces import IComponent, ISurrogate, ICaseRecorder, \
     ICaseIterator
from openmdao.main.mp_support import has_interface
from openmdao.util.log import logger

_missing = object()
__surrogate_prefix__ = 'sur_'

class MetaModel(Component):

    # pylint: disable-msg=E1101
    model = Slot(IComponent, allow_none=True,
                   desc='Slot for the Component or Assembly being '
                   'encapsulated.')
    includes = List(Str, iotype='in',
                    desc='A list of names of variables to be included '
                         'in the public interface.')
    excludes = List(Str, iotype='in',
                    desc='A list of names of variables to be excluded '
                         'from the public interface.')

    warm_start_data = Slot(ICaseIterator, iotype="in",
                              desc="CaseIterator containing cases to use as "
                              "initial training data. When this is set, all "
                              "previous training data is cleared and replaced "
                              "with data from this CaseIterator.")

    default_surrogate = Slot(ISurrogate, allow_none=True,
                             desc="This surrogate will be used for all "
                             "outputs that don't have a specific surrogate assigned "
                             "to them in their sur_<name> slot.")
    
    report_errors = Bool(True, iotype="in",
                         desc="If True, metamodel will report errors reported from the component. "
                         "If False, metamodel will swallow the errors but log that they happened and "
                         "exclude the case from the training set.")

    recorder = Slot(ICaseRecorder,
                    desc='Records training cases')

    # when fired, the next execution will train the metamodel
    train_next = Event()
    #when fired, the next execution will reset all training data
    reset_training_data = Event()

    def __init__(self, *args, **kwargs):
        super(MetaModel, self).__init__(*args, **kwargs)
        self._surrogate_input_names = None
        self._surrogate_output_names = None
        self._training_data = {}
        self._training_input_history = []
        self._const_inputs = {}  # dict of constant training inputs indices and their values
        self._train = False
        self._new_train_data = False
        self._failed_training_msgs = []
        self._default_surrogate_copies = {} # need to maintain separate copy of default surrogate for each sur_* that doesn't
                                            # have a surrogate defined
        
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
        self._failed_training_msgs = []

        # remove output history from surrogate_info
        for name in self._training_data:
            self._training_data[name] = []

    def _warm_start_data_changed(self, oldval, newval):
        self.reset_training_data = True

        # build list of inputs
        for case in newval:
            if self.recorder:
                self.recorder.record(case)
            inputs = []
            for inp_name in self.surrogate_input_names():
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

            for output_name in self.surrogate_output_names():
                #grab value from case data
                var_name = '.'.join([self.name, output_name])
                try:
                    val = case.get_output(var_name)
                except KeyError:
                    self.raise_exception('The output "%s" was not found '
                                         'in one of the cases provided for '
                                         'warm_start_data' % var_name, ValueError)
                else:  # save to training output history
                    self._training_data[output_name].append(val)

        self._new_train_data = True

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise,
        predict outputs.
        """

        if self._train:
            if self.model is None:
                self.raise_exception("MetaModel object must have a model!",
                                     RuntimeError)
            try:
                inputs = self.update_model_inputs()

                #print '%s training with inputs: %s' % (self.get_pathname(), inputs)
                self.model.run(force=True)

            except Exception as err:
                if self.report_errors:
                    raise err
                else:
                    self._failed_training_msgs.append(str(err))
            else:  # if no exceptions are generated, save the data
                self._training_input_history.append(inputs)
                self.update_outputs_from_model()
                case_outputs = []

                for name, output_history in self._training_data.items():
                    case_outputs.append(('.'.join([self.name, name]),
                                         output_history[-1]))
                # save the case, making sure to add out name to the local input name since
                # this Case is scoped to our parent Assembly
                case_inputs = [('.'.join([self.name, name]), val) for name, val in zip(self.surrogate_input_names(), 
                                                                                       inputs)]
                if self.recorder:
                    self.recorder.record(Case(inputs=case_inputs, outputs=case_outputs))

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
                        training_input_history.append([val for i, val in enumerate(inputs)
                                                       if i not in self._const_inputs])
                else:
                    training_input_history = self._training_input_history
                for name, output_history in self._training_data.items():
                    surrogate = self._get_surrogate(name)
                    if surrogate is not None:
                        surrogate.train(training_input_history, output_history)

                self._new_train_data = False

            inputs = []
            for i, name in enumerate(self.surrogate_input_names()):
                val = getattr(self, name)
                cval = self._const_inputs.get(i, _missing)
                if cval is _missing:
                    inputs.append(val)

                elif val != cval:
                    self.raise_exception("ERROR: training input '%s' was a constant value of (%s) but the value has changed to (%s)." %
                                         (name, cval, val), ValueError)

            for name in self._training_data:
                surrogate = self._get_surrogate(name)
                # copy output to boundary
                if surrogate is None:
                    setattr(self, name, getattr(self.model, name)) # no surrogate. use outputs from model
                else:
                    setattr(self, name, surrogate.predict(inputs))

    def _post_run(self):
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
        """called whenever the model variable is set or when includes/excludes change."""
        # TODO: check for pre-connected traits on the new model
        # TODO: disconnect traits corresponding to old model (or leave them if the new model has the same ones?)
        # TODO: check for nested MMs?  Is this a problem?
        # TODO: check for name collisions between MetaModel class traits and traits from model
        if newmodel is not None and not has_interface(newmodel, IComponent):
            self.raise_exception('model of type %s does not implement the IComponent interface' % type(newmodel).__name__,
                                 TypeError)

        new_model_traitnames = set()
        self._training_input_history = []
        self._training_data = {}
        self._failed_training_msgs = []

        self._update_surrogate_list()
        
        if self.default_surrogate is None:
            no_sur = []
            for name in self.surrogate_output_names():
                if getattr(self, __surrogate_prefix__+name) is None:
                    no_sur.append(name)
            if len(no_sur) > 0 and len(no_sur) != len(self._surrogate_output_names):
                self.raise_exception("No default surrogate model is defined and the following outputs do not have a surrogate model: %s. Either specify default_surrogate, or specify a surrogate model for all outputs." %
                                     no_sur, RuntimeError)
        
        if newmodel:
            newmodel.parent = self
            newmodel.name = 'model'

    def _surrogate_updated(self, obj, name, old, new):
        """Called when a surrogate Slot (sur_*) is updated."""
        if new is None:
            if self.default_surrogate:
                self._default_surrogate_copies[name] = deepcopy(self.default_surrogate)
        else:
            varname = name[len(__surrogate_prefix__):]
            val = getattr(self, name).get_uncertain_value(getattr(self.model, varname))
            self.add(varname, Slot(val.__class__, iotype='out', desc=self.model.trait(varname).desc))
            setattr(self, varname, val)
            if old is None and name in self._default_surrogate_copies:
                del self._default_surrogate_copies[name]
    
    def update_inputs(self, compname, varnames):
        if compname != 'model':
            self.raise_exception("cannot update inputs for child named '%s'" % compname)
        self.model.set_valid(varnames, True)

    def update_model_inputs(self):
        """Copy the values of the MetaModel's inputs into the inputs of the model.
        Returns the values of the inputs.
        """
        input_values = []
        for name in self.surrogate_input_names():
            inp = getattr(self, name)
            input_values.append(inp)
            setattr(self.model, name, inp)
        return input_values

    def _get_surrogate(self, name):
        """Return the designated surrogate for the given output."""
        surrogate = getattr(self, __surrogate_prefix__+name, None)
        if surrogate is None and self.default_surrogate is not None:
            surrogate = self._default_surrogate_copies[__surrogate_prefix__+name]
        return surrogate
    
    def update_outputs_from_model(self):
        """Copy output values from the model into the MetaModel's outputs, and
        if training, save the output associated with surrogate.
        """
        for name in self.surrogate_output_names():
            out = getattr(self.model, name)
            surrogate = self._get_surrogate(name)
            if surrogate is None:
                setattr(self, name, out)
            else:
                setattr(self, name, surrogate.get_uncertain_value(out))
            if self._train:
                self._training_data[name].append(out)  # save to training output history

    def _add_input(self, name):
        """Adds the specified input variable."""
        self.add_trait(name, _clone_trait(self.model.trait(name)))
        setattr(self, name, getattr(self.model, name))
    
    def _add_output(self, name):
        """Adds the specified output variable and its associated surrogate."""
        sur_name = __surrogate_prefix__+name
        
        if hasattr(self, sur_name):
            logger.warning("name collision of surrogate with exising variable 'sur_%s'. Surrogate was not added" % name)
            return
        
        self.add_trait(sur_name, Slot(ISurrogate, allow_none=True))
        self.on_trait_change(self._surrogate_updated, sur_name)

        if self.default_surrogate is not None:
            self._default_surrogate_copies[sur_name] = deepcopy(self.default_surrogate)
            val = self._default_surrogate_copies[sur_name].get_uncertain_value(getattr(self.model, name))
            self.add(name, Slot(val.__class__, iotype='out', desc=self.model.trait(name).desc))
            setattr(self, name, val)
        else:
            self.add_trait(name, _clone_trait(self.model.trait(name)))

        self._training_data[name] = [] 
    
    def _remove_input(self, name):
        """Removes the specified input variable."""
        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))
        self.remove_trait(name)
    
    def _remove_output(self, name):
        """Removes the specified output variable and its associated surrogate."""
        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))
        self.remove_trait(name)
        self.remove_trait(__surrogate_prefix__+name)
    
    def surrogate_input_names(self):
        """Return the list of names of public inputs that correspond
        to model inputs.
        """
        if self._surrogate_input_names is None:
            if self.model:
                self._surrogate_input_names = [n for n in self.model._alltraits(iotype='in').keys() 
                                               if self._eligible(n)]
            else:
                return []
        return self._surrogate_input_names

    def surrogate_output_names(self):
        """Return the list of names of public outputs that correspond
        to model outputs.
        """
        if self._surrogate_output_names is None:
            if self.model:
                self._surrogate_output_names = [n for n in self.model._alltraits(iotype='out').keys() 
                                               if self._eligible(n) and 
                                                  n not in self._mm_class_traitnames]
            else:
                return []
        return self._surrogate_output_names

    def _update_surrogate_list(self):
        old_in = set()
        if self._surrogate_input_names is not None:
            old_in.update(self._surrogate_input_names)
        old_out = set()
        if self._surrogate_output_names is not None:
            old_out.update(self._surrogate_output_names)
            
        self._surrogate_input_names = None
        self._surrogate_output_names = None
        
        new_in = set(self.surrogate_input_names())
        new_out = set(self.surrogate_output_names())
        
        for name in (old_in - new_in):
            self._remove_input(name)
        for name in (new_in - old_in):
            self._add_input(name)
            
        for name in (old_out - new_out):
            self._remove_output(name)
        for name in (new_out - old_out):
            self._add_output(name)
        
    def _includes_changed(self, old, new):
        if self.excludes and new is not None:
            self.__dict__['includes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self._update_surrogate_list()

    def _excludes_changed(self, old, new):
        if self.includes and new is not None:
            self.__dict__['excludes'] = old
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)
        self._update_surrogate_list()

    def _default_surrogate_changed(self, old_obj, new_obj):
        if old_obj:
            old_obj.on_trait_change(self._def_surrogate_trait_modified, remove=True)
        if new_obj:
            new_obj.on_trait_change(self._def_surrogate_trait_modified)
        
    def _def_surrogate_trait_modified(self, surrogate, name, old, new):
        # default_surrogate was changed, so we need to replace all of the default copies
        for name in self._default_surrogate_copies:
            self._default_surrogate_copies[name] = deepcopy(self.default_surrogate)
        print name, 'changed!'
    
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
