""" Metamodel provides basic Meta Modeling capability."""

# pylint: disable-msg=C0111,C0103
# disable complaints about Module 'numpy' has no 'array' member
# pylint: disable-msg=E1101

# Disable complaints Invalid name "setUp" (should match [a-z_][a-z0-9_]{2,30}$)
# pylint: disable-msg=C0103

# Disable complaints about not being able to import modules that Python
#     really can import
# pylint: disable-msg=F0401,E0611

# Disable complaints about Too many arguments (%s/%s)
# pylint: disable-msg=R0913

# Disable complaints about Too many local variables (%s/%s) Used
# pylint: disable-msg=R0914


from copy import deepcopy, copy

from traits.trait_base import not_none
from traits.has_traits import _clone_trait

from openmdao.main.case import flatteners
from openmdao.main.api import Component, Case, VariableTree
from openmdao.main.datatypes.uncertaindist import UncertainDistVar
from openmdao.main.interfaces import IComponent, ISurrogate, ICaseRecorder, \
     ICaseIterator, IUncertainVariable
from openmdao.main.mp_support import has_interface

from openmdao.lib.datatypes.api import Slot, List, Str, Float, Int, Event, \
     Dict, Bool

from openmdao.util.typegroups import int_types, real_types

_missing = object()


def check_model_only_one_level_vartree(model_node):
    for model_varname in model_node.list_vars():
        if isinstance(model_node.get(model_varname), VariableTree):
            vartree = model_node.get(model_varname)
            for varname in vartree.list_vars():
                if isinstance(vartree.get(varname), VariableTree):
                    return False
    return True


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
                             "outputs that don't have a specific surrogate "
                             "assigned to them in their sur_<name> slot.")

    surrogates = Dict(key_trait=Str,
                      value_trait=Slot(ISurrogate),
                      desc='surrogates for output variables')

    report_errors = Bool(True, iotype="in",
                         desc="If True, metamodel will report errors reported "
                         "from the component. If False, metamodel will swallow "
                         "the errors but log that they happened and "
                         "exclude the case from the training set.")

    recorder = Slot(ICaseRecorder,
                    desc='Records training cases')

    # when fired, the next execution will train the metamodel
    train_next = Event(desc='Train metamodel on next execution')

    #when fired, the next execution will reset all training data
    reset_training_data = Event(desc='Reset training data on next execution')

    def __init__(self):
        super(MetaModel, self).__init__()
        self._surrogate_input_names = None
        self._surrogate_output_names = None
        self._surrogate_overrides = set()  # keeps track of which sur_<name> slots are full
        self._training_data = {}
        self._training_input_history = []
        self._const_inputs = {}  # dict of constant training inputs indices and their values
        self._train = False
        self._new_train_data = False
        self._failed_training_msgs = []
        self._default_surrogate_copies = {}  # need to maintain separate copy of
                                             # default surrogate for each sur_*
                                             # that doesn't have a surrogate defined

        # the following line will work for classes that inherit from MetaModel
        # as long as they declare their traits in the class body and not in
        # the __init__ function.  If they need to create traits dynamically
        # during initialization they'll have to provide the value of
        # _mm_class_traitnames
        self._mm_class_traitnames = set(self.traits(iotype=not_none).keys())

        self.on_trait_change(self._surrogate_updated, "surrogates_items")

    def _train_next_fired(self):
        self._train = True
        self._new_train_data = True

    def _reset_training_data_fired(self):
        self._training_input_history = []
        self._const_inputs = {}
        self._failed_training_msgs = []

        # remove output history from training_data
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
                try:
                    inp_val = case[var_name]
                except KeyError:
                    pass
                    #self.raise_exception('The variable "%s" was not '
                                         #'found as an input in one of the cases provided '
                                         #'for warm_start_data.' % var_name, ValueError)
                else:
                    if inp_val is not None:
                        inputs.append(inp_val)
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

    def check_config(self):
        '''Called as part of pre_execute.'''

        # 1. model must be set
        if self.model is None:
            self.raise_exception("MetaModel object must have a model!",
                                 RuntimeError)

        # 2. can't have both includes and excludes
        if self.excludes and self.includes:
            self.raise_exception("includes and excludes are mutually exclusive",
                                 RuntimeError)

        # 3. the includes and excludes must match actual inputs and outputs of the model
        input_names = self.surrogate_input_names()
        output_names = self.surrogate_output_names()
        input_and_output_names = input_names + output_names
        for include in self.includes:
            if include not in input_and_output_names:
                self.raise_exception('The include "%s" is not one of the '
                                     'model inputs or outputs ' % include, ValueError)
        for exclude in self.excludes:
            if exclude not in input_and_output_names:
                self.raise_exception('The exclude "%s" is not one of the '
                                     'model inputs or outputs ' % exclude, ValueError)

        # 4. Either there are no surrogates set and no default surrogate
        #    ( just do passthrough )
        #        or
        #    all outputs must have surrogates assigned either explicitly
        #    or through the default surrogate
        if self.default_surrogate is None:
            no_sur = []
            for name in self.surrogate_output_names():
                if not self.surrogates[name]:
                    no_sur.append(name)
            if len(no_sur) > 0 and len(no_sur) != len(self._surrogate_output_names):
                self.raise_exception("No default surrogate model is defined and"
                                     " the following outputs do not have a"
                                     " surrogate model: %s. Either specify"
                                     " default_surrogate, or specify a"
                                     " surrogate model for all outputs." %
                                     no_sur, RuntimeError)

        # 5. All the explicitly set surrogates[] should match actual outputs of the model
        for surrogate_name in self.surrogates.keys():
            if surrogate_name not in output_names:
                self.raise_exception('The surrogate "%s" does not match one of the '
                                     'model outputs ' % surrogate_name, ValueError)

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise,
        predict outputs.
        """

        if self._train:
            try:
                inputs = self.update_model_inputs()

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
                # save the case, making sure to add out name to the local input
                # name since this Case is scoped to our parent Assembly
                case_inputs = [('.'.join([self.name, name]), val)
                               for name, val in zip(self.surrogate_input_names(),
                                                    inputs)]
                if self.recorder:
                    self.recorder.record(Case(inputs=case_inputs,
                                              outputs=case_outputs))

            self._train = False
        else:
            # NO surrogates defined. just run model and get outputs
            if self.default_surrogate is None and not self._surrogate_overrides:
                inputs = self.update_model_inputs()
                self.model.run()
                self.update_outputs_from_model()
                return

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
                val = self.get(name)
                cval = self._const_inputs.get(i, _missing)
                if cval is _missing:
                    inputs.append(val)

                elif val != cval:
                    self.raise_exception("ERROR: training input '%s' was a"
                                         " constant value of (%s) but the value"
                                         " has changed to (%s)." %
                                         (name, cval, val), ValueError)

            for name in self._training_data:
                surrogate = self._get_surrogate(name)
                # copy output to boundary
                if surrogate is None:
                    self._set_output(name, self.model.get(name))
                else:
                    self._set_output(name, surrogate.predict(inputs))

    def _set_output(self, path, value):
        """
        Since the set method of container does not allow setting
        of variables with iotype of out, this method needed to be written.
        """

        # get the leaf object
        names = path.split('.')
        obj = self
        for name in names[:-1]:
            obj = getattr(obj, name)
        setattr(obj, names[-1], value)

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
            self.raise_exception('model of type %s does not implement the'
                                 ' IComponent interface' % type(newmodel).__name__,
                                 TypeError)

        self.reset_training_data = True

        if newmodel:
            if not check_model_only_one_level_vartree(newmodel):
                self.raise_exception('metamodels currently do not support multi'
                                     ' level vartrees', TypeError)

        self._update_surrogate_list()

        if newmodel:
            newmodel.parent = self
            newmodel.name = 'model'

        self.config_changed()

    def _add_var_for_surrogate(self, surrogate, varname):
        """Different surrogates have different types of output values, so create
        the appropriate type of output Variable based on the return value
        of get_uncertain_value on the surrogate.
        """

        val = surrogate.get_uncertain_value(self.model.get(varname))
        if has_interface(val, IUncertainVariable):
            ttype = UncertainDistVar
        elif isinstance(val, real_types):
            ttype = Float
        elif isinstance(val, int_types):
            ttype = Int
        else:
            self.raise_exception("value type of '%s' is not a supported"
                                 " surrogate return value" %
                                 val.__class__.__name__)

        if "." not in varname:  # non vartree variable
            self.add(varname, ttype(default_value=val, iotype='out',
                                    desc=self.model.trait(varname).desc,
                                    units=self.model.trait(varname).units))
            setattr(self, varname, val)

        else:  # vartree sub variable
            vartreename, subvarname = varname.split(".")

            metamodel_vartree = self.get(vartreename)
            model_vartree_node = self.model.get(vartreename)
            metamodel_vartree.add(subvarname, ttype(default_value=val, iotype='out',
                                  desc=model_vartree_node.trait(subvarname).desc,
                                  units=model_vartree_node.trait(subvarname).units))
            setattr(metamodel_vartree, subvarname, val)

        return

    def _surrogate_updated(self, obj, name, old, new):
        """Called when self.surrogates is updated."""

        # if surrogate set to be None
           # put copies of the default surrogate
           # remove that surrogate name from the list of overrides
        if new.changed:
            varname = new.changed.keys()[0]
            if self.surrogates[varname] is None:
                if self.default_surrogate:
                    self._default_surrogate_copies[varname] = deepcopy(self.default_surrogate)
                if varname in self._surrogate_overrides:
                    self._surrogate_overrides.remove(varname)
            else:
                self._surrogate_overrides.add(varname)
                self._add_var_for_surrogate(self.surrogates[varname], varname)
                if name in self._default_surrogate_copies:
                    del self._default_surrogate_copies[name]

        self.config_changed()

    def update_inputs(self, compname, varnames):
        if compname != 'model':
            self.raise_exception("cannot update inputs for child named '%s'"
                                 % compname)
        self.model.set_valid(varnames, True)

    def update_model_inputs(self):
        """Copy the values of the MetaModel's inputs into the inputs of the
        model. Returns the values of the inputs.
        """
        input_values = []
        for name in self.surrogate_input_names():
            inp = self.get(name)
            input_values.append(inp)
            self.model.set(name, inp)
        return input_values

    def _get_surrogate(self, name):
        """Return the designated surrogate for the given output."""

        surrogate = self.surrogates.get(name)
        if surrogate is None and self.default_surrogate is not None:
            surrogate = self._default_surrogate_copies.get(name)

        return surrogate

    def update_outputs_from_model(self):
        """Copy output values from the model into the MetaModel's outputs and
        if training, save the output associated with surrogate.
        """

        for name in self.surrogate_output_names():

            out = self.model.get(name)
            surrogate = self._get_surrogate(name)
            if surrogate is None:
                self._set_output(name, out)
            else:
                self._set_output(name, surrogate.get_uncertain_value(out))

            if self._train:
                self._training_data[name].append(out)  # save to training output history

    def _add_input(self, name):
        """Adds the specified input variable."""

        if "." not in name:  # non vartree variable
            self.add_trait(name, _clone_trait(self.model.trait(name)))
            setattr(self, name, getattr(self.model, name))
        else:
            vartreename, subvarname = name.split(".")

            if not hasattr(self, vartreename):
                self.add_trait(vartreename,
                               _clone_trait(self.model.trait(vartreename)))
                setattr(self, vartreename, copy(getattr(self.model, vartreename)))

            metamodel_vartree_node = self.get(vartreename)
            model_vartree_node = self.model.get(vartreename)
            metamodel_vartree_node.add_trait(subvarname,
                                             _clone_trait(model_vartree_node.trait(subvarname)))
            metamodel_vartree_node.set(subvarname, model_vartree_node.get(subvarname))

    def _add_output(self, name):
        """Adds the specified output variable and its associated surrogate Slot."""

        if "." not in name:  # non vartree variable
            self.surrogates[name] = None
            if self.default_surrogate is not None:
                surrogate = deepcopy(self.default_surrogate)
                self._default_surrogate_copies[name] = surrogate
                self._add_var_for_surrogate(surrogate, name)
            else:
                self.add_trait(name, _clone_trait(self.model.trait(name)))
        else:
            self.surrogates[name] = None
            vartreename = name.split(".")[0]
            subvarname = name.split(".")[1]
            if not hasattr(self, vartreename):
                self.add_trait(vartreename,
                               _clone_trait(self.model.trait(vartreename)))
                setattr(self, vartreename, copy(getattr(self.model, vartreename)))

            if self.default_surrogate is not None:
                surrogate = deepcopy(self.default_surrogate)
                self._default_surrogate_copies[name] = surrogate
                self._add_var_for_surrogate(surrogate, name)
            else:
                metamodel_vartree_node = self.get(vartreename)
                model_vartree_node = self.model.get(vartreename)
                metamodel_vartree_node.add_trait(subvarname,
                                                 _clone_trait(model_vartree_node.trait(subvarname)))

        self._training_data[name] = []

    def _remove_input(self, name):
        """Removes the specified input variable.
        Assumes one level of vartree.
        """

        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))

        if "." in name:  # vartree
            vartreename = name.split(".")[0]
            subvarname = name.split(".")[1]
            self.get(vartreename).remove_trait(subvarname)
        else:
            self.remove_trait(name)

    def _remove_output(self, name):
        """Removes the specified output variable and its associated surrogate.
        Assuming that there is only one level of vartrees and that users can only
        exclude entire vartrees, not sub parts."""

        if self.parent:
            self.parent.disconnect('.'.join([self.name, name]))

        if "." in name:  # vartree
            del self.surrogates[name]
            if name in self._training_data:
                del self._training_data[name]

            vartreename = name.split(".")[0]
            subvarname = name.split(".")[1]
            self.get(vartreename).remove_trait(subvarname)

        else:
            del self.surrogates[name]
            self.remove_trait(name)
            if name in self._training_data:
                del self._training_data[name]

    def surrogate_input_names(self):
        """Return the list of names of public inputs that correspond
        to model inputs.
        """

        if self._surrogate_input_names is None:
            if self.model:
                self._surrogate_input_names = []
                for name in self.model._alltraits(iotype='in').keys():
                    if not isinstance(self.model.get(name), VariableTree):
                        if self._eligible(name) and name not in self._mm_class_traitnames:
                            t = type(self.model.get(name))
                            if t not in [float, int]:
                                self.raise_exception("Metamodel only supports"
                                                     " int and float inputs",
                                                     RuntimeError)
                            self._surrogate_input_names.append(name)
                    else:
                        subnames = [subvar[0] for subvar in flatteners[VariableTree](name, self.model.get(name))]
                        for subname in subnames:
                            if self._eligible(subname) and name not in self._mm_class_traitnames:
                                t = type(self.model.get(subname))
                                if t not in [float, int]:
                                    self.raise_exception("Metamodel only supports int and float inputs",
                                                         RuntimeError)
                                self._surrogate_input_names.append(subname)
            else:
                return []
        return self._surrogate_input_names

    def surrogate_output_names(self):
        """Return the list of names of public outputs that correspond
        to model outputs.
        """

        if self._surrogate_output_names is None:
            if self.model:
                self._surrogate_output_names = []
                for name in self.model._alltraits(iotype='out').keys():
                    if not isinstance(self.model.get(name), VariableTree):
                        if self._eligible(name) and name not in self._mm_class_traitnames:
                            t = type(self.model.get(name))
                            if t not in [float, int]:
                                self.raise_exception("Metamodel only supports"
                                                     " int and float outputs",
                                                     RuntimeError)
                            self._surrogate_output_names.append(name)
                    else:
                        subnames = [subvar[0] for subvar in flatteners[VariableTree](name, self.model.get(name))]
                        for subname in subnames:
                            if self._eligible(subname) and name not in self._mm_class_traitnames:
                                t = type(self.model.get(subname))
                                if t not in [float, int]:
                                    self.raise_exception("Metamodel only supports"
                                                         " int and float outputs",
                                                         RuntimeError)
                                self._surrogate_output_names.append(subname)
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

        added_outs = new_out - old_out
        added_ins = new_in - old_in

        removed_outs = old_out - new_out
        removed_ins = old_in - new_in

        if added_outs or added_ins or removed_ins:
            self.reset_training_data = True

        for name in removed_ins:
            self._remove_input(name)
        for name in added_ins:
            self._add_input(name)

        for name in removed_outs:
            self._remove_output(name)
        for name in added_outs:
            self._add_output(name)

    def _includes_changed(self, old, new):
        for name in new:
            if "." in name:
                self.raise_exception("Can only include top level variable"
                                     " trees, not leaves", RuntimeError)
        self._update_surrogate_list()
        self.config_changed()

    def _excludes_changed(self, old, new):
        for name in new:
            if "." in name:
                self.raise_exception("Can only exclude top level variable"
                                     " trees, not leaves", RuntimeError)
        self._update_surrogate_list()
        self.config_changed()

    def _default_surrogate_changed(self, old_obj, new_obj):
        if old_obj:
            old_obj.on_trait_change(self._def_surrogate_trait_modified,
                                    remove=True)
        if new_obj:
            new_obj.on_trait_change(self._def_surrogate_trait_modified)

            # due to the way "add" works, container will always remove the old
            #  before it adds the new one. So you actually get this method called
            #  twice on a replace. You only do this update when the new one gets set

            for name in self.surrogate_output_names():
                if name not in self._surrogate_overrides:
                    surrogate = deepcopy(self.default_surrogate)
                    self._default_surrogate_copies[name] = surrogate
                    self._add_var_for_surrogate(surrogate, name)

        self.config_changed()

    def _def_surrogate_trait_modified(self, surrogate, name, old, new):
        # a trait inside of the default_surrogate was changed, so we need to
        # replace all of the default copies

        for name in self._default_surrogate_copies:
            self._default_surrogate_copies[name] = deepcopy(self.default_surrogate)

    def _eligible(self, name):
        """Return True if the named trait is not excluded from the public
        interface based on the includes and excludes lists.
        """

        # includes and excludes only are allowed at the top level of vartrees
        if "." in name:
            name = name.split(".")[0]

        if name in self._mm_class_traitnames:
            return False
        if self.includes and name not in self.includes:
            return False
        elif self.excludes and name in self.excludes:
            return False
        return True
