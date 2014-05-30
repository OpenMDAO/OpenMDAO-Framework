""" Metamodel provides basic Meta Modeling capability."""

# Disable complaints about not being able to import modules that Python
#     really can import
# pylint: disable-msg=F0401,E0611

from copy import deepcopy

from openmdao.main.api import Component
from openmdao.main.datatypes.api import List, Bool, Dict, Float, Slot, Str, \
                                        VarTree
from openmdao.main.datatypes.uncertaindist import UncertainDistVar
from openmdao.main.interfaces import ISurrogate, ICaseRecorder, \
                                     IUncertainVariable
from openmdao.main.mp_support import has_interface
from openmdao.main.vartree import VariableTree
from openmdao.util.typegroups import int_types, real_types

class MetaModel(Component):
    """ Class that creates a reduced order model for a tuple of outputs from
    a tuple of inputs. Accepts surrogate models that adhere to ISurrogate.
    Multiple surrogate models can be used. Training inputs and outputs should
    be provided in the params and responses variable trees.

    For a Float variable, the training data is an array of length m.
    """

    default_surrogate = Slot(ISurrogate, allow_none=True,
                             desc="This surrogate will be used for all "
                             "outputs that don't have a specific surrogate "
                             "assigned to them in their sur_<name> slot.")

    surrogates = Dict(key_trait=Str,
                      value_trait=Slot(ISurrogate),
                      desc='surrogates for output variables')

    params = VarTree(VariableTree(), iotype='in')

    responses = VarTree(VariableTree(), iotype='in')

    recorder = Slot(ICaseRecorder,
                    desc='Records training cases')

    warm_restart = Bool(False, iotype='in',
                        desc="When set to False (default), the metamodel "
                             "retrains with the new dataset whenever the "
                             "training data values are changed. When set to "
                             "True, the new data is appended to the old data "
                             "and all of the data is used to train.")

    def __init__(self, params=None, responses=None):
        super(MetaModel, self).__init__()

        if params is None or not isinstance(params, tuple):
            msg = "Metamodel params argument needs to be a tuple of " + \
                  "variable names."
            self.raise_exception(msg, ValueError)

        if responses is None or not isinstance(responses, tuple):
            msg = "Metamodel responses argument needs to be a tuple of " + \
                  "variable names."
            self.raise_exception(msg, ValueError)

        # Inputs and Outputs created immediately.

        input_tree = self.get('params')
        self._param_data = []
        for name in params:
            self.add(name, Float(0.0, iotype='in', desc='metamodel param'))
            input_tree.add(name, List([], desc='training param'))

        output_tree = self.get('responses')
        self._response_data = {}
        for name in responses:
            self.add(name, Float(0.0, iotype='out', desc='metamodel response'))
            output_tree.add(name, List([], desc='training response'))
            self._response_data[name] = []
            self.surrogates[name] = None

        self._surrogate_input_names = params
        self._surrogate_output_names = responses

        self._train = True

        # keeps track of which sur_<name> slots are full
        self._surrogate_overrides = set()

        # need to maintain separate copy of default surrogate for each sur_*
        # that doesn't have a surrogate defined
        self._default_surrogate_copies = {}

        # Special callback for whenver anything changes in the surrogates
        # Dict items.
        self.on_trait_change(self._surrogate_updated, "surrogates_items")

    def _input_updated(self, name, fullpath=None):
        ''' Set _train if anything changes in our inputs so that training
        occurs on the next execution.'''

        if fullpath is not None:
            if fullpath.startswith('params.') or \
               fullpath.startswith('responses.'):
                self._train = True

        super(MetaModel, self)._input_updated(name.split('.',1)[0])

    def check_config(self, strict=False):
        '''Called as part of pre_execute. Does some simple error checking.'''

        super(MetaModel, self).check_config(strict=strict)

        # Either there are no surrogates set and no default surrogate (just
        # do passthrough ) or all outputs must have surrogates assigned
        # either explicitly or through the default surrogate
        if self.default_surrogate is None:
            no_sur = []
            for name in self._surrogate_output_names:
                if name not in self.surrogates or \
                               self.surrogates[name] is None:
                    no_sur.append(name)
            if len(no_sur) > 0:
                self.raise_exception("No default surrogate model is defined and"
                                     " the following outputs do not have a"
                                     " surrogate model: %s. Either specify"
                                     " default_surrogate, or specify a"
                                     " surrogate model for all outputs." %
                                     no_sur, RuntimeError)

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise,
        predict outputs.
        """

        # Train first
        if self._train:

            input_data = self._param_data
            if self.warm_restart is False:
                input_data = []
                base = 0
            else:
                base = len(input_data)

            for name in self._surrogate_input_names:
                train_name = "params.%s" % name
                val = self.get(train_name)
                num_sample = len(val)

                for j in xrange(base, base + num_sample):

                    if j > len(input_data) - 1:
                        input_data.append([])
                    input_data[j].append(val[j-base])

            # Surrogate models take an (m, n) list of lists
            # m = number of training samples
            # n = number of inputs
            #
            # TODO - Why not numpy array instead?

            for name in self._surrogate_output_names:

                train_name = "responses.%s" % name
                output_data = self._response_data[name]

                if self.warm_restart is False:
                    output_data = []

                output_data.extend(self.get(train_name))
                surrogate = self._get_surrogate(name)

                if surrogate is not None:
                    surrogate.train(input_data, output_data)

            self._train = False

        # Now Predict for current inputs

        inputs = []
        for name in self._surrogate_input_names:
            val = self.get(name)
            inputs.append(val)

        for name in self._surrogate_output_names:
            surrogate = self._get_surrogate(name)
            if surrogate is not None:
                setattr(self, name, surrogate.predict(inputs))

    def _get_surrogate(self, name):
        """Return the designated surrogate for the given output."""

        surrogate = self.surrogates.get(name)
        if surrogate is None and self.default_surrogate is not None:
            surrogate = self._default_surrogate_copies.get(name)

        return surrogate

    def _default_surrogate_changed(self, old_obj, new_obj):
        """Callback whenever the default_surrogate model is changed."""

        if old_obj:
            old_obj.on_trait_change(self._def_surrogate_trait_modified,
                                    remove=True)
        if new_obj:
            new_obj.on_trait_change(self._def_surrogate_trait_modified)

            # due to the way "add" works, container will always remove the
            # old before it adds the new one. So you actually get this method
            # called twice on a replace. You only do this update when the new
            # one gets set

            for name in self._surrogate_output_names:
                if name not in self._surrogate_overrides:
                    surrogate = deepcopy(self.default_surrogate)
                    self._default_surrogate_copies[name] = surrogate
                    self._update_var_for_surrogate(surrogate, name)

        self.config_changed()
        self._train = True

    def _def_surrogate_trait_modified(self, surrogate, name, old, new):
        # a trait inside of the default_surrogate was changed, so we need to
        # replace all of the default copies

        for name in self._default_surrogate_copies:
            surr_copy = deepcopy(self.default_surrogate)
            self._default_surrogate_copies[name] = surr_copy

    def _surrogate_updated(self, obj, name, old, new):
        """Called when self.surrogates Dict is updated."""

        all_changes = new.changed.keys() + new.added.keys() + \
                      new.removed.keys()

        for varname in all_changes:
            surr = self.surrogates.get(varname)
            if surr is None:
                if self.default_surrogate:
                    def_surr = deepcopy(self.default_surrogate)
                    self._default_surrogate_copies[varname] = def_surr
                    self._update_var_for_surrogate(def_surr, varname)
                if varname in self._surrogate_overrides:
                    self._surrogate_overrides.remove(varname)
            else:
                self._surrogate_overrides.add(varname)
                if name in self._default_surrogate_copies:
                    del self._default_surrogate_copies[name]

                self._update_var_for_surrogate(surr, varname)

        self.config_changed()
        self._train = True

    def _update_var_for_surrogate(self, surrogate, varname):
        """Different surrogates have different types of output values, so create
        the appropriate type of output Variable based on the return value
        of get_uncertain_value on the surrogate.

        Presently, this just adds the UncertainVariable for Kriging
        """

        # TODO - ISurrogate should have a get_datatype or get_uncertainty_type
        val = surrogate.get_uncertain_value(1.0)

        if has_interface(val, IUncertainVariable):
            ttype = UncertainDistVar
        #elif isinstance(val, int_types):
        #    ttype = Int
        elif isinstance(val, real_types):
            ttype = Float
        else:
            self.raise_exception("value type of '%s' is not a supported"
                                 " surrogate return value" %
                                 val.__class__.__name__)

        self.add(varname, ttype(default_value=val, iotype='out',
                                desc=self.trait(varname).desc))
        setattr(self, varname, val)

        return
