"""
Partial! M4 variable fidelity component.
Consider this only as a hint as to how this might be done.
The code here has only been developed to test feasibility,
and was written by someone without much 'mool' knowledge.
"""

__all__ = ('MidFidelity',)


from openmdao.main.datatypes.api import Float, Int, Array, Str, Slot

import mool.Optimization.MidFiModel

from openmdao.main.api import Assembly, Component

import wrapper


class MidFidelity(Assembly):
    """ Wrapper for M4 variable fidelity capability. """

    # Slots.
    lofi_model = Slot(Component, desc='Low fidelity model', required=True)
    hifi_model = Slot(Component, desc='High fidelity model', required=True)

    # Inputs.
    # No 'Option' variables yet.
    doe_type = Str('lhs', iotype='in', 
                   desc='Type of DOE used to generate response surface.')
    rs_type = Str('quadratic', iotype='in', desc='Type of response surface.')
    n_samples = Int(value=1, low=1, iotype='in', desc='Number of samples.')
    tolerance = Float(1.0e10, iotype='in', desc='?')
    correction_function = Int(1, iotype='in',
                              desc='Type of correction function.')
    w_h = Float(0.5, iotype='in', desc='?')
    accuracy_test_type = Int(2, iotype='in', 
                             desc='Method for testing accuracy of response.')
    n_samples_test = Int(value=10, low=1, iotype='in',
                           desc='Number of additional samples for additional-points test.')
    ntheta = Int(3, iotype='in', 
                 desc='For Kriging method, ntheta=1(SA),2(Cobyla),3(BFGS)')
    
    # TODO: change these to delegates or passthroughs
    
    sample_points = Array(iotype='out', desc='Points used to make response',
                          ref_name='sample_points', ref_parent='midfi_model')

    lofi_results = Array(iotype='out', desc='Points used to make response',
                         ref_name='lofi_results', ref_parent='midfi_model')

    hifi_results = Array(iotype='out', desc='Points used to make response',
                         ref_name='hifi_results', ref_parent='midfi_model')
    
    #name='M4_MidFi', 
    def configure(self):
        self.need_updated_corrections = True

        self.input_mappings = []
        self.output_mappings = []

        self._lofi_m4model = None
        self._hifi_m4model = None

        self._midfi_model = mool.Optimization.MidFiModel.Mid_Fi_Model()


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

    def set_hifi_model(self, hifi):
        """ Set high fidelity model. """
        self.hifi_model = hifi
        self._hifi_m4model = None
        self.need_updated_corrections = True

    def set_lofi_model(self, lofi):
        """ Set low fidelity model. """
        self.lofi_model = lofi
        self._lofi_m4model = None
        self.need_updated_corrections = True

    def add_input_mapping(self, mid, low, high):
        """ Add mapping for input variable. """
        self.input_mappings.append((mid, low, high))
        self.need_updated_corrections = True

    def add_output_mapping(self, mid, low, high):
        """ Add mapping for output variable. """
        self.output_mappings.append((mid, low, high))
        self.need_updated_corrections = True

    def execute(self):
        """ Compute results based on mid-fidelity approximation. """
        if self.lofi_model is None:
            self.raise_exception('No lofi model plugin', ValueError)

        if self.hifi_model is None:
            self.raise_exception('No hifi model plugin', ValueError)

        # Wrap low fidelity model.
        if self._lofi_m4model is None:
            inputs = []
            for mid, low, high in self.input_mappings:
                inputs.append(low)
            outputs = []
            for mid, low, high in self.output_mappings:
                outputs.append(low)
            self._lofi_m4model = \
                wrapper.M4Model(self.lofi_model, inputs, outputs)

        # Wrap high fidelity model.
        if self._hifi_m4model is None:
            inputs = []
            for mid, low, high in self.input_mappings:
                inputs.append(high)
            outputs = []
            for mid, low, high in self.output_mappings:
                outputs.append(high)
            self._hifi_m4model = \
                wrapper.M4Model(self.hifi_model, inputs, outputs)

        # If needed, update correction functions.
        if self.need_updated_corrections:
            nvars = len(self.input_mappings)

            # Check for sufficient DOE samples.
            if self.doe_type == 'ccd':
                # CCD requires an exact value
                if nvars == 1:
                    min_samples = 3
                else:
                    min_samples = 2**nvars + 2*nvars + 1
                if self.n_samples != min_samples:
                    self._logger.warning('Setting n_samples to CCD required value: %d',
                                 min_samples)
                self.n_samples = min_samples
            elif self.doe_type == 'lhs':
                min_samples = nvars
            elif self.doe_type == 'rand_lhs':
                min_samples = nvars
            elif self.doe_type == 'oa2':
                min_samples = (nvars-1)**2
            elif self.doe_type == 'oa3':
                min_samples = (nvars-1)**3
            else:
                msg = "Unknown DOE type '%s'" % self.doe_type
                self.raise_exception(msg, ValueError)

            if self.n_samples < min_samples:
                self._logger.warning('Updating n_samples to minimum for DOE: %d',
                             min_samples)
                self.n_samples = min_samples

            # Check for sufficient response surface samples.
            if self.rs_type == 'linear':
                min_samples = nvars + 1
            elif self.rs_type == 'quadratic':
                min_samples = (nvars**2 + 3*nvars + 2)/2
            elif self.rs_type == 'cubic':
                min_samples = (nvars**3 + 6*nvars**2 + 11*nvars + 6) / 6
            elif self.rs_type == 'rbf':
                min_samples = self.n_samples
            elif self.rs_type == 'kriging':
                min_samples = nvars
            else:
                msg = "Unknown RS type '%s'" % self.rs_type
                self.raise_exception(msg, ValueError)

            if self.n_samples < min_samples:
                self._logger.warning('Updating n_samples to minimum for RS: %d',
                             min_samples)
                self.n_samples = min_samples

            # Collect upper and lower bounds.
            xlb = []
            xub = []
            for i, mapping in enumerate(self.input_mappings):
                trait = self.get_trait(mapping[0]).trait_type
                if isinstance(trait, (Int, Float)):
                    low = trait.low
                    high = trait.high
                else:
                    msg = 'Unexpected input %d trait type %r' % (i, trait)
                    self.raise_exception(msg, ValueError)
                xlb.append(low)
                xub.append(high)

            self._midfi_model.Set(
                hifi=self._hifi_m4model,
                lofi=self._lofi_m4model,
                xlb=xlb,
                xub=xub,
                nd=nvars,
                nsamples=self.n_samples,
                nsamples_test=self.n_samples_test,
                nf=len(self.output_mappings),
                n_th_func=None,
                accu_test_type=self.accuracy_test_type,
                rs_type=self.rs_type,
                correct_func_type=self.correction_function,
                w_h=self.w_h,
                doe_type=self.doe_type,
                tolerance=self.tolerance,
                ntheta=self.ntheta)

            self._midfi_model.Construct_set_run_type(
                run_type=('doe', 'rs_fast'),
                iflag_doe=3,
                iflag_accu=1)

            self.need_updated_corrections = False

        vec = []
        for mapping in self.input_mappings:
            vec.append(self.get(mapping[0]))

        for i, mapping in enumerate(self.output_mappings):
            self._logger.debug('Running mid-fidelity model %s %d %s',
                       str(vec), i, str(mapping[0]))
            result = self._midfi_model.RunModel(vec, i)
            self._logger.debug('    result %s', str(result))
            setattr(self, mapping[0], result)

