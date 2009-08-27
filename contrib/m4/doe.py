"""
Partial! M4 Design Of Experiments driver.
Consider this only as a hint as to how this might be done.
The code here has only been developed to test feasibility,
and was written by someone without much 'mool' knowledge.
"""

__all__ = ('DOE',)


from enthought.traits.api import Str, Range

import mool.Optimization.DOE

from openmdao.main.api import Case, ListCaseIterator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver


class DOE(CaseIteratorDriver):
    """ M4 Design Of Experiments driver. """

    # No 'Option' variables yet.
    type = Str('ccd', iostatus='in', desc='Type of experiment design.')
    n_samples = Range(value=1, low=1, iostatus='in', desc='Number of samples.')
    lhs = Range(value=2, low=1, iostatus='in',
                desc='???, used by LHS and Rand_LHS.')

    def __init__(self, *args, **kwargs):
        super(DOE, self).__init__(*args, **kwargs)
        self.design_variables = []    # List of (name, min, max) tuples.
        self.response_variables = []  # List of names.

    def _pre_execute(self):
        """ Generate cases. """
        cases = self.generate_cases()
        if cases is None:
            self.raise_exception('No cases generated', RuntimeError)
        self.iterator = ListCaseIterator(cases)
        self.outerator = []
        super(DOE, self)._pre_execute()


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

    def generate_cases(self):
        """ Generate cases to be run based on configuration. """
        nvars = len(self.design_variables)
        if self.type == 'ccd':
            # CCD requires an exact value
            if nvars == 1:
                min_samples = 3
            else:
                min_samples = 2**nvars + 2*nvars + 1
            if self.n_samples != min_samples:
                self.warning('Setting n_samples to CCD required value: %d' % \
                             min_samples)
            self.n_samples = min_samples
        elif self.type == 'lhs':
            min_samples = nvars
        elif self.type == 'rand_lhs':
            min_samples = nvars
        elif self.type == 'oa2':
            min_samples = (nvars-1)**2
        elif self.type == 'oa3':
            min_samples = (nvars-1)**3
        else:
            self.error("Unknown type '%s'" % self.type)
            return None

        if self.n_samples < min_samples:
            self.warning('Updating n_samples to minimum: %d' % min_samples)
            self.n_samples = min_samples

        xmin = []
        xmax = []
        for name, min_val, max_val in self.design_variables:
            xmin.append(min_val)
            xmax.append(max_val)

        doe = mool.Optimization.DOE.DOE(xmin, xmax, self.type, self.n_samples,
                                        self.lhs)
        sample_points = doe.x
        cases = []
        for point in sample_points:
            inputs = []
            for i, var in enumerate(self.design_variables):
                inputs.append((var[0], None, point[i]))
            outputs = []
            for var in self.response_variables:
                outputs.append((var, None, None))
            cases.append(Case(inputs, outputs))
        return cases

