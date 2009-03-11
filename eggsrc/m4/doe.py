"""
Partial! M4 Design Of Experiments driver.
Consider this only as a hint as to how this might be done.
The code here has only been developed to test feasibility,
and was written by someone without much 'mool' knowledge.
"""

__all__ = ('DOE',)
__version__ = '0.1'

import mool.Optimization.DOE

from openmdao.main import Driver, Int, String, Case, ListCaseIterator
from openmdao.main.component import RUN_OK, RUN_FAILED
from openmdao.main.variable import INPUT

class FakeROSE(Driver):
    """
    Just a stand-in until we have a concurrent-capable driver.
    The intent is that a set of cases to be run is provided by
    an ICaseIterator, and the system does its best to run those cases.
    Other than that, don't assume this API is valid for the real code.
    """

    def __init__(self, name='FakeROSE', parent=None):
        super(FakeROSE, self).__init__(name, parent)
        self.iterator = None
        self.outerator = None

    def execute(self):
        """ Run each case in iterator. """
        self.outerator = []
        for case in self.iterator:
            for name, index, value in case.inputs:
                try:
                    self.parent.set(name, value, index)
                except Exception, exc:
                    msg = "Exception setting '%s': %s" % (name, str(exc))
                    self.error(msg)
                    self.outerator.append(Case(case.inputs, None,
                                               RUN_FAILED, msg))
                    break
            else:
                status = self.parent.workflow.run()
                if status == RUN_OK:
                    results = []
                    for name, index, value in case.outputs:
                        try:
                            value = self.parent.get(name, index)
                            results.append((name, index, value))
                        except Exception, exc:
                            msg = "Exception getting '%s': %s" % (name, str(exc))
                            self.error(msg)
                            self.outerator.append(Case(case.inputs, None,
                                                       RUN_FAILED, msg))
                            break
                    else:
                        self.outerator.append(Case(case.inputs, results,
                                                   RUN_OK, ''))
                else:
                    self.outerator.append(Case(case.inputs, None, status, ''))
        return RUN_OK


class DOE(FakeROSE):
    """ M4 Design Of Experiments driver. """

    def __init__(self, name='M4_DOE', parent=None):
        super(DOE, self).__init__(name, parent)
        self.design_variables = []    # List of (name, min, max) tuples.
        self.response_variables = []  # List of names.

        # No 'Option' variable (yet?).
        String('type', self, INPUT, default='ccd',
               desc='Type of experiment design.')

        Int('n_samples', self, INPUT, default=1, min_limit=1,
            desc='Number of samples.')

        Int('lhs', self, INPUT, default=2, min_limit=1,
            desc='???, used by LHS and Rand_LHS.')

    def execute(self):
        """ Generate cases and run them. """
        cases = self.generate_cases()
        if cases is None:
            return RUN_FAILED
        else:
            self.iterator = ListCaseIterator(cases)
        return super(DOE, self).execute()


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

