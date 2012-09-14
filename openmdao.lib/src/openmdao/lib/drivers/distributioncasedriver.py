"""
   ``distributioncasedriver.py`` -- Driver that executes
          cases for distributions of points
          in the neighborhood of a given point.
"""

# pylint: disable-msg=E0611,F0401,E1101

from zope.interface import Attribute, Interface

# E0611 - name cannot be found in a module
# F0401 - Unable to import module
# E1101 - Used when a variable is accessed for an unexistent member
from openmdao.main.numpy_fallback import zeros

from openmdao.lib.datatypes.api import List, Str, Slot, Int, Enum, Bool
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.main.api import Container
from openmdao.main.case import Case
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters
from openmdao.main.interfaces import implements, IHasParameters


class IDistributionGenerator(Interface):
    """An iterator that returns lists of input
    values that are mapped from a single design
    point via some point distribution.
    """

    num_parameters = Attribute("number of parameters")

    def __iter__():
        """Return an iterator object where each iteration returns
        a set of values.
        """


class FiniteDifferenceGenerator(Container):
    """
    Generate the input cases for finite differences.
    """
    implements(IDistributionGenerator)

    num_parameters = Int(2, desc="Number of parameters, or dimensions")

    order = Int(1, desc="Order of the finite differences")

    form = Enum("CENTRAL", ["CENTRAL", "FORWARD", "BACKWARD"],
                desc="Form of finite difference used")

    skip_baseline = Bool(False,
                         desc="Set to True to skip running the baseline case.")

    def __init__(self, driver):
        super(FiniteDifferenceGenerator, self).__init__()
        self.driver = driver

    def __iter__(self):
        """Return an iterator over our sets of input values."""
        return self._get_input_values()

    def _get_input_values(self):
        '''Generator for the values'''

        params = self.driver.get_parameters().values()

        baseline = zeros(self.num_parameters, 'd')
        delta = zeros(self.num_parameters, 'd')
        mask = zeros(self.num_parameters, 'd')

        for i, param in enumerate(params):
            baseline[i] = param.evaluate()
            delta[i] = param.fd_step

        # baseline case
        if not self.skip_baseline:
            if not (self.form == "CENTRAL" and self.order % 2 == 1):
                yield baseline

        if self.form == "FORWARD":
            offset = 1
        elif self.form == "BACKWARD":
            offset = - self.order
        elif self.form == "CENTRAL":
            if self.order % 2 == 1:
                offset = (0.5 - self.order)
            else:
                offset = 1 - self.order

        # non-baseline cases for forward and backward
        if self.form in ["BACKWARD", "FORWARD"]:
            for iparam in range(self.num_parameters):
                mask[iparam] = 1.0
                for i in range(self.order):
                    var_val = baseline + (offset + i) * delta * mask
                    yield var_val
                mask[iparam] = 0.0
        else:  # for central form
            for iparam in range(self.num_parameters):
                mask[iparam] = 1.0
                if self.order % 2 == 1:
                    for i in range(self.order + 1):
                        var_val = baseline + (offset + i) * delta * mask
                        yield var_val
                else:
                    for i in range(self.order + 1):
                        if (offset + i) != 0:
                            var_val = baseline + (offset + i) * delta * mask
                            yield var_val
                mask[iparam] = 0.0


@add_delegate(HasParameters)
class DistributionCaseDriver(CaseIterDriverBase):
    """ Driver for evaluating models at point distributions. """

    implements(IHasParameters)

    distribution_generator = Slot(IDistributionGenerator,
                                  iotype='in', required=True,
                       desc='Iterator supplying values of point distribitions.')

    case_outputs = List(Str, iotype='in',
                           desc='A list of outputs to be saved with each case.')

    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()

    def _get_cases(self):
        """Iterator over the cases"""

        params = self.get_parameters().values()
        self.distribution_generator.num_parameters = len(params)

        for row in self.distribution_generator:
            case = self.set_parameters(row, Case(parent_uuid=self._case_id))
            case.add_outputs(self.case_outputs)

            yield case
