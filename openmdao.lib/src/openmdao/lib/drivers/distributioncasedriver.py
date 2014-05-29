"""
   ``distributioncasedriver.py`` -- Driver that executes
          cases for distributions of points
          in the neighborhood of a given point.
"""

# pylint: disable-msg=E0611,F0401,E1101

from zope.interface import Interface

# E0611 - name cannot be found in a module
# F0401 - Unable to import module
# E1101 - Used when a variable is accessed for an unexistent member
from numpy import zeros

from openmdao.main.api import Container
from openmdao.main.datatypes.api import Slot, Int, Enum, Bool
from openmdao.main.interfaces import implements
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver


class IDistributionGenerator(Interface):
    """An iterator that returns lists of input
    values that are mapped from a single design
    point via some point distribution.
    """

    def __iter__():
        """Return an iterator object where each iteration returns
        a set of values.
        """


class FiniteDifferenceGenerator(Container):
    """
    Generate the input cases for finite differences.
    """
    implements(IDistributionGenerator)

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

        baseline = self.driver.eval_parameters()
        delta = self.driver.get_fd_steps()
        mask = zeros(baseline.size, 'd')

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
            for iparam in range(len(mask)):
                mask[iparam] = 1.0
                for i in range(self.order):
                    var_val = baseline + (offset + i) * delta * mask
                    yield var_val
                mask[iparam] = 0.0
        else:  # for central form
            for iparam in range(len(mask)):
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


class DistributionCaseDriver(CaseIteratorDriver):
    """ Driver for evaluating models at point distributions. """

    distribution_generator = Slot(IDistributionGenerator,
                                  required=True,
                       desc='Iterator supplying values of point distribitions.')

    def execute(self):
        """Generate and evaluate cases."""
        self.set_inputs(self.distribution_generator)
        super(DistributionCaseDriver, self).execute()

