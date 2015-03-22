"""
.. _`DOEdriver.py`:

``doedriver.py`` -- Driver that executes a Design of Experiments.

"""

import csv
import numpy as np

# pylint: disable-msg=E0611,F0401
from openmdao.main.hasparameters import ParameterGroup
from openmdao.main.datatypes.api import Bool, Slot, Float, Str
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver

def check_parameter(parameter):
    try:
        if parameter.vartypename == 'Array':
            if 'float' not in parameter.valtypename:
                msg = "DOEdriver cannot add array parameter '{}'" \
                      " because target is not of type 'numpy.float'."
                msg = msg.format(parameter.name)

                raise TypeError(msg)

        elif not parameter.vartypename == 'Float':
            msg = "DOEdriver cannot add parameter '{}'" \
                  " because target is not of type 'Float'."
            msg = msg.format(parameter.name)

            raise TypeError(msg)

    except AttributeError as exception:
        if not parameter.typename == 'float':
            msg = "DOEdriver cannot add parameter group '{}'" \
                  " because targets are not of type 'float'."

            msg = msg.format(parameter.name)

            raise TypeError(msg)

class DOEdriver(CaseIteratorDriver):
    """ Driver for Design of Experiments. """

    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, required=True,
                        desc='Iterator supplying normalized DOE values.')

    record_doe = Bool(True, iotype='in',
                      desc='Record normalized DOE values to CSV file.')

    doe_filename = Str('', iotype='in',
                       desc='Name of CSV file to record to'
                            ' (default is <driver-name>.csv).')

    def add_parameter(self, target, low=None, high=None,
                      scaler=None, adder=None, start=None,
                      fd_step=None, name=None, scope=None):
        super(DOEdriver, self).add_parameter(target, low, high, scaler, adder,
            start, fd_step, name, scope, case_inputs_iotype='out')

        parameters = self.get_parameters()

        try:
            parameter = parameters[target]
        except KeyError as exception:
            try:
                parameter = parameters[name]
            except KeyError as exception:
                parameter = target

        try:
            check_parameter(parameter)
        except TypeError as type_error:
            try:
                self.remove_parameter(parameter.name)
            except AttributeError as attr_error:
                self.remove_parameter(tuple(parameter.targets))
                self.raise_exception(type_error.message, type_error.__class__)


    def execute(self):
        """Generate and evaluate cases."""
        self.set_inputs(self._get_cases())
        self._csv_file = None
        try:
            super(DOEdriver, self).execute()
        finally:
            if self._csv_file is not None:
                self._csv_file.close()

    def _get_cases(self):
        """Generate each case."""
        self.DOEgenerator.num_parameters = self.total_parameters()
        record_doe = self.record_doe
        if record_doe:
            if not self.doe_filename:
                self.doe_filename = '%s.csv' % self.name
            self._csv_file = open(self.doe_filename, 'wb')
            csv_writer = csv.writer(self._csv_file)

        lower = self.get_lower_bounds()
        delta = self.get_upper_bounds() - lower

        for row in self.DOEgenerator:
            if record_doe:
                csv_writer.writerow(['%.16g' % val for val in row])
            vals = lower + delta*row
            yield vals

        if record_doe:
            self._csv_file.close()
            self._csv_file = None


class NeighborhoodDOEdriver(CaseIteratorDriver):
    """Driver for Design of Experiments within a specified neighborhood
    around a point."""

    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, required=True,
                        desc='Iterator supplying normalized DOE values.')

    alpha = Float(.3, low=.01, high=1.0, iotype='in',
                  desc='Multiplicative factor for neighborhood DOE Driver.')

    beta = Float(.01, low=.001, high=1.0, iotype='in',
                 desc='Another factor for neighborhood DOE Driver.')

    def execute(self):
        """Generate and evaluate cases."""
        self.set_inputs(self._get_cases())
        super(NeighborhoodDOEdriver, self).execute()

    def _get_cases(self):
        """Generate each case."""
        self.DOEgenerator.num_parameters = self.total_parameters()

        upper = self.get_upper_bounds()
        lower = self.get_lower_bounds()
        P = self.eval_parameters()
        M = (P - lower) / (upper - lower)

        for row in list(self.DOEgenerator)+[tuple(M)]:
            delta_low = P - lower
            k_low = 1.0/(1.0+(1-self.beta)*delta_low)
            new_low = P - self.alpha*k_low*delta_low#/(self.exec_count+1)

            delta_high = upper - P
            k_high = 1.0/(1.0+(1-self.beta)*delta_high)
            new_high = P + self.alpha*k_high*delta_high#/(self.exec_count+1)

            vals = new_low + (new_high-new_low)*row
            yield vals
