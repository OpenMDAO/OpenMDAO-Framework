import csv

from openmdao.main.datatypes.api import Int, Str
from openmdao.main.interfaces import implements, IDOEgenerator
from openmdao.main.api import Container


class CSVFile(Container):
    """
    DOEgenerator that returns rows in a CSV file.
    Plugs into the DOEgenerator socket on a DOEdriver.
    """

    implements(IDOEgenerator)

    num_parameters = Int(0, iotype='in',
                         desc='Expected number of parameters in the DOE')

    doe_filename = Str('', iotype='in', desc='Name of CSV file.')

    def __init__(self, doe_filename='doe_inputs.csv', *args, **kwargs):
        super(CSVFile, self).__init__(*args, **kwargs)
        self.doe_filename = doe_filename

    def __iter__(self):
        """ Return an iterator over our sets of input values. """
        return self._next_row()

    def _next_row(self):
        """ Generate float values from CSV file. """
        inp = open(self.doe_filename, 'rb')
        num_params = self.num_parameters
        for i, row in enumerate(csv.reader(inp)):
            if len(row) != num_params:
                raise RuntimeError('%s line %d: expected %d parameters, got %d'
                                   % (self.doe_filename, i + 1,
                                      num_params, len(row)))
            yield [float(val) for val in row]
