"""
.. _`DOEdriver.py`:
   
``doedriver.py`` -- Driver that executes a Design of Experiments.
    
"""

import csv

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Bool, List, Slot, Float, Str

from openmdao.main.case import Case
from openmdao.main.interfaces import IDOEgenerator, ICaseFilter, implements, \
                                     IHasParameters
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters


@add_delegate(HasParameters)
class DOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments. """

    implements(IHasParameters)
    
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                        desc='Iterator supplying normalized DOE values.')

    record_doe = Bool(True, iotype='in',
                      desc='Record normalized DOE values to CSV file.')

    doe_filename = Str('', iotype='in',
                       desc='Name of CSV file to record to'
                            ' (default is <driver-name>.csv).')

    case_outputs = List(Str, iotype='in', 
                        desc='A list of outputs to be saved with each case.')

    case_filter = Slot(ICaseFilter, iotype='in',
                       desc='Selects cases to be run.')

    def execute(self):
        """Generate and evaluate cases."""
        self._csv_file = None
        try:
            super(DOEdriver, self).execute()
        finally:
            if self._csv_file is not None:
                self._csv_file.close()

    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        """Generate each case."""
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        record_doe = self.record_doe
        events = self.get_events()
        outputs = self.case_outputs
        case_filter = self.case_filter

        if record_doe:
            if not self.doe_filename:
                self.doe_filename = '%s.csv' % self.name
            self._csv_file = open(self.doe_filename, 'wb')
            csv_writer = csv.writer(self._csv_file)

        for i, row in enumerate(self.DOEgenerator):
            if record_doe:
                csv_writer.writerow(['%.16g' % val for val in row])
            vals = [p.low+(p.high-p.low)*val for p,val in zip(params,row)]
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))
            # now add events
            for varname in events: 
                case.add_input(varname, True)
            case.add_outputs(outputs)    
            if case_filter is None or case_filter.select(i, case):
                yield case

        if record_doe:
            self._csv_file.close()
            self._csv_file = None


@add_delegate(HasParameters)            
class NeighborhoodDOEdriver(CaseIterDriverBase):
    """Driver for Design of Experiments within a specified neighborhood
    around a point."""
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                          desc='Iterator supplying normalized DOE values.')
    
    case_outputs = List(Str, iotype='in',
                           desc='A list of outputs to be saved with each case.')
    
    alpha = Float(.3, low=.01, high =1.0, iotype='in',
                  desc='Multiplicative factor for neighborhood DOE Driver.')
    
    beta = Float(.01, low=.001, high=1.0, iotype='in',
                 desc='Another factor for neighborhood DOE Driver.')

    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        
        M = []
        P = []

        for p in params:
            temp = p.evaluate()
            P.append(temp)
            M.append((temp-p.low)/(p.high-p.low))

        for row in list(self.DOEgenerator)+[tuple(M)]:
            vals = []
            for p,val,curval in zip(params, row, P):                
                delta_low = curval-p.low
                k_low = 1.0/(1.0+(1-self.beta)*delta_low)
                new_low = curval - self.alpha*k_low*delta_low#/(self.exec_count+1)

                delta_high = p.high-curval
                k_high = 1.0/(1.0+(1-self.beta)*delta_high)
                new_high = curval + self.alpha*k_high*delta_high#/(self.exec_count+1)

                newval = new_low+(new_high-new_low)*val

                vals.append(newval)
                
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))
            # now add events
            for varname in self.get_events(): 

                case.add_input(varname, True)
            case.add_outputs(self.case_outputs)
            
            yield case            
