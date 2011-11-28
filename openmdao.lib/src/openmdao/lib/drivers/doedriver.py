"""
    ``doedriver.py`` -- Driver that executes a Design of Experiments.
    
"""

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import ListStr, Slot

from openmdao.main.case import Case
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters


@add_delegate(HasParameters)
class DOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments """
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                          desc='Iterator supplying normalized DOE values.')
    
    case_outputs = ListStr([], iotype='in', 
                           desc='A list of outputs to be saved with each case.')
    
    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        
        for row in self.DOEgenerator:
            vals = [p.low+(p.high-p.low)*val for p,val in zip(params,row)]
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))
            # now add events
            for varname in self.get_events(): 
                case.add_input(varname,True)
            case.add_outputs(self.case_outputs)    
            
            yield case
