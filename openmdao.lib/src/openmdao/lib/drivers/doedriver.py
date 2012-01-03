"""
    ``doedriver.py`` -- Driver that executes a Design of Experiments.
    
"""

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import ListStr, Slot,Float

from openmdao.main.case import Case
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters

@add_delegate(HasParameters)

class DOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments within a specified neighborhood around a point"""
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                          desc='Iterator supplying normalized DOE values.')
    
    case_outputs = ListStr([], iotype='in',desc='A list of outputs to be saved with each case.')
    
    alpha=Float(1., iotype='in',desc='Multiplicative factor for neighborhood DOE Driver')
    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        
        M=[]
        P=[]
        for p in params:
            temp= p.evaluate()
            P.append(temp)
            M.append((temp-p.low)/(p.high-p.low))
        for row in list(self.DOEgenerator)+[tuple(M)]:
            vals=[]
            for p,val,curval in zip(params,row,P):
                newval=(p.low+(p.high-p.low)*val-curval)*self.alpha+curval

                if newval<p.low:
                    newval=p.low
                elif newval>p.high:
                    newval=p.high
                vals.append(newval)
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))

            # now add events
            for varname in self.get_events(): 
                case.add_input(varname,True)
            case.add_outputs(self.case_outputs)              
            yield case
