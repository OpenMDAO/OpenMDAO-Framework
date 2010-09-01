"""
    doedriver.py -- Driver that executes a Design of Experiments.
"""

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import ListStr, Instance

from openmdao.main.case import Case
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters


@add_delegate(HasParameters)
class DOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments """
    
    def __init__(self, *args, **kwargs):
        super(DOEdriver, self).__init__(*args, **kwargs)
    
    # pylint: disable-msg=E1101
    DOEgenerator = Instance(IDOEgenerator, iotype='in', required=True,
                            desc='Iterator supplying normalized DOE values')
    
    case_outputs = ListStr([], iotype='in', 
                           desc='A list of outputs to be saved with each case.')
    
    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        if self.DOEgenerator.num_parameters != len(params):
            self.raise_exception('Number of DOE values (%s) != number of '
                                 'parameters (%s).'%
                (self.DOEgenerator.num_parameters, len(params)), ValueError)
        for row in self.DOEgenerator:
            inputs = []
            for val, parameter in zip(row, params):
                
                #convert DOE values to variable values
                value = parameter.low+(parameter.high-parameter.low)*val
                if '[' in parameter.expreval:
                    raise ValueError('Array entry design vars '
                                     'not supported yet.')
                else:
                    inputs.append((str(parameter.expreval), None, value))
            
            # now add any event variables
            for varname in self.get_events():
                inputs.append((varname, None, True))

            outputs = [(x, None, None) for x in self.case_outputs]
            yield Case(inputs=inputs, outputs=outputs)
            
