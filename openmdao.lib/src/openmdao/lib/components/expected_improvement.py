"""Expected Improvement calculation for one or more objectives.""" 

import logging
try:
    from numpy import exp, abs, pi
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
_check = ['numpy']
try:
    from math import erf   # py27 and later has erf in the math module
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    try:
        from scipy.special import erf
    except ImportError as err:
        logging.warn("In %s: %r" % (__file__, err))
        _check.append('scipy')

from openmdao.main.datatypes.api import Slot, Str, Float
from openmdao.lib.casehandlers.api import CaseSet

from openmdao.main.api import Component
from openmdao.util.decorators import stub_if_missing_deps

from openmdao.main.uncertain_distributions import NormalDistribution

@stub_if_missing_deps(*_check)
class ExpectedImprovement(Component):
    best_case = Slot(CaseSet, iotype="in",
                       desc="CaseSet which contains a single case "
                            "representing the criteria value.", required=True)
    
    criteria = Str(iotype="in",
                   desc="Name of the variable to maximize the expected "
                        "improvement around. Must be a NormalDistrubtion type.")
    
    predicted_value = Slot(NormalDistribution,iotype="in",
                             desc="The Normal Distribution of the predicted value "
                                  "for some function at some point where you wish to"
                                  " calculate the EI.")
    
    EI = Float(0.0, iotype="out", 
               desc="The expected improvement of the predicted_value.")
    
    PI = Float(0.0, iotype="out", 
               desc="The probability of improvement of the predicted_value.")
    
    def execute(self): 
        """ Calculates the expected improvement of the model at a given point.
        """
        
        mu = self.predicted_value.mu
        sigma = self.predicted_value.sigma
        best_case = self.best_case[0]
        try: 
            target = best_case[self.criteria]
        except KeyError: 
            self.raise_exception("best_case did not have an output which "
                                 "matched the criteria, '%s'"%self.criteria,
                                 ValueError)  
        try:
            
            self.PI = 0.5+0.5*erf((1/2**.5)*(target-mu/sigma))
            
            T1 = (target-mu)*.5*(1.+erf((target-mu)/(sigma*2.**.5)))
            T2 = sigma*((1./((2.*pi)**.05))*exp(-0.5*((target-mu)/sigma)**2.))
            self.EI = abs(T1+T2)

        except (ValueError,ZeroDivisionError): 
            self.EI = 0
            self.PI = 0            
            
    
    
