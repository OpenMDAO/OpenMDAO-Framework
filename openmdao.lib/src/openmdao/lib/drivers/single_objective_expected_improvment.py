from __future__ import division
from math import exp,log10,pi

from numpy import array,ones,argsort,min,sort,zeros,isnan
from scipy.optimize import fmin,fmin_cg,brute,anneal
from scipy.special import erf
from pyevolve import G1DList,GSimpleGA,GAllele,Consts
from pyevolve import Initializators,Mutators,Crossovers,Selectors

from enthought.traits.api import implements


from openmdao.lib.traits.float import Float
from openmdao.main.expression import Expression

from openmdao.main.exprEval import ExprEvaluator
from openmdao.main.driver import Driver
from openmdao.main.interfaces import IDriverParameter
from openmdao.main.driver_parameters import DriverParameters

class SingleObjectiveExpectedImprovement(Driver):
    
    target = Float(0,iotype="in",desc="value which the EI process should seek the objective too")
    implements(IDriverParameter)
    
    objective = Expression(iotype="in",desc="string representing the objective to maximize the expected improvement around. Must be a NormalDistrubtion type")
    
    def __init__(self,*args,**kwargs):
        super(SingleObjectiveExpectedImprovement,self).__init__(self,*args,**kwargs)
        
        self._parameters = DriverParameters()
    
        
    def add_parameter(self,param_name,low,high):
        self._parameters.add_parameter(param_name,low,high)
    def remove_parameter(self,param_name):
        self._parameters.remove_parameter(param_name)
    def list_parameters(self): 
        self._parameters.list_parameters()
    def clear_parameters(self):
        self._parameters.clear_parameters()
    
    def _calc_ei(self,X): 
        """ calculates the expect improvement of the model at a given point, X """
        #set inputs to model
        self._parameters.set_parameters(X)
        #run the model    
        self.run_iteration()
        #get prediction, sigma
        obj = self.objective.evaluate()
        
        mu = obj.mu
        sigma = obj.sigma
        
        try:
            T1 = (target-mu)*(0.5+0.5*erf((1./(2.**0.5))*((target-mu)/sigma)))
            T2 = sigma*((1./((2.*pi)**.05))*exp(-0.5*((target-mu)/sigma)**2.))
            return abs(T1+T2)
        except ValueError: 
            return 0
        
    def execute(self): 
        """Optimize the Expected Improvement and calculate the next training point to run"""
        
        set_of_alleles = GAllele.GAlleles()
        for param_name,param in self._parameters.iteritems(): 
            a = GAllele.GAlleleRange(param['low'],param['high'],real=True)
            set_of_allels.add(a)
        
        genome = 