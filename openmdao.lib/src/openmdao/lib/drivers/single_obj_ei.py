from __future__ import division
from math import exp,log10,pi

from numpy import array,ones,argsort,min,sort,zeros,isnan
from scipy.optimize import fmin,fmin_cg,brute,anneal
from scipy.special import erf
from pyevolve import G1DList,GSimpleGA,GAllele,Consts
from pyevolve import Initializators,Mutators,Crossovers,Selectors

from enthought.traits.api import implements,Instance,Str


from openmdao.lib.traits.float import Float

from openmdao.main.expression import Expression

from openmdao.main.driver import Driver
from openmdao.main.interfaces import IHasParameters
from openmdao.main.driver_parameters import HasParameters
from openmdao.main.case import Case

from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator

class SingleObjectiveExpectedImprovement(Driver):
    implements(IHasParameters)
     
    best_case = Instance(ICaseIterator, iotype="in",
                         desc="CaseIterator which containes a single case, representing the target objective value")
    next_case = Instance(ICaseIterator, iotype="out",
                         desc="CaseIterator which contains the case which maximize expected improvement")
    
    criteria = Expression(iotype="in",
                           desc="name of the variable to maximize the expected improvement around. Must be a NormalDistrubtion type")
    
    def __init__(self,*args,**kwargs):
        super(SingleObjectiveExpectedImprovement,self).__init__(self,*args,**kwargs)
        
        self._parameters = HasParameters()
    
        
    def add_parameter(self,param_name,low,high):
        self._parameters.add_parameter(param_name,low=None,high=None)
        
        self.set_of_alleles = GAllele.GAlleles()
        for param_name,param in self._parameters.iteritems(): 
            a = GAllele.GAlleleRange(param['low'],param['high'],real=True)
            self.set_of_alleles.add(a)
            
    def remove_parameter(self,param_name):
        self._parameters.remove_parameter(param_name)
        
    def list_parameters(self): 
        self._parameters.list_parameters()
        
    def clear_parameters(self):
        self._parameters.clear_parameters()
    
    def _calc_ei(self, X): 
        """ calculates the expected improvement of the model at a given point, X """
        #set inputs to model
        self._parameters.set_parameters(X)
        #run the model    
        self.run_iteration()
        #get prediction, sigma
        obj = self.objective.evaluate()
        
        mu = obj.mu
        sigma = obj.sigma
                
        target = self.target        
        
        try:
            T1 = (target-mu)*(0.5+0.5*erf((1./(2.**0.5))*((target-mu)/sigma)))
            T2 = sigma*((1./((2.*pi)**.05))*exp(-0.5*((target-mu)/sigma)**2.))
            return abs(T1+T2)
        except ValueError: 
            return 0
        
    def execute(self): 
        """Optimize the Expected Improvement and calculate the next training point to run"""
        
        #TODO: This is not a good way to do this
        #grab the target objective value out of the input best_case
        for case in self.best_case: 
            best_case = case
            break
        for output in best_case.outputs: 
            if output[0] == self.objective:
                self.target = output[2]
                break
        
        genome = G1DList.G1DList(len(self.set_of_alleles))
        genome.setParams(allele=self.set_of_alleles)
        genome.evaluator.set(self._calc_ei)
        genome.initializator.set(Initializators.G1DListInitializatorAllele)
        genome.mutator.set(Mutators.G1DListMutatorAllele)
        genome.crossover.set(Crossovers.G1DListCrossoverUniform)
        ga = GSimpleGA.GSimpleGA(genome,seed=2)
        #ga.setElitism(True)
        #ga.selector.set(Selectors.GTournamentSelector)
        ga.setGenerations(25)
        ga.setPopulationSize(75)
        ga.setMinimax(Consts.minimaxType["maximize"])
        ga.evolve()
        new_x = array([x for x in ga.bestIndividual()])
        
        case = Case(inputs=[(name,None,value) for value,name in zip(new_x,self._parameters.list(parameters))])
        self.next_case = ListCaseIterator([case,])
        
