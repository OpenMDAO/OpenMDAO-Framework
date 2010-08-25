from math import exp,log10,pi

from numpy import array,ones,argsort,min,sort,zeros,isnan
from scipy.optimize import fmin,fmin_cg,brute,anneal
from scipy.special import erf
from pyevolve import G1DList,GSimpleGA,GAllele,Consts
from pyevolve import Initializators,Mutators,Crossovers,Selectors

from enthought.traits.api import implements,Instance,Str

from openmdao.lib.traits.float import Float
from openmdao.lib.traits.array import Array
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator

from openmdao.main.expression import Expression
from openmdao.main.driver import Driver
from openmdao.main.interfaces import IHasParameters
from openmdao.main.hasparameters import HasParameters
from openmdao.main.case import Case
from openmdao.main.interfaces import ICaseIterator

from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters)  # this adds a member called _hasparameters of type HasParameters
class SingleCritEI(Driver):
    """Driver which implements the Expected Improvement(EI) process for single criteria problems. It uses components 
    which outputs are instances of NormalDistribution, combined with a provided optimal case, 
    to find the point in the design space with the best Expected Improvement."""
    
    implements(IHasParameters)
    
    best_case = Instance(ICaseIterator, iotype="in",
                    desc="CaseIterator which contains a single case, representing the criteria value")
    criteria = Expression(iotype="in",
                    desc="Name of the variable to maximize the expected improvement around. "
                          "Must be a NormalDistrubtion type")
    next_case = Instance(ICaseIterator, iotype="out", copy=None,
                    desc="CaseIterator which contains the case which maximizes expected improvement")
    
    EI = Float(0.0, iotype="out", desc="The expected improvement of the next_case")
    
    def __init__(self,*args,**kwargs):
        super(SingleCritEI,self).__init__(self,*args,**kwargs)
        self.set_of_alleles = []
        
    def _make_alleles(self):
        self.set_of_alleles = GAllele.GAlleles()
        for param in self.get_parameters().values(): 
            a = GAllele.GAlleleRange(param.low, param.high, real=True)
            self.set_of_alleles.add(a)
    
    def add_parameter(self,param_name,low=None,high=None):
        self._hasparameters.add_parameter(param_name,low,high)
        self._make_alleles()    
            
    def remove_parameter(self,param_name):
        self._hasparameters.remove_parameter(param_name)
        self._make_alleles()
            
    def _calc_ei(self, X): 
        """ calculates the expected improvement of the model at a given point, X """
        #set inputs to model
        
        self.set_parameters(X)
        #run the model    
        self.run_iteration()
        #get prediction, sigma
        obj = self.criteria.evaluate()
        
        mu = obj.mu
        sigma = obj.sigma
        
        target = self.target
        try:
            #(1./(2.**0.5))*((target-mu)/sigma)
            T1 = (target-mu)*.5*(1.+erf((target-mu)/(sigma*2.**.5)))
            T2 = sigma*((1./((2.*pi)**.05))*exp(-0.5*((target-mu)/sigma)**2.))
            return abs(T1+T2)
        except ValueError: 
            return 0.

        
    def execute(self): 
        """Optimize the Expected Improvement and calculate the next training point to run"""
        if self.criteria == "": 
            self.raise_exception("no criteria was specified",RuntimeError)
        elif not self.set_of_alleles:
            self.raise_exception("no parameters were added to the driver",RuntimeError)
            
        #TODO: This is not a good way to do this
        #grab the target criteria value out of the input best_case
        best_case = None
        for case in self.best_case:
            best_case = case
            break
        self.target = None
        for output in best_case.outputs: 
            if output[0] == self.criteria: 
                #TODO: check that criteria is only one thing, error if not
                self.target = output[2]
                break
                
        if not self.target: 
            self.raise_exception("best_case did not have an output which matched the criteria, '%s'"%self.criteria,ValueError)
        
        genome = G1DList.G1DList(len(self.set_of_alleles))
        genome.setParams(allele=self.set_of_alleles)
        genome.evaluator.set(self._calc_ei)
        genome.initializator.set(Initializators.G1DListInitializatorAllele)
        genome.mutator.set(Mutators.G1DListMutatorAllele)
        genome.crossover.set(Crossovers.G1DListCrossoverUniform)
        ga = GSimpleGA.GSimpleGA(genome)
        ga.setElitism(True)
        ga.selector.set(Selectors.GTournamentSelector)
        ga.setGenerations(10)
        ga.setPopulationSize(100)
        ga.setMinimax(Consts.minimaxType["maximize"])
        ga.evolve()
        bi = ga.bestIndividual()
        
        self.EI = bi.score
        new_x = array([x for x in bi])
        ins = [(name,None,value) for value,name in zip(new_x,self.get_parameters().keys())]    
        outs = [(self.criteria,None,None)]
        case = Case(inputs=ins,outputs=outs)
        #print "ei: ",self.parent.iter._iterations, self.EI   
        
        self.next_case = ListCaseIterator([case])
        #print "ei: ",self.next_case[0].inputs
        
