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

from openmdao.main.exprEval import ExprEvaluator
from openmdao.main.driver import Driver
from openmdao.main.interfaces import IDriverParameter
from openmdao.main.driver_parameters import DriverParameters
from openmdao.main.case import Case

from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator

class MuliObjectiveExpectedImprovement(Driver):

    objectives = Str("",iotype="in",desc="names of the output from cases to be used as the objectives")
    best_cases = Instance(ICaseIterator,iotype="in",desc="CaseIterator which contains pareto optimal cases, representing the target objective values")
    infill = Str(["EI","PI"],iotype="in",desc="infill criterion about which to maximize")
    next_case = Instance(ICaseIterator,iotype="out",desc="CaseIterator which contains the case that maximizes specified infill criterion")
    
    implements(IDriverParameter)

    objective = Expression(iotype="in",desc="string representing the objectives about which the infill criterion is maximized. Must be a NormalDistrubtion type")
    
    def __init__(self,*args,**kwargs):
        super(MultiObjectiveExpectedImprovement,self).__init__(self,*args,**kwargs)
        
        self._parameters = DriverParameters()
    
        
    def add_parameter(self,param_name,low,high):
        self._parameters.add_parameter(param_name,low,high)
        
        self.set_of_alleles = GAllele.GAlleles()
        for param_name,param in self._parameters.iteritems(): 
            a = GAllele.GAlleleRange(param['low'],param['high'],real=True)
            self.set_of_allels.add(a)
            
    def remove_parameter(self,param_name):
        self._parameters.remove_parameter(param_name)
    def list_parameters(self): 
        self._parameters.list_parameters()
    def clear_parameters(self):
        self._parameters.clear_parameters()
    
    def _mcpi(self,mu,sigma):
        """Calculates the multi-criteria probability of improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point"""
        
        #pull out values of objectives from best_cases        
        y_star = self.y_star
        
        PI1 = (0.5+0.5*erf((1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))
        PI3 = (1-(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0]))))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))
     
        PI2 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                PI2=PI2+((0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -(0.5+0.5*erf((1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0]))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))

        return PI1+PI2+PI3
    
    def _mcei(self,mu,sigma):
        """Calculates the multi-criteria expected improvement
        for a new point with two responses. Takes as input a 
        pareto frontier, mean and sigma of new point"""

        #pull out values of objectives from best_cases
        y_star = self.y_star

        ybar11 = mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][0]-mu[0])**2/sigma[0]**2))
        ybar13 = (mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][0]-mu[0])**2/sigma[0]**2)))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))
        
        ybar12 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                ybar12 = ybar12+((mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][0]-mu[0])**2/sigma[0]**2)))\
                -(mu[0]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][0]-mu[0])**2/sigma[0]**2))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))

        ybar1 = (ybar11+ybar12+ybar13)/PI
        
        ybar21 = mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[0][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][1]-mu[1])**2/sigma[1]**2))
        ybar23 = (mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][1]-mu[1])**2/sigma[1]**2)))\
        *(0.5+0.5*erf((1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))

        ybar22 = 0
        if len(y_star)>1:
            for i in range(len(y_star)-1):
                ybar22 = ybar22+((mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][1]-mu[1])**2/sigma[1]**2)))\
                -(mu[1]*(0.5+0.5*erf((1/(2**0.5))*((y_star[i][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][1]-mu[1])**2/sigma[1]**2))))\
                *(0.5+0.5*erf((1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))
        
        ybar2 = (ybar21+ybar22+ybar23)/self.PI
        dists = [((ybar1-point[0])**2+(ybar2-point[1])**2)**0.5 for point in y_star]
        mcei = self.PI*min(dists)
        if isnan(mcei):
            mcei = 0
        return mcei
    
    def _calc_infill(self, X): 
        """ calculates either PI or EI of the model at a given point, X """
        #set inputs to model
        self._parameters.set_parameters(X)
        #run the model    
        self.run_iteration()
        #get prediction, sigma from each output of the metamodel
        obj = self.objective.evaluate()
        
        mu = obj.mu
        sigma = obj.sigma
                
        target = self.target        
        
        self.PI = self._mcpi(mu,sigma)
        if self.infill == "EI":
            return _mcei(mu,sigma)
        if self.infill == "PI":
            return self.PI
        
        
    def execute(self): 
        """Optimize the infill criterion and return the next point to run"""
        
        #TODO: This is not a good way to do this
        #grab the target objective value out of the input best_cases
        #for case in self.best_cases: 
        #    best_case = case
        #    break
        #for output in best_cases.outputs: 
        #    if output[0] == self.objective:
        #        self.target = output[2]
        #        break
        
        #Get objective values of pareto optimal points 
        #self.y_star = values of objectives of cases listed in best_cases
        #arrange cases of y_star to be increasing along first objective
        #self.y_star = array(self.y_star)[array([i[0] for i in self.y_star]).argsort()]

        genome = G1DList.G1DList(len(self.set_of_alleles))
        genome.setParams(allele=self.set_of_alleles)
        genome.evaluator.set(self._calc_infill)
        genome.initializator.set(Initializators.G1DListInitializatorAllele)
        genome.mutator.set(Mutators.G1DListMutatorAllele)
        genome.crossover.set(Crossovers.G1DListCrossoverUniform)
        ga = GSimpleGA.GSimpleGA(genome)
        ga.setElitism(True)
        ga.setGenerations(25)
        ga.setPopulationSize(75)
        ga.setMinimax(Consts.minimaxType["maximize"])
        ga.evolve()
        new_x = array([x for x in ga.bestIndividual()])
        
        case = Case(inputs=[(name,None,value) for value,name in zip(new_x,self._parameters.list(parameters))])
        self.next_case = ListCaseIterator([case,])