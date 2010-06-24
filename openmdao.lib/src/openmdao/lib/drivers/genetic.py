"""A simple Pyevolve-based driver for OpenMDAO"""

import random
import re

from numpy import int32,int64,float32,float64

from enthought.traits.api import Python

from pyevolve import G1DList, G1DBinaryString, G2DList, GAllele, GenomeBase
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

from openmdao.main.api import Driver, ExprEvaluator, set_as_top, Component, Assembly,Expression
from openmdao.lib.api import Float, Int, Enum, Array,Bool, Instance

array_test = re.compile("(\[[0-9]+\])+$")

class Genetic(Driver):
    """Genetic algorithm for the OpenMDAO frameowork, based on the Pyevolve Genetic algorithm module. 
    """
    objective = Expression(iotype='in',
                          desc='A string containing the objective function expression to be optimized') 
    
    opt_type = Enum("minimize", values=["minimize","maximize"],
                    iotype="in",
                    desc="sets the optimization to either minimize or maximize the objective function")
    
    generations = Int(Consts.CDefGAGenerations,iotype="in",
                      desc="the maximum number of generations the algorithm will "
                      "evolve to before stopping")
    population_size = Int(Consts.CDefGAPopulationSize, iotype="in",
                          desc = "the size of the population in each generation")
    crossover_rate = Float(Consts.CDefGACrossoverRate, iotype="in",low=0.0,high=1.0,
                           desc="the crossover rate used when two parent genomes repoduce to form a "
                           "child genome")
    mutation_rate = Float(Consts.CDefGAMutationRate, iotype="in",low=0.0,high=1.0,
                           desc="the mutation rate applied to population members")
    
    selection_method = Enum("roulette_wheel",("roulette_wheel","tournament","rank","uniform"),
                         desc="the selection method used to pick population members who will survive for "
                         "breeding into the next generation",
                         iotype="in")
    _selection_mapping = {"roulette_wheel":Selectors.GRouletteWheel,
                          "tournament":Selectors.GTournamentSelector,
                          "rank":Selectors.GRankSelector,
                          "uniform":Selectors.GUniformSelector}
    
    elitism = Bool(False,iotype="in",desc="controls the use of elitism in the createion of new "
                   "generations.")
    
    best_individual = Instance(klass = GenomeBase.GenomeBase, iotype="out", desc="the genome with the "
                               "best score from the optimization") 
    
    seed = Int(None,iotype="in",
               desc="Random seed for the optimizer. Set to a specific value for repeatable "
               "results, otherwise leave as None for truely random seeding")
    
    def __init__(self,doc=None):
        super(Genetic,self).__init__(doc)
        
        self._parameters = []
        self._parameter_ranges = dict()
    
    def _make_alleles(self): 
        """ Returns a GAllelle.Galleles instance with alleles corresponding to 
        the parameters specified by the user"""
        alleles = GAllele.GAlleles()
        for str_ref in self._parameters:
            val = str_ref.evaluate() #now grab the value 
            ref = str(str_ref)
            
            #split up the ref string to be able to get the trait. 
            path = ".".join(ref.split(".")[0:-1]) #get the path to the object
            target = ref.split(".")[-1] #get the last part of the string after the last "."
            
            low,high = self._parameter_ranges[ref]
            
            #bunch of logic to check for array elements being passed as refs
            
            obj = getattr(self.parent,path)
           
            t = obj.trait(target) #get the trait
                                 
            if (t and (t.is_trait_type(Float) or t.is_trait_type(Python))) or (array_test.search(target) and isinstance(val,float)):
                allele = GAllele.GAlleleRange(begin=low,end=high,real=True)
                alleles.add(allele)
                
            elif (t and (t.is_trait_type(Int) or t.is_trait_type(Python))) or (array_test.search(target) and isinstance(val,int)):
                allele = GAllele.GAlleleRange(begin=low,end=high,real=False)
                alleles.add(allele)                
                    
            elif t and t.is_trait_type(Enum): 
                allele = GAllele.GAlleleList(t.values)
                alleles.add(allele)
        
        return alleles
    
    def remove_parameter(self,param_name):
        """removes the specified parameter
        
        param_name : str
            name of the parameter to remove from the driver
        """
        
        try:
            i = [str(x) for x in self._parameters].index(param_name)
        except ValueError:
            self.raise_exception("Trying to remove parameter '%s', but it is not in the genetic driver"%param_name,RuntimeError)
        self._parameters.pop(i)
        self._parameter_ranges.pop(param_name)
        return True
    
    def list_parameters(self):
        """Returns a list of the names of the parameters currently in the genetic instance"""
        return [str(x) for x in self._parameters]
    
    def clear_parameters(self):
        """Removes all parameters from the genetic instance"""
        self._parameters = []
        self._parameter_ranges = {}
    
    def add_parameter(self,param_name,low=None,high=None):
        """adds a parameter to the driver. 
        
        param_name : str 
            name of the parameter to add to the driver
        low : number, optional
            minimum allowed value the optimzier can use for this parameter. If not specified, 
            then the 'low' value from the public variable is used. 
        high : number, optional
            maximum allowed value the optimizer can use for this parameter. If not specified, 
            then the 'high' value from the public variable is used.
        """

        #check to see if this param_name is already in the driver
        try: 
            i = [str(x) for x in self._parameters].index(param_name)
            #if found one, so it's already in there
            self.raise_exception("Trying to add '%s' to the genetic driver, but it is already in the driver"%param_name,RuntimeError)
        except ValueError: #not in the list, so you're good to go!
            pass

        #indexed the same as self._allels
        expreval = ExprEvaluator(param_name, self.parent, single_name=True)
        self._parameters.append(expreval) #add it to the list of string refs
        val = expreval.evaluate()
        if low is not None and high is not None: #use specified, overrides any trait defaults that would have been found
            self._parameter_ranges[param_name] = (low,high)
        else: 
            ranges = [0,0]
            #split up the param_name string to be able to get the trait. 
            path = ".".join(param_name.split(".")[0:-1]) #get the path to the object
            target = param_name.split(".")[-1] #get the last part of the string after the last "."
            obj = getattr(self.parent,path)
            
            t = obj.trait(target) #get the trait
            if t and t.is_trait_type(Enum):
                self._parameter_ranges[param_name]=(None,None)
            elif t and (t.is_trait_type(Float) or t.is_trait_type(Int)): #can't be an Enum, so maybe it's a Float, Int
                
                if hasattr(t,"low"): 
                    ranges[0] = t.low
                elif low: 
                    ranges[0] = low
                else: 
                    self.raise_exception("No value was specified for the 'low' argument, "
                                         "and no default was found in the public variable metadata",ValueError)
                
                if hasattr(t,"high"):
                    ranges[1] = t.high
                elif high: 
                    ranges[1] = high                
                else: 
                    self.raise_exception("No value was specified for the 'high' argument, "
                                         "and no default was found in the public variable metadata",ValueError)
                self._parameter_ranges[param_name] = tuple(ranges)
             
            elif array_test.search(target): #can't figure out what the ranges should be
                if not(isinstance(val,float) or isinstance(val,int) or isinstance(val,int32) or \
                       isinstance(val,int64) or isinstance(val,float32) or isinstance(val,float64)
                      ):
                    self.raise_exception("Only array values of type 'int' or 'float' are allowed as "
                                         "parameters")
                    
                self.raise_exception("values for 'high' and 'low' arguments are required when specifying "
                                     "an Array element as a parameter. They were not given for '%s'"%param_name,TypeError)
            else: 
                self.raise_exception("Improper parameter type. Must be Float,Int, or an element of "
                                     "an Array.",ValueError)
        return True
            
    def execute(self):
        """Perform the optimization"""
        alleles = self._make_alleles()
        
        genome = G1DList.G1DList(len(alleles))
        genome.setParams(allele=alleles)
        genome.evaluator.set(self._run_model)
        
        genome.mutator.set(Mutators.G1DListMutatorAllele)
        genome.initializator.set(Initializators.G1DListInitializatorAllele)
        #TODO: fix tournament size settings        
        #genome.setParams(tournamentPool=self.tournament_size)
        
        # Genetic Algorithm Instance
        #print self.seed
        
        #configuring the iptions
        ga = GSimpleGA.GSimpleGA(genome,interactiveMode = False, seed=self.seed)
        ga.setMinimax(Consts.minimaxType[self.opt_type])
        ga.setGenerations(self.generations)
        ga.setMutationRate(self.mutation_rate)
        ga.setCrossoverRate(self.crossover_rate)
        ga.setPopulationSize(self.population_size)
        ga.setElitism(self.elitism)
        
        #setting the selector for the algorithm
        ga.selector.set(self._selection_mapping[self.selection_method])
        
        #GO
        ga.evolve(freq_stats=0)
        
        self.best_individual = ga.bestIndividual()
        
        #run it once to get the model into the optimal state
        self._run_model(self.best_individual) 
           

    def _run_model(self,chromosome):
        for i,value in enumerate(chromosome):
            self._parameters[i].set(value)
        #    print i,value    
        self.run_iteration()
        #exit()        
        return self.objective.evaluate()