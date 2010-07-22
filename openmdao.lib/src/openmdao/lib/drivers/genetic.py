"""A simple Pyevolve-based driver for OpenMDAO"""

import random
import re

from numpy import int32,int64,float32,float64

from enthought.traits.api import Python

from pyevolve import G1DList, G1DBinaryString, G2DList, GAllele, GenomeBase
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

from openmdao.main.api import Driver, ExprEvaluator, set_as_top, Component, Assembly,Expression
from openmdao.lib.api import Float, Int, Enum, Array,Bool, Instance

from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate

array_test = re.compile("(\[[0-9]+\])+$")

@add_delegate(HasParameters)
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
    
    elitism = Bool(False,iotype="in",desc="controls the use of elitism in the creation of new "
                   "generations.")
    
    best_individual = Instance(klass = GenomeBase.GenomeBase, iotype="out", desc="the genome with the "
                               "best score from the optimization") 
    
    seed = Int(None,iotype="in",
               desc="Random seed for the optimizer. Set to a specific value for repeatable "
               "results, otherwise leave as None for truely random seeding")
    
    def __init__(self,doc=None):
        super(Genetic,self).__init__(doc)
    
    def _make_alleles(self): 
        """ Returns a GAllelle.Galleles instance with alleles corresponding to 
        the parameters specified by the user"""
        alleles = GAllele.GAlleles()
        for param in self.get_parameters():
            expreval = param.expreval
            val = expreval.evaluate() #now grab the value 
            ref = str(expreval)
            
            #split up the ref string to be able to get the trait. 
            path = ".".join(ref.split(".")[0:-1]) #get the path to the object
            target = ref.split(".")[-1] #get the last part of the string after the last "."
            
            low = param.low
            high = param.high
            
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
           

    def _run_model(self, chromosome):
        self.set_parameters([val for val in chromosome])
        self.run_iteration()
        return self.objective.evaluate()
    
    