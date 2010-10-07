"""A simple Pyevolve-based driver for OpenMDAO

   See Appendix B for additional information on the :ref:`Genetic` driver."""

import re

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Python

from pyevolve import G1DList, GAllele, GenomeBase
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

from openmdao.main.api import Driver 
from openmdao.lib.datatypes.enum import Enum
from openmdao.lib.datatypes.float import Float
from openmdao.lib.datatypes.int import Int
from enthought.traits.api import Bool, Instance

from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.util.decorators import add_delegate

array_test = re.compile("(\[[0-9]+\])+$")

@add_delegate(HasParameters, HasObjective)
class Genetic(Driver):
    """Genetic algorithm for the OpenMDAO framework, based on the Pyevolve
    Genetic algorithm module. 
    """
    
    # pylint: disable-msg=E1101    
    opt_type = Enum("minimize", values=["minimize", "maximize"],
                    iotype="in",
                    desc='Sets the optimization to either minimize or maximize '
                         'the objective function.')
    
    generations = Int(Consts.CDefGAGenerations, iotype="in",
                      desc="The maximum number of generations the algorithm "
                           "will evolve to before stopping.")
    population_size = Int(Consts.CDefGAPopulationSize, iotype="in",
                          desc = "The size of the population in each "
                                 "generation.")
    crossover_rate = Float(Consts.CDefGACrossoverRate, iotype="in", low=0.0,
                           high=1.0, desc="The crossover rate used when two "
                          "parent genomes reproduce to form a child genome.")
    mutation_rate = Float(Consts.CDefGAMutationRate, iotype="in", low=0.0, 
                          high=1.0, desc="The mutation rate applied to "
                                         "population members.")
    
    selection_method = Enum("roulette_wheel",
                            ("roulette_wheel", "tournament", "rank", "uniform"),
                            desc="The selection method used to pick population "
                                 "members who will survive for "
                                 "breeding into the next generation.",
                            iotype="in")
    _selection_mapping = {"roulette_wheel":Selectors.GRouletteWheel,
                          "tournament":Selectors.GTournamentSelector,
                          "rank":Selectors.GRankSelector,
                          "uniform":Selectors.GUniformSelector}
    
    elitism = Bool(False, iotype="in", desc="Controls the use of elitism in "
                                            "the creation of new generations.")
    
    best_individual = Instance(klass = GenomeBase.GenomeBase, iotype="out", 
                               desc="The genome with the "
                               "best score from the optimization.") 
    
    seed = Int(None, iotype="in",
               desc="Random seed for the optimizer. Set to a specific value "
                    "for repeatable results; otherwise leave as None for truly "
                    "random seeding.")
    
    def __init__(self, doc=None):
        super(Genetic, self).__init__(doc)
    
    def _make_alleles(self): 
        """ Returns a GAllelle.Galleles instance with alleles corresponding to 
        the parameters specified by the user"""
        
        alleles = GAllele.GAlleles()
        count = 0
        for param in self.get_parameters().values():
            count += 1    
            expreval = param.expreval
            val = expreval.evaluate() #now grab the value 
            ref = str(expreval)
        
            
            #split up the ref string to be able to get the trait.
            
            #get the path to the object
            path = ".".join(ref.split(".")[0:-1]) 
            #get the last part of the string after the last "."
            target = ref.split(".")[-1] 
            
            low = param.low
            high = param.high
            
            #bunch of logic to check for array elements being passed as refs
            
            obj = getattr(self.parent, path)
            
            t = obj.traits().get(target) #get the trait
            
            if (t and (t.is_trait_type(Float) or t.is_trait_type(Python))) \
                or (array_test.search(target) and isinstance(val,float)):
                allele = GAllele.GAlleleRange(begin=low, end=high, real=True)
                alleles.add(allele)
                
            elif (t and (t.is_trait_type(Int) or t.is_trait_type(Python))) \
                  or (array_test.search(target) and isinstance(val,int)):
                allele = GAllele.GAlleleRange(begin=low, end=high, real=False)
                alleles.add(allele)                
                    
            elif t and t.is_trait_type(Enum): 
                allele = GAllele.GAlleleList(t.values)
                alleles.add(allele)
        self.count = count
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
        
        #configuring the options
        ga = GSimpleGA.GSimpleGA(genome, interactiveMode = False, 
                                 seed=self.seed)
        ga.setMinimax(Consts.minimaxType[self.opt_type])
        ga.setGenerations(self.generations)
        ga.setMutationRate(self.mutation_rate)
        if self.count > 1:
            ga.setCrossoverRate(self.crossover_rate)
        else:   
            ga.setCrossoverRate(0)
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
        return self.eval_objective()
    
    
