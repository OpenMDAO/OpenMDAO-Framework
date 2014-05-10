"""A simple Pyevolve-based driver for OpenMDAO."""

import re

#pyevolve calls multiprocessing.cpu_count(), which can raise NotImplementedError
#so try to monkeypatch it here to return 1 if that's the case
try:
    import multiprocessing
    multiprocessing.cpu_count()
except ImportError:
    pass
except NotImplementedError:
    multiprocessing.cpu_count = lambda: 1

from pyevolve import G1DList, GAllele, GenomeBase, Scaling
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

# pylint: disable-msg=E0611,F0401
from openmdao.main.datatypes.api import Enum, Float, Int, Bool, Slot

from openmdao.main.api import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasevents import HasEvents
from openmdao.main.interfaces import IHasParameters, IHasObjective, \
                                     implements, IOptimizer
from openmdao.util.decorators import add_delegate
from openmdao.util.typegroups import real_types, int_types, iterable_types

array_test = re.compile("(\[[0-9]+\])+$")


@add_delegate(HasParameters, HasObjective, HasEvents)
class Genetic(Driver):
    """Genetic algorithm for the OpenMDAO framework, based on the Pyevolve
    Genetic algorithm module.
    """

    implements(IHasParameters, IHasObjective, IOptimizer)

    # pylint: disable-msg=E1101
    opt_type = Enum("minimize", values=["minimize", "maximize"],
                    iotype="in",
                    desc='Sets the optimization to either minimize or maximize '
                         'the objective function.')

    generations = Int(Consts.CDefGAGenerations, iotype="in",
                      desc="The maximum number of generations the algorithm "
                           "will evolve to before stopping.")
    population_size = Int(Consts.CDefGAPopulationSize, iotype="in",
                          desc="The size of the population in each generation.")
    crossover_rate = Float(Consts.CDefGACrossoverRate, iotype="in", low=0.0,
                           high=1.0, desc="The crossover rate used when two "
                          "parent genomes reproduce to form a child genome.")
    mutation_rate = Float(Consts.CDefGAMutationRate, iotype="in", low=0.0,
                          high=1.0, desc="The mutation rate applied to "
                                         "population members.")

    selection_method = Enum("roulette_wheel",
                            ("roulette_wheel",
                             #"tournament", #this seems to be broken
                             "rank",
                             "uniform"),
                            desc="The selection method used to pick population "
                                 "members who will survive for "
                                 "breeding into the next generation.",
                            iotype="in")
    _selection_mapping = {"roulette_wheel":Selectors.GRouletteWheel,
                          #"tournament":Selectors.GTournamentSelector,
                          #this does not seem to function right for pyevolve
                          "rank":Selectors.GRankSelector,
                          "uniform":Selectors.GUniformSelector}

    elitism = Bool(False, iotype="in", desc="Controls the use of elitism in "
                                            "the creation of new generations.")

    best_individual = Slot(klass=GenomeBase.GenomeBase,
                           desc="The genome with the "
                                "best score from the optimization.")

    seed = Int(None, iotype="in",
               desc="Random seed for the optimizer. Set to a specific value "
                    "for repeatable results; otherwise leave as None for truly "
                    "random seeding.")

    def _make_alleles(self):
        """ Returns a GAllelle.Galleles instance with alleles corresponding to
        the parameters specified by the user"""

        alleles = GAllele.GAlleles()
        count = 0
        for param in self.get_parameters().values():
            allele = None
            count += 1
            val = param.evaluate()[0] #now grab the value
            low = param.low
            high = param.high

            metadata = param.get_metadata()[1]
            #then it's a float or an int, or a member of an array
            if ('low' in metadata or 'high' in metadata) or \
                array_test.search(param.targets[0]):
                #some kind of int
                if isinstance(val, int_types):
                    allele = GAllele.GAlleleRange(begin=low, end=high, real=False)
                elif isinstance(val, real_types):
                    #some kind of float
                    allele = GAllele.GAlleleRange(begin=low, end=high, real=True)

            elif "values" in metadata and \
                 isinstance(metadata['values'], iterable_types):
                allele = GAllele.GAlleleList(metadata['values'])

            if allele:
                alleles.add(allele)
            else:
                self.raise_exception("%s is not a float, int, or enumerated "
                                     "datatype. Only these 3 types are allowed"
                                     % param.targets[0], ValueError)
        self.count = count
        return alleles

    def execute(self):
        """Perform the optimization"""
        self.set_events()

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
        ga = GSimpleGA.GSimpleGA(genome, interactiveMode=False,
                                 seed=self.seed)
        pop = ga.getPopulation()
        pop = pop.scaleMethod.set(Scaling.SigmaTruncScaling)
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

