"""A pyevolve based driver for OpenMDAO"""

__version__ = "0.1"

import random

from enthought.traits.api import Int, Float, CBool, Str, Any

from pyevolve import G1DList,G1DBinaryString,G2DList,GAllele,GenomeBase
from pyevolve import GSimpleGA,Selectors,Initializators,Mutators,Consts,DBAdapters
from pyevolve import GenomeBase

from openmdao.main.api import Driver, StringRef

def G1DListCrossOverRealHypersphere(genome, **args):
    """ A genome reproduction algorithm, developed by Tristan Hearn at 
    the NASA Glenn Research Center, which uses a hypersphere defined
    by a pair of parents to find the space of possible children. 
    Children are then picked at random from that space. """
    
    gMom = args['mom']
    gDad = args['dad']
    
    sister = gMom.clone()
    brother = gDad.clone()
    
    bounds = (genome.getParam("rangemin",0),genome.getParam("rangemax",100))
    dim = len(genome)
    numparents = 2.0
        
    # find the center of mass (average value) between the two parents for each dimension
    cmass = [(gm+gd)/2.0 for gm,gd in zip(gMom,gDad)]
    
    radius = max(sum([(cm-gM)**2 for cm,gM in zip(cmass,gMom)]),
                 sum([(cm-gD)**2 for cm,gD in zip(cmass,gDad)])
             )**.5
    
    #generate a random unit vectors in the hyperspace
    seed_sister = [random.uniform(-1,1) for i in range(0,dim)]
    magnitude = sum([x**2 for x in seed_sister])**.5
    while magnitude > 1: #checksum to enforce a circular distribution of random numbers
        seed_sister = [random.uniform(-1,1) for i in range(0,dim)]
        magnitude = sum([x**2 for x in seed_sister])**.5        
    
    seed_brother = [random.uniform(-1,1) for i in range(0,dim)]
    magnitude = sum([x**2 for x in seed_brother])**.5
    while magnitude > 1: #checksum to enforce a circular distribution of random numbers
        seed_brother = [random.uniform(-1,1) for i in range(0,dim)]
        magnitude = sum([x**2 for x in seed_brother])**.5    
    
    #create a children
    sister.resetStats()
    brother.resetStats()
    
    sister.genomeList = [cm+radius*sd for cm,sd in zip(cmass,seed_sister)]
    brother.genomeList = [cm+radius*sd for cm,sd in zip(cmass,seed_brother)]
    
    if type(gMom.genomeList[0]) == int: #preserve the integer type of the genome if necessary
        sister.genomeList = [int(round(x)) for x in sister.genomeList]   
        brother.genomeList = [int(round(x)) for x in brother.genomeList]  
    
    return (sister,brother)


class pyevolvedriver(Driver):
    """OpenMDAO wrapper for the pyevolve genetic algorithm framework. The 
    wrapper uses two attributes to configure the optmization: 
    
    The wrapper conforms to the pyevolve API for configuring the GA. It makes use 
    of two attributes which map to pyevolve objects: genome and GA. The default 
    selection for genome is G1DList. By default the wrapper uses the GsimpleGA engine 
    from pyevolve with all its default settings. Currently, only the default configuration for the 
    genome is supported. 

    The standard pyevolve library is provided:
        G1Dlist,G1DBinaryString,G2dList,GAllele
        GsimpleGA,Initializators,Mutators,Consts,DBadapters

    TODO: Implement function-slots as sockets
    """

    # inputs
    objective = StringRef(iostatus='in',
                          desc= 'A string containing the objective function expression.')
    freq_stats = Int(0, iostatus='in')
    seed = Float(0., iostatus='in')
    population_size = Float(Consts.CDefGAPopulationSize, iostatus='in')
    
    sort_type = CBool(Consts.sortType["scaled"], iostatus='in',
                     desc='use Consts.sortType["raw"],Consts.sortType["scaled"] ') # can accept
    mutation_rate = Float(Consts.CDefGAMutationRate, iostatus='in')
    crossover_rate = Float(Consts.CDefGACrossoverRate, iostatus='in')
    generations = Int(Consts.CDefGAGenerations, iostatus='in')
    mini_max = CBool(Consts.minimaxType["minimize"], iostatus='in',
                    desc = 'use Consts.minimaxType["minimize"] or Consts.minimaxType["maximize"]')
    elitism = CBool(True, iostatus='in',desc='True of False')
    
    #outputs
    best_individual = Any(GenomeBase.GenomeBase(), iostatus='out')
        
    def __init__(self,name,parent=None,doc=None): 
        super(pyevolvedriver,self).__init__(name,parent,doc)

        self.genome = GenomeBase.GenomeBase() #TODO: Mandatory Socket
        self.GA = GSimpleGA.GSimpleGA(self.genome) #TODO: Mandatory Socket, with default plugin

        # value of None means use default
        self.decoder = None #TODO: mandatory socket       
        self.selector = None #TODO: optional socket
        self.stepCallback = None #TODO: optional socket
        self.terminationCriteria = None #TODO: optional socket
        self.DBAdapter = None #TODO: optional socket

    def _set_GA_FunctionSlot(self,slot,funcList,RandomApply=False,):
        if funcList == None: return
        slot.clear()
        if not isinstance(funcList,list): funcList = [funcList]
        for func in funcList: 
            if slot.isEmpty(): 
                slot.set(func)
            else: slot.add(func)
        slot.setRandomApply(RandomApply)

    def evaluate(self,genome):
        self.decoder(genome)
        self.run_iteration()
        return self.objective.evaluate()

    def verify(self):
        #genome verify
        if not isinstance(self.genome,GenomeBase.GenomeBase):
            self.raise_exception(
                "genome provided is not valid. Does not inherit from pyevolve.GenomeBase.GenomeBase",
                TypeError)

        #decoder verify
        if self.decoder == None: # check if None first
            self.raise_exception("decoder specified as 'None'. A valid decoder must be present",
                                 TypeError)
        try: # won't work if decoder is None
            self.decoder(self.genome)
        except TypeError:
            self.raise_exception(
                "decoder specified as does not have the right signature. Must take only 1 argument",
                TypeError)

    def execute(self):
        """ Perform the optimization"""
        
        self.verify()
        #configure the evaluator function of the genome
        self.genome.evaluator.set(self.evaluate)
        
        self.GA = GSimpleGA.GSimpleGA(self.genome, self.seed)
        
        self.GA.setPopulationSize(self.population_size)
        self.GA.setSortType(self.sort_type)
        self.GA.setMutationRate(self.mutation_rate)
        self.GA.setCrossoverRate(self.crossover_rate)
        self.GA.setGenerations(self.generations)
        self.GA.setMinimax(self.mini_max)
        self.GA.setElitism(self.elitism)

        #self.GA.setDBAdapter(self.DBAdapter) #
        
        self._set_GA_FunctionSlot(self.GA.selector,self.selector)
        self._set_GA_FunctionSlot(self.GA.stepCallback,self.stepCallback)
        self._set_GA_FunctionSlot(self.GA.terminationCriteria,self.terminationCriteria)
        
        self.GA.evolve(freq_stats = self.freq_stats)
        self.best_individual = self.GA.bestIndividual()

