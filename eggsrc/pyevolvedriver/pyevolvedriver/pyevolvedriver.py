"""A pyevolve based driver for OpenMDAO"""

__version__ = "0.1"

from openmdao.main.component import Component

from pyevolve import G1Dlist,G1DBinaryString,G2dList,GAllele
from pyevolve import GsimpleGA,Selectors,Initializators,Mutators,Consts,DBadapters

import NASAevolve.G1DListCrossOverRealHypersphere



class pyevolvedriver(Driver):
    """OpenMDAO wrapper for the pyevolve genetic algorithm framework. The 
    wrapper uses two attributes to configure the GA: The wrapper conforms to the
    pyevolve API for configuring the GA. It makes use of two attributes which
    map to pyevolve objects: genome and GA. The default selection for genome is
    G1DList. By default the wrapper uses the GsimpleGA engine from pyevolve with 
    all its default settings. Currently, only the default configuration for the 
    genome is supported. 
    
    The standard pyevolve library is provided:
        G1Dlist,G1DBinaryString,G2dList,GAllele
        GsimpleGA,Initializators,Mutators,Consts,DBadapters
        
    ** Example **
    Setting up Genome
    >>> genome = G1DList
    >>> genome.initializator.set(Initializators.G1DListInitializatorReal)
    >>> genome.mutator.set(Mutators.G1DListMutatorIntegerGaussian)
    >>> genome.evaluator.set(somefunc)
    
    :param freq_stats: the frequency which the driver records population information
    :param genome: the particular genome for optimization
    :param GA: the GA algorithm object, default configuration used GsimpleGA
    
    """
    def __init__(self,name,parent=None,desc=None): 
        super(pyevolvedriver,self).__init__(name,parent,desc)
       
        self.genome = None
        self.GA = GsimpleGA(self.genome)
        
        self.freq_stats = 10
        self.make_public(['freq_stats','GA','genome','minmax'])
        
        self.best_individual= None
    
    def execute(self):
        """ Perform the optimization""" 
        self.GA.evolve(freq_stats = self.freq_stats)
        self.best_individual = self.GA.bestIndividual()