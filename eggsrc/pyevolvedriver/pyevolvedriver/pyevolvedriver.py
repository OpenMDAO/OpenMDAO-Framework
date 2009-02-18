"""A pyevolve based driver for OpenMDAO"""

__version__ = "0.1"

from openmdao.main.driver import Driver

from pyevolve import G1DList,G1DBinaryString,G2DList,GAllele,GenomeBase
from pyevolve import GSimpleGA,Selectors,Initializators,Mutators,Consts,DBAdapters
from pyevolve import GPopulation,FunctionSlot
from openmdao.main.expreval import ExprEvaluator

#Testing BZR

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
   
    
    
    def _get_objective(self):
        if self._objective is None:
            return ''
        else:
            return self._objective.text
            
    def _set_objective(self,obj):
        self._objective = None
        try:
            self._objective = ExprEvaluator(obj,self)
        except RuntimeError,err:
            self.raise_exception(str(err) + "objective '"+str(obj)+"' is invalid",
                                 RuntimeError)            
    objective = property(_get_objective,_set_objective)
    
    
    def __init__(self,name,parent=None,desc=None): 
        super(pyevolvedriver,self).__init__(name,parent,desc)
       
        self.genome = GenomeBase.GenomeBase()
        self.GA = GSimpleGA.GSimpleGA(self.genome)
        
        self.freq_stats = 10
        #self.make_public(['freq_stats','minmax'])
        
        self.best_individual= None
        
        self.decoder = None
        
        #self.GAconfigurator = FunctionSlot.FunctionSlot("GAconfigurator")
        self.GAconfigurator = None
        
        self.best = None
        
        self._objective = None
        self.objective_val = 0
        
        
        
    def update_objective_val(self):
        """evaluate the new objective"""
        if self.objective is None:
            self.raise_exception('No objective has been set', RuntimeError)
        else:
            return self._objective.evaluate()   
            
    def evaluate(self,genome):
        self.decoder(genome)
        
        self.parent.workflow.run()
        return self.update_objective_val()
    
    def verify(self):
        pass
    
    
    def execute(self):
        """ Perform the optimization"""
        self.verify()
        self.genome.evaluator.set(self.evaluate)
        

        self.GA = GSimpleGA.GSimpleGA(self.genome)
        
        
        self.GAconfigurator()
        
        self.GA.evolve(freq_stats = self.freq_stats)
        self.best = self.GA.bestIndividual()
        
        