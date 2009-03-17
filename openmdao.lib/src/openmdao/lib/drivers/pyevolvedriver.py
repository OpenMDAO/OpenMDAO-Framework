"""A pyevolve based driver for OpenMDAO"""

__version__ = "0.1"

from openmdao.main.driver import Driver

from pyevolve import G1DList,G1DBinaryString,G2DList,GAllele,GenomeBase
from pyevolve import GSimpleGA,Selectors,Initializators,Mutators,Consts,DBAdapters
from pyevolve import GenomeBase
from openmdao.main.expreval import ExprEvaluator
import random

def G1DListCrossOverRealHypersphere(genome, **args):
    """ A genome reproduction algorithm, developed by Tristan Hearn at 
    the NASA Glenn Research Center, which uses a hypersphere defined
    by a pair of parents to find the space of possible children. 
    children are then picked at random from that space. """

    
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
        except AttributeError,err:
            self.raise_exception('No objective has been set', RuntimeError)
        except RuntimeError,err:
            self.raise_exception("objective specified, '"+str(obj)+"', is not valid a valid OpenMDAO object. If it does exist in the model, a framework variable may need to be created",
                                 RuntimeError)            
    objective = property(_get_objective,_set_objective)

    def get_objective_val(self):
        """evaluate the new objective"""
        if self.objective is None:
            return None
        else:
            return self._objective.evaluate()     

    def __init__(self,name,parent=None,desc=None): 
        super(pyevolvedriver,self).__init__(name,parent,desc)

        self.genome = GenomeBase.GenomeBase()
        self.GA = GSimpleGA.GSimpleGA(self.genome)

        #inputs - value of None means use default
        self.decoder = None
        self.freq_stats = None
        self.selector = None
        self.stepCallback = None
        self.terminationCriteria = None
        self.seed = None
        self.DBAdapter = None
        self.PopulationSize = None
        self.SortType = None
        self.MutationRate = None
        self.CrossoverRate = None
        self.Generations = None
        self.Minimax = None
        self.Elitism = None

        #outputs
        self.best_individual= None
        self.decoder = None

        #internal stuff
        self._objective = None

        #TODO Need to make a Genome socket




    def set_GA_property(self,setFunc,val): 
        if val != None: 
            return setFunc(val)

    def set_GA_FunctionSlot(self,slot,funcList,RandomApply=False,):
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

        self.parent.workflow.run()
        return self.get_objective_val()

    def verify(self):
        #genome verify
        if not isinstance(self.genome,GenomeBase.GenomeBase):
            self.raise_exception("genome provided is not valid. Does not inherit from pyevolve.GenomeBase.GenomeBase",TypeError)

            #decoder verify
        if self.decoder == None: # check if None first
            self.raise_exception("decoder specified as 'None'. A valid decoder must be present",TypeError)
        try: # won't work if decoder is None
            self.decoder(self.genome)
        except TypeError:
            self.raise_exception("decoder specified as does not have the right signature. Must take only 1 argument",TypeError)


    def execute(self):
        """ Perform the optimization"""
        
        if self.objective == '':
            self.raise_exception("objective specified as None, please provide an objective expression.",RuntimeError)
        
        self.verify()
        #configure the evaluator function of the genome
        self.genome.evaluator.set(self.evaluate)

        self.GA = GSimpleGA.GSimpleGA(self.genome, self.seed)
        self.set_GA_property(self.GA.setDBAdapter,self.DBAdapter)
        self.set_GA_property(self.GA.setPopulationSize,self.PopulationSize)
        self.set_GA_property(self.GA.setSortType,self.SortType)
        self.set_GA_property(self.GA.setMutationRate,self.MutationRate)
        self.set_GA_property(self.GA.setCrossoverRate,self.CrossoverRate)
        self.set_GA_property(self.GA.setGenerations,self.Generations)
        self.set_GA_property(self.GA.setMinimax,self.Minimax)
        self.set_GA_property(self.GA.setElitism,self.Elitism)

        self.set_GA_FunctionSlot(self.GA.selector,self.selector)
        self.set_GA_FunctionSlot(self.GA.stepCallback,self.stepCallback)
        self.set_GA_FunctionSlot(self.GA.terminationCriteria,self.terminationCriteria)

        self.GA.evolve(freq_stats = self.freq_stats)
        self.best = self.GA.bestIndividual()

