"""
Test the pyevolve optimizer driver
"""


import unittest
import numpy


from openmdao.main.component import Component
from openmdao.main.assembly import Assembly
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.variable import INPUT,OUTPUT
from openmdao.main.float import Float
from openmdao.lib.drivers import pyevolvedriver



import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

class SphereFunction(Component):
    """ 
    """
    
    def __init__(self, name, parent=None, desc=None):
        super(SphereFunction, self).__init__(name, parent, desc)
        self.points = []
        self.total = 0
        
        Float('total',self,iostatus=OUTPUT)
    
    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = sum([x**2 for x in self.points])
        
class pyevolvedriverTestCase(unittest.TestCase):
    """ test case for the pyevolve component""" 
    
    #define decoder to map the genome to the model
    # the decoder is called automatically with the function 
    #   evaluation 
    def decoder(self,genome):
        sphere = self.top.comp
        sphere.points = [x for x in genome]
        
    
    
    def setUp(self):
        self.top = Assembly('top',None)
        self.top.add_child(SphereFunction('comp',self.top))
        self.top.workflow.add_node(self.top.comp)
        self.top.add_child(pyevolvedriver.pyevolvedriver('driver'))

    def tearDown(self):
        self.top = None
    
    #basic test to make sure optmizer is working 
    def test_optimizeSphere(self):
        self.top.driver.objective = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        
        #configure the GAengine 
          #None will configure the defualt
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.terminationCriteria = None
        self.top.driver.DBAdapter = None
        self.top.driver.PopulationSize = None
        self.top.driver.SortType = None
        self.top.driver.MutationRate = .02
        self.top.driver.CrossoverRate = None
        self.top.driver.Generations = 1
        self.top.driver.Minimax = pyevolvedriver.Consts.minimaxType["minimize"]
        self.top.driver.Elitism = None
        
        self.top.driver.selector = None
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector] #this is a default, just for testing
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best.score,0.1906,places = 4)
        x0,x1 = [x for x in self.top.driver.best] 
        self.assertAlmostEqual(x0,.1966,places = 4)
        self.assertAlmostEqual(x1,.3897,places = 4)
    
    def test_noObjectiveSet(self):
        #self.top.driver.objective = "comp.total" 
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        
        #configure the GAengine 
          #None will configure the defualt
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.terminationCriteria = None
        self.top.driver.DBAdapter = None
        self.top.driver.PopulationSize = None
        self.top.driver.SortType = None
        self.top.driver.MutationRate = .02
        self.top.driver.CrossoverRate = None
        self.top.driver.Generations = 1
        self.top.driver.Minimax = pyevolvedriver.Consts.minimaxType["minimize"]
        self.top.driver.Elitism = None
        
        self.top.driver.selector = None
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector] #this is a default, just for testing
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        try:
            self.top.run()
        except RuntimeError, err: 
            self.assertEqual(str(err),"top.driver: objective specified as None, please provide an objective expression." )
        else: 
            self.fail("expecting RuntimeError")
        
    def test_invalidObjective(self):
        try:
            self.top.driver.objective = "comp.badojbjective"        
        except RuntimeError, err:
            self.assertEqual(str(err), "top.driver: objective specified, 'comp.badojbjective', is not valid a valid OpenMDAO object. If it does exist in the model, a framework variable may need to be created")
        else: 
            self.fail("RuntimeError expected")
    
    def test_noComp(self):
        try: 
            self.top.driver.objective = None
        except RuntimeError, err: 
            self.assertEqual(str(err), 'top.driver: No objective has been set')
        else:
            self.fail("RuntimeError expected")
    
    #should throw an error because no decode function is provided
    def test_noDecoder(self):
        self.top.driver.objective = "comp.total" 
        self.top.driver.decoder = None
        try:
            self.top.driver.run()
        except TypeError, err:
            self.assertEqual(str(err), "top.driver: decoder specified as 'None'. A valid decoder must be present")
        else: 
            self.fail("TypeError expected")

    
    def test_wrongDecoderSignature(self):
        self.top.driver.objective = "comp.total" 
        def decodeBad(genome,secondArgument):
            pass
        self.top.driver.decoder = decodeBad
        try:
            self.top.driver.run()
        except TypeError, err:
            self.assertEqual(str(err), "top.driver: decoder specified as does not have the right signature. Must take only 1 argument")
        else: 
            self.fail("TypeError expected")

    
    #should throw and error about the lack of a genome
    def test_noGenomeTest(self): 
        self.top.driver.objective = "comp.total" 
        self.top.driver.genome = None
        try:
            self.top.driver.run()
        except TypeError, err:
            self.assertEqual(str(err), "top.driver: genome provided is not valid. Does not inherit from pyevolve.GenomeBase.GenomeBase")
        else: 
            self.fail("TypeError expected")
  
    
    #should throw an error becuase genome does not inherit from GenomeBase
    def test_GenomeNotGenomeTest(self):
        self.top.driver.objective = "comp.total" 
        self.top.driver.genome = [1,2]
        try:
            self.top.driver.run()
        except TypeError, err:
            self.assertEqual(str(err), "top.driver: genome provided is not valid. Does not inherit from pyevolve.GenomeBase.GenomeBase")
        else: 
            self.fail("TypeError expected")
        
    

        
    
if __name__ == '__main__':
    unittest.main()