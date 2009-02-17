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
from openmdao.eggsrc.pyevolvedriver import pyevolvedriver


import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

class SphereFunction(Component):
    """ 
    """
    
    def __init__(self, name, parent=None, desc=None):
        super(SimpleComponent, self).__init__(name, parent, desc)
        self.points = []
        self.total = 0
    
    def exectute(self):
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
        self.top = Aseembly('top',None)
        self.top.add_child(SphereFunction('comp',self.top))
        self.top.workflow.add_node(comp)
        self.top.add_child(pyevolvedriver('driver'))

        #objective expression assumes you are in the scope of self.top
        self.top.driver.objective = "comp.total" 
        self.top.driver.decoder = self.decoder
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1Dlist.G1Dlist(2)

        #TODO GAengine shoudl be plugged into a socket
        self.top.driver.GA = pyevolvedriver.GsimpleGA.GsimpleGA(self.top.driver.genome)

        
    
    def tearDown(self):
        self.top = None
    
    
    def test_optimizeSphere(self):
        #TODO: seed the GAengine so the outcome is predictable

        #configure the genome
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(Mutators.G1DListMutatorRealGaussian)
        self.top.driver.decoder = self.decoder
        
        #configure the GAengine
        self.top.driver.GA.minimax = Consts.minimaxType["minimize"]
        self.top.driver.GA.setGenerations(200)
        self.top.driver.GA.setMutationRate(0.02)
        
        #set the frequency of the collection of population statistics and go
        self.top.driver.freq_stat = 20
        self.top.driver.rand_seed = 123 #used to ensure the same random sequence 
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best.score,0.000038,places = 4)
        self.assertAlmostEqual([x for x in self.top.driver.best],[0,0],places = 2)
    
    #should throw an error because no decode function is provides
    def test_noDecoder(self):
        self.top.driver.decoder = None
        try:
            self.top.run()
        except: RuntimeError, err:
            self.assertEqual(srt(err), "top.driver: decoder specified as 'None'. A valid decoder must be present")
        else: 
            self.fail("RuntimeError expected")
        pass
    
    def test_wrongDecoderSignature(self):
        def decodeBad(genome,secondArgument):
            pass
        
        try:
            self.top.run()
        except: RuntimeError, err:
            self.assertEqual(srt(err), "top.driver: decoder specified as does not have the right signature. Must take only one as argument")
        else: 
            self.fail("RuntimeError expected")
        pass
    
    #should throw and error about the lack of a genome
    def test_noGenomeTest(self): 
        self.top.driver.genome = None
        try:
            self.top.verify()
        except: TypeError, err:
            self.assertEqual(srt(err), "top.driver: genome specified as 'None'. A valid genome instance must be provided")
        else: 
            self.fail("RuntimeError expected")
        pass
    
    #shoudl throw and error about the lack of a GAengine
    def test_noGAengineTest(self): 
        self.top.driver.GA = None
        try:
            self.top.run()
        except: RuntimeError, err:
            self.assertEqual(srt(err), "top.driver: GA specified as 'None'. A valid GAengine instance must be provided")
        else: 
            self.fail("RuntimeError expected")
        pass
    
    #should throw and error beacuse the user tried to put that is not a
    #  genome into the genome slot
    def test_genomeNotGenomeTest(self): 
        testGenome = [1,1]
        self.top.driver.genome = testGenome
        try: 
            self.top.run()
        
    
    #should throw an error because the user tried to put something that is not 
    #  a GAengine in the GA slot
    def test_GAengineNotGAengineTest(self):
        pass
    
if __name__ == '__main__':
    unittest.main()