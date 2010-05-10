"""
Test the pyevolve optimizer driver
"""


import logging
import pkg_resources
import sys
import unittest
import numpy

from enthought.traits.api import TraitError

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, Array
from openmdao.lib.drivers import pyevolvedriver
from openmdao.main.eggchecker import check_save_load

# pylint: disable-msg=E1101


class SphereFunction(Component):
    total = Float(0., iotype='out')
    points = Array(value=[], iotype='in')
    
    def __init__(self, desc=None):
        super(SphereFunction, self).__init__(desc)
            
    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = sum([x**2 for x in self.points])
        

class TestCase(unittest.TestCase):
    """ test case for the pyevolve component""" 
    
    #define decoder to map the genome to the model
    # the decoder is called automatically with the function 
    #   evaluation 
    def decoder(self, genome):
        sphere = self.top.comp
        sphere.points = [x for x in genome]
    
    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add_container('comp', SphereFunction())
        self.top.add_container('driver', 
                               pyevolvedriver.pyevolvedriver())

    def tearDown(self):
        self.top = None
    
    #def test_weirdVariableNameProblem(self):
        #x = Float("PopulationSize",self.top.driver,iotype='in',default=80)
        #self.assertEqual(x.get_value(),80)
    
    #basic test to make sure optmizer is working 
    def test_optimizeSphere(self):
        self.top.driver.objective = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        
        #configure the GAengine 
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.mini_max = pyevolvedriver.Consts.minimaxType["minimize"]
        
        #self.top.driver.DBAdapter = None #TODO: Implement this
        self.top.driver.selector = None
        #this is a default, just for testing
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector] #this is a default, just for testing
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               0.1519,places = 4)
        x0,x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0, 0.0063, places = 4)
        self.assertAlmostEqual(x1, .3897, places = 4)

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        # Using config from test_optimize_sphere.
        self.top.driver.objective = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        
        #configure the GAengine 
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.mini_max = pyevolvedriver.Consts.minimaxType["minimize"]
        
        #self.top.driver.DBAdapter = None #TODO: Implement this
        self.top.driver.selector = None
        #this is a default, just for testing
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector]
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        # Set local dir in case we're running in a different directory.
        py_dir = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')
        retcode = check_save_load(self.top, py_dir=py_dir)
        self.assertEqual(retcode, 0)
        
    def test_hypersphereCrossover_real(self):
        self.top.driver.objective = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        self.top.driver.genome.crossover.set(pyevolvedriver.G1DListCrossOverRealHypersphere)

        #configure the GAengine 
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.mini_max = pyevolvedriver.Consts.minimaxType["minimize"]
        
        #self.top.driver.DBAdapter = None #TODO: Implement this
        self.top.driver.selector = None
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector] #this is a default, just for testing
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,0.0058,places = 4)
        x0,x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0, -0.0130,places = 4)
        self.assertAlmostEqual(x1,-0.0753,places = 4)

    #for some reason this test changes the answers of the other two optimzier tests above
    # may have to do with random number generation... if you remove this test, the previous two will fail
    def test_hypersphereCrossover_int(self): 
    
        self.top.driver.objective = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5, rangemax=6)
        self.top.driver.genome.initializator.set(pyevolvedriver.Initializators.G1DListInitializatorInteger)
        self.top.driver.genome.mutator.set(pyevolvedriver.Mutators.G1DListMutatorIntegerGaussian)
        self.top.driver.genome.crossover.set(pyevolvedriver.G1DListCrossOverRealHypersphere)

        #configure the GAengine 
        self.top.driver.decoder = self.decoder  
        self.top.driver.freq_stats = 0
        self.top.driver.seed = 123
        
        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.mini_max = pyevolvedriver.Consts.minimaxType["minimize"]
        
        #self.top.driver.DBAdapter = None #TODO: Implement this
        self.top.driver.selector = None
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector] #this is a default, just for testing
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,0.0,places = 4)
        x0,x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0,0,places = 4)
        self.assertAlmostEqual(x1,0,places = 4)   
    
    def test_noObjectiveSet(self):
        #self.top.driver.objective = "comp.total" 
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        
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
        #this is a default, just for testing
        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector]
        self.top.driver.stepCallback = None
        self.top.driver.terminationCriteria = None
        
        try:
            self.top.run()
        except TraitError, err: 
            self.assertEqual(str(err),
                "Expression: string reference is undefined")
        else: 
            self.fail("expecting TraitError")
        
    def test_invalidObjective(self):
        try:
            self.top.driver.objective = "comp.badojbjective"        
        except TraitError, err:
            self.assertEqual(str(err), 
                "driver: invalid value 'comp.badojbjective' for input ref variable 'objective': comp: cannot get valid flag of 'badojbjective' because it's not an io trait.")
        else: 
            self.fail("TraitError expected")
    
    def test_no_comp(self):
        try: 
            self.top.driver.objective = None
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'objective' trait of a pyevolvedriver instance must be a string, but a value of None <type 'NoneType'> was specified.")
        else:
            self.fail("TraitError expected")
    
    #should throw an error because no decode function is provided
    def test_noDecoder(self):
        self.top.driver.objective = "comp.total" 
        self.top.driver.decoder = None
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "driver: decoder specified as 'None'." \
                  " A valid decoder must be present"
            self.assertEqual(str(err), msg)
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
            msg = "driver: decoder as specified does not have the right" \
                  " signature. Must take only 1 argument: decodeBad()" \
                  " takes exactly 2 arguments (1 given)"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")

    
    #should throw and error about the lack of a genome
    def test_noGenomeTest(self): 
        self.top.driver.objective = "comp.total" 
        self.top.driver.genome = None
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "driver: genome provided is not valid." \
                  " Does not inherit from pyevolve.GenomeBase.GenomeBase"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")
  
    
    #should throw an error becuase genome does not inherit from GenomeBase
    def test_GenomeNotGenomeTest(self):
        self.top.driver.objective = "comp.total" 
        self.top.driver.genome = [1,2]
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "driver: genome provided is not valid." \
                  " Does not inherit from pyevolve.GenomeBase.GenomeBase"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")
        
    
if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

