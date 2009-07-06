"""
Test the pyevolve optimizer driver
"""

import logging
import pkg_resources
import sys
import unittest

from openmdao.main import Assembly, Component, Float, ArrayVariable
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.lib.drivers import pyevolvedriver

# pylint: disable-msg=E1101


class SphereFunction(Component):
    def __init__(self, name, parent=None, desc=None):
        super(SphereFunction, self).__init__(name, parent, desc)
        self.points = []
        self.total = 0
        
        Float('total', self, iostatus=OUTPUT)
        ArrayVariable('points', self, iostatus=INPUT)
    
    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = sum([x**2 for x in self.points])
        

class pyevolvedriverTestCase(unittest.TestCase):
    """ test case for the pyevolve component""" 
    
    #define decoder to map the genome to the model
    # the decoder is called automatically with the function 
    #   evaluation 
    def decoder(self, genome):
        sphere = self.top.comp
        sphere.set('points', [x for x in genome])
    
    def setUp(self):
        self.top = Assembly('top', None)
        self.top.add_child(SphereFunction('comp', self.top))
        self.top.add_child(pyevolvedriver.pyevolvedriver('driver'))

    def tearDown(self):
        self.top = None
    
    def test_weird_variable_name_problem(self):
        x = Float("PopulationSize", self.top.driver, INPUT, default=80)
        self.assertEqual(x.get_value(), 80)
    
    #basic test to make sure optmizer is working 
    def test_optimize_sphere(self):
        self.top.driver.objective.value = "comp.total" 
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
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               0.1519, places=4)
        x0, x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0, 0.0063, places=4)
        self.assertAlmostEqual(x1, .3897, places=4)
        
# check_save_load reuires correct pythonV.R
#    def test_save_load(self):
#        logging.debug('')
#        logging.debug('test_save_load')
#
#        # Using config from test_optimize_sphere.
#        self.top.driver.objective.value = "comp.total" 
#        #configure the genome
#        #TODO: genome should be plugged into a socket
#        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
#        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
#        self.top.driver.genome.initializator.set(
#            pyevolvedriver.Initializators.G1DListInitializatorReal)
#        self.top.driver.genome.mutator.set(
#            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
#        
#        #configure the GAengine 
#        self.top.driver.decoder = self.decoder  
#        self.top.driver.freq_stats = 0
#        self.top.driver.seed = 123
#        
#        self.top.driver.mutation_rate = .02
#        self.top.driver.generations = 1
#        self.top.driver.mini_max = pyevolvedriver.Consts.minimaxType["minimize"]
#        
#        #self.top.driver.DBAdapter = None #TODO: Implement this
#        self.top.driver.selector = None
#        #this is a default, just for testing
#        self.top.driver.selector = [pyevolvedriver.Consts.CDefGASelector]
#        self.top.driver.stepCallback = None
#        self.top.driver.terminationCriteria = None
#        
#        if sys.platform != 'win32':
#            # Set local dir in case we're running in a different directory.
#            py_dir = pkg_resources.resource_filename('openmdao.lib.drivers',
#                                                     'test')
#            retcode = self.top.check_save_load(py_dir=py_dir)
#            self.assertEqual(retcode, 0)

    def test_hypersphere_crossover_real(self):
        self.top.driver.objective.value = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorRealGaussian)
        self.top.driver.genome.crossover.set(
            pyevolvedriver.G1DListCrossOverRealHypersphere)

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
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               0.0058, places=4)
        x0, x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0, -0.0130, places=4)
        self.assertAlmostEqual(x1, -0.0753, places=4)

    #for some reason this test changes the answers of the other two optimizer
    # tests above
    # may have to do with random number generation...
    # if you remove this test, the previous two will fail
    def test_hypersphere_crossover_int(self): 
    
        self.top.driver.objective.value = "comp.total" 
        #configure the genome
        #TODO: genome should be plugged into a socket
        self.top.driver.genome = pyevolvedriver.G1DList.G1DList(2)
        self.top.driver.genome.setParams(rangemin=-5, rangemax=6)
        self.top.driver.genome.initializator.set(
            pyevolvedriver.Initializators.G1DListInitializatorInteger)
        self.top.driver.genome.mutator.set(
            pyevolvedriver.Mutators.G1DListMutatorIntegerGaussian)
        self.top.driver.genome.crossover.set(
            pyevolvedriver.G1DListCrossOverRealHypersphere)

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
        
        self.top.run()
        
        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               0.0, places=4)
        x0, x1 = [x for x in self.top.driver.best_individual] 
        self.assertAlmostEqual(x0, 0, places=4)
        self.assertAlmostEqual(x1, 0, places=4)   
    
    def test_no_objective_set(self):
        #self.top.driver.objective.value = "comp.total" 
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
        except RuntimeError, err: 
            msg = "top.driver.objective: reference is undefined"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("expecting RuntimeError")
        
    def test_invalid_objective(self):
        try:
            self.top.driver.objective.value = "comp.badojbjective"        
        except RuntimeError, err:
            msg = "top.driver.objective: cannot find variable" \
                  " 'comp.badojbjective'"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("RuntimeError expected")
    
    def test_no_comp(self):
        try: 
            self.top.driver.objective.value = None
        except TypeError, err:
            msg = "top.driver.objective: reference must be a string"
            self.assertEqual(str(err), msg)
        else:
            self.fail("RuntimeError expected")
    
    #should throw an error because no decode function is provided
    def test_no_decoder(self):
        self.top.driver.objective.value = "comp.total" 
        self.top.driver.decoder = None
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "top.driver: decoder specified as 'None'." \
                  " A valid decoder must be present"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")

    
    def test_wrong_decoder_signature(self):
        self.top.driver.objective.value = "comp.total" 
        def decode_bad(genome, second_argument):
            pass
        self.top.driver.decoder = decode_bad
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "top.driver: decoder specified as does not have the right" \
                  " signature. Must take only 1 argument"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")

    
    #should throw and error about the lack of a genome
    def test_no_genome(self): 
        self.top.driver.objective.value = "comp.total" 
        self.top.driver.genome = None
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "top.driver: genome provided is not valid." \
                  " Does not inherit from pyevolve.GenomeBase.GenomeBase"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")
  
    
    #should throw an error becuase genome does not inherit from GenomeBase
    def test_genome_not_genome(self):
        self.top.driver.objective.value = "comp.total" 
        self.top.driver.genome = [1, 2]
        try:
            self.top.driver.run()
        except TypeError, err:
            msg = "top.driver: genome provided is not valid." \
                  " Does not inherit from pyevolve.GenomeBase.GenomeBase"
            self.assertEqual(str(err), msg)
        else: 
            self.fail("TypeError expected")
        
    
if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

