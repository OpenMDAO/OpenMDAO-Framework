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
        self.points = numpy.array(50)
        self.total = 0
    
    def exectute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = sum([x**2 for x in self.points])
        
class pyevolvedriverTestCase(unittest.TestCase):
    """ test case for the pyevolve component""" 
    
    #define a method to map the GA genome object to the model
    #  this one is very simple! 
    def evaluator(genome):
        sphere = 
        sphere.points = [x for x in genome]
    
    def setUp(self):
        self.top = Aseembly('top',None)
        comp = SphereFunction('comp',self.top)
        self.top.add_child(comp)
        self.top.workflow.add_node(comp)
        self.top.add_child(pyevolvedriver('driver'))
        
        self.top.driver.genome = pyevolvedriver.G1Dlist.G1Dlist(50)
        self.top.driver.genome.setParams(rangemin=-5.12, rangemax=5.13)
        self.top.driver.genome.initializator.set(Initializators.G1DListInitializatorReal)
        self.top.driver.genome.mutator.set(Mutators.G1DListMutatorRealGaussian)
        self.top.driver.evaluator.set(
        
    
    def tearDow(self):
        self.top = None
    
    def testfucnt1(self):
        pass
    
if __name__ == '__main__':
    unittest.main()