"""A simple Pyevolve-based driver for OpenMDAO"""

import random


from pyevolve import G1DList, G1DBinaryString, G2DList, GAllele, GenomeBase
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

try:
    from pyevolve import DBAdapters
except ImportError:
    # Apparently the egg doesn't record it's dependencies.
    import logging
    logging.warning('No pyevolve.DBAdaptors available.')

from openmdao.main.api import Driver, StringRef, StringRefArray, set_as_top, Component, Assembly
from openmdao.lib.api import Float, Int, Array


class Genetic(Driver):
    """Genetic algorithm for the OpenMDAO frameowork, based on the Pyevolve Genetic algorithm module. 
    """
    objective = StringRef(iotype='out',desc='A string containing the objective function expression') 
    design_vars = StringRefArray(iotype='out', desc="An array of design variable names. These names can include array indexing")
    
    def __init__(self,doc=None):
        super(Genetic,self).__init__(doc)
        
        self._alleles = GAllele.GAlleles()
        
        
    def add_des_var(self,ref):
        self.design_vars.append(ref)
        
        #split up the ref string to be able to get the trait. 
        path = ".".join(ref.split(".")[0:-1]) #get the path to the object
        target = ref.split(".")[-1] #get the last part of the string after the last "."
        
        obj = getattr(self.parent,path)
       
        t = obj.get_dyn_trait(target)
        
        if t.is_trait_type(Float): 
            print t.desc
            print t.low
            print t.high
            print t.units
            obj.set(target,30)
        
    def execute(self):
        """Perform the optimization"""
        

        
if __name__ == "__main__":
    import numpy
    class Rosenbrock(Component):
        x = Float(3,low=-10,high=10,desc="just an abrbitrary variable",units="ft",iotype="in")
        y = Float(low=-100.0,high=100.0,iotype="in")
        result = Float(iotype = 'out')
        
        def __init__(self, doc=None):
            super(Rosenbrock, self).__init__(doc)
            self.x = 10
            self.y = 10
            self.result = (1-self.x**2) + 100*(self.y - self.y**2)**2
        

        def execute(self):
            """calculate the new objective value"""
            self.result = (1-self.x**2) + 100*(self.y - self.x**2)**2
    
    top = set_as_top(Assembly())
    top.add_container("rosen",Rosenbrock())
    top.add_container("genetic",Genetic())
    
    top.genetic.add_des_var("rosen.x")
    