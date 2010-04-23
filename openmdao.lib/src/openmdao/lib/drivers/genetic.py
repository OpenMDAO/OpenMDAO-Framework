"""A simple Pyevolve-based driver for OpenMDAO"""

import random
import re

from enthought.traits.api import Python,BaseEnum

from pyevolve import G1DList, G1DBinaryString, G2DList, GAllele, GenomeBase
from pyevolve import GSimpleGA, Selectors, Initializators, Mutators, Consts

try:
    from pyevolve import DBAdapters
except ImportError:
    # Apparently the egg doesn't record it's dependencies.
    import logging
    logging.warning('No pyevolve.DBAdaptors available.')

from openmdao.main.api import Driver, StringRef, StringRefArray, set_as_top, Component, Assembly
from openmdao.lib.api import Float, Int, Enum, Array

array_test = re.compile("\[[0-9]+\]$")


class Genetic(Driver):
    """Genetic algorithm for the OpenMDAO frameowork, based on the Pyevolve Genetic algorithm module. 
    """
    objective = StringRef(iotype='out',desc='A string containing the objective function expression') 
    _design_vars = StringRefArray(iotype='out', desc="An array of design variable names. These names can include array indexing")
    
    def __init__(self,doc=None):
        super(Genetic,self).__init__(doc)
        
        self._alleles = GAllele.GAlleles()
        
        
    def add_des_var(self,ref,low=None,high=None):
        """adds a design variable to the driver. [ref] is a string refering to the public variable the driver should 
        vary during execution. [low] and [high] refer to the minimum and maximum values allowed values for the optimizer to use. 
        If neither are specified, the values will default to the values in the metadata of the public variable being referenced.
        If they are not specified in the metadata and not provided as arguments, the values default to 0 and 100 repectively. 
        """
        self._design_vars.append(ref) #add it to the list of string refs
        val = self._design_vars[-1].evaluate() #now grab the value 
        
        #split up the ref string to be able to get the trait. 
        path = ".".join(ref.split(".")[0:-1]) #get the path to the object
        target = ref.split(".")[-1] #get the last part of the string after the last "."
        
        #bunch of logic to check for array elements being passed as refs
        
        
        obj = getattr(self.parent,path)
       
        t = obj.trait(target) #get the trait
        if t: 
            minimum = low or t.low or 0
            maximum = high or t.high or 100
            
            if t.is_trait_type(Float) or (t.is_trait_type(Python) and isinstance(val,float)):
                allele = GAllele.GAlleleRange(begin=minimum,end=maximum,real=True)
                self._allels.add(allele)
            
            elif t.is_trait_type(Int) or (t.is_trait_type(Python) and isinstance(val,int)):
                allele = GAllele.GAlleleRange(begin=minimum,end=maximum,real=False)
                self._allels.add(allele)                
                
            elif t.is_trait_type(Enum): 
                print "TESTING"
                print t.values()
            
            else: 
                self.raise_exception("Improper design variable type. Must be Float,Int, or an element of an Array.",ValueError)
        
        elif array_test.search(target): 
            maximum = high or 100
            minimum = low or 0
            
            
            
        else:
            self.raise_exception("Improper design variable type. Must be Float,Int or an element of an Array.",ValueError)
        
        
    def execute(self):
        """Perform the optimization"""
        

        
if __name__ == "__main__":
    import numpy
    class Rosenbrock(Component):
        x = Float(3,low=-10,high=10,iotype="in")
        y = Int(low=-100,high=100,iotype="in")
        z = Array(dtype=float, value=[1.0,2.0,3.0,4.0], iotype="in")
        a = Enum(1,2,3,"test",iotype="in")
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
    
    top.genetic.add_des_var("rosen.a")
    