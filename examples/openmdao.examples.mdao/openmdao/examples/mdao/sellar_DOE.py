from openmdao.main.api import Assembly

from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial

from openmdao.examples.mdao.disciplines import SellarDiscipline1, SellarDiscipline2

class SellarDOE(Assembly): 
    """Example assembly for running a Design Of Experiments (DOE) on a set of components"""
    
    def __init__(self): 
        super(SellarDOE,self).__init__()
        
        self.add("driver",DOEdriver)
        self.driver.doegenerator = FullFactorial()
        
        self.add("dis1",SellarDiscipline1)
        self.add("dis2",SellarDiscipline2)
        
        