from openmdao.main.api import Assembly

from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial

from openmdao.lib.optproblems import sellar

class SellarDOE(Assembly): 
    """Example assembly for running a Design Of Experiments (DOE) on a set of components"""
    
    def __init__(self): 
        super(SellarDOE,self).__init__()
        
        self.add("driver",DOEdriver())
        self.driver.recorders = [ListCaseRecorder()]
        self.driver.DOEgenerator = FullFactorial()
        #configure the specific DOE options
        self.driver.DOEgenerator.num_levels = 3
        
        self.add("dis1",sellar.Discipline1())
        self.add("dis2",sellar.Discipline2())
        
        #setting some variables to fixed values
        self.dis1.y2 = 3.15
        self.dis2.y1 = 3.78
        
        #adding three parameters to the DOEDriver
        self.driver.add_parameter(("dis1.z1","dis2.z1"),low=-10.0, high=10.0)
        self.driver.add_parameter(("dis1.z2","dis2.z2"),low=0.0, high=10.0)
        self.driver.add_parameter("dis1.x1",low=0.0,high=10.0)
        
if __name__ == "__main__": 
    
    analysis = SellarDOE()
    
    analysis.run()
    
        
        
        
        
        