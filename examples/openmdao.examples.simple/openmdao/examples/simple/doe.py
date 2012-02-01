"""
    doe.py - Top level assembly for the problem.
"""

from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial
from openmdao.lib.casehandlers.api import ListCaseRecorder

from openmdao.examples.simple.paraboloid import Paraboloid


class Analysis(Assembly): 
    
    def __init__(self): 
        super(Analysis,self).__init__()
        
        self.add('paraboloid',Paraboloid())
        
        self.add('driver',DOEdriver())
        #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
        self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
        
        #DOEdriver will automatically record the values of any parameters for each case
        self.driver.add_parameter('paraboloid.x',low=-50,high=50)
        self.driver.add_parameter('paraboloid.y',low=-50,high=50)
        #tell the DOEdriver to also record any other variables you want to know for each case
        self.driver.case_outputs = ['paraboloid.f_xy',]
        
        #Simple recorder which stores the cases in memory. 
        self.driver.recorders = [ListCaseRecorder(),]
        
        self.driver.workflow.add('paraboloid')
  
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    analysis = Analysis()
    set_as_top(analysis)

    tt = time.time()
    analysis.run() 
    
    print "Elapsed time: ", time.time()-tt, "seconds"
    
    #write the case output to the screen
    for c in analysis.driver.recorders[0].get_iterator():
        print "x: %f, y: %f, z: %f"%(c['paraboloid.x'],c['paraboloid.y'],c['paraboloid.f_xy'])
    
   
        
    
    
    
    

    
         
        
        