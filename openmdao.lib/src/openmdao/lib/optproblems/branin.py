from math import cos, pi

from openmdao.main.api import Component
from openmdao.main.problem_formulation import OptProblem

from openmdao.main.datatypes.api import Float

class BraninComponent(Component): 
    x = Float(0.,iotype="in")
    y = Float(0.,iotype="in")
    
    f_xy = Float(0.,iotype="out")
    
    def execute(self):
        self.f_xy = (self.y-(5.1/(4.*pi**2.))*self.x**2.+5.*self.x/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(self.x)+10.
    
class BraninProblem(OptProblem): 
    """Branin Test Problem Definition""" 
    
    def configure(self): 
        self.add("branin",BraninComponent())
        
        #Problem Formulation 
        
        #Local Des Vars
        self.add_parameter('branin.x',low=-5.,high=10.,start=0)        
        self.add_parameter('branin.y',low=0.,high=15.,start=0)
                
        #No Global Des Vars or Coupling Vars
        
        #Objective (single only)    
        self.add_objective('branin.f_xy')
        #No constraints for this problem
        
        self.solution = {
            'branin.x': -pi,
            'branin.y': 12.275,
            'branin.f_xy': 0.397887
        }
    

