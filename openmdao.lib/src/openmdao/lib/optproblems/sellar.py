"""
Two discipline components.
From Sellar's analytic problem.

    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
"""

from openmdao.main.api import Component, ComponentWithDerivatives
from openmdao.main.problem_formulation import OptProblem
from openmdao.lib.datatypes.api import Float

class Discipline1(Component):
    """Component containing Discipline 1"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable')
    y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y1 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = z1**2 + z2 + x1 - 0.2*y2"""
                
        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2
        
        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "(%f, %f, %f)" % (z1, z2, x1)
        
        
class Discipline1_WithDerivatives(ComponentWithDerivatives):
    """Component containing Discipline 1"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable')
    y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y1 = Float(iotype='out', desc='Output of this Discipline')        
   
    def __init__(self): 
        super(SellarDiscipline1_WithDerivatives,self).__init__()
        
        self.derivatives.declare_first_derivative(self, 'y1', 'z1')
        self.derivatives.declare_first_derivative(self, 'y1', 'z2')
        self.derivatives.declare_first_derivative(self, 'y1', 'x1')
        self.derivatives.declare_first_derivative(self, 'y1', 'y2')
        
    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
    
        self.derivatives.set_first_derivative('y1', 'z1', 2.0*self.z1)
        self.derivatives.set_first_derivative('y1', 'z2', 1.0)
        self.derivatives.set_first_derivative('y1', 'x1', 1.0)
        self.derivatives.set_first_derivative('y1', 'y2', -0.2)
    
        
    def execute(self):
        """Evaluates the equation  
        y1 = z1**2 + z2 + x1 - 0.2*y2"""
                
        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2
        
        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "(%f, %f, %f)" % (z1, z2, x1)        



class Discipline2(Component):
    """Component containing Discipline 2"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y2 = y1**(.5) + z1 + z2"""
                
        z1 = self.z1
        z2 = self.z2
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.y2 = y1**(.5) + z1 + z2
        
        
class Discipline2_WithDerivatives(ComponentWithDerivatives):
    """Component containing Discipline 2"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline') 
    
    def __init__(self): 
        super(SellarDiscipline2_WithDerivatives,self).__init__()
        
        self.derivatives.declare_first_derivative(self, 'y2', 'z1')
        self.derivatives.declare_first_derivative(self, 'y2', 'z2')
        self.derivatives.declare_first_derivative(self, 'y2', 'y1')
        
    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
    
        self.derivatives.set_first_derivative('y2', 'z1', 1.0)
        self.derivatives.set_first_derivative('y2', 'z2', 1.0)
        self.derivatives.set_first_derivative('y2', 'y1', .5*self.y1**-0.5)
       
    
    def execute(self):
        """Evaluates the equation  
        y2 = y1**(.5) + z1 + z2"""
                
        z1 = self.z1
        z2 = self.z2
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.y2 = y1**(.5) + z1 + z2        
           
class SellarProblem(OptProblem):
    """ Sellar test problem definition."""
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        super(SellarProblem, self).__init__()
        
        #add the discipline components to the assembly
        self.add('dis1', Discipline1())
        self.add('dis2', Discipline2())
        
        #START OF MDAO Problem Definition
        #Global Des Vars
        self.add_parameter(("dis1.z1","dis2.z1"),name="z1",low=-10,high=10,start=5.0)
        self.add_parameter(("dis1.z2","dis2.z2"),name="z2",low=0,high=10,start=2.0)
        
        #Local Des Vars 
        self.add_parameter("dis1.x1",low=0,high=10,start=1.0)
        
        #Coupling Vars
        self.add_coupling_var(("dis2.y1","dis1.y1"),name="y1",start=0.0)
        self.add_coupling_var(("dis1.y2","dis2.y2"),name="y2",start=0.0)
        #self.add_coupling_var(("dis1.y2","dis2.y2"),start=0.0)
                           
        self.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)',name="obj1")
        self.add_constraint('3.16 < dis1.y1')
        self.add_constraint('dis2.y2 < 24.0')
        
        #solution to the opt problem
        self.solution = {
            "z1":1.9776,
            "z2":0.0,
            "dis1.x1":0.0,
            "y1":3.16,
            "y2": 3.756,
            'obj1':3.1834
        }
        
        #END OF MDAO Problem Definition
              

        
