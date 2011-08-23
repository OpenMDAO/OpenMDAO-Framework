"""
Two discipline components.
From Sellar's analytic problem.

    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
"""

from openmdao.main.api import Component, ComponentWithDerivatives
from openmdao.lib.datatypes.api import Float


class SellarDiscipline1(Component):
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



class SellarDiscipline2(Component):
    """Component containing Discipline 2"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) + z1 + z2"""
        
        z1 = self.z1
        z2 = self.z2
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.y2 = y1**(.5) + z1 + z2



class SellarDiscipline1withDerivatives(ComponentWithDerivatives):
    """Component containing Discipline 1"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable')
    y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y1 = Float(iotype='out', desc='Output of this Discipline')        


    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(SellarDiscipline1withDerivatives, self).__init__()

        self.derivatives.declare_first_derivative('y1', 'z1')
        self.derivatives.declare_first_derivative('y1', 'z2')
        self.derivatives.declare_first_derivative('y1', 'x1')
        self.derivatives.declare_first_derivative('y1', 'y2')


    def execute(self):
        """Evaluates the equation  
        y1 = z1**2 + z2 + x1 - 0.2*y2"""
        
        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2
        
        self.y1 = z1**2 + z2 + x1 - 0.2*y2
        #print "(%f, %f, %f)" % (z1, z2, x1)


    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        dy1_dz1 = 2.0*self.z1
        dy1_dz2 = 1.0
        dy1_dx1 = 1.0
        dy1_dy2 = -0.2
    
        self.derivatives.set_first_derivative('y1', 'z1', dy1_dz1)
        self.derivatives.set_first_derivative('y1', 'z2', dy1_dz2)
        self.derivatives.set_first_derivative('y1', 'x1', dy1_dx1)
        self.derivatives.set_first_derivative('y1', 'y2', dy1_dy2)
        


class SellarDiscipline2withDerivatives(ComponentWithDerivatives):
    """Component containing Discipline 2"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(SellarDiscipline2withDerivatives, self).__init__()

        self.derivatives.declare_first_derivative('y2', 'z1')
        self.derivatives.declare_first_derivative('y2', 'z2')
        self.derivatives.declare_first_derivative('y2', 'y1')

    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) + z1 + z2"""
        
        z1 = self.z1
        z2 = self.z2
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.y2 = y1**(.5) + z1 + z2

        
    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        y1 = abs(self.y1)
        
        dy2_dz1 = 1.0
        dy2_dz2 = 1.0
        dy2_dy1 = .5*y1**(-.5)
    
        self.derivatives.set_first_derivative('y2', 'z1', dy2_dz1)
        self.derivatives.set_first_derivative('y2', 'z2', dy2_dz2)
        self.derivatives.set_first_derivative('y2', 'y1', dy2_dy1)

        