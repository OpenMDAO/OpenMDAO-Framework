"""
Two discipline components.
From Sellar's analytic problem.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.api import Float

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
        #print "Discipline 1 - %f, %f, %f, %f, %f!!" % (self.y1, self.y2, \
        #                                    self.z1, self.z2, self.x1)


class Discipline2(Component):
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
        #print "Discipline 2 - %f, %f, %f, %f!!" % (self.y1, self.y2, self.z1, \
        #                                          self.z2)


class Discipline2a(Component):
    """Component containing Discipline 2a"""
    
    # pylint: disable-msg=E1101
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    temp1 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = [y1**(.5)] + z1 + z2"""
        
        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will 
        # throw it out
        y1 = abs(self.y1)
        
        self.temp1 = y1**(.5)

        
class Discipline2b(Component):
    """Component containing Discipline 2b"""
    
    # pylint: disable-msg=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    temp1 = Float(0.0, iotype='in', desc='Intermediate Variable')

    temp2 = Float(iotype='out', desc='Intermediate Variable')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) [+ z1] + z2"""
        
        z1 = self.z1
        
        self.temp2 = self.temp1 + z1

        
class Discipline2c(Component):
    """Component containing Discipline 2c"""
    
    # pylint: disable-msg=E1101
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    temp2 = Float(0.0, iotype='in', desc='Intermediate Variable')

    y2 = Float(iotype='out', desc='Output of this Discipline')        

        
    def execute(self):
        """Evaluates the equation  
        y1 = y1**(.5) + z1 [+ z2]"""
        
        z2 = self.z2
        
        self.y2 = self.temp2 + z2
        
        
class Coupler(Component):
    """Component that holds some design variables.
    This is only needed because we can't hook an optimizer up to multiple
    locations of the same design variable"""
    
    # pylint: disable-msg=E1101
    z1_in = Float(0.0, iotype='in', desc='Global Design Variable')
    z2_in = Float(0.0, iotype='in', desc='Global Design Variable')
    x1_in = Float(0.0, iotype='in', desc='Local Design Variable for CO')
    y1_in = Float(0.0, iotype='in', desc='Coupling Variable')
    y2_in = Float(0.0, iotype='in', desc='Coupling Variable')
    z1 = Float(0.0, iotype='out', desc='Global Design Variable')
    z2 = Float(0.0, iotype='out', desc='Global Design Variable')
    x1 = Float(0.0, iotype='out', desc='Local Design Variable for CO')
    y1 = Float(0.0, iotype='out', desc='Coupling Variable')
    y2 = Float(0.0, iotype='out', desc='Coupling Variable')
    
    def execute(self):
        """ Pass everything through"""
        self.z1 = self.z1_in
        self.z2 = self.z2_in
        self.x1 = self.x1_in
        self.y1 = self.y1_in
        self.y2 = self.y2_in
        
# End discipline.py