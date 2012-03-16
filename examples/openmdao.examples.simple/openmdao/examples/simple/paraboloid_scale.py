"""
    paraboloid.py - Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float


class Paraboloid_scale(Component):
    """ Evaluates the equation f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*y) + (0.01*y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')        

        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 0.0066666666666666671; y = -733.33333333333337
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (1000.*x-3.)**2 + (1000.*x)*(0.01*y) + (0.01*y+4.)**2 - 3.
		
# End paraboloid.py
