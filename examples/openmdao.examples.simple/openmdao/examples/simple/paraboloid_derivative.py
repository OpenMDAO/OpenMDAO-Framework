"""
    paraboloid.py - Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3
"""

from numpy import array

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float

class ParaboloidDerivative(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')        

        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
        
    def provideJ(self):
        """Caculate and return the Jacobian"""
        
        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x
    
        return array([[df_dx, df_dy]])
    
    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        
        return input_keys, output_keys

# End paraboloid_derivative.py
