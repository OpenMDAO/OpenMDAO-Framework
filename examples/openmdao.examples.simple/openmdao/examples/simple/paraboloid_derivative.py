"""
    paraboloid.py - Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class ParaboloidDerivative(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')        

        
    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(ParaboloidDerivative, self).__init__()

        self.derivatives.declare_first_derivative(self, 'f_xy', 'x')
        self.derivatives.declare_first_derivative(self, 'f_xy', 'y')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'x', 'x')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'x', 'y')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'y', 'y')

        
    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x
    
        self.derivatives.set_first_derivative('f_xy', 'x', df_dx)
        self.derivatives.set_first_derivative('f_xy', 'y', df_dy)
        
    def calculate_second_derivatives(self):
        """Analytical second derivatives"""
        
        df_dxdx = 2.0
        df_dxdy = 1.0
        df_dydy = 2.0
        
        self.derivatives.set_second_derivative('f_xy', 'x', 'x', df_dxdx)
        self.derivatives.set_second_derivative('f_xy', 'x', 'y', df_dxdy)
        self.derivatives.set_second_derivative('f_xy', 'y', 'y', df_dydy)
        

# End paraboloid_derivative.py
