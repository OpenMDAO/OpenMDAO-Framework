from math import cos, pi

from openmdao.main.api import Component
from openmdao.lib.api import Float

class BraninComponent(Component): 
    x = Float(0,iotype="in",low=-5,high=10)
    y = Float(0,iotype="in",low=0,high=15)
    
    f_xy = Float(0,iotype="out")
    
    def execute(self):
        self.f_xy = (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x)+10.
    