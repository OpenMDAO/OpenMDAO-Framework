import math

from openmdao.main.api import Component
from openmdao.lib.api import Float

class SinComponent(Component): 
    x = Float(0,iotype="in")
    
    f_x = Float(0,iotype="out")
    
    def execute(self):
        self.f_x = math.sin(self.x)
    