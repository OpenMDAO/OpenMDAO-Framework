from numpy import sin

from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float


class Alg_Component1(Component): 
    x = Float(iotype="in",low=0.,high=1.0)
    
    f1 = Float(0.,iotype="out")
    f2 = Float(0.,iotype="out")
    
    def execute(self):
        self.f1 = (6.*self.x-2)**2*sin(12.*self.x-4.)
        self.f2 = 0.5*self.f1+10.*(self.x-0.5)-5.
