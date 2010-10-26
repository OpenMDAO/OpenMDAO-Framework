from numpy import sin

from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class Alg_Component2(Component):
    y = Float(iotype="in",low=-0.315,high=1.000001)
    
    f1 = Float(0.,iotype="out")
    f2 = Float(0.,iotype="out")
    
    def execute(self):
        if self.y<-0.075:
            self.f1 = -5308.7*self.y**4-3860.1*self.y**3-916.14*self.y**2-87.99 *self.y-1.971
            self.f2 = -11941 *self.y**4-9952.4*self.y**3-2784.1*self.y**2-283.04*self.y-13.425
        else:
            self.f1 = (6.*self.y-2)**2*sin(12.*self.y-4.)-5
            self.f2 = 0.5*self.f1+10.*(self.y-0.5)+1.25
