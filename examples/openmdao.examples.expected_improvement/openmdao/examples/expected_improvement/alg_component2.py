
from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float

class Alg_Component2(Component):
    y = Float(iotype="in",low=-3.5,high=10.000001)
    
    f1 = Float(0.,iotype="out")
    f2 = Float(0.,iotype="out")
    
    def execute(self):
        self.f1 = self.y
        self.f2 = 12./(self.y+4.)-20.

