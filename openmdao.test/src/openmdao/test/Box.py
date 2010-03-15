
from openmdao.main.api import Component
from openmdao.lib.api import Float

class Box(Component):
    # set up interface to the framework
    width = Float(1., iotype='in', units='cm')
    height = Float(1., iotype='in', units='cm')
    depth = Float(1., iotype='in', units='cm')
    thickness = Float(0.05, iotype='in', units='cm')
    density = Float(0.01, iotype='in', units='g/cm**3')
    
    mass = Float(0., iotype='out', units='g')
    volume = Float(0., iotype='out', units='cm**3')
    surface_area = Float(0., iotype='out', units='cm**2')
        
    def __init__(self, doc=None, directory=''):
        super(Box, self).__init__(doc, directory)
        
    def execute(self):
        self.surface_area = (self.width*(self.height+self.depth)+
                     self.depth*self.height)*2
        self.mass = self.surface_area*self.thickness*self.density        
        self.volume = self.width*self.height*self.depth

