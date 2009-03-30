
from openmdao.main import Component, Float
from openmdao.main.variable import INPUT, OUTPUT

class Box(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Box, self).__init__(name, parent, doc, directory)
        self.width = 1
        self.height = 1
        self.depth = 1
        self.thickness = 0.05
        self.density = .01
        self.mass = 0.
        self.volume = 0.
        self.surface_area = 0.
        
        # set up interface to the framework
        Float('width', self, INPUT, units='cm')
        Float('height', self, INPUT, units='cm')
        Float('depth', self, INPUT, units='cm')
        Float('thickness', self, INPUT, units='cm')
        Float('density', self, INPUT, units='g/cm**3')
        
        Float('mass', self, OUTPUT, units='g')
        Float('volume', self, OUTPUT, units='cm**3')
        Float('surface_area', self, OUTPUT, units='cm**2')
        
    def execute(self):
        self.surface_area = (self.width*(self.height+self.depth)+
                     self.depth*self.height)*2
        self.mass = self.surface_area*self.thickness*self.density        
        self.volume = self.width*self.height*self.depth

