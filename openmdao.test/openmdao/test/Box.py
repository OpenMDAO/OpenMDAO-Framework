
from openmdao.main.component import Component
from openmdao.main.variable import Float, INPUT, OUTPUT

class Box(Component):
    def __init__(self, name):
        Component.__init__(self, name)
        self.width = 1
        self.height = 1
        self.depth = 1
        self.thickness = 0.05
        self.density = .01
        self.mass = None
        self.volume = None
        self.surface_area = None
        
        # set up interface to the framework
        Float('width',INPUT,parent=self,units='cm')
        Float('height',INPUT,parent=self,units='cm')
        Float('depth',INPUT,parent=self,units='cm')
        Float('thickness',INPUT,parent=self,units='cm')
        Float('density',INPUT,parent=self,units='g/cm^3')
        
        Float('mass',OUTPUT,parent=self,units='g')
        Float('volume',OUTPUT,parent=self,units='cm^3')
        Float('surface_area',OUTPUT,parent=self,units='cm^2')
        
    def execute(self):
        self.surface_area = (self.width*(self.height+self.depth)+
                     self.depth*self.height)*2
        self.mass = self.surface_area*self.thickness*self.density        
        self.volume = self.width*self.height*self.depth

        
