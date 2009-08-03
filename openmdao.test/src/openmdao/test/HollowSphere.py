
from openmdao.main.api import Component
from math import pi

class HollowSphere(Component):
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(HollowSphere, self).__init__(name, parent, doc, directory) 
        self.radius = 1.0
        self.thickness = 0.05
        self.volume = 0.
        self.inner_volume = 0.
        self.solid_volume = 0.
        self.surface_area = 0.
        
        # set up interface to the framework
        Float('radius', self, iostatus='in', units='cm')
        Float('thickness', self, iostatus='in', units='cm')
        
        Float('inner_volume', self, 'out', units='cm**3')
        Float('volume', self, 'out', units='cm**3')
        Float('solid_volume', self, 'out', units='cm**3')
        Float('surface_area', self, 'out', units='cm**2')

    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume

