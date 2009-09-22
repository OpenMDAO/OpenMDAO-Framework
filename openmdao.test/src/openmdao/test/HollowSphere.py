
from openmdao.main.api import Component
from openmdao.lib.traits.unitsfloat import UnitsFloat
from math import pi

class HollowSphere(Component):

    # set up interface to the framework
    radius       = UnitsFloat(1.0,  iostatus='in', units='cm')
    thickness    = UnitsFloat(0.05, iostatus='in', units='cm')
        
    inner_volume = UnitsFloat(0., iostatus='out', units='cm**3')
    volume       = UnitsFloat(0., iostatus='out', units='cm**3')
    solid_volume = UnitsFloat(0., iostatus='out', units='cm**3')
    surface_area = UnitsFloat(0., iostatus='out', units='cm**2')

    def __init__(self, doc=None, directory=''):
        super(HollowSphere, self).__init__(doc, directory) 
        
    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume

