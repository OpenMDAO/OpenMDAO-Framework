
from openmdao.main.api import Component
from openmdao.lib.api import Float
from math import pi

class HollowSphere(Component):

    # set up interface to the framework
    radius       = Float(1.0,  iotype='in', units='cm')
    thickness    = Float(0.05, iotype='in', units='cm')
        
    inner_volume = Float(0., iotype='out', units='cm**3')
    volume       = Float(0., iotype='out', units='cm**3')
    solid_volume = Float(0., iotype='out', units='cm**3')
    surface_area = Float(0., iotype='out', units='cm**2')

    def __init__(self, doc=None, directory=''):
        super(HollowSphere, self).__init__(doc, directory) 
        
    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume

