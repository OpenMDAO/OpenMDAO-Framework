
from openmdao.main.component import Component, RUN_OK
from openmdao.main import Float
from openmdao.main.variable import INPUT, OUTPUT
from math import pi

class HollowSphere(Component):
    def __init__(self, name):
        Component.__init__(self, name)
        self.radius = 1.0
        self.thickness = 0.05
        self.volume = 0.
        self.inner_volume = 0.
        self.solid_volume = 0.
        self.surface_area = 0.
        
        # set up interface to the framework
        Float('radius', self, INPUT, units='cm')
        Float('thickness', self, INPUT, units='cm')
        
        Float('inner_volume', self, OUTPUT, units='cm**3')
        Float('volume', self, OUTPUT, units='cm**3')
        Float('solid_volume', self, OUTPUT, units='cm**3')
        Float('surface_area', self, OUTPUT, units='cm**2')

    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume
        return RUN_OK

