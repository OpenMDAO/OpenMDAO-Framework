from math import pi
from numpy import sin,cos

from openmdao.main.api import Component, plugin
from openmdao.lib.datatypes.api import Float

@plugin('openmdao.component')
class SpiralComponent(Component): 
    x = Float(iotype="in",low=0.75,high=5.*pi)
    y = Float(iotype="in",low=0.75,high=5.*pi)
    
    f1_xy = Float(0.,iotype="out")
    f2_xy = Float(0.,iotype="out")
    
    def execute(self):
        self.f1_xy = cos(self.x)/self.x + sin(self.y)/self.y
        self.f2_xy = sin(self.x)/self.x + cos(self.y)/self.y
