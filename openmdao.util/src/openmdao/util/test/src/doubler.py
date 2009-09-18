
from enthought.traits.api import Float

from openmdao.main.api import Component

class Doubler(Component):
    x = Float(0.0, iostatus='in')
    y = Float(0.0, iostatus='out')

    def execute(self):
        self.y = self.x * 2

