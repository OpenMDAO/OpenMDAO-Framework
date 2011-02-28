

from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float


class Doubler(Component):
    x = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')

    def execute(self):
        self.y = self.x * 2

