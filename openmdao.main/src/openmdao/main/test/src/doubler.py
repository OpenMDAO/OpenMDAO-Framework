

from openmdao.main.api import Component, plugin
from openmdao.lib.datatypes.api import Float

@plugin('openmdao.component')
class Doubler(Component):
    x = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')

    def execute(self):
        self.y = self.x * 2

