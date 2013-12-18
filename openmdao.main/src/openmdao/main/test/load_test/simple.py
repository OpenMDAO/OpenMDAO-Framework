# pylint: disable-msg=C0111,C0103

from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float


class Simple(Component):
    a = Float(iotype='in', units='ft')
    b = Float(iotype='in', units='ft')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
