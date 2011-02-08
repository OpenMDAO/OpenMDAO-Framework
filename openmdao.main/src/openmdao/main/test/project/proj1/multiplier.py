
from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float

class Multiplier(Component):
    rval_in = Float(iotype='in')
    rval_out = Float(iotype='out')
    mult = Float(iotype='in')
    
    def __init__(self):
        super(Multiplier, self).__init__()
        self.rval_in = 4.
        self.rval_out = 7.
        self.mult = 1.5

    def execute(self):
        self.rval_out = self.rval_in * self.mult

