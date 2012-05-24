"""
Stuff for testing the GUI with VariableTrees.
"""

from openmdao.main.api import Component, Slot, VariableTree
from openmdao.lib.datatypes.api import Float


class VT(VariableTree):

    x = Float()
    y = Float()

    def __init__(self, x=0, y=0):
        super(VT, self).__init__()
        self.x = x
        self.y = y


class VTComp(Component):

    vt_in = Slot(VT, iotype='in')
    vt_out = Slot(VT, iotype='out')

    def __init__(self):
        super(VTComp, self).__init__()
        self.vt_in = VT(4, 2)
        self.vt_out = VT()

    def execute(self):
        self.vt_out.x = self.vt_in.x + 1
        self.vt_out.y = self.vt_in.y * 2

