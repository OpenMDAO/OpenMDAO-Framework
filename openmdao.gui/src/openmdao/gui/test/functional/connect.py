"""
Trivial model for testing connect/disconnect.
"""

from openmdao.main.api import Assembly, Component
from openmdao.lib.datatypes.api import Bool, Enum, Float, Int, Str


class Connectable(Component):

    b_in = Bool(True, iotype='in')
    e_in = Enum(3, values=(1, 2, 3), iotype='in')
    f_in = Float(2.781828, iotype='in')
    i_in = Int(42, iotype='in')
    s_in = Str('xyzzy', iotype='in')

    b_out = Bool(iotype='out')
    e_out = Enum(values=(1, 2, 3), iotype='out')
    f_out = Float(iotype='out')
    i_out = Int(iotype='out')
    s_out = Str(iotype='out')

    def execute(self):
        self.b_out = self.b_in
        self.e_out = self.e_in
        self.f_out = self.f_in
        self.i_out = self.i_in
        self.s_out = self.s_in


class Top(Assembly):

    def configure(self):
        self.add('comp1', Connectable())
        self.add('comp2', Connectable())
        self.driver.workflow.add(('comp1', 'comp2'))


if __name__ == '__main__':
    top = Top()
    for prefix in ('b', 'e', 'f', 'i', 's'):
        top.connect('comp1.'+prefix+'_out', 'comp2.'+prefix+'_in')
    top.run()

