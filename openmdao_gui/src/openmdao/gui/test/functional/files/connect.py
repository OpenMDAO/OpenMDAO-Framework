"""
Trivial model for testing connect/disconnect.
"""

from openmdao.main.api import Assembly, Component
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.main.datatypes.api import Bool, Enum, Float, Int, Str


class Connectable(Component):

    b_in = Bool(iotype='in')
    e_in = Enum(values=(1, 2, 3), iotype='in')
    f_in = Float(iotype='in')
    i_in = Int(iotype='in')
    s_in = Str(iotype='in')
    x_in = Float(iotype='in')
    w_in = Float(iotype='in', units='g')

    b_out = Bool(iotype='out')
    e_out = Enum(values=(1, 2, 3), iotype='out')
    f_out = Float(iotype='out')
    i_out = Int(iotype='out')
    s_out = Str(iotype='out')
    x_out = Float(iotype='out')
    w_out = Float(5.0, iotype='out', units='kg')

    def execute(self):
        self.b_out = self.b_in
        self.e_out = self.e_in
        self.f_out = self.f_in
        self.i_out = self.i_in
        self.s_out = self.s_in
        self.x_out = self.x_in


class Conn_Bounds(Connectable):

    x = Float(3.3, iotype='in', low=0, high=299)


class Conn_Assy(Assembly):

    def configure(self):
        self.add('comp', Conn_Bounds())
        self.add('driver', CONMINdriver())
        self.driver.workflow.add(('comp'))


class Topp(Assembly):

    def configure(self):
        self.add('comp1', Connectable())
        self.add('comp2', Connectable())
        self.driver.workflow.add(('comp1', 'comp2'))


if __name__ == '__main__':
    top = Topp()

    top.comp1.b_in = True
    top.comp1.e_in = 3
    top.comp1.f_in = 2.781828
    top.comp1.i_in = 42
    top.comp1.s_in = 'xyzzy'

    for prefix in ('b', 'e', 'f', 'i', 's', 'w'):
        top.connect('comp1.'+prefix+'_out', 'comp2.'+prefix+'_in')

    top.connect('comp1.f_out+comp1.i_out', 'comp2.x_in')

    assert top.comp2.b_out is False
    assert top.comp2.e_out == 1
    assert top.comp2.f_out == 0.
    assert top.comp2.i_out == 0
    assert top.comp2.s_out == ''
    assert top.comp2.x_out == 0.

    top.run()

    assert top.comp2.b_out is True
    assert top.comp2.e_out == 3
    assert top.comp2.f_out == 2.781828
    assert top.comp2.i_out == 42
    assert top.comp2.s_out == 'xyzzy'
    assert top.comp2.x_out == 44.781828
    assert top.comp2.w_in == 1000*top.comp1.w_out
