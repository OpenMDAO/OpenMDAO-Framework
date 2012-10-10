from openmdao.main.api import Component, Assembly
from openmdao.lib.datatypes.api import Float


class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(0.0, iotype='out', desc='F(x,y)')

    def execute(self):

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
        
class A1(Assembly):
    """ test assembly. """
    def configure(self):
        self.add('p2', Paraboloid())

        self.driver.workflow.add('p2')
        
        self.create_passthrough('p2.f_xy')
        

class Topp(Assembly):
    def configure(self):
        self.add('A1', A1())
        self.add('p1', Paraboloid())

        self.driver.workflow.add(['A1', 'p1'])
        
        self.create_passthrough('p1.y')
        self.connect('A1.f_xy','p1.x')
        