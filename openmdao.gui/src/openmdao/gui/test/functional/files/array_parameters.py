from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Array, Float
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver


class ArrayParaboloid(Component):

    x = Array([[0., 0.]], iotype='in', desc='The variable x')
    f_x = Float(iotype='out', desc='F(x)')

    def execute(self):
        """f(x) = (x[0][0]-3)^2 + x[0][0]x[0][1] + (x[0][1]+4)^2 - 3
        Optimal solution (minimum): x[0][0] = 6.6667; x[0][1] = -7.3333
        """
        x = self.x
        self.f_x = (x[0][0]-3.0)**2 + x[0][0]*x[0][1] + (x[0][1]+4.0)**2 - 3.0


class ArrayParameters(Assembly):

    def configure(self):
        self.add('paraboloid', ArrayParaboloid())
        driver = self.add('driver', SLSQPdriver())
        driver.add_objective('paraboloid.f_x')
        driver.workflow.add('paraboloid')


if __name__ == '__main__':
    top = ArrayParameters()
    top.driver.add_parameter('paraboloid.x', low=-50, high=[40, 50],
                             scaler=[[1., 1]])
    top.run()
    print 'f_x %s at %s' % (top.paraboloid.f_x, top.paraboloid.x)
    print 'expecting optimum at 6.6667, -7.3333'

