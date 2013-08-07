from openmdao.main.api import Assembly, Component

from openmdao.lib.datatypes.api import Array, Float
from openmdao.lib.drivers.api import SLSQPdriver

class ArrayParaboloid(Component):
    inArray = Array([0.0, 0.0], iotype="in")
    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        x = self.inArray[0]
        y = self.inArray[1]

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 -3.0

class PartialConnectionAssembly(Assembly):

    def configure(self):
        self.add('driver', SLSQPdriver())

        self.add("paraboloid_1", ArrayParaboloid())
        self.add("paraboloid_2", ArrayParaboloid())

        self.driver.workflow.add(['paraboloid_1', 'paraboloid_2'])

        self.driver.iprint = 0

        self.driver.add_objective('paraboloid_1.f_xy')

        self.driver.add_parameter('paraboloid_1.inArray[0]', low=-50., high=50.)
        self.driver.add_parameter('paraboloid_1.inArray[1]', low=-50., high=50.)

        self.connect("paraboloid_1.f_xy", "paraboloid_2.inArray[0]")
        self.connect("paraboloid_1.f_xy", "paraboloid_2.inArray[1]")

class ArrayComponent(Component):
    inArray = Array([[0.0, 0.0, 0.0],[0.0, 0.0, 0.0]], iotype="in")
    f_inArray = Float(iotype='out', desc='F(inArray)')

    def execute(self):
        pass

class PartialConnectionAssembly2(Assembly):
    def configure(self):
        self.add('driver', SLSQPdriver())

        self.add("array_comp_1", ArrayComponent())
        self.add("array_comp_2", ArrayComponent())

        self.driver.workflow.add(['array_comp_1', 'array_comp_2'])

        self.driver.iprint = 0

        self.driver.add_objective('array_comp_1.f_inArray')

        self.driver.add_parameter('array_comp_1.inArray[0][0]', low=-50., high=50.)
        self.driver.add_parameter('array_comp_1.inArray[0][1]', low=-50., high=50.)
        self.driver.add_parameter('array_comp_1.inArray[1][0]', low=-50., high=50.)
        self.driver.add_parameter('array_comp_1.inArray[1][1]', low=-50., high=50.)

        self.connect("array_comp_1.f_inArray", "array_comp_2.inArray[0][0]")
        self.connect("array_comp_1.f_inArray", "array_comp_2.inArray[0][2]")
        self.connect("array_comp_1.f_inArray", "array_comp_2.inArray[1][0]")
        self.connect("array_comp_1.f_inArray", "array_comp_2.inArray[1][2]")

if __name__ == "__main__":
    myasm = PartialConnectionAssembly()
