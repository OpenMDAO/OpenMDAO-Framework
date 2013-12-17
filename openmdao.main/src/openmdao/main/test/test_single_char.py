import unittest

import numpy as np
from openmdao.main.api import Component, Assembly
from openmdao.main.datatypes.api import Float

class C(Component):

    # variables
    x = Float(iotype='in')
    y = Float(iotype='in')

    # outputs
    v = Float(iotype='out')
    w = Float(iotype='out')

    def execute(self):

        self.v = self.x + self.y
        self.w = self.x*self.y

    def linearize(self):

        dv = np.array([1.0, 1.0])
        dw = np.array([self.y, self.x])

        self.J = np.vstack((dv, dw))

    def provideJ(self):

        inputs = ('x', 'y')
        outputs = ('v', 'w')

        return inputs, outputs, self.J

class A(Assembly):

    def configure(self):

        self.add('c', C())
        self.driver.workflow.add('c')

        self.create_passthrough('c.x')
        self.create_passthrough('c.y')


class CheckGradientTestCase(unittest.TestCase):
    def test_check_gradient(self):
        a = A()
        a.x = 3.0
        a.y = 4.0
        a.run()
        suspects = a.check_gradient('c')
        self.assertEqual(suspects, [])
        
if __name__ == '__main__':
    unittest.main()
