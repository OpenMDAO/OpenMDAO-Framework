import unittest

import numpy as np
from openmdao.main.api import Component, Assembly, set_as_top
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

    def provideJ(self):

        dv = np.array([1.0, 1.0])
        dw = np.array([self.y, self.x])

        self.J = np.vstack((dv, dw))
        return self.J

    def list_deriv_vars(self):

        inputs = ('x', 'y')
        outputs = ('v', 'w')

        return inputs, outputs


class A(Assembly):

    def configure(self):

        self.add('c', C())
        self.driver.workflow.add('c')

        self.create_passthrough('c.x')
        self.create_passthrough('c.y')


class CheckGradientTestCase(unittest.TestCase):
    def test_check_gradient(self):
        a = set_as_top(A())
        a.x = 3.0
        a.y = 4.0
        a.run()
        Jbase, J, io_pairs, suspects = a.check_gradient('c', stream=None)
        self.assertEqual(suspects, [])
        
if __name__ == '__main__':
    unittest.main()
