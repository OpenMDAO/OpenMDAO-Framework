"""
Wrappers for test models.
"""

import mool.Optimization.Models_test

from openmdao.main import Component, Float
from openmdao.main.component import RUN_OK
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class Model_A2d(Component):
    """ Wrapper for M4 Model_A2d. """

    def __init__(self, name='Model_A2d', parent=None):
        super(Model_A2d, self).__init__(name, parent)
        self._m4_comp = mool.Optimization.Models_test.Model_A2d()

        Float('x', self, INPUT, doc='X input value.', default=0.)
        Float('y', self, INPUT, doc='Y input value.', default=0.)

        Float('z1', self, OUTPUT, doc='exp(x) + exp(y)', default=0.)
        Float('z2', self, OUTPUT, doc='10.0*(x-2.0)**2 + 10.0*(y-1.5)**2 + 10.0',
              default = 0.)

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z1 = self._m4_comp.RunModel(vec, 0)
        self.z2 = self._m4_comp.RunModel(vec, 1)
        self.debug('function(%f, %f) = %f, %f',
                   self.x, self.y, self.z1, self.z2)
        return RUN_OK


class Model_B2d(Component):
    """ Wrapper for M4 Model_B2d. """

    def __init__(self, name='Model_B2d', parent=None):
        super(Model_B2d, self).__init__(name, parent)
        self._m4_comp = mool.Optimization.Models_test.Model_B2d()

        Float('x', self, INPUT, doc='X input value.', default=0.)
        Float('y', self, INPUT, doc='Y input value.', default=0.)

        Float('z', self, OUTPUT, doc='24.*x+24.*y', default=0.)

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z = self._m4_comp.RunModel(vec, 0)
        self.debug('function(%f, %f) = %f', self.x, self.y, self.z)
        return RUN_OK

