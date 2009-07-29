"""
Wrappers for test models.
"""

from enthought.traits.api import Float

import mool.Optimization.Models_test

from openmdao.main.api import Component

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class Model_A2d(Component):
    """ Wrapper for M4 Model_A2d. """

    def __init__(self, name='Model_A2d', *args, **kwargs):
        super(Model_A2d, self).__init__(name, *args, **kwargs)
        self._m4_comp = mool.Optimization.Models_test.Model_A2d()

        Float('x', self, iostatus='in', desc='X input value.', default=0.)
        Float('y', self, iostatus='in', desc='Y input value.', default=0.)

        Float('z1', self, iostatus='out', desc='exp(x) + exp(y)', default=0.)
        Float('z2', self, iostatus='out', desc='10.0*(x-2.0)**2 + 10.0*(y-1.5)**2 + 10.0',
              default = 0.)

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z1 = self._m4_comp.RunModel(vec, 0)
        self.z2 = self._m4_comp.RunModel(vec, 1)
        self.debug('function(%f, %f) = %f, %f',
                   self.x, self.y, self.z1, self.z2)


class Model_B2d(Component):
    """ Wrapper for M4 Model_B2d. """

    def __init__(self, name='Model_B2d', *args, **kwargs):
        super(Model_B2d, self).__init__(name, *args, **kwargs)
        self._m4_comp = mool.Optimization.Models_test.Model_B2d()

        Float('x', self, iostatus='in', desc='X input value.', default=0.)
        Float('y', self, iostatus='in', desc='Y input value.', default=0.)

        Float('z', self, iostatus='out', desc='24.*x+24.*y', default=0.)

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z = self._m4_comp.RunModel(vec, 0)
        self.debug('function(%f, %f) = %f', self.x, self.y, self.z)

