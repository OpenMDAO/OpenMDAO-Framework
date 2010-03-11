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

    x = Float(0., iotype='in', desc='X input value.')
    y = Float(0., iotype='in', desc='Y input value.')

    z1 = Float(0., iotype='out', desc='exp(x) + exp(y)')
    z2 = Float(0., iotype='out', 
               desc='10.0*(x-2.0)**2 + 10.0*(y-1.5)**2 + 10.0')
        
    #name='Model_A2d', 
    def __init__(self, *args, **kwargs):
        super(Model_A2d, self).__init__(*args, **kwargs)
        self._m4_comp = mool.Optimization.Models_test.Model_A2d()

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z1 = self._m4_comp.RunModel(vec, 0)
        self.z2 = self._m4_comp.RunModel(vec, 1)
        self.debug('function(%f, %f) = %f, %f',
                   self.x, self.y, self.z1, self.z2)


class Model_B2d(Component):
    """ Wrapper for M4 Model_B2d. """

    x = Float(0., iotype='in', desc='X input value.')
    y = Float(0., iotype='in', desc='Y input value.')

    z = Float(0., iotype='out', desc='24.*x+24.*y')
        
    #name='Model_B2d'
    def __init__(self, *args, **kwargs):
        super(Model_B2d, self).__init__(*args, **kwargs)
        self._m4_comp = mool.Optimization.Models_test.Model_B2d()

    def execute(self):
        """ Run M4 component. """
        vec = [self.x, self.y]
        self.z = self._m4_comp.RunModel(vec, 0)
        self.debug('function(%f, %f) = %f', self.x, self.y, self.z)

