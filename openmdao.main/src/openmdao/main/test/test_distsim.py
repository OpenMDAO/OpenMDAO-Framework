"""
Test distributed simulation.
"""

import logging
from math import pi
import os
import sys
import time
import unittest
import nose

from openmdao.main.api import Component
from openmdao.main.mp_support import read_server_config
from openmdao.main.objserverfactory import connect, start_server
from openmdao.main.rbac import Credentials, set_credentials

from openmdao.lib.api import Float, Int

from openmdao.util.testutil import assert_rel_error

_MODULE = 'openmdao.main.test.test_distsim'


class Box(Component):
    """ Simple component for testing. """

    width = Float(1., iotype='in', units='cm')
    height = Float(1., iotype='in', units='cm')
    depth = Float(1., iotype='in', units='cm')
    thickness = Float(0.05, iotype='in', units='cm')
    density = Float(0.01, iotype='in', units='g/cm**3')

    mass = Float(0., iotype='out', units='g')
    volume = Float(0., iotype='out', units='cm**3')
    surface_area = Float(0., iotype='out', units='cm**2')
    pid = Int(iotype='out')

    def __init__(self, doc=None, directory=''):
        super(Box, self).__init__(doc, directory)
        self.pid = os.getpid()

    def execute(self):
        self.surface_area = (self.width*(self.height+self.depth)+
                     self.depth*self.height)*2
        self.mass = self.surface_area*self.thickness*self.density        
        self.volume = self.width*self.height*self.depth


class HollowSphere(Component):
    """ Simple component for testing. """

    radius = Float(1.0,  iotype='in', units='cm')
    thickness = Float(0.05, iotype='in', units='cm')

    inner_volume = Float(0., iotype='out', units='cm**3')
    volume = Float(0., iotype='out', units='cm**3')
    solid_volume = Float(0., iotype='out', units='cm**3')
    surface_area = Float(0., iotype='out', units='cm**2')
    pid = Int(iotype='out')

    def __init__(self, doc=None, directory=''):
        super(HollowSphere, self).__init__(doc, directory) 
        self.pid = os.getpid()

    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume


class TestCase(unittest.TestCase):
    """ Test distributed simulation. """

    def setUp(self):
        """ Start server process. """
        self.server = start_server()

    def tearDown(self):
        """ Shut down server process. """
        if self.server is not None:
            self.server.terminate()
            self.server = None
        if os.path.exists('server.out'):
            os.remove('server.out')

    def test_client(self):
#        raise nose.SkipTest()
        logging.debug('')
        logging.debug('test_client')

        # Connects to ObjServerFactory service described by 'server.cfg' file,
        # creates some components, and runs them.
        set_credentials(Credentials())

        address, port, key = read_server_config('server.cfg')
        logging.debug('server address: %s', address)
        logging.debug('server port: %s', port)
        logging.debug('server key: %s', key)

        # Could hang forever (no timeout!)
        factory = connect(address, port, key)
        logging.debug('factory: %r', factory)

        types = factory.get_available_types()
        logging.debug('Available types:')
        for typname, version in types:
            logging.debug('   %s %s', typname, version)

        # First a HollowSphere, accessed via get()/set().
        obj = factory.create(_MODULE+'.HollowSphere')
        sphere_pid = obj.get('pid')
        self.assertNotEqual(sphere_pid, os.getpid())

        radius = obj.get('radius')
        self.assertEqual(radius, 1.)
        radius += 1
        obj.set('radius', radius)
        new_radius = obj.get('radius')
        self.assertEqual(new_radius, 2.)
        self.assertEqual(obj.get('inner_volume'), 0.)
        self.assertEqual(obj.get('volume'), 0.)
        self.assertEqual(obj.get('solid_volume'), 0.)
        self.assertEqual(obj.get('surface_area'), 0.)
        obj.run()
        assert_rel_error(self, obj.get('inner_volume'), 33.510321638, 0.000001)
        assert_rel_error(self, obj.get('volume'),       36.086951213, 0.000001)
        assert_rel_error(self, obj.get('solid_volume'), 2.5766295747, 0.000001)
        assert_rel_error(self, obj.get('surface_area'), 50.265482457, 0.000001)

        # Now a Box, accessed via attribute methods.
        obj = factory.create(_MODULE+'.Box')
        box_pid = obj.get('pid')
        self.assertNotEqual(box_pid, os.getpid())
        self.assertNotEqual(box_pid, sphere_pid)

        obj.width += 1
        obj.height += 1
        obj.depth += 1
        self.assertEqual(obj.width, 2.)
        self.assertEqual(obj.height, 2.)
        self.assertEqual(obj.depth, 2.)
        self.assertEqual(obj.thickness, 0.05)
        self.assertEqual(obj.density, 0.01)
        self.assertEqual(obj.mass, 0.)
        self.assertEqual(obj.volume, 0.)
        self.assertEqual(obj.surface_area, 0.)
        obj.run()
        assert_rel_error(self, obj.mass, 0.012, 0.000001)
        self.assertEqual(obj.volume, 8.0)
        self.assertEqual(obj.surface_area, 24.0)

    def test_shutdown(self):
#        raise nose.SkipTest()
        logging.debug('')
        logging.debug('test_shutdown')
        # At one time merely having this after test_client caused the test
        # process to never exit.


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

