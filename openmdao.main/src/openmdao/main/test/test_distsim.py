"""
Test distributed simulation.
"""

import logging
from math import pi
import os
import shutil
import sys
import time
import unittest
import nose

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.util.decorators import add_delegate
from openmdao.main.hasobjective import HasObjectives
from openmdao.main.hasparameters import HasParameters
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

    mass = Float(iotype='out', units='g')
    volume = Float(iotype='out', units='cm**3')
    surface_area = Float(iotype='out', units='cm**2')
    pid = Int(iotype='out')

    def __init__(self, doc=None, directory=''):
        super(Box, self).__init__(doc, directory)
        self.pid = os.getpid()

    def execute(self):
        print 'Box.execute(), %f %f %f on %d' % (self.width, self.height, self.depth, os.getpid())
        self.surface_area = (self.width*(self.height+self.depth)+
                     self.depth*self.height)*2
        self.mass = self.surface_area*self.thickness*self.density        
        self.volume = self.width*self.height*self.depth


class HollowSphere(Component):
    """ Simple component for testing. """

    radius = Float(1.0,  iotype='in', units='cm')
    thickness = Float(0.05, iotype='in', units='cm')

    inner_volume = Float(iotype='out', units='cm**3')
    volume = Float(iotype='out', units='cm**3')
    solid_volume = Float(iotype='out', units='cm**3')
    surface_area = Float(iotype='out', units='cm**2')
    pid = Int(iotype='out')

    def __init__(self, doc=None, directory=''):
        super(HollowSphere, self).__init__(doc, directory) 
        self.pid = os.getpid()

    def execute(self):
        self.surface_area = 4.0*pi*self.radius*self.radius
        self.inner_volume = 4.0/3.0*pi*self.radius**3
        self.volume = 4.0/3.0*pi*(self.radius+self.thickness)**3
        self.solid_volume = self.volume-self.inner_volume


@add_delegate(HasParameters)
@add_delegate(HasObjectives)
class BoxDriver(Driver):
    """ Just drives :class:`Box` inputs and records results. """

    def execute(self):
        """ Runs with various box parameter values. """
        for width in range(1, 3):
            for height in range(1, 4):
                for depth in range(1, 5):
                    self._logger.debug('w,h,d %s, %s, %s', width, height, depth)
                    self.set_parameters((width, height, depth))
                    self.workflow.run()
                    volume, area = self.eval_objectives()
                    self._logger.debug('    v,a %s, %s', volume, area)


class BoxSource(Component):
    """ Just a pass-through for :class:`BoxDriver` input values. """

    width_in  = Float(1., low=0., exclude_low=True, iotype='in', units='cm')
    height_in = Float(1., low=0., exclude_low=True, iotype='in', units='cm')
    depth_in  = Float(1., low=0., exclude_low=True, iotype='in', units='cm')

    width_out  = Float(iotype='out', units='cm')
    height_out = Float(iotype='out', units='cm')
    depth_out  = Float(iotype='out', units='cm')

    def execute(self):
        """ Copy inputs to outputs. """
        self.width_out  = self.width_in
        self.height_out = self.height_in
        self.depth_out  = self.depth_in


class BoxSink(Component):
    """ Just a pass-through for :class:`BoxDriver` result values. """

    volume_in = Float(0., iotype='in', units='cm**3')
    area_in   = Float(0., iotype='in', units='cm**2')

    volume_out = Float(iotype='out', units='cm**3')
    area_out   = Float(iotype='out', units='cm**2')

    def execute(self):
        """ Copy inputs to outputs. """
        self.volume_out = self.volume_in
        self.area_out   = self.area_in


class Model(Assembly):
    """ Drive a remote :class:`Box` via connections to local components. """

    def __init__(self, box):
        super(Model, self).__init__()
        self.add('driver', BoxDriver())
        self.driver.workflow.add(self.add('source', BoxSource()))
        self.driver.workflow.add(self.add('box', box))
        self.driver.workflow.add(self.add('sink', BoxSink()))

        self.driver.add_parameter('source.width_in')
        self.driver.add_parameter('source.height_in')
        self.driver.add_parameter('source.depth_in')

        self.connect('source.width_out',  'box.width')
        self.connect('source.height_out', 'box.height')
        self.connect('source.depth_out',  'box.depth')

        self.connect('box.volume',       'sink.volume_in')
        self.connect('box.surface_area', 'sink.area_in')

        self.driver.add_objective('sink.volume_out')
        self.driver.add_objective('sink.area_out')


class TestCase(unittest.TestCase):
    """ Test distributed simulation. """

    def setUp(self):
        """ Start server process. """
        if os.path.exists('server_dir'):
            shutil.rmtree('server_dir')
        os.mkdir('server_dir')
        os.chdir('server_dir')
        try:
            self.server = start_server()
            set_credentials(Credentials())
            address, port, key = read_server_config('server.cfg')
            logging.debug('server address: %s', address)
            logging.debug('server port: %s', port)
            logging.debug('server key: %s', key)
        finally:
            os.chdir('..')

        # Could hang forever (no timeout!)
        self.factory = connect(address, port, key)
        logging.debug('factory: %r', self.factory)

    def tearDown(self):
        """ Shut down server process. """
        if self.server is not None:
            self.server.terminate()
            self.server = None
#        shutil.rmtree('server_dir')

    def zest_client(self):
        logging.debug('')
        logging.debug('test_client')

        # Connects to ObjServerFactory service described by 'server.cfg' file,
        # creates some components, and runs them.

        # List available types.
        types = self.factory.get_available_types()
        logging.debug('Available types:')
        for typname, version in types:
            logging.debug('   %s %s', typname, version)

        # First a HollowSphere, accessed via get()/set().
        obj = self.factory.create(_MODULE+'.HollowSphere')
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
        obj = self.factory.create(_MODULE+'.Box')
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

    def test_model(self):
        logging.debug('')
        logging.debug('test_model')
        box = self.factory.create(_MODULE+'.Box')
#        box = Box()
        model = set_as_top(Model(box))
        model.run()

    def zest_shutdown(self):
        logging.debug('')
        logging.debug('test_shutdown')
        # At one time merely having this after test_client caused the test
        # process to never exit.


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

