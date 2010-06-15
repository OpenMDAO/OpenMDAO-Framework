"""
Test :class:`DomainObj` operations.
"""

import logging
import unittest

import numpy

from openmdao.lib.traits.domain import DomainObj, GridCoordinates, Vector, \
                                       write_plot3d_q
from openmdao.lib.traits.domain.test.wedge import create_wedge_3d, \
                                                  create_wedge_2d
from openmdao.util.testutil import assert_raises


class TestCase(unittest.TestCase):
    """ Test :class:`DomainObj` operations. """

    def test_domain(self):
        logging.debug('')
        logging.debug('test_domain')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30., 'x')

        domain = DomainObj()
        self.assertEqual(domain.shape, [])
        self.assertEqual(domain.extent, [])

        domain.add_domain(wedge, make_copy=True)
        self.assertTrue(domain.is_equivalent(wedge))
        self.assertEqual(domain.shape, [(30, 20, 10)])
        self.assertEqual(domain.extent[0][:2], (0., 5.))
        self.assertEqual(domain.xyzzy.flow_solution.momentum.shape,
                         (30, 20, 10))
        self.assertEqual(domain.xyzzy.flow_solution.momentum.extent[:2],
                         (0., 5.))

        domain.make_left_handed()
        tmp = wedge.xyzzy.grid_coordinates.z * -1.
        self.assertTrue((domain.xyzzy.grid_coordinates.z == tmp).all())
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.make_right_handed()
        self.assertTrue(domain.is_equivalent(wedge, logger))

        # Strange translation for test coverage.
        domain.translate(0., 0., 0.5)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.translate(0., 1., 0.)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.translate(2.5, 0., 0.)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.rotate_about_x(90.)

        domain.add_domain(wedge)
        self.assertEqual(domain.shape, [(30, 20, 10), (30, 20, 10)])

        # Uncomment to visualize result.
#        write_plot3d_q(domain, 'doubled.xyz', 'doubled.q')

        domain.rotate_about_y(90.)
        domain.rotate_about_z(90.)

        domain.deallocate()
        self.assertEqual(domain.shape, [])
        self.assertEqual(domain.extent, [])

    def test_coordinate_systems(self):
        logging.debug('')
        logging.debug('test_coordinate_systems')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        cyl = wedge.copy()
        cyl.make_cylindrical()

        cart = cyl.copy()
        cart.make_cartesian()
        self.assertTrue(cart.is_equivalent(wedge, tolerance=0.000001))

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30., 'x')

        cyl = wedge.copy()
        cyl.make_cylindrical('x')

        cart = cyl.copy()
        cart.make_cartesian('x')
        self.assertTrue(cart.is_equivalent(wedge, tolerance=0.000001))

    def test_grid(self):
        logging.debug('')
        logging.debug('test_grid')

        grid = GridCoordinates()
        self.assertEqual(grid.shape, ())
        self.assertEqual(grid.extent, ())
        self.assertEqual(grid.ghosts, [0, 0, 0, 0, 0, 0])

        assert_raises(self, 'grid.ghosts = []', globals(), locals(),
                      ValueError, 'ghosts must be a 6-element array',
                      use_exec=True)

        assert_raises(self, 'grid.ghosts = [0, 1, 2, 3, 4, -5]',
                      globals(), locals(), ValueError,
                      'All ghost values must be >= 0', use_exec=True)

        assert_raises(self, 'grid.translate(1., 0., 0.)', globals(), locals(),
                      AttributeError, 'no X coordinates')

        assert_raises(self, 'grid.translate(0., 1., 0.)', globals(), locals(),
                      AttributeError, 'no Y coordinates')

        assert_raises(self, 'grid.translate(0., 0., 1.)', globals(), locals(),
                      AttributeError, 'no Z coordinates')

    def test_vector(self):
        logging.debug('')
        logging.debug('test_vector')

        vec = Vector()
        self.assertEqual(vec.shape, ())
        self.assertEqual(vec.extent, ())

        assert_raises(self, 'vec.flip_z()', globals(), locals(),
                      AttributeError, 'flip_z: no Z component')

        assert_raises(self, 'vec.rotate_about_x(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_x: no Y component')

        assert_raises(self, 'vec.rotate_about_y(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_y: no X component')

        assert_raises(self, 'vec.rotate_about_z(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_z: no X component')

    def test_misc(self):
        logging.debug('')
        logging.debug('test_misc')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        domain = DomainObj()
        domain.add_domain(wedge, make_copy=True)
        self.assertTrue(domain.is_equivalent(wedge, logger))

        domain.xyzzy.flow_solution.add_vector('fred',
                                              numpy.zeros(domain.xyzzy.shape,
                                                          float))
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.xyzzy.flow_solution.momentum.z += 1.
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.xyzzy.flow_solution.momentum.y += 1.
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.xyzzy.flow_solution.momentum.x += 1.
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.rename_zone('wedge', domain.xyzzy)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        self.assertFalse(domain.is_equivalent([], logger))
        self.assertFalse(domain.wedge.is_equivalent([], logger))
        self.assertFalse(domain.wedge.grid_coordinates.is_equivalent([], logger))
        self.assertFalse(
            domain.wedge.flow_solution.momentum.is_equivalent([], 'momentum',
                                                              logger))
    def test_errors(self):
        logging.debug('')
        logging.debug('test_errors')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        domain = wedge.copy()

        assert_raises(self, "domain.add_zone('xyzzy', wedge)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

        assert_raises(self, "domain.rename_zone('xyzzy', domain.xyzzy)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

        assert_raises(self,
                      "domain.xyzzy.flow_solution.add_array('density',"
                      " numpy.zeros(domain.xyzzy.shape, numpy.float32))",
                      globals(), locals(), ValueError,
                      "name 'density' is already bound")

        assert_raises(self,
                      "domain.xyzzy.flow_solution.add_vector('momentum',"
                      " numpy.zeros(domain.xyzzy.shape, numpy.float32))",
                      globals(), locals(), ValueError,
                      "name 'momentum' is already bound")


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.traits.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

