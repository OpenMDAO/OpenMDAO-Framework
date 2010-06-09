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
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

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

    def test_coords(self):
        logging.debug('')
        logging.debug('test_coords')

        coords = GridCoordinates()
        self.assertEqual(coords.shape, ())
        self.assertEqual(coords.extent, ())

        logger = logging.getLogger()
        wedge3d = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        self.assertFalse(coords.is_equivalent(wedge3d.xyzzy.grid_coordinates,
                                              logger))

        wedge2d = create_wedge_2d((20, 10), 0.5, 2., 30.)
        self.assertFalse(coords.is_equivalent(wedge2d.xyzzy.grid_coordinates,
                                              logger))

        assert_raises(self, 'coords.flip_z()', globals(), locals(),
                      AttributeError, 'no Z coordinates')

        assert_raises(self, 'coords.translate(0., 0., 1.)', globals(), locals(),
                      AttributeError, 'no Z coordinates')

        assert_raises(self, 'coords.translate(0., 1., 0.)', globals(), locals(),
                      AttributeError, 'no Y coordinates')

        assert_raises(self, 'coords.translate(1., 0., 0.)', globals(), locals(),
                      AttributeError, 'no X coordinates')

        assert_raises(self, 'coords.rotate_about_x(0.)', globals(), locals(),
                      AttributeError, 'no Y coordinates')

        assert_raises(self, 'coords.rotate_about_y(0.)', globals(), locals(),
                      AttributeError, 'no X coordinates')

        assert_raises(self, 'coords.rotate_about_z(0.)', globals(), locals(),
                      AttributeError, 'no X coordinates')

    def test_vector(self):
        logging.debug('')
        logging.debug('test_vector')

        vec = Vector()
        self.assertEqual(vec.shape, ())
        self.assertEqual(vec.extent, ())

        logger = logging.getLogger()
        wedge3d = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        self.assertFalse(vec.is_equivalent(wedge3d.xyzzy.flow_solution.momentum,
                                           'momentum', logger))

        wedge2d = create_wedge_2d((20, 10), 0.5, 2., 30.)
        self.assertFalse(vec.is_equivalent(wedge2d.xyzzy.flow_solution.momentum,
                                           'momentum', logger))

        assert_raises(self, 'vec.flip_z()', globals(), locals(),
                      AttributeError, 'vector has no Z component')

        assert_raises(self, 'vec.rotate_about_x(0.)', globals(), locals(),
                      AttributeError, 'vector has no Y component')

        assert_raises(self, 'vec.rotate_about_y(0.)', globals(), locals(),
                      AttributeError, 'vector has no X component')

        assert_raises(self, 'vec.rotate_about_z(0.)', globals(), locals(),
                      AttributeError, 'vector has no X component')

    def test_misc(self):
        logging.debug('')
        logging.debug('test_misc')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        domain = DomainObj()
        domain.add_domain(wedge, make_copy=True)

        self.assertTrue(domain.is_equivalent(wedge, logger))

        # Just for test coverage.
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

        assert_raises(self, "domain.add_zone('xyzzy', wedge)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

        assert_raises(self, "domain.rename_zone('xyzzy', domain.xyzzy)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

        assert_raises(self,
                      "domain.xyzzy.flow_solution.add_vector('momentum',"
                      " numpy.zeros(domain.xyzzy.shape, numpy.float32))",
                      globals(), locals(), ValueError,
                      "name 'momentum' is already bound")

        domain.rename_zone('wedge', domain.xyzzy)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        self.assertFalse(domain.is_equivalent([], logger))
        self.assertFalse(domain.wedge.is_equivalent([], logger))
        self.assertFalse(domain.wedge.grid_coordinates.is_equivalent([], logger))
        self.assertFalse(
            domain.wedge.flow_solution.momentum.is_equivalent([], 'momentum',
                                                              logger))


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.traits.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

