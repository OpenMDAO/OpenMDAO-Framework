"""
Test :class:`DomainObj` operations.
"""

import logging
import os.path
import unittest

import numpy  # don't remove, this is needed in globals

from openmdao.lib.datatypes.domain import DomainObj, FlowSolution, \
                                          GridCoordinates, Vector, Zone, \
                                          read_plot3d_q, write_plot3d_q

from openmdao.lib.datatypes.domain.test.wedge import create_wedge_3d, \
                                                     create_wedge_2d, \
                                                     create_curve_2d

from openmdao.util.testutil import assert_raises, assert_rel_error


class TestCase(unittest.TestCase):
    """ Test :class:`DomainObj` operations. """

    def test_domain(self):
        logging.debug('')
        logging.debug('test_domain')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30., 'x')

        domain = DomainObj()
        self.assertEqual(domain.reference_state, None)
        self.assertEqual(domain.zones, [])
        self.assertEqual(domain.shape, [])
        self.assertEqual(domain.extent, [])

        self.assertFalse(domain.is_equivalent([], logger))

        domain.add_domain(wedge, make_copy=True)
        self.assertTrue(domain.is_equivalent(wedge))
        self.assertEqual(domain.shape, [(30, 20, 10)])
        self.assertEqual(domain.extent[0][:2], (0., 5.))
        self.assertEqual(domain.xyzzy.flow_solution.momentum.shape,
                         (30, 20, 10))

        domain.rename_zone('wedge', domain.xyzzy)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain = wedge.copy()
        zone = domain.remove_zone('xyzzy')
        self.assertFalse(domain.is_equivalent(wedge, logger))
        domain.add_zone('xyzzy', zone)
        self.assertTrue(domain.is_equivalent(wedge, logger))
        zone = domain.remove_zone(zone)
        self.assertFalse(domain.is_equivalent(wedge, logger))
        domain.add_zone('xyzzy', zone)
        self.assertTrue(domain.is_equivalent(wedge, logger))

        assert_raises(self, 'domain.remove_zone(Zone())', globals(), locals(),
                      ValueError, 'cannot find zone!')

        # Translations ordered for test coverage.
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

        domain = wedge.copy()

        assert_raises(self, "domain.add_zone('xyzzy', wedge)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

        assert_raises(self, "domain.rename_zone('xyzzy', domain.xyzzy)",
                      globals(), locals(), ValueError,
                      "name 'xyzzy' is already bound")

    def test_coordinate_systems(self):
        logging.debug('')
        logging.debug('test_coordinate_systems')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        domain = wedge.copy()
        domain.make_left_handed()
        tmp = wedge.xyzzy.grid_coordinates.z * -1.
        self.assertTrue((domain.xyzzy.grid_coordinates.z == tmp).all())
        self.assertFalse(domain.is_equivalent(wedge, logger))

        domain.make_right_handed()
        self.assertTrue(domain.is_equivalent(wedge, logger))

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

    def test_flow(self):
        logging.debug('')
        logging.debug('test_flow')

        logger = logging.getLogger()

        flow = FlowSolution()
        self.assertEqual(flow.grid_location, 'Vertex')
        self.assertEqual(flow.ghosts, (0, 0, 0, 0, 0, 0))

        self.assertFalse(flow.is_equivalent([], logger))

        other = FlowSolution()
        self.assertTrue(flow.is_equivalent(other, logger))
        other.grid_location = 'CellCenter'
        self.assertFalse(flow.is_equivalent(other, logger))
        other.grid_location = 'Vertex'
        other.ghosts = [1, 1, 1, 1, 1, 1]
        self.assertFalse(flow.is_equivalent(other, logger))

        assert_raises(self, "flow.grid_location = 'pluto'",
                      globals(), locals(), ValueError,
                      "'pluto' is not a valid grid location", use_exec=True)

        assert_raises(self, "flow.ghosts = [1, 2, 3, 4, 5, -6]",
                      globals(), locals(), ValueError,
                      'All ghost values must be >= 0', use_exec=True)

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        domain = wedge.copy()

        assert_raises(self, "domain.xyzzy.flow_solution.ghosts = []",
                      globals(), locals(), ValueError,
                      'ghosts must be a 6-element array', use_exec=True)

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

        self.assertTrue(domain.is_equivalent(wedge, logger))
        pressure = wedge.xyzzy.flow_solution.density.copy()
        domain.xyzzy.flow_solution.add_array('pressure', pressure)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        pressure2 = wedge.xyzzy.flow_solution.density.copy()
        wedge.xyzzy.flow_solution.add_array('pressure', pressure2)
        self.assertTrue(domain.is_equivalent(wedge, logger))

        assert_raises(self,
                      "domain.xyzzy.flow_solution.add_array('empty', numpy.zeros(()))",
                      globals(), locals(), ValueError,
                      'array shape () != existing shape (30, 20, 10)')

        assert_raises(self,
                      "domain.xyzzy.flow_solution.add_vector('empty', Vector())",
                      globals(), locals(), ValueError,
                      'vector real shape () != existing real shape (30, 20, 10)')

        self.assertTrue(domain.is_equivalent(wedge, logger))
        pressure2[0][0][0] += 1.
        self.assertFalse(domain.is_equivalent(wedge, logger))
        self.assertFalse(domain.is_equivalent(wedge, logger, tolerance=0.00001))
 
        pressure2[0][0][0] -= 1.
        self.assertTrue(domain.is_equivalent(wedge, logger))
        domain.xyzzy.flow_solution.add_vector('mom2',
                                            domain.xyzzy.flow_solution.momentum)
        self.assertFalse(domain.is_equivalent(wedge, logger))

        flow = FlowSolution()
        self.assertEqual(flow.shape, ())

    def test_grid(self):
        logging.debug('')
        logging.debug('test_grid')

        logger = logging.getLogger()

        grid = GridCoordinates()
        self.assertEqual(grid.shape, ())
        self.assertEqual(grid.extent, ())
        self.assertEqual(grid.ghosts, (0, 0, 0, 0, 0, 0))

        self.assertFalse(grid.is_equivalent([], logger))

        other = GridCoordinates()
        self.assertTrue(grid.is_equivalent(other, logger))
        other.ghosts = [1, 1, 1, 1, 1, 1]
        self.assertFalse(grid.is_equivalent(other, logger))

        assert_raises(self, 'grid.ghosts = [0, 1, 2, 3, 4, -5]',
                      globals(), locals(), ValueError,
                      'All ghost values must be >= 0', use_exec=True)

        assert_raises(self, 'grid.translate(1., 0., 0.)',
                      globals(), locals(), AttributeError, 'no X coordinates')

        assert_raises(self, 'grid.translate(0., 1., 0.)',
                      globals(), locals(), AttributeError, 'no Y coordinates')

        assert_raises(self, 'grid.translate(0., 0., 1.)',
                      globals(), locals(), AttributeError, 'no Z coordinates')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        assert_raises(self, 'wedge.xyzzy.grid_coordinates.ghosts = []',
                      globals(), locals(), ValueError,
                      'ghosts must be a 6-element array', use_exec=True)

        assert_raises(self,
                      "wedge.xyzzy.grid_coordinates.make_cylindrical(axis='q')",
                      globals(), locals(), ValueError,
                      "axis must be 'z' or 'x'")

        wedge.xyzzy.grid_coordinates.make_cylindrical(axis='z')
        assert_rel_error(self, wedge.xyzzy.grid_coordinates.extent,
                         (0.5, 2.0, 0.0, 0.52359879, 0.0, 5.0),
                         0.000001)
        assert_raises(self,
                      "wedge.xyzzy.grid_coordinates.make_cartesian(axis='q')",
                      globals(), locals(), ValueError,
                      "axis must be 'z' or 'x'")
        wedge.xyzzy.grid_coordinates.make_cartesian(axis='z')
        assert_rel_error(self, wedge.xyzzy.grid_coordinates.extent,
                         (0.43301269, 2.0, 0.0, 1.0, 0.0, 5.0),
                         0.000001)

        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)
        wedge.xyzzy.grid_coordinates.make_cylindrical()
        assert_rel_error(self, wedge.xyzzy.grid_coordinates.extent,
                         (0.5, 2.0, 0.0, 0.52359879), 0.000001)

        curve = create_curve_2d(20, 0.5, 30.)
        assert_rel_error(self, curve.xyzzy.grid_coordinates.extent,
                         (0.43301269, 0.5, 0.0, 0.25), 0.000001)
        curve.xyzzy.grid_coordinates.make_cylindrical()
        assert_rel_error(self, curve.xyzzy.grid_coordinates.extent,
                         (0.5, 0.5, 0.0, 0.52359879), 0.000001)

    def test_vector(self):
        logging.debug('')
        logging.debug('test_vector')

        logger = logging.getLogger()

        vec = Vector()
        self.assertEqual(vec.x, None)
        self.assertEqual(vec.y, None)
        self.assertEqual(vec.z, None)
        self.assertEqual(vec.r, None)
        self.assertEqual(vec.t, None)
        self.assertEqual(vec.shape, ())

        self.assertFalse(vec.is_equivalent([], 'vec', logger))

        assert_raises(self, 'vec.flip_z()', globals(), locals(),
                      AttributeError, 'flip_z: no Z component')

        assert_raises(self, 'vec.rotate_about_x(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_x: no Y component')

        assert_raises(self, 'vec.rotate_about_y(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_y: no X component')

        assert_raises(self, 'vec.rotate_about_z(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_z: no X component')

        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)
        vec = wedge.xyzzy.flow_solution.momentum
        assert_raises(self, 'vec.rotate_about_x(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_x: no Z component')

        assert_raises(self, 'vec.rotate_about_y(0.)', globals(), locals(),
                      AttributeError, 'rotate_about_y: no Z component')

        wedge.make_cylindrical()

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        grid = wedge.xyzzy.grid_coordinates
        grid.make_cylindrical(axis='z')
        vec = wedge.xyzzy.flow_solution.momentum
        assert_raises(self, "vec.make_cylindrical(grid, axis='q')",
                      globals(), locals(), ValueError,
                      "axis must be 'z' or 'x'")
        assert_raises(self, "vec.make_cartesian(grid, axis='q')",
                      globals(), locals(), ValueError,
                      "axis must be 'z' or 'x'")
        grid.make_cartesian(axis='z')

        domain = wedge.copy()
        domain.xyzzy.flow_solution.momentum.z += 1.
        self.assertFalse(domain.is_equivalent(wedge, logger))
        self.assertFalse(domain.is_equivalent(wedge, logger, tolerance=0.00001))

        domain = wedge.copy()
        grid = domain.xyzzy.grid_coordinates
        grid.make_cylindrical(axis='z')
        vec = domain.xyzzy.flow_solution.momentum
        vec.make_cylindrical(grid)
        self.assertFalse(vec.is_equivalent(wedge.xyzzy.flow_solution.momentum,
                                           'momentum', logger))

    def test_zone(self):
        logging.debug('')
        logging.debug('test_zone')

        logger = logging.getLogger()

        zone = Zone()
        self.assertEqual(zone.reference_state, None)
        self.assertEqual(zone.coordinate_system, 'Cartesian')
        self.assertEqual(zone.right_handed, True)
        self.assertEqual(zone.symmetry, None)
        self.assertEqual(zone.symmetry_axis, None)
        self.assertEqual(zone.symmetry_instances, 1)
        self.assertEqual(zone.shape, ())
        self.assertEqual(zone.extent, ())

        self.assertFalse(zone.is_equivalent([], logger))

        assert_raises(self, "zone.coordinate_system = 'pluto'",
                      globals(), locals(), ValueError,
                      "invalid coordinate system 'pluto'", use_exec=True)

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        domain = wedge.copy()

        domain.xyzzy.symmetry_instances = 5
        self.assertFalse(domain.is_equivalent(wedge))

        domain.xyzzy.symmetry_axis = 'x'
        self.assertFalse(domain.is_equivalent(wedge))

        domain.xyzzy.symmetry = 'rotational'
        self.assertFalse(domain.is_equivalent(wedge))

        domain.make_cylindrical()
        self.assertFalse(domain.is_equivalent(wedge))
        assert_raises(self, 'domain.translate(0., 0., 0.)', globals(), locals(),
                      RuntimeError, 'Zone not in cartesian coordinates')
        assert_raises(self, 'domain.rotate_about_x(0.)', globals(), locals(),
                      RuntimeError, 'Zone not in cartesian coordinates')
        assert_raises(self, 'domain.rotate_about_y(0.)', globals(), locals(),
                      RuntimeError, 'Zone not in cartesian coordinates')
        assert_raises(self, 'domain.rotate_about_z(0.)', globals(), locals(),
                      RuntimeError, 'Zone not in cartesian coordinates')

    def test_extract(self):
        logging.debug('')
        logging.debug('test_extract')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        domain = wedge.extract([(10, -10, 10, 15, 0, -1)])
        volume = domain.xyzzy
        self.assertEqual(volume.shape, (11, 6, 10))
        self.assertEqual(volume.flow_solution.shape, (11, 6, 10))
        assert_rel_error(self, volume.extent,
                         (1.116717, 1.6842105, 0.0, 0.84210527, 1.7241379, 3.4482758),
                         0.000001)

        surface = wedge.xyzzy.extract(0, -1, 10, 10, 0, -1)
        self.assertEqual(surface.shape, (30, 1, 10))
        self.assertEqual(surface.flow_solution.shape, (30, 1, 10))
        assert_rel_error(self, surface.extent,
                         (1.116717, 1.2894737, 0.0, 0.64473683, 0.0, 5.0),
                         0.000001)

        curve = wedge.xyzzy.extract(-1, -1, 10, 10, 0, -1)
        self.assertEqual(curve.shape, (1, 1, 10))
        self.assertEqual(curve.flow_solution.shape, (1, 1, 10))
        assert_rel_error(self, curve.extent,
                         (1.116717, 1.2894737, 0.0, 0.64473683, 5.0, 5.0),
                         0.000001)

        code = 'wedge.xyzzy.flow_solution.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '3D extract requires jmin, jmax, kmin, and kmax')
        code = 'wedge.xyzzy.flow_solution.momentum.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '3D extract requires jmin, jmax, kmin, and kmax')
        code = 'wedge.xyzzy.grid_coordinates.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '3D extract requires jmin, jmax, kmin, and kmax')

        code = 'wedge.xyzzy.flow_solution.extract(0, 99, 0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99, 0, 99)'
                      ' exceeds original (0, 30, 0, 20, 0, 10)')
        code = 'wedge.xyzzy.flow_solution.momentum.extract(0, 99, 0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99, 0, 99)'
                      ' exceeds original (0, 30, 0, 20, 0, 10)')
        code = 'wedge.xyzzy.grid_coordinates.extract(0, 99, 0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99, 0, 99)'
                      ' exceeds original (0, 30, 0, 20, 0, 10)')

        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)

        surface = wedge.xyzzy.extract(5, -5, 1, -2)
        self.assertEqual(surface.shape, (11, 8))
        self.assertEqual(surface.flow_solution.shape, (11, 8))
        assert_rel_error(self, surface.extent,
                         (0.79956603, 1.6813611, 0.05202432, 0.75587231),
                         0.000001)

        curve = wedge.xyzzy.extract(0, -1, 5, 5)
        self.assertEqual(curve.shape, (20, 1))
        self.assertEqual(curve.flow_solution.shape, (20, 1))
        assert_rel_error(self, curve.extent,
                         (0.47899476, 1.915979, 0.14340162, 0.57360649),
                         0.000001)

        code = 'wedge.xyzzy.flow_solution.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract undefined for kmin or kmax')
        code = 'wedge.xyzzy.flow_solution.momentum.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract undefined for kmin or kmax')
        code = 'wedge.xyzzy.grid_coordinates.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract undefined for kmin or kmax')

        code = 'wedge.xyzzy.flow_solution.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract requires jmin and jmax')
        code = 'wedge.xyzzy.flow_solution.momentum.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract requires jmin and jmax')
        code = 'wedge.xyzzy.grid_coordinates.extract(0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '2D extract requires jmin and jmax')

        code = 'wedge.xyzzy.flow_solution.extract(0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99)'
                      ' exceeds original (0, 20, 0, 10)')
        code = 'wedge.xyzzy.flow_solution.momentum.extract(0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99)'
                      ' exceeds original (0, 20, 0, 10)')
        code = 'wedge.xyzzy.grid_coordinates.extract(0, 99, 0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99, 0, 99)'
                      ' exceeds original (0, 20, 0, 10)')

        curve = create_curve_2d(20, 0.5, 30.)
        subcurve = curve.xyzzy.extract(1, -2)
        self.assertEqual(subcurve.shape, (18,))
        self.assertEqual(subcurve.flow_solution.shape, (18,))
        assert_rel_error(self, subcurve.extent,
                         (0.43973687, 0.49981016, 0.013777171, 0.23797369),
                         0.000001)

        code = 'curve.xyzzy.flow_solution.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '1D extract undefined for jmin, jmax, kmin, or kmax')
        code = 'curve.xyzzy.flow_solution.momentum.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '1D extract undefined for jmin, jmax, kmin, or kmax')
        code = 'curve.xyzzy.grid_coordinates.extract(0, -1, 0, -1, 0, -1)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      '1D extract undefined for jmin, jmax, kmin, or kmax')

        code = 'curve.xyzzy.flow_solution.extract(0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99) exceeds original (0, 20)')
        code = 'curve.xyzzy.flow_solution.momentum.extract(0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99) exceeds original (0, 20)')
        code = 'curve.xyzzy.grid_coordinates.extract(0, 99)'
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Extraction region (0, 99) exceeds original (0, 20)')

        assert_raises(self, 'FlowSolution().extract(0, -1)',
                      globals(), locals(), RuntimeError,
                      'FlowSolution is empty!')
        assert_raises(self, 'GridCoordinates().extract(0, -1)',
                      globals(), locals(), RuntimeError,
                      'Vector is empty!')
        assert_raises(self, 'Vector().extract(0, -1)',
                      globals(), locals(), RuntimeError,
                      'Vector is empty!')

        # No-op extractions.
        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        domain = wedge.extract([(0, 29, 0, 19, 0, 9)])
        self.assertTrue(domain.is_equivalent(wedge, logger))
        domain = wedge.extract([(-30, -1, -20, -1, -10, -1)])
        self.assertTrue(domain.is_equivalent(wedge, logger))
        vec = wedge.xyzzy.flow_solution.momentum.extract(0, 29, 0, 19, 0, 9)
        self.assertTrue(vec.is_equivalent(wedge.xyzzy.flow_solution.momentum,
                                          'momentum', logger))
        vec = wedge.xyzzy.flow_solution.momentum.extract(-30, -1, -20, -1, -10, -1)
        self.assertTrue(vec.is_equivalent(wedge.xyzzy.flow_solution.momentum,
                                          'momentum', logger))

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        domain = wedge.extract([(0, 29, 0, 19)])
        self.assertTrue(domain.is_equivalent(wedge, logger))
        domain = wedge.extract([(-30, -1, -20, -1)])
        self.assertTrue(domain.is_equivalent(wedge, logger))
        vec = wedge.xyzzy.flow_solution.momentum.extract(0, 29, 0, 19)
        self.assertTrue(vec.is_equivalent(wedge.xyzzy.flow_solution.momentum,
                                          'momentum', logger))
        vec = wedge.xyzzy.flow_solution.momentum.extract(-30, -1, -20, -1)
        self.assertTrue(vec.is_equivalent(wedge.xyzzy.flow_solution.momentum,
                                          'momentum', logger))

        curve = create_curve_2d(20, 0.5, 30.)
        domain = curve.extract([(0, 19)])
        self.assertTrue(domain.is_equivalent(curve, logger))
        domain = curve.extract([(-20, -1)])
        self.assertTrue(domain.is_equivalent(curve, logger))
        vec = curve.xyzzy.flow_solution.momentum.extract(0, 19)
        self.assertTrue(vec.is_equivalent(curve.xyzzy.flow_solution.momentum,
                                          'momentum', logger))
        vec = curve.xyzzy.flow_solution.momentum.extract(-20, -1)
        self.assertTrue(vec.is_equivalent(curve.xyzzy.flow_solution.momentum,
                                          'momentum', logger))

    def test_extend(self):
        logging.debug('')
        logging.debug('test_extend')
        logger = logging.getLogger()

        # Create vanilla 3D wedge.
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        self.assertEqual(wedge.xyzzy.shape, (30, 20, 10))
        self.assertEqual(wedge.xyzzy.flow_solution.shape, (30, 20, 10))

        # Extend for ghost planes.
        domain = wedge.extend([('i', +1., 2, 1)])
        zone = domain.xyzzy
        self.assertEqual(zone.shape, (32, 20, 10))
        self.assertEqual(zone.flow_solution.shape, (31, 20, 10))
        zone = zone.extend('i', -1., 3, 1)
        self.assertEqual(zone.shape, (35, 20, 10))
        self.assertEqual(zone.flow_solution.shape, (32, 20, 10))
        zone = zone.extend('j', +1., 2, 1)
        self.assertEqual(zone.shape, (35, 22, 10))
        self.assertEqual(zone.flow_solution.shape, (32, 21, 10))
        zone = zone.extend('j', -1., 3, 1)
        self.assertEqual(zone.shape, (35, 25, 10))
        self.assertEqual(zone.flow_solution.shape, (32, 22, 10))
        zone = zone.extend('k', +1., 2, 1)
        self.assertEqual(zone.shape, (35, 25, 12))
        self.assertEqual(zone.flow_solution.shape, (32, 22, 11))
        zone = zone.extend('k', -1., 3, 1)
        self.assertEqual(zone.shape, (35, 25, 15))
        self.assertEqual(zone.flow_solution.shape, (32, 22, 12))
        self.assertEqual(zone.grid_coordinates.shape, (35, 25, 15))

        # Uncomment to visualize result.
        # Note that unless you fix the flow extensions above to match the grid
        # extensions the flow will have strange artifacts.
#        write_plot3d_q(zone, 'ghost_wedge.xyz', 'ghost_wedge.q')

        zone.grid_coordinates.ghosts = (3, 2, 3, 2, 3, 2)
        self.assertEqual(zone.grid_coordinates.shape, (30, 20, 10))
        zone.flow_solution.ghosts = (1, 1, 1, 1, 1, 1)
        self.assertEqual(zone.flow_solution.shape, (30, 20, 10))

        # Extract to no-ghosts zone and show equivalent to original.
        zone2 = zone.extract(0, -1, 0, -1, 0, -1,
                             grid_ghosts=(0, 0, 0, 0, 0, 0),
                             flow_ghosts=(0, 0, 0, 0, 0, 0))
        self.assertEqual(zone2.shape, (30, 20, 10))
        self.assertEqual(zone2.flow_solution.shape, (30, 20, 10))
        self.assertTrue(zone2.is_equivalent(wedge.xyzzy, logger))

        # Write out with ghosts in place. Read and show equivalent to original.
        try:
            write_plot3d_q(zone, 'zone_3d.xyz', 'zone_3d.q')
            domain = read_plot3d_q('zone_3d.xyz', 'zone_3d.q', multiblock=False)
            self.assertTrue(domain.zone_1.is_equivalent(wedge.xyzzy, logger))
        finally:
            for name in ('zone_3d.xyz', 'zone_3d.q'):
                if os.path.exists(name):
                    os.remove(name)

        # Create vanilla 2D wedge.
        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)
        self.assertEqual(wedge.xyzzy.shape, (20, 10))
        self.assertEqual(wedge.xyzzy.flow_solution.shape, (20, 10))

        # Extend for ghost planes.
        zone = wedge.xyzzy.extend('i', +1., 2, 1)
        self.assertEqual(zone.shape, (22, 10))
        self.assertEqual(zone.flow_solution.shape, (21, 10))
        zone = zone.extend('i', -1., 3, 1)
        self.assertEqual(zone.shape, (25, 10))
        self.assertEqual(zone.flow_solution.shape, (22, 10))
        zone = zone.extend('j', +1., 2, 1)
        self.assertEqual(zone.shape, (25, 12))
        self.assertEqual(zone.flow_solution.shape, (22, 11))
        zone = zone.extend('j', -1., 3, 1)
        self.assertEqual(zone.shape, (25, 15))
        self.assertEqual(zone.flow_solution.shape, (22, 12))

        # Uncomment to visualize result.
        # Note that unless you fix the flow extensions above to match the grid
        # extensions the flow will have strange artifacts.
#        write_plot3d_q(zone, 'ghost_sector.xyz', 'ghost_sector.q')

        zone.grid_coordinates.ghosts = (3, 2, 3, 2)
        self.assertEqual(zone.grid_coordinates.shape, (20, 10))
        zone.flow_solution.ghosts = (1, 1, 1, 1)
        self.assertEqual(zone.flow_solution.shape, (20, 10))
        write_plot3d_q(wedge, 'zone_2d.xyz', 'zone_2d.q')

        # Extract to no-ghosts zone and show equivalent to original.
        zone2 = zone.extract(0, -1, 0, -1,
                             grid_ghosts=(0, 0, 0, 0, 0, 0),
                             flow_ghosts=(0, 0, 0, 0, 0, 0))
        self.assertEqual(zone2.shape, (20, 10))
        self.assertEqual(zone2.flow_solution.shape, (20, 10))
        self.assertTrue(zone2.is_equivalent(wedge.xyzzy, logger))

        # Write out with ghosts in place. Read and show equivalent to original.
        try:
            write_plot3d_q(zone, 'zone_2d.xyz', 'zone_2d.q')
            domain = read_plot3d_q('zone_2d.xyz', 'zone_2d.q',
                                   dim=2, multiblock=False)
            self.assertTrue(domain.zone_1.is_equivalent(wedge.xyzzy, logger))
        finally:
            for name in ('zone_2d.xyz', 'zone_2d.q'):
                if os.path.exists(name):
                    os.remove(name)

        # Create vanilla 2D curve.
        curve = create_curve_2d(20, 0.5, 30.)
        self.assertEqual(curve.xyzzy.shape, (20,))
        self.assertEqual(curve.xyzzy.flow_solution.shape, (20,))

        # Extend for ghost planes.
        zone = curve.xyzzy.extend('i', +1., 2, 1)
        self.assertEqual(zone.shape, (22,))
        self.assertEqual(zone.flow_solution.shape, (21,))
        zone = zone.extend('i', -1., 3, 1)
        self.assertEqual(zone.shape, (25,))
        self.assertEqual(zone.flow_solution.shape, (22,))

        zone.grid_coordinates.ghosts = (3, 2)
        self.assertEqual(zone.grid_coordinates.shape, (20,))
        zone.flow_solution.ghosts = (1, 1)
        self.assertEqual(zone.flow_solution.shape, (20,))

        # Extract to no-ghosts zone and show equivalent to original.
        zone = zone.extract(0, -1,
                            grid_ghosts=(0, 0, 0, 0, 0, 0),
                            flow_ghosts=(0, 0, 0, 0, 0, 0))
        self.assertEqual(zone.shape, (20,))
        self.assertEqual(zone.flow_solution.shape, (20,))
        self.assertTrue(zone.is_equivalent(curve.xyzzy, logger))

        # No-op extend.
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        domain = wedge.extend([('i', +1., 0, 0)])
        self.assertTrue(domain.is_equivalent(wedge, logger))

        assert_raises(self, "wedge.xyzzy.flow_solution.extend('i', 0., 10)",
                      globals(), locals(), ValueError, 'delta must be non-zero')
        assert_raises(self, "wedge.xyzzy.grid_coordinates.extend('i', 0., 10)",
                      globals(), locals(), ValueError, 'delta must be non-zero')
        assert_raises(self, "wedge.xyzzy.flow_solution.momentum.extend('i', 0., 10)",
                      globals(), locals(), ValueError, 'delta must be non-zero')

        assert_raises(self, "wedge.xyzzy.flow_solution.extend('i', 1., 0)",
                      globals(), locals(), ValueError, 'npoints must be >= 1')
        assert_raises(self, "wedge.xyzzy.grid_coordinates.extend('i', 1., 0)",
                      globals(), locals(), ValueError, 'npoints must be >= 1')
        assert_raises(self, "wedge.xyzzy.flow_solution.momentum.extend('i', 1., 0)",
                      globals(), locals(), ValueError, 'npoints must be >= 1')

        assert_raises(self, "wedge.xyzzy.flow_solution.extend('l', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i, j, or k')
        assert_raises(self, "wedge.xyzzy.grid_coordinates.extend('l', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i, j, or k')
        assert_raises(self, "wedge.xyzzy.flow_solution.momentum.extend('l', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i, j, or k')

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        assert_raises(self, "wedge.xyzzy.flow_solution.extend('k', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i or j')
        assert_raises(self, "wedge.xyzzy.grid_coordinates.extend('k', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i or j')
        assert_raises(self, "wedge.xyzzy.flow_solution.momentum.extend('k', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i or j')

        curve = create_curve_2d(20, 0.5, 30.)
        assert_raises(self, "curve.xyzzy.flow_solution.extend('j', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i')
        assert_raises(self, "curve.xyzzy.grid_coordinates.extend('j', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i')
        assert_raises(self, "curve.xyzzy.flow_solution.momentum.extend('j', 1., 10)",
                      globals(), locals(), ValueError, 'axis must be i')

        assert_raises(self, "FlowSolution().extend('j', 1., 10)",
                      globals(), locals(), RuntimeError, 'FlowSolution is empty!')
        assert_raises(self, "GridCoordinates().extend('j', 1., 10)",
                      globals(), locals(), RuntimeError, 'Grid is empty!')
        assert_raises(self, "Vector().extend('j', 1., 10)",
                      globals(), locals(), RuntimeError, 'Vector is empty!')


    def test_promote(self):
        logging.debug('')
        logging.debug('test_promote')
        logger = logging.getLogger()

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        self.assertEqual(wedge.shape, [(30, 20)])
        wedge.promote()
        self.assertEqual(wedge.shape, [(30, 20, 1)])

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        wedge.make_cylindrical()
        self.assertEqual(wedge.shape, [(30, 20)])
        wedge.promote()
        self.assertEqual(wedge.shape, [(1, 30, 20)])

        curve = create_curve_2d(20, 0.5, 30.)
        self.assertEqual(curve.shape, [(20,)])
        curve.promote()
        self.assertEqual(curve.shape, [(20,1)])

        curve = create_curve_2d(20, 0.5, 30.)
        curve.make_cylindrical()
        self.assertEqual(curve.shape, [(20,)])
        curve.promote()
        self.assertEqual(curve.shape, [(20,1)])

        assert_raises(self, 'FlowSolution().promote()', globals(), locals(),
                      RuntimeError, 'FlowSolution is empty!')
        assert_raises(self, 'Vector().promote()', globals(), locals(),
                      RuntimeError, 'Vector is empty!')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        assert_raises(self, 'wedge.xyzzy.flow_solution.promote()',
                      globals(), locals(),
                      RuntimeError, 'FlowSolution is 3D')
        assert_raises(self, 'wedge.xyzzy.grid_coordinates.promote()',
                      globals(), locals(),
                      RuntimeError, 'Vector is 3D')

    def test_demote(self):
        logging.debug('')
        logging.debug('test_demote')
        logger = logging.getLogger()

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        ij_surface = wedge.extract([(0, -1, 0, -1, 0, 0)])
        self.assertEqual(ij_surface.shape, [(30, 20, 1)])
        ij_surface.demote()
        self.assertEqual(ij_surface.shape, [(30, 20)])

        jk_surface = wedge.extract([(0, 0, 0, -1, 0, -1)])
        self.assertEqual(jk_surface.shape, [(1, 20, 10)])
        jk_surface.demote()
        self.assertEqual(jk_surface.shape, [(20, 10)])

        ik_surface = wedge.extract([(0, -1, 0, 0, 0, -1)])
        self.assertEqual(ik_surface.shape, [(30, 1, 10)])
        ik_surface.demote()
        self.assertEqual(ik_surface.shape, [(30, 10)])

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        wedge.make_cylindrical()

        ij_surface = wedge.extract([(0, -1, 0, -1, 0, 0)])
        self.assertEqual(ij_surface.shape, [(30, 20, 1)])
        ij_surface.demote()
        self.assertEqual(ij_surface.shape, [(30, 20)])

        jk_surface = wedge.extract([(0, 0, 0, -1, 0, -1)])
        self.assertEqual(jk_surface.shape, [(1, 20, 10)])
        jk_surface.demote()
        self.assertEqual(jk_surface.shape, [(20, 10)])

        ik_surface = wedge.extract([(0, -1, 0, 0, 0, -1)])
        self.assertEqual(ik_surface.shape, [(30, 1, 10)])
        ik_surface.demote()
        self.assertEqual(ik_surface.shape, [(30, 10)])

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)

        i_curve = wedge.extract([(0, -1, 0, 0)])
        self.assertEqual(i_curve.shape, [(30, 1)])
        i_curve.demote()
        self.assertEqual(i_curve.shape, [(30,)])

        j_curve = wedge.extract([(0, 0, 0, -1)])
        self.assertEqual(j_curve.shape, [(1, 20)])
        j_curve.demote()
        self.assertEqual(j_curve.shape, [(20,)])

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        wedge.make_cylindrical()

        i_curve = wedge.extract([(0, -1, 0, 0)])
        self.assertEqual(i_curve.shape, [(30, 1)])
        i_curve.demote()
        self.assertEqual(i_curve.shape, [(30,)])

        j_curve = wedge.extract([(0, 0, 0, -1)])
        self.assertEqual(j_curve.shape, [(1, 20)])
        j_curve.demote()
        self.assertEqual(j_curve.shape, [(20,)])

        assert_raises(self, 'FlowSolution().demote()', globals(), locals(),
                      RuntimeError, 'FlowSolution is empty!')

        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        assert_raises(self, 'wedge.xyzzy.flow_solution.demote()',
                      globals(), locals(),
                      RuntimeError, 'No i, j, or k plane to collapse')
        assert_raises(self, 'wedge.xyzzy.grid_coordinates.demote()',
                      globals(), locals(),
                      RuntimeError, 'No i, j, or k plane to collapse')

        wedge = create_wedge_2d((30, 20), 0.5, 2., 30.)
        assert_raises(self, 'wedge.xyzzy.flow_solution.demote()',
                      globals(), locals(),
                      RuntimeError, 'No i or j plane to collapse')
        assert_raises(self, 'wedge.xyzzy.grid_coordinates.demote()',
                      globals(), locals(),
                      RuntimeError, 'No i or j plane to collapse')

        curve = create_curve_2d(20, 0.5, 30.)
        assert_raises(self, 'curve.xyzzy.flow_solution.demote()',
                      globals(), locals(),
                      RuntimeError, 'FlowSolution is 1D')
        assert_raises(self, 'curve.xyzzy.grid_coordinates.demote()',
                      globals(), locals(),
                      RuntimeError, 'Vector is 1D')


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main.datatypes.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

