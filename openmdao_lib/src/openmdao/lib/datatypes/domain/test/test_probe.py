"""
Test :class:`DomainObj` mesh_probe() operations.
"""

import logging
import os.path
import pkg_resources
import unittest

from math import pi

from openmdao.lib.datatypes.domain import mesh_probe
from openmdao.lib.datatypes.domain.test import restart, overflow
from openmdao.lib.datatypes.domain.test.cube import create_cube
from openmdao.lib.datatypes.domain.test.wedge import create_wedge_3d

from openmdao.util.testutil import assert_raises, assert_rel_error

ORIG_DIR = os.getcwd()


class TestCase(unittest.TestCase):
    """ Test :class:`Domain` mesh_probe() operations. """

    directory = os.path.realpath(
        pkg_resources.resource_filename('openmdao.lib.datatypes.domain', 'test'))

    def setUp(self):
        """ Called before each test in this class. """
        os.chdir(TestCase.directory)

    def tearDown(self):
        """ Called after each test in this class. """
        os.chdir(ORIG_DIR)

    def test_cube(self):
        logging.debug('')
        logging.debug('test_cube')

        cube = create_cube((41, 17, 9), 5., 4., 3.)

        # I face.
        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'), ('density', None))
        area, density = mesh_probe(cube, regions, variables)
        logging.debug('I area = %g (%g ft**2)', area, area / 144.)
        logging.debug('I density = %g', density)
        self.assertEqual(area, 4. * 3. * 144.)
        self.assertEqual(density, 0.25)

        surface = cube.extract([(2, 2, 0, -1, 0, -1)])
        surface.demote()
        regions = (('xyzzy', 0, -1, 0, -1),)
        area, density = mesh_probe(surface, regions, variables)
        self.assertEqual(area, 4. * 3. * 144.)
        self.assertEqual(density, 0.25)

        # J face.
        regions = (('xyzzy', 0, -1, 2, 2, 0, -1),)
        area, density = mesh_probe(cube, regions, variables)
        logging.debug('J area = %g (%g ft**2)', area, area / 144.)
        logging.debug('J density = %g', density)
        self.assertEqual(area, 5. * 3. * 144.)
        self.assertEqual(density, 2.5)

        surface = cube.extract([(0, -1, 2, 2, 0, -1)])
        surface.demote()
        regions = (('xyzzy', 0, -1, 0, -1),)
        area, density = mesh_probe(surface, regions, variables)
        self.assertEqual(area, 5. * 3. * 144.)
        self.assertEqual(density, 2.5)

        # K face.
        regions = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        area, density = mesh_probe(cube, regions, variables)
        logging.debug('K area = %g (%g ft**2)', area, area / 144.)
        logging.debug('K density = %g', density)
        self.assertEqual(area, 5. * 4. * 144.)
        self.assertEqual(density, 2.5)

        surface = cube.extract([(0, -1, 0, -1, 2, 2)])
        surface.demote()
        regions = (('xyzzy', 0, -1, 0, -1),)
        area, density = mesh_probe(surface, regions, variables)
        self.assertEqual(area, 5. * 4. * 144.)
        self.assertEqual(density, 2.5)

        # 1D curve.
        variables = (('length', 'inch'), ('density', None))
        curve = surface.extract([(0, -1, 5, 5)])
        curve.demote()
        regions = (('xyzzy', 0, -1),)
        length, density = mesh_probe(curve, regions, variables)
        # It seems turning the value into a PhysicalQuantity isn't exact.
        assert_rel_error(self, length, 5. * 12., 0.00000001)
        self.assertEqual(density, 2.5)

        # 2D curves.
        regions = (('xyzzy', 0, -1, 5, 5),)
        length, density = mesh_probe(surface, regions, variables)
        assert_rel_error(self, length, 5. * 12., 0.00000001)
        self.assertEqual(density, 2.5)

        regions = (('xyzzy', 5, 5, 0, -1),)
        length, density = mesh_probe(surface, regions, variables)
        assert_rel_error(self, length, 4. * 12., 0.00000001)
        self.assertEqual(density, 0.625)

        # 3D curves.
        regions = (('xyzzy', 0, -1, 5, 5, 5, 5),)
        length, density = mesh_probe(cube, regions, variables)
        assert_rel_error(self, length, 5. * 12., 0.00000001)
        self.assertEqual(density, 2.5)

        regions = (('xyzzy', 5, 5, 0, -1, 5, 5),)
        length, density = mesh_probe(cube, regions, variables)
        assert_rel_error(self, length, 4. * 12., 0.00000001)
        self.assertEqual(density, 0.625)

        regions = (('xyzzy', 5, 5, 5, 5, 0, -1),)
        length, density = mesh_probe(cube, regions, variables)
        assert_rel_error(self, length, 3. * 12., 0.00000001)
        self.assertEqual(density, 0.625)

    def test_wedge(self):
        logging.debug('')
        logging.debug('test_wedge')

        wedge = create_wedge_3d((30, 20, 100), 5., 0.5, 2., 30.)

        # I face.
        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = (((pi*2.**2.) - (pi*0.5**2.)) * 30./360.) * 144.
        logging.debug('area = %g (%g ft**2), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # J face.
        regions = (('xyzzy', 0, -1, 0, 0, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = (2.*pi*0.5 * 30./360.) * 5. * 144.
        logging.debug('area = %g (%g ft**2), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        regions = (('xyzzy', 0, -1, -1, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = (2.* pi*2. * 30./360.) * 5. * 144.
        logging.debug('area = %g (%g ft**2), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # K face.
        regions = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = 5. * (2. - 0.5) * 144.
        logging.debug('area = %g (%g ft**2), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.000001)

    def test_adpac(self):
        # Verify correct metric values for data from real scenario.
        logging.debug('')
        logging.debug('test_adpac')

        domain = restart.read('lpc-test', logging.getLogger())
        regions = [('zone_1', 2, 2, 0, -1, 0, -1),
                   ('zone_2', 2, 2, 0, -1, 0, -1)]
        variables = [('area', 'inch**2'),
                     ('pressure_stagnation', 'psi'),
                     ('pressure', 'psi'),
                     ('temperature_stagnation', 'degR'),
                     ('temperature', 'degR'),
                     ('mass_flow', 'lbm/s'),
                     ('corrected_mass_flow', 'lbm/s')]
        metrics = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('lpc-test I face data:')
        for i, (name, units) in enumerate(variables):
            logging.debug('    %s = %g %s' % (name, metrics[i], units))

        assert_rel_error(self, metrics[0], 830.494, 0.00001)
        assert_rel_error(self, metrics[1], 8.95673, 0.00001)
        assert_rel_error(self, metrics[2], 6.97191, 0.00001)
        assert_rel_error(self, metrics[3], 547.784, 0.00001)
        assert_rel_error(self, metrics[4], 509.909, 0.00001)
        assert_rel_error(self, metrics[5], 120.092, 0.00001)
        assert_rel_error(self, metrics[6], 202.570, 0.00001)

        regions = [('zone_1', 0, -1, 2, 2, 0, -1)]
        metrics = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('lpc-test J face data:')
        for i, (name, units) in enumerate(variables):
            logging.debug('    %s = %g %s' % (name, metrics[i], units))

        assert_rel_error(self, metrics[0],  1089.77, 0.00001)
        assert_rel_error(self, metrics[1],  7.09975, 0.00001)
        assert_rel_error(self, metrics[2],  6.52045, 0.00001)
        assert_rel_error(self, metrics[3],  554.000, 0.00001)
        assert_rel_error(self, metrics[4],  540.770, 0.00001)
        assert_rel_error(self, metrics[5], 0.421583, 0.00001)
        assert_rel_error(self, metrics[6], 0.898204, 0.00001)

        regions = [('zone_1', 0, -1, 0, -1, 2, 2)]
        metrics = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('lpc-test K face data:')
        for i, (name, units) in enumerate(variables):
            logging.debug('    %s = %g %s' % (name, metrics[i], units))

        assert_rel_error(self, metrics[0],  2870.64, 0.00001)
        assert_rel_error(self, metrics[1],  8.70534, 0.00001)
        assert_rel_error(self, metrics[2],  6.89906, 0.00001)
        assert_rel_error(self, metrics[3],  546.989, 0.00001)
        assert_rel_error(self, metrics[4],  511.875, 0.00001)
        assert_rel_error(self, metrics[5], -156.175, 0.00001)
        assert_rel_error(self, metrics[6], -270.858, 0.00001)

        surface = domain.extract([(0, -1, 0, -1, 2, 2)])
        surface.demote()
        regions = [('zone_1', 0, -1, 0, -1)]
        metrics = mesh_probe(surface, regions, variables, 'mass')
        # These are different than the above metrics since the extraction
        # doesn't average cell values on either side of the surface.
        assert_rel_error(self, metrics[0],  2870.64, 0.00001)
        assert_rel_error(self, metrics[1],  8.56340, 0.00001)
        assert_rel_error(self, metrics[2],  6.92024, 0.00001)
        assert_rel_error(self, metrics[3],  541.789, 0.00001)
        assert_rel_error(self, metrics[4],  509.850, 0.00001)
        assert_rel_error(self, metrics[5], -149.525, 0.00001)
        assert_rel_error(self, metrics[6], -262.976, 0.00001)

    def test_errors(self):
        logging.debug('')
        logging.debug('test_errors')

        wedge = create_wedge_3d((30, 20, 100), 5., 0.5, 2., 30.)

        regions = (('xyzzy',),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      "region specification 1 invalid: ('xyzzy',)")

        regions = (('no-such-zone', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      "region 1: Domain does not contain zone 'no-such-zone'")

        regions = (('xyzzy', -200, 2, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: index dimensionality mismatch')

        regions = (('xyzzy', -200, 2, 0, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: imin -170 invalid (max 30)')

        regions = (('xyzzy', 2, 200, 0, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: imax 200 invalid (max 30)')

        regions = (('xyzzy', 2, 2, -200, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: jmin -180 invalid (max 20)')

        regions = (('xyzzy', 2, 2, 0, 200, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: jmax 200 invalid (max 20)')

        regions = (('xyzzy', 2, 2, 0, -1, -200, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: kmin -100 invalid (max 100)')

        regions = (('xyzzy', 2, 2, 0, -1, 0, 200),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'region 1: kmax 200 invalid (max 100)')

        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('no-such-variable', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      "Unsupported variable 'no-such-variable'")

        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),
                   ('xyzzy', 3, 3, 0, -1, 0, -1))
        variables = (('pressure', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), RuntimeError,
                      "Zone 'xyzzy' used more than once")

        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables, "scheme")',
                      globals(), locals(), ValueError,
                      "Unknown/unsupported weighting scheme 'scheme'")

        wedge.reference_state = None
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'No zone or domain reference_state dictionary supplied'
                      ' for zone xyzzy.')

        wedge.reference_state = {'dummy': 42}
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For area,"
                      " reference_state is missing 'length_reference'.")

        variables = (('mass_flow', 'lbm/s'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For mass_flow,"
                      " reference_state is missing one or more of"
                      " ('length_reference', 'pressure_reference',"
                      " 'ideal_gas_constant', 'temperature_reference').")

        variables = (('corrected_mass_flow', 'lbm/s'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For corrected_mass"
                      "_flow, zone xyzzy is missing one or more of"
                      " ('density', 'momentum', 'pressure').")

        variables = (('pressure', 'psi'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), NotImplementedError,
                      'Get dimensional pressure from Q variables')

        variables = (('pressure_stagnation', 'psi'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For pressure"
                      "_stagnation, reference_state is missing one or more of"
                      " ('pressure_reference', 'ideal_gas_constant',"
                      " 'temperature_reference', 'specific_heat_ratio').")

        variables = (('temperature', 'degR'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For temperature,"
                      " reference_state is missing one or more of"
                      " ('pressure_reference', 'ideal_gas_constant',"
                      " 'temperature_reference').")

        variables = (('temperature_stagnation', 'degR'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), AttributeError, "For temperature"
                      "_stagnation, reference_state is missing one or more of"
                      " ('pressure_reference', 'ideal_gas_constant',"
                      " 'temperature_reference', 'specific_heat_ratio').")

    def test_overflow(self):
        # Verify correct metric values for data from real scenario.
        # However, exit plane values are repeated in 'J' direction.
        logging.debug('')
        logging.debug('test_overflow')

        domain = overflow.read_q('grid.in', 'q.save', multiblock=False,
                                 logger=logging.getLogger())
        variables = [('pressure_stagnation', None),]

        # Exit surface (3D index space).
        regions = [('zone_1', -1, -1, 0, -1, 0, -1)]
        pt_area_3d, = mesh_probe(domain, regions, variables, 'area')
        pt_mass_3d, = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('surface pt_area_3d %r, pt_mass_3d %r', pt_area_3d, pt_mass_3d)
        assert_rel_error(self, pt_area_3d, 10.1090358495, 0.000001)
        assert_rel_error(self, pt_mass_3d, 10.489294904, 0.000001)

        # Exit surface (2D index space).
        surface = domain.extract([(-1, -1, 0, -1, 0, -1)])
        surface.demote()
        regions = [('zone_1', 0, -1, 0, -1)]
        pt_area_2d, = mesh_probe(surface, regions, variables, 'area')
        pt_mass_2d, = mesh_probe(surface, regions, variables, 'mass')
        logging.debug('surface pt_area_2d %r, pt_mass_2d %r', pt_area_2d, pt_mass_2d)
        self.assertEqual(pt_area_2d, pt_area_3d)
# FIXME: why is this only really close and not exact?
        assert_rel_error(self, pt_mass_2d, pt_mass_3d, 0.0000000001)

        # Exit curve (3D index space).
        regions = [('zone_1', -1, -1, 1, 1, 0, -1)]
        pt_area_3d, = mesh_probe(domain, regions, variables, 'area')
        logging.debug('curve pt_area_3d %r', pt_area_3d)
        assert_rel_error(self, pt_area_3d, 10.1090358495, 0.000001)

        # Exit curve (2D index space).
        regions = [('zone_1', 1, 1, 0, -1)]
        pt_area_2d, = mesh_probe(surface, regions, variables, 'area')
        logging.debug('curve pt_area_2d %r', pt_area_2d)
        self.assertEqual(pt_area_2d, pt_area_3d)

        # Exit curve (1D index space).
        curve = surface.extract([(1, 1, 0, -1)])
        curve.demote()
        regions = [('zone_1', 0, -1)]
        pt_area_1d, = mesh_probe(curve, regions, variables, 'area')
        logging.debug('curve pt_area_1d %r', pt_area_1d)
        self.assertEqual(pt_area_1d, pt_area_3d)

        # Exit point (3D index space).
        regions = [('zone_1', -1, -1, 1, 1, 50, 50)]
        pt_area_3d, = mesh_probe(domain, regions, variables, 'area')
        logging.debug('point pt_area_3d %r', pt_area_3d)

        # Exit point (2D index space).
        regions = [('zone_1', 1, 1, 50, 50)]
        pt_area_2d, = mesh_probe(surface, regions, variables, 'area')
        logging.debug('point pt_area_2d %r', pt_area_2d)
        self.assertEqual(pt_area_2d, pt_area_3d)

        # Exit point (1D index space).
        regions = [('zone_1', 50, 50)]
        pt_area_1d, = mesh_probe(curve, regions, variables, 'area')
        logging.debug('point pt_area_1d %r', pt_area_1d)
        self.assertEqual(pt_area_1d, pt_area_3d)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.datatypes.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

