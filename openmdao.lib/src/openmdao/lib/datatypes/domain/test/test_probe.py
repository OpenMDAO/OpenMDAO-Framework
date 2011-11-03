"""
Test :class:`DomainObj` mesh_probe() operations.
"""

import logging
import os.path
import pkg_resources
import unittest

from math import pi

from openmdao.lib.datatypes.domain import mesh_probe
from openmdao.lib.datatypes.domain.test import restart
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

        cube = create_cube((30, 20, 10), 5., 4., 3.)

        # I face.
        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(cube, regions, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 4. * 3. * 144.)

        # J face.
        regions = (('xyzzy', 0, -1, 2, 2, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(cube, regions, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 5. * 3. * 144.)

        # K face.
        regions = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(cube, regions, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 5. * 4. * 144.)

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
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # J face.
        regions = (('xyzzy', 0, -1, 0, 0, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = (2.*pi*0.5 * 30./360.) * 5. * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        regions = (('xyzzy', 0, -1, -1, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = (2.* pi*2. * 30./360.) * 5. * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # K face.
        regions = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        variables = (('area', 'inch**2'),)
        metrics = mesh_probe(wedge, regions, variables)
        area = metrics[0]
        expected = 5. * (2. - 0.5) * 144.
        logging.debug('area = %g (%g ft), expected %g',
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

        # These values have been verified with ADSPIN.
        assert_rel_error(self, metrics[0], 830.494, 0.00001)
        assert_rel_error(self, metrics[1], 8.95658, 0.00001)
        assert_rel_error(self, metrics[2], 6.97191, 0.00001)
        assert_rel_error(self, metrics[3], 547.784, 0.00001)
        assert_rel_error(self, metrics[4], 509.909, 0.00001)
        assert_rel_error(self, metrics[5], 120.092, 0.00001)
        assert_rel_error(self, metrics[6], 202.573, 0.00001)

        regions = [('zone_1', 0, -1, 2, 2, 0, -1)]
        metrics = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('lpc-test J face data:')
        for i, (name, units) in enumerate(variables):
            logging.debug('    %s = %g %s' % (name, metrics[i], units))

        assert_rel_error(self, metrics[0],  1089.77, 0.00001)
        assert_rel_error(self, metrics[1],  7.08859, 0.00001)
        assert_rel_error(self, metrics[2],  6.52045, 0.00001)
        assert_rel_error(self, metrics[3],  553.772, 0.00001)
        assert_rel_error(self, metrics[4],  540.711, 0.00001)
        assert_rel_error(self, metrics[5], 0.421583, 0.00001)
        assert_rel_error(self, metrics[6], 0.903318, 0.00001)

        regions = [('zone_1', 0, -1, 0, -1, 2, 2)]
        metrics = mesh_probe(domain, regions, variables, 'mass')
        logging.debug('lpc-test K face data:')
        for i, (name, units) in enumerate(variables):
            logging.debug('    %s = %g %s' % (name, metrics[i], units))

        assert_rel_error(self, metrics[0],  2870.64, 0.00001)
        assert_rel_error(self, metrics[1],  8.69595, 0.00001)
        assert_rel_error(self, metrics[2],  6.89906, 0.00001)
        assert_rel_error(self, metrics[3],  546.863, 0.00001)
        assert_rel_error(self, metrics[4],  511.839, 0.00001)
        assert_rel_error(self, metrics[5], -156.175, 0.00001)
        assert_rel_error(self, metrics[6], -271.477, 0.00001)

    def test_errors(self):
        logging.debug('')
        logging.debug('test_errors')

        wedge = create_wedge_3d((30, 20, 100), 5., 0.5, 2., 30.)

        regions = (('no-such-zone', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      "Domain does not contain zone 'no-such-zone'")

        regions = (('xyzzy', -200, 2, 0, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy imin -170 invalid (max 30)')

        regions = (('xyzzy', 2, 200, 0, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy imax 200 invalid (max 30)')

        regions = (('xyzzy', 2, 2, -200, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy jmin -180 invalid (max 20)')

        regions = (('xyzzy', 2, 2, 0, 200, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy jmax 200 invalid (max 20)')

        regions = (('xyzzy', 2, 2, 0, -1, -200, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy kmin -100 invalid (max 100)')

        regions = (('xyzzy', 2, 2, 0, -1, 0, 200),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy kmax 200 invalid (max 100)')

        regions = (('xyzzy', 0, -1, 0, -1, 0, -1),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy volume specified: 0,29 0,1')

        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('no-such-variable', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      "Unknown/unsupported variable 'no-such-variable'")

        regions = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'mesh_probe(wedge, regions, variables, "scheme")',
                      globals(), locals(), ValueError,
                      "Unknown/unsupported weighting scheme 'scheme'")

        ref = wedge.reference_state
        wedge.reference_state = None
        assert_raises(self, 'mesh_probe(wedge, regions, variables)',
                      globals(), locals(), ValueError,
                      'No zone or domain reference_state dictionary supplied'
                      ' for xyzzy.')

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


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.datatypes.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

