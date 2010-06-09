"""
Test :class:`DomainObj` surface_probe() operations.
"""

import logging
import numpy
import unittest

from math import pi

from openmdao.lib.traits.domain import surface_probe
from openmdao.lib.traits.domain.test.cube import create_cube
from openmdao.lib.traits.domain.test.wedge import create_wedge_3d

from openmdao.util.testutil import assert_raises, assert_rel_error


class TestCase(unittest.TestCase):
    """ Test :class:`Domain` surface_probe() operations. """

    def test_cube(self):
        logging.debug('')
        logging.debug('test_cube')

        cube = create_cube((30, 20, 10), 5., 4., 3.)

        # I face.
        surfaces = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(cube, surfaces, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 4. * 3. * 144.)

        # J face.
        surfaces = (('xyzzy', 0, -1, 2, 2, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(cube, surfaces, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 5. * 3. * 144.)

        # K face.
        surfaces = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(cube, surfaces, variables)
        area = metrics[0]
        logging.debug('area = %g (%g ft)', area, area / 144.)
        self.assertEqual(area, 5. * 4. * 144.)

    def test_wedge(self):
        logging.debug('')
        logging.debug('test_wedge')

        wedge = create_wedge_3d((30, 20, 100), 5., 0.5, 2., 30.)

        # I face.
        surfaces = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(wedge, surfaces, variables)
        area = metrics[0]
        expected = (((pi*2.**2.) - (pi*0.5**2.)) * 30. / 360.) * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # J face.
        surfaces = (('xyzzy', 0, -1, 0, 0, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(wedge, surfaces, variables)
        area = metrics[0]
        expected = (2.* pi*0.5 * 30. / 360.) * 5. * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        surfaces = (('xyzzy', 0, -1, -1, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(wedge, surfaces, variables)
        area = metrics[0]
        expected = (2.* pi*2. * 30. / 360.) * 5. * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.00001)

        # K face.
        surfaces = (('xyzzy', 0, -1, 0, -1, 2, 2),)
        variables = (('area', 'inch**2'),)
        metrics = surface_probe(wedge, surfaces, variables)
        area = metrics[0]
        expected = 5. * (2. - 0.5) * 144.
        logging.debug('area = %g (%g ft), expected %g',
                      area, area / 144., expected)
        assert_rel_error(self, area, expected, 0.000001)

    def test_errors(self):
        logging.debug('')
        logging.debug('test_errors')

        wedge = create_wedge_3d((30, 20, 100), 5., 0.5, 2., 30.)

        surfaces = (('no-such-zone', 2, 2, 0, -1, 0, -1),)
        variables = (('area', 'inch**2'),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      "Domain does not contain zone 'no-such-zone'")

        surfaces = (('xyzzy', -200, 2, 0, -1, 0, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy imin -170 invalid (max 30)')

        surfaces = (('xyzzy', 2, 200, 0, -1, 0, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy imax 200 invalid (max 30)')

        surfaces = (('xyzzy', 2, 2, -200, -1, 0, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy jmin -180 invalid (max 20)')

        surfaces = (('xyzzy', 2, 2, 0, 200, 0, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy jmax 200 invalid (max 20)')

        surfaces = (('xyzzy', 2, 2, 0, -1, -200, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy kmin -100 invalid (max 100)')

        surfaces = (('xyzzy', 2, 2, 0, -1, 0, 200),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy kmax 200 invalid (max 100)')

        surfaces = (('xyzzy', 0, -1, 0, -1, 0, -1),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'Zone xyzzy volume specified: 0,29 0,1')

        surfaces = (('xyzzy', 2, 2, 0, -1, 0, -1),)
        variables = (('no-such-variable', 'inch**2'),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      "Unknown/unsupported variable 'no-such-variable'")

        variables = (('area', 'inch**2'),)
        assert_raises(self, 'surface_probe(wedge, surfaces, variables, "scheme")',
                      globals(), locals(), ValueError,
                      "Unknown/unsupported weighting scheme 'scheme'")

        ref = wedge.reference_state
        wedge.reference_state = None
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), ValueError,
                      'No zone or domain reference_state dictionary supplied'
                      ' for xyzzy.')

        wedge.reference_state = {'dummy': 42}
        assert_raises(self, 'surface_probe(wedge, surfaces, variables)',
                      globals(), locals(), AttributeError,
                      "For area, reference_state is missing 'length_reference'.")


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.traits.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

