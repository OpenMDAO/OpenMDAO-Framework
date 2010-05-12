"""
Test Plot3D operations on :class:`DomainObj` objects.
"""

import logging
import os.path
import unittest

from openmdao.lib.traits.domain import read_plot3d_q, write_plot3d_q
from openmdao.lib.traits.domain.test.wedge import create_wedge_2d, \
                                                  create_wedge_3d


class TestCase(unittest.TestCase):
    """ Test Plot3D operations on :class:`DomainObj` objects. """

    def tearDown(self):
        """ Clean up generated files. """
        for path in ('be-binary.xyz', 'be-binary.q',
                     'unformatted.xyz', 'unformatted.q'):
            if os.path.exists(path):
                os.remove(path)

    def test_q_3d(self):
        logging.debug('')
        logging.debug('test_q_3d')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)

        # Big-endian binary.
        write_plot3d_q(wedge, 'be-binary.xyz', 'be-binary.q', logger=logger,
                       big_endian=True, unformatted=False)
        domain = read_plot3d_q('be-binary.xyz', 'be-binary.q', logger=logger,
                               multiblock=False, big_endian=True,
                               unformatted=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))

        # Little-endian unformatted.
        write_plot3d_q(domain, 'unformatted.xyz', 'unformatted.q',
                       logger=logger)
        domain = read_plot3d_q('unformatted.xyz', 'unformatted.q',
                               logger=logger, multiblock=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))

        # Multiblock.
        wedge2 = create_wedge_3d((30, 20, 10), 5., 2.5, 4., 30.)
        domain.add_domain(wedge2)
        write_plot3d_q(domain, 'unformatted.xyz', 'unformatted.q',
                       logger=logger)
        domain = read_plot3d_q('unformatted.xyz', 'unformatted.q',
                               logger=logger)
        # Errors.
        try:
            read_plot3d_q('unformatted.xyz', 'unformatted.q', blanking=True,
                          logger=logger, multiblock=False)
        except NotImplementedError as exc:
            self.assertEqual(str(exc), 'blanking not supported yet')
        else:
            self.fail('Expected NotImplementedError')

        try:
            read_plot3d_q('unformatted.xyz', 'unformatted.q', planes=True,
                          logger=logger, multiblock=False)
        except NotImplementedError as exc:
            self.assertEqual(str(exc), 'planar format not supported yet')
        else:
            self.fail('Expected NotImplementedError')

    def test_q_2d(self):
        logging.debug('')
        logging.debug('test_q_2d')

        logger = logging.getLogger()
        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)

        write_plot3d_q(wedge, 'be-binary.xyz', 'be-binary.q', logger=logger,
                       big_endian=True, unformatted=False)
        domain = read_plot3d_q('be-binary.xyz', 'be-binary.q', logger=logger,
                               dim=2, multiblock=False, big_endian=True,
                               unformatted=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))

        write_plot3d_q(domain, 'unformatted.xyz', 'unformatted.q',
                       logger=logger)
        domain = read_plot3d_q('unformatted.xyz', 'unformatted.q',
                               logger=logger, dim=2, multiblock=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.traits.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

