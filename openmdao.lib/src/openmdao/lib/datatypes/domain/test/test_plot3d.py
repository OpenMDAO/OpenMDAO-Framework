"""
Test Plot3D operations on :class:`DomainObj` objects.
"""

import logging
import os.path
import unittest

from openmdao.lib.datatypes.domain import read_plot3d_q, write_plot3d_q, \
                                       read_plot3d_f, write_plot3d_f, \
                                       read_plot3d_shape

from openmdao.lib.datatypes.domain.test.wedge import create_wedge_2d, \
                                                  create_wedge_3d


class TestCase(unittest.TestCase):
    """ Test Plot3D operations on :class:`DomainObj` objects. """

    def tearDown(self):
        """ Clean up generated files. """
        for path in ('be-binary.xyz', 'be-binary.q', 'be-binary.f',
                     'unformatted.xyz', 'unformatted.q', 'unformatted.f'):
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
        wedge2 = create_wedge_3d((29, 19, 9), 5., 2.5, 4., 30.)
        domain.add_domain(wedge2)
        write_plot3d_q(domain, 'unformatted.xyz', 'unformatted.q',
                       logger=logger)
        domain = read_plot3d_q('unformatted.xyz', 'unformatted.q',
                               logger=logger)

        shape = read_plot3d_shape('unformatted.xyz', logger=logger)
        self.assertEqual(shape, [(30, 20, 10), (29, 19, 9)])

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

        # Big-endian binary.
        write_plot3d_q(wedge, 'be-binary.xyz', 'be-binary.q', logger=logger,
                       big_endian=True, unformatted=False)
        domain = read_plot3d_q('be-binary.xyz', 'be-binary.q', logger=logger,
                               dim=2, multiblock=False, big_endian=True,
                               unformatted=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))

        # Little-endian unformatted.
        write_plot3d_q(domain, 'unformatted.xyz', 'unformatted.q',
                       logger=logger)
        domain = read_plot3d_q('unformatted.xyz', 'unformatted.q',
                               logger=logger, dim=2, multiblock=False)
        self.assertFalse(domain.is_equivalent(wedge, logger=logger))
        domain.rename_zone('xyzzy', domain.zone_1)
        self.assertTrue(domain.is_equivalent(wedge, logger=logger))


    def test_f_3d(self):
        logging.debug('')
        logging.debug('test_f_3d')

        logger = logging.getLogger()
        wedge = create_wedge_3d((30, 20, 10), 5., 0.5, 2., 30.)
        wedge_flow = wedge.xyzzy.flow_solution
        varnames = ('density', 'momentum', 'energy_stagnation_density')

        # Big-endian binary.
        write_plot3d_f(wedge, 'be-binary.xyz', 'be-binary.f', varnames,
                       logger=logger, big_endian=True, unformatted=False)
        domain = read_plot3d_f('be-binary.xyz', 'be-binary.f', logger=logger,
                               multiblock=False, big_endian=True,
                               unformatted=False)
        test_flow = domain.zone_1.flow_solution
        self.assertTrue((test_flow.f_1 == wedge_flow.density).all())
        self.assertTrue((test_flow.f_2 == wedge_flow.momentum.x).all())
        self.assertTrue((test_flow.f_3 == wedge_flow.momentum.y).all())
        self.assertTrue((test_flow.f_4 == wedge_flow.momentum.z).all())
        self.assertTrue((test_flow.f_5 == wedge_flow.energy_stagnation_density).all())

        # Little-endian unformatted.
        write_plot3d_f(domain, 'unformatted.xyz', 'unformatted.f',
                       logger=logger)
        domain = read_plot3d_f('unformatted.xyz', 'unformatted.f',
                               logger=logger, multiblock=False)
        test_flow = domain.zone_1.flow_solution
        self.assertTrue((test_flow.f_1 == wedge_flow.density).all())
        self.assertTrue((test_flow.f_2 == wedge_flow.momentum.x).all())
        self.assertTrue((test_flow.f_3 == wedge_flow.momentum.y).all())
        self.assertTrue((test_flow.f_4 == wedge_flow.momentum.z).all())
        self.assertTrue((test_flow.f_5 == wedge_flow.energy_stagnation_density).all())

    def test_f_2d(self):
        logging.debug('')
        logging.debug('test_f_2d')

        logger = logging.getLogger()
        wedge = create_wedge_2d((20, 10), 0.5, 2., 30.)
        wedge_flow = wedge.xyzzy.flow_solution
        varnames = ('density', 'momentum', 'energy_stagnation_density')

        # Big-endian binary.
        write_plot3d_f(wedge, 'be-binary.xyz', 'be-binary.f', varnames,
                       logger=logger, big_endian=True, unformatted=False)
        domain = read_plot3d_f('be-binary.xyz', 'be-binary.f', logger=logger,
                               dim=2, multiblock=False, big_endian=True,
                               unformatted=False)
        test_flow = domain.zone_1.flow_solution
        self.assertTrue((test_flow.f_1 == wedge_flow.density).all())
        self.assertTrue((test_flow.f_2 == wedge_flow.momentum.x).all())
        self.assertTrue((test_flow.f_3 == wedge_flow.momentum.y).all())
        self.assertTrue((test_flow.f_4 == wedge_flow.energy_stagnation_density).all())

        # Little-endian unformatted.
        write_plot3d_f(domain, 'unformatted.xyz', 'unformatted.f',
                       logger=logger)
        varnames = ('density', 'momentum_x')
        domain = read_plot3d_f('unformatted.xyz', 'unformatted.f', varnames,
                               logger=logger, dim=2, multiblock=False)
        test_flow = domain.zone_1.flow_solution
        self.assertTrue((test_flow.density == wedge_flow.density).all())
        self.assertTrue((test_flow.momentum_x == wedge_flow.momentum.x).all())
        self.assertTrue((test_flow.f_3 == wedge_flow.momentum.y).all())
        self.assertTrue((test_flow.f_4 == wedge_flow.energy_stagnation_density).all())


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib.datatypes.domain')
    sys.argv.append('--cover-erase')
    nose.runmodule()

