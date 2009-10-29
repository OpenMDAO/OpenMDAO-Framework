import os.path
import sys
import unittest

import pkg_resources

from openmdao.util.testutil import assert_rel_error

from openmdao.lib.components.axod_comp import AxodComp


class TestCase(unittest.TestCase):
    """ Test AxodComp. """

    def setUp(self):
        """ Set up environment before each test. """
        pass

    def tearDown(self):
        """ Clean up environment after each test. """
        for filename in ('axod.inp', 'axod.out', 'fort.7', 'pltfile'):
            if os.path.exists(filename):
                os.remove(filename)

    def test_eee_hpt(self):
        inp = pkg_resources.resource_filename('openmdao.lib.components.test',
                                              'eee_hpt.inp')
        comp = AxodComp(input_filename=inp)
        comp.run()

        # The 'places' values here are just a guess...
        assert_rel_error(comp.hpower, 3323.77880859375, 0.000001)
        assert_rel_error(comp.tott[0], 757.75458, 0.000001)
        assert_rel_error(comp.totp[0], 8.223134, 0.000001)
        assert_rel_error(comp.mflow[0], 4.9717932, 0.000001)
        assert_rel_error(comp.effs[0], 0.95300001, 0.000001)
        assert_rel_error(comp.effr[0], 0.90600002, 0.000001)
        self.assertEqual(len(comp.results), 19773)

    def test_one_stage(self):
        inp = pkg_resources.resource_filename('openmdao.lib.components.test',
                                              'one_stage.inp')
        comp = AxodComp(input_filename=inp)
        comp.run()

        # The 'places' values here are just a guess...
        assert_rel_error(comp.hpower, 696.33050537109375, 0.000001)
        assert_rel_error(comp.tott[0], 430.1795, 0.000001)
        assert_rel_error(comp.totp[0], 7.0516329, 0.000001)
        assert_rel_error(comp.mflow[0], 7.3931241, 0.000001)
        assert_rel_error(comp.effs[0], 0.96280003, 0.000001)
        assert_rel_error(comp.effr[0],  0.92559999, 0.000001)
        self.assertEqual(len(comp.results), 3196)

    def test_no_input(self):
        comp = AxodComp(input_filename='no-such-file')
        try:
            comp.run()
        except IOError, exc:
            msg = ": [Errno 2] No such file or directory: 'no-such-file'"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected IOError')


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

