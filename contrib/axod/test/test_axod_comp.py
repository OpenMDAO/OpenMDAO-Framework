import os.path
import sys
import unittest

import pkg_resources

if '.' not in sys.path:
    sys.path.append('.')

from openmdao.util.testutil import assert_rel_error
from openmdao.main.api import Assembly, set_as_top,  Component
from openmdao.lib.api import Str, Bool, Int, Array, Enum, Float, File

from axod_comp import AxodComp

from readclas  import readfile
import shutil


class temp_data(Component):

    """ for assigning new values   for axod input """

    ttout = Float(518.19,iotype ='out',desc='input temperature',units='degR')
    ptout = Float(14.71,iotype ='out',desc='input pressure',units='atm')

    def __init_(self, directory=''):
        """Constructor for temp_data component"""

        super(temp_data, self).__init__(directory)
    def execute(self):

        """
        execute
        """


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

        from platform import architecture
        
        # 'desired' from Linux, 'tolerance' for Windows/Mac.
        assert_rel_error(self, comp.hpower, 3323.77880859375, 0.00015)
        assert_rel_error(self, comp.tott[0], 757.75458, 0.001)
        assert_rel_error(self, comp.totp[0], 8.223134, 0.001)
        assert_rel_error(self, comp.mflow[0], 4.9717932, 0.001)
        assert_rel_error(self, comp.effs[0], 0.95300001, 0.0001)
        assert_rel_error(self, comp.effr[0], 0.90600002, 0.0001)
            
        self.assertEqual(len(comp.results), 19773)


    def test_one_stage(self):
        inp = pkg_resources.resource_filename('openmdao.lib.components.test',
                                              'one_stage.inp')
        comp = AxodComp(input_filename=inp)
        comp.run()

        # 'desired' from Linux, 'tolerance' for Windows.
        assert_rel_error(self, comp.hpower, 696.33050537109375, 0.0001)
        assert_rel_error(self, comp.tott[0], 430.1795, 0.001)
        assert_rel_error(self, comp.totp[0], 7.0516329, 0.0001)
        assert_rel_error(self, comp.mflow[0], 7.3931241, 0.0001)
        assert_rel_error(self, comp.effs[0], 0.96280003, 0.00001)
        assert_rel_error(self, comp.effr[0],  0.92559999, 0.00001)
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

    def test_transdata_input(self):
        self.top = set_as_top(Assembly())
        self.top.add('tempdata',temp_data())
        #inp = 'one_stage.inp'
        # variable's new value , getting from component tempdata
        self.TTPINP = [self.top.tempdata.ttout, self.top.tempdata.ptout]
        # variable name in input file whose values are to be changed
        self.newvar0= ['TTIN', 'PTIN']
        try:
            one = readfile(inpf_name='one_stage.inp',outf_name='one_stageO.inp', \
                  newvar0=self.newvar0, newvar1=self.TTPINP)
            one.generate()
            #   execute axod with new output file...      
            comp = AxodComp(input_filename='one_stageO.inp')
            comp.run()
            # 'desired' from Linux, 'tolerance' for Windows.
            assert_rel_error(self, comp.hpower, 696.92260742, 0.0001)
            assert_rel_error(self, comp.tott[0], 429.664, 0.001)
            assert_rel_error(self, comp.totp[0], 7.05674, 0.0001)
        except IOError:
            print  ' problem running code'


    def tearDown(self):
        self.top = None


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

