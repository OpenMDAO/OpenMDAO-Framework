import os.path
import sys
import unittest

import pkg_resources

if '.' not in sys.path:
    sys.path.append('.')

from openmdao.util.testutil import assert_rel_error
from openmdao.main.api import Assembly, set_as_top,  Component
from openmdao.lib.api import Str, Bool, Int, Array, Enum, Float, File

from axod_compn import AxodCompn

#from readclas  import readfile
import shutil


class temp_data(Component):

    """ for assigning new values   for axod input """

    ttout = Float(518.19,iotype ='out',desc='input temperature',units='degR')
    ptout = Float(14.71,iotype ='out',desc='input pressure',units='psi')

    def __init_(self, directory=''):
        """Constructor for temp_data component"""
        super(temp_data, self).__init__(directory)

    def execute(self):
        """
        execute
        """
        self._logger.debug('running')
        self.ttout = 518.191
        self.ptout = 14.711
        self._logger.debug('done')

class next_data(Component):

    """ for assigning new values  from axod output """

    hpower = Float(iotype='in',units='hp',desc='input power')


    def __init_(self, directory=''):
        """Constructor for temp_data component"""
        super(next_data, self).__init__(directory)
        self.hpower = 100.0

    def execute(self):
        """
        execute
        """
        self._logger.debug('running')

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
#   one_stage input data   

    def test_one_stage(self):
        # inp =  'one_stage.inp'
        #comp = set_as_top(AxodCompn(input_filename=inp))
        comp = set_as_top(AxodCompn(input_filename='one_stage.inp'))
        comp.run()

        # 'desired' from Linux, 'tolerance' for Windows.
        assert_rel_error(self, comp.hpower, 696.33050537109375, 0.0001)
        assert_rel_error(self, comp.tott[0], 430.1795, 0.001)
        assert_rel_error(self, comp.totp[0], 7.0516329, 0.0001)
        assert_rel_error(self, comp.mflow[0], 7.3931241, 0.0001)
        assert_rel_error(self, comp.effs[0], 0.96280003, 0.00001)
        assert_rel_error(self, comp.effr[0],  0.92559999, 0.00001)
        self.assertEqual(len(comp.results), 3196)

#   multi-case multi-stage input data   

    def test_eee_hpt(self):
        #  inp =  'eee_hpt.inp'
        comp = set_as_top(AxodCompn(input_filename='eee_hpt.inp'))
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


    def test_no_input(self):
        try:
            set_as_top(AxodCompn(input_filename='no-such-file'))
        except IOError, exc:
            msg = "[Errno 2] No such file or directory: 'no-such-file'"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected IOError')

    def test_transdata_input(self):
        self.top = set_as_top(Assembly())
        self.top.add('tempdata',temp_data())
        self.top.add('axodcompn',AxodCompn(input_filename='one_stage.inp'))
        self.top.add('nextdata',next_data())

        self.top.driver.workflow.add([self.top.tempdata, self.top.axodcompn])
        self.top.driver.workflow.add([self.top.tempdata, self.top.axodcompn, self.top.nextdata])
        self.top.connect('tempdata.ttout', 'axodcompn.Case1.Stage1.ttin') 
        self.top.connect('tempdata.ptout', 'axodcompn.Case1.Stage1.ptin') 
        self.top.connect('axodcompn.hpower', 'nextdata.hpower') 
        
        try:
            #   execute axod with new output file...      
            self.top.run()
            # 'desired' from Linux, 'tolerance' for Windows.
            assert_rel_error(self, self.top.axodcompn.hpower, 696.92260742, 0.0001)
            assert_rel_error(self, self.top.axodcompn.tott[0], 429.664, 0.001)
            assert_rel_error(self, self.top.axodcompn.totp[0], 7.05674, 0.0001)
            assert_rel_error(self, self.top.nextdata.hpower, 696.92260742, 0.0001)
        except IOError:
            print  ' problem running code'


#    def tearDown(self):
#        self.top = None


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

