import  os.path
import sys
import unittest

import pkg_resources

if '.' not in sys.path:
    sys.path.append('.')

from openmdao.util.testutil import assert_rel_error
from openmdao.main.api import Assembly, set_as_top,  Component
from openmdao.lib.datatypes.api import Str, Bool, Int, Array, Enum, Float, File

#import  pdcylm as pdcyl
from pdcyl_comp import PdcylComp, pdcyl

#from readclas  import readfile
import shutil


class temp_data(Component):

    """ for assigning new values   for pdcyl input """

    war  = Float(9.453,iotype ='out',desc='wing aspect ratio')
    wgto  = Float(153800.0,iotype ='out',desc='Gross Takeout weight',units='lb')
    ckf  = Float(5.243, iotype='out',desc='frame stiffness coefficient') 

    def __init_(self, directory=''):
        """Constructor for temp_data component"""
        super(temp_data, self).__init__(directory)

    def execute(self):
        """
        execute
        """
        self._logger.debug('running')
#       war = wing aspect ratio,  wgto = Gross Takeoff wt
        self.war = 9.453  
        self.wgto = 153800.0
        self.ckf = 5.243
        self._logger.debug('done')

class next_data(Component):

    """ for assigning new values  from PDCYL output """

    wwingt     = Float(iotype='in',units='lb',desc='Wing Total structural weight')
    wfuselaget = Float(iotype='in',units='lb',desc='Fuselage Total structural weight')


    def __init_(self, directory=''):
        """Constructor for temp_data component"""
        super(next_data, self).__init__(directory)
        self.wwingt  = 700.0
        self.wfuselaget  = 1000.0

    def execute(self):
        """
        execute
        """
        self._logger.debug('running')

class TestCase(unittest.TestCase):
    """ Test PdcylComp """

    def setUp(self):
        """ Set up environment before each test. """
        pass

    def tearDown(self):
        """ Clean up environment after each test. """
        for filename in ('PDCYL.in', 'PDCYL.out'):
            if os.path.exists(filename):
                os.remove(filename)
#   one_stage input data   

    def test_stage(self):
        comp = set_as_top(PdcylComp(input_filename='PDCYL.inp'))
        comp.run()

        # 'desired' from Linux, 'tolerance' for Windows.
        assert_rel_error(self, comp.wfuselaget, 9532.01953125, 0.0001)
        assert_rel_error(self, comp.wwingt, 8207.7421875, 0.001)
        self.assertEqual(len(comp.results), 334)

            


    def test_no_input(self):
        try:
            set_as_top(PdcylComp(input_filename='no-such-file'))
        except IOError, exc:
            msg = "[Errno 2] No such file or directory: 'no-such-file'"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected IOError')

    def test_transdata_input(self):
        self.top = set_as_top(Assembly())
        self.top.add('tempdata',temp_data())
        self.top.add('pdcylcomp',PdcylComp(input_filename='PDCYL.inp'))
        self.top.add('nextdata',next_data())

        self.top.driver.workflow.add([self.top.tempdata, self.top.pdcylcomp, self.top.nextdata])
        self.top.connect('tempdata.war',  'pdcylcomp.stagein.war') 
        self.top.connect('tempdata.wgto', 'pdcylcomp.stagein.wgto') 
        self.top.connect('tempdata.ckf',  'pdcylcomp.stagein.ckf') 
        self.top.connect('pdcylcomp.wfuselaget', 'nextdata.wfuselaget') 
        self.top.connect('pdcylcomp.wwingt', 'nextdata.wwingt') 
        
        try:
            #   execute pdcyl with new output file...      
            self.top.run()
            # 'desired' from Linux, 'tolerance' for Windows.
            assert_rel_error(self, self.top.pdcylcomp.wfuselaget,  9532.00976562,  0.0001)
            assert_rel_error(self, self.top.pdcylcomp.wwingt, 8209.4921875, 0.001)
            assert_rel_error(self, self.top.nextdata.wfuselaget, 9532.00976562, 0.0001)
            assert_rel_error(self, self.top.nextdata.wwingt, 8209.4921875, 0.0001)
        except IOError:
            print  ' problem running code'


#    def tearDown(self):
#        self.top = None


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

