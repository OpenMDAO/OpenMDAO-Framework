#pdcyl_comp.py
"""
    pdcyl_comp.py - pdcylc omponent.
    allows to get input values from other components
              modifies input file with new values
              executes PDCYL code
              
    input values:  input_filename = input filename
    use as:  Axod_Compn(input_filename='   ')
                
    The program was tested for only scaler or 1 dim array variables.


"""

import os.path
import shutil
import sys

if '.' not in sys.path:
    sys.path.append('.')

# pylint: disable-msg=E0611,F0401
from numpy import float32, zeros

#import axod as axod
import  pdcylm as pdcyl
#from  readclas  import readfile

from openmdao.main.api import Component, set_as_top, Container
from openmdao.lib.api import Float, Array, Str, Int
from datain import Datain

__all__ = ('AxodCompn',)

_ZEROS48 = zeros((48,),float32)
_ZEROS5  = zeros((5,),float32)
_case = int(0)
_stage = int(0)
nml_name = Str('DATAIN')
_nstages = int(1)
_ncases = int(1)




class PdcylComp(Component):
    """ OpenMDAO component wrapper for PDCYL. """

    input_filename = Str(iotype='in')
    results = []

    # 'float32' here could be just 'float', but AXOD is single-precision
    # so it just takes more space.  Not an issue with such small arrays,
    # but for larger data it may be important.
    #tott  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    #totp  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    #mflow = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    #effs  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    #effr  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')

    def __init__(self, doc=None, directory='', input_filename=''):
        super(PdcylComp, self).__init__(doc, directory)
        self.input_filename = input_filename
        self.out_filename=self.input_filename+'O'
        self.ncases = _ncases
        self._cases = []
        self.stagein = Datain()
# This can be determined on-the-fly, but this works for now.

    def read_input(self, infilename):
        inp = open(infilename, 'r')
        status = 1
        while status == 1:
            #stagein = Datain()
            stagein = self.stagein
            status = stagein.read(inp)
            if status >= 0:
                print  'status  in PDCYL_comp (read_input)    =',status 
        inp.close()
        #print 'After  Case read .........'

    #outfname = self.out_filename
    def write_input(self, inpfname, outfname):
        inp = open(inpfname, 'r')

        outp = open(outfname, 'w')
        #for casein in self._cases:
        #stageout = Datain()
        #stageout = Datain()
        stageout = self.stagein
        status = stageout.write(inp, outp)
        #status = stagein.write(inp, outp)
        #                         
        outp.close()
        inp.close()


    def tree_rooted(self):
        super(PdcylComp, self).tree_rooted()
        with self.dir_context:
            self.read_input(self.input_filename)

    def execute(self):
        """ Run PDCYL """
        if os.path.exists('PDCYL.out'):
            os.remove('PDCYL.out')
        self.read_input(self.input_filename)
        self.results = []

        #def_value = [getattr(self, name) for name in self._inputs]
        self.write_input(self.input_filename, self.out_filename)

        self._logger.debug('running')
        try:
            shutil.copyfile(self.out_filename, 'PDCYL.in')
            one = pdcyl.pdcylm()
            
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        self._logger.debug('done')

        if os.path.exists('axod.out'):
            inp = open('PDCYL.out', 'rU')
            self.results = inp.readlines()
            inp.close()

if  __name__ == '__main__':
    one = set_as_top(PdcylComp(input_filename='PDCYL.inp'))
    one.run()


