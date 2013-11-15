#axod_compn.py
"""
    axod_compn.py - axod component.
    allows to get input values from other components
              modifies input file with new values
              executes axod code
              
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

import axod as axod
#from  readclas  import readfile

from openmdao.main.api import Component, Container
from openmdao.main.datatypes.api import Float, Array, Str
from datain import Datain

__all__ = ('AxodCompn',)

_ZEROS48 = zeros((48,),float32)
_ZEROS5  = zeros((5,),float32)
_case = int(0)
_stage = int(0)
nml_name = Str('DATAIN')
_nstages = int(1)
_ncases = int(1)

class Case (Container):

    title1 = Str()
    title2 = Str()
    oline = Str()
    def __init__(self):
        super(Case, self).__init__()
        self._stages = []

    def read (self, stream):
        """
        Read from `stream`. Returns:

        - 1 if read OK and more to go.
        - 0 if read OK and end of data.
        - -1 if end-of-file or read error.
        """
        #read title1
        line = stream.readline()
        if not line:
            return False
        self.title1 = line
        line = stream.readline()
        if not line:
            return False
        self.title2 = line

        #  stage loop          
        pos = stream.tell()
        line = stream.readline()
        while line and '&DATAIN' in line:
            stagein = Datain()
            if stagein.read(stream):
                stage_num = len(self._stages) + 1
                self.add('Stage%d'%stage_num, stagein)
                self._stages.append(stagein)
                                                     
                if stagein.endjob:
                    return 0
                if stagein.endstg:
                    return 1
            else:
                return -1
            pos = stream.tell()
            line = stream.readline()

        if line:
            stream.seek(os.SEEK_SET, pos)

        return 1

    def write (self, stream):
        stream.write(self.title1)
        stream.write(self.title2)
                                                       
        #print '   .before going to write() ..  self.nstages =', len(self._stages)
        for stagein in self._stages:
            stagein.write(stream)
        #print '  after write ...  self.nstages =', len(self._stages)
        



class AxodCompn(Component):
    """ OpenMDAO component wrapper for AXOD. """

    input_filename = Str(iotype='in')
    results = []
    hpower = Float(iotype='out')

    # 'float32' here could be just 'float', but AXOD is single-precision
    # so it just takes more space.  Not an issue with such small arrays,
    # but for larger data it may be important.
    tott  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    totp  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    mflow = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    effs  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')
    effr  = Array(_ZEROS48, dtype=float32, shape=(48,), iotype='out')

    def __init__(self, input_filename=''):
        super(AxodCompn, self).__init__()
        self.input_filename = input_filename
        self.out_filename=self.input_filename+'O'
        self.ncases = _ncases
        self._cases = []
# This can be determined on-the-fly, but this works for now.

    def read_input(self, infilename):
        inp = open(infilename, 'r')
        status = 1
        while status == 1:
            casein = Case()
            status = casein.read(inp)
            if status >= 0:
                self.add('Case%d'%self.ncases, casein)
                self._cases.append(casein)
                self.ncases = self.ncases + 1
                #print  'self.ncases =',self.ncases
        inp.close()
        #print 'After  Case read .........'
 
    #outfname = self.out_filename
    def write_input(self, outfname):
        #inp = open(self.input_filename, 'r')
                                              
        outp = open(outfname, 'w')
        for casein in self._cases:
            casein.write(outp)
        #                         
        outp.close()
                                               
                
    def configure(self):
        with self.dir_context:
            self.read_input(self.input_filename)

    def execute(self):
        """ Run AXOD. """
        if os.path.exists('axod.out'):
            os.remove('axod.out')
        #self.read_input(self.input_filename)
        self.results = []
                                                                 
        #def_value = [getattr(self, name) for name in self._inputs]
        self.write_input(self.out_filename)

        self._logger.debug('running')
        try:
            shutil.copyfile(self.out_filename, 'axod.inp')
            (self.hpower, self.tott, self.totp,
             self.mflow, self.effs, self.effr) = axod.axod()
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        self._logger.debug('done')

        if os.path.exists('axod.out'):
            inp = open('axod.out', 'rU')
            self.results = inp.readlines()
            inp.close()

#if  __name__ == '__main__':
    #one = AxodCompn(input_filename='one_stage.inp')
    #one = AxodCompn(input_filename='eee_hpt.inp')
    #one.Case1.Stage1.ptin = 51.00
    #one.Case1.Stage1.ttin = 1284.0
    #one.Case1.Stage1.vctd = 1.0
    #one.Case1.Stage1.rwg = [1.0,1.0895,1.105,1.1759,1.1760]
    #one.Case1.Stage2.rwg = [1.1759,1.2007,1.2008,1.2009,1.2007]
    #one.Case1.Stage1.seta = [0.953,0.954,0.953,0.954,0.950]
    #one.Case1.Stage1.rpm = 3500.0
    #one.Case3.Stage1.rpm = 4900.0
#    one.Case1.Stage1.seta = 0.9600
#    one.Case1.Stage1.dt = 29.0   
    #one.run()
