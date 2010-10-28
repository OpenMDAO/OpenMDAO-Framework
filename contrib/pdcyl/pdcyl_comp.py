#pdcyl_comp.py
"""
    pdcyl_comp.py - pdcylc component.
    allows to get input values from other components
              modifies input file with new values
              executes PDCYL code
              
    input values:  input_filename = input filename
    use as:  Pdcyl_comp(input_filename='   ')
                
    The program was tested for only scaler variables.


"""

import os.path
import shutil
import sys

if '.' not in sys.path:
    sys.path.append('.')

# pylint: disable-msg=E0611,F0401
from numpy import float32, zeros

import  pdcylm as pdcyl

from openmdao.main.api import Component, set_as_top, Container
from openmdao.lib.api import Float, Array, Str, Int
from datain import Datain

__all__ = ('PdcylComp',)

nml_name = Str('DATAIN')




class PdcylComp(Component):
    """ OpenMDAO component wrapper for PDCYL. """

    input_filename = Str(iotype='in')
    wfuselaget = Float(iotype='out', units='lb', desc='Total fuselage weight')
    wwingt = Float(iotype='out', units='lb', desc='Total wing weight')
    results = []


    def __init__(self, doc=None, directory='', input_filename=''):
        super(PdcylComp, self).__init__(doc, directory)
        self.input_filename = input_filename
        self.out_filename=self.input_filename+'O'
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
                pass
                #print  'status  in PDCYL_comp (read_input)    =',status 
        inp.close()

    #outfname = self.out_filename
    def write_input(self, inpfname, outfname):
        inp = open(inpfname, 'r')

        outp = open(outfname, 'w')
        stageout = self.stagein
        status = stageout.write(inp, outp)
        #                         
        outp.close()
        inp.close()


    def tree_rooted(self):
        super(PdcylComp, self).tree_rooted()
        with self.dir_context:
            self.read_input(self.input_filename)
        #self._logger.critical('tree_tooted, stagein.ckf = %s', self.stagein.ckf)

    def execute(self):
        """ Run PDCYL """
        #self._logger.critical('execute, stagein.ckf = %s', self.stagein.ckf)
        if os.path.exists('PDCYL.out'):
            os.remove('PDCYL.out')
        self.results = []

        #def_value = [getattr(self, name) for name in self._inputs]
        self.write_input(self.input_filename, self.out_filename)

        self._logger.debug('running')
        try:
            shutil.copyfile(self.out_filename, 'PDCYL.in')
            one = pdcyl.pdcylm()
            #print  ' Total Wing Structural Weight(WWINGT) =',pdcyl.load.wwingt
            #print  ' Fuselage Total Structural Weight(PHI) =',pdcyl.pdcylcom.phi
            #print  'PHI =',one.pdcylcom.phi
            self.wfuselaget = float(pdcyl.pdcylcom.phi) 
            #print  ' Fuselage Total Structural Weight(wfuselaget) =',self.wfuselaget
            self.wwingt = float(pdcyl.load.wwingt)
            #print  ' Total Wing Structural Weight(wwingt) =',self.wwingt  
            
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        self._logger.debug('done')

        if os.path.exists('PDCYL.out'):
            inp = open('PDCYL.out', 'rU')
            self.results = inp.readlines()
            inp.close()
        #print  'self.results =', len(self.results)

if  __name__ == '__main__':
    one = set_as_top(PdcylComp(input_filename='PDCYL.inp'))
    #one.stagein.ckf = 5.245
    one.run()


