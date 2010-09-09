#newtest1.py   



"""
  axod code wrapper:
  receives  data from another component
  creates a new input file with new received data
  executes axod code "

"""

import sys
if '.' not in sys.path:
    sys.path.append('.')


from numpy import float32, zeros 
import unittest
import os

from openmdao.main.api import Assembly, set_as_top,  Component
from openmdao.lib.api import Str, Bool, Int, Array, Enum, Float, File
from enthought.traits.api import TraitError
from readclas import readfile
import axod as axod
from axod_comp import AxodComp


_ZEROS48 = zeros((48.,),float32) 


class temp_data(Component):
    """ for assigning new values   for axod input """

    ttout = Float(518.19,iotype ='out',desc='input temperature',units='degR')
    ptout = Float(14.7,iotype ='out',desc='input pressure',units='atm')
    num_variables = Int(2,iotype='out',desc='number of variables')

    def __init_(self, directory=''):
        """Constructor for temp_data component"""

        super(temp_data, self).__init__(directory)
    def execute(self):

        """
        execute
        """

class AxodWrapper_data(Component):
    """Container for output Axod code           """

    # OpenMDAO Variables
    ttin = Float(518.17,iotype ='in',desc='input temperature',units='degR')
    ptin = Float(14.6,iotype ='in',desc='input pressure',units='atm')
    hpower = Float(iotype='out')

    # 'float32' here could be just 'float', but AXOD is single-precision
    # so it just takes more space.  Not an issue with such small arrays,
    # but for larger data it may be important.
    tott  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    totp  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    mflow = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    effs  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    effr  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')

    def __init__(self, doc=None, directory='', input_filename=''):
        super(AxodWrapper_data,self).__init__(doc, directory)
        """Constructor for temp_data component"""
        self.inputfile_name = input_filename

    def setup(self):
        self.top = set_as_top(Assembly())
        self.top.add('tempdata',  temp_data())
        newvar1= [self.top.tempdata.ttout, self.top.tempdata.ptout]
        self.TTPINP = [self.top.tempdata.ttout, self.top.tempdata.ptout]
        self.newvar0 = ['TTIN', 'PTIN']
        

    def execute(self):
        """
            execute
#           create a new input file
        """
        try:
            one = readfile(inpf_name='one_stage.inp',outf_name='one_stageO.inp', \
                  newvar0=self.newvar0, newvar1=self.TTPINP)
            one.generate()
            #shutil.copy('one_stageO.inp','inp')

        except IOError:
            print  ' problem running code'
            pass 

        
        
    def tearDown(self):
        self.top = None



if __name__ == '__main__': # pragma no cover
    one = AxodWrapper_data(input_filename='one_stage.inp')
    one.setup() 
    one.top.run()
    one.execute()
    two = AxodComp(input_filename='one_stageO.inp')
    #two = AxodComp(input_filename='eee_hpt.inp')
    two.execute()
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')

