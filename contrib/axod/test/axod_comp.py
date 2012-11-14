"""
    axod_comp.py - axod component.
"""

import os.path
import shutil
import sys

if '.' not in sys.path:
    sys.path.append('.')

# pylint: disable-msg=E0611,F0401
from numpy import float32, zeros

import axod as axod

from openmdao.main.api import Component
from openmdao.lib.datatypes.api import Float, Array, Str

__all__ = ('AxodComp',)

_ZEROS48 = zeros((48,),float32)


class AxodComp(Component):
    """ OpenMDAO component wrapper for AXOD. """

    input_filename = Str(iotype='in')
    results = []

    hpower = Float(iotype='out')

    # 'float32' here could be just 'float', but AXOD is single-precision
    # so it just takes more space.  Not an issue with such small arrays,
    # but for larger data it may be important.
    tott  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    totp  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    mflow = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    effs  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    effr  = Array(_ZEROS48,dtype=float32, shape=(48,), iotype='out')
    # tott  = Array(dtype=numpy_float32, shape=(48,), iotype='out')
    # otp  = Array(dtype=numpy_float32, shape=(48,), iotype='out')
    # mflow = Array(dtype=numpy_float32, shape=(48,), iotype='out')
    # effs  = Array(dtype=numpy_float32, shape=(48,), iotype='out')
    # effr  = Array(dtype=numpy_float32, shape=(48,), iotype='out')

    def __init__(self, input_filename=''):
        super(AxodComp, self).__init__()
        self.input_filename = input_filename

    def execute(self):
        """ Run AXOD. """
        if os.path.exists('axod.out'):
            os.remove('axod.out')
        self.results = []

        self._logger.debug('running')
        try:
            shutil.copyfile(self.input_filename, 'axod.inp')
            (self.hpower, self.tott, self.totp,
             self.mflow, self.effs, self.effr) = axod.axod()
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        self._logger.debug('done')

        if os.path.exists('axod.out'):
            inp = open('axod.out', 'rU')
            self.results = inp.readlines()
            inp.close()

