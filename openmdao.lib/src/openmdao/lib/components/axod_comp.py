import os.path
import shutil
import sys

import numpy

import axod.axod as axod

from enthought.traits.api import Array, Float, Str

from openmdao.main.api import Component
from openmdao.lib.traits.unitsfloat import UnitsFloat

__all__ = ('AxodComp',)


class AxodComp(Component):
    """ OpenMDAO component wrapper for AXOD. """

    input_filename = Str(iostatus='in')
    results = []

    hpower = Float(iostatus='out')

    # 'float32' here could be just 'float', but AXOD is single-precision
    # so it just takes more space.  Not an issue with such small arrays,
    # but for larger data it may be important.
    tott  = Array(dtype=numpy.float32, shape=(48,), iostatus='out')
    totp  = Array(dtype=numpy.float32, shape=(48,), iostatus='out')
    mflow = Array(dtype=numpy.float32, shape=(48,), iostatus='out')
    effs  = Array(dtype=numpy.float32, shape=(48,), iostatus='out')
    effr  = Array(dtype=numpy.float32, shape=(48,), iostatus='out')

    # An alternative mechanism for accessing data would be to
    # directly access the DATTRF common block.  Need to find out how to
    # associate a Trait with a Python object for that (possibly a
    # property trait).

    # TODO: support multiple instances.

    def __init__(self, doc=None, directory='', input_filename=''):
        super(AxodComp, self).__init__(doc, directory)
        self.input_filename = input_filename

    def execute(self):
        """ Run AXOD. """
        if os.path.exists('axod.out'):
            os.remove('axod.out')
        results = []

        self.debug('running')
        try:
            shutil.copyfile(self.input_filename, 'axod.inp')
            (self.hpower, self.tott, self.totp,
             self.mflow, self.effs, self.effr) = axod.axod()
        except Exception, exc:
            self.raise_exception(str(exc), type(exc))
        self.debug('done')

        if os.path.exists('axod.out'):
            inp = open('axod.out', 'rU')
            self.results = inp.readlines()
            inp.close()

