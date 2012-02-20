"""
Test OptLatinHypercube.
"""

import sys
import unittest
import random

from numpy import array, zeros

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.lib.doegenerators.optlh import LHC_indivudal, OptLatinHypercube, _mmlhs, \
                                             rand_latin_hypercube, is_latin_hypercube

class TestCase(unittest.TestCase):
    def setUp(self):
        random.seed(10)

    def test_mmlhs(self):
        lh = LHC_indivudal(rand_latin_hypercube(10,2), 2, 1) 
        phi1 = lh.mmphi()
        lh_opt = _mmlhs(lh, 20, 20)
        opt_phi = lh_opt.mmphi()
        self.assertTrue(is_latin_hypercube(lh_opt))
        self.assertTrue(opt_phi < phi1)
        
    def test_OptLatinHypercube(self):
        olh = OptLatinHypercube()
        olh.num_samples = 10
        olh.num_parameters = 2
        z = zeros((olh.num_samples, olh.num_parameters))
        for i,row in enumerate(olh):
            z[i,:] = row
        self.assertTrue(is_latin_hypercube(z))
    

if __name__ == "__main__":
    unittest.main()

