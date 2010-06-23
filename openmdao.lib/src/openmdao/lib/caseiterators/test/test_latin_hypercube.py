"""
Test BaseLatinHypercube.
"""

import sys
import unittest

from openmdao.main.api import Assembly, Component, Case, set_as_top
from openmdao.lib.caseiterators.bestlh import LatinHypercube, BestLatinHypercube, _mmlhs
from openmdao.util.mdo import rand_latin_hypercube

class TestCase(unittest.TestCase):
    def test_bestlh(self):
        x = rand_latin_hypercube(10,2)
        lh = LatinHypercube(x, 2, 1) 
        phi1 = lh.mmphi()
        lh_opt = _mmlhs(lh, 20, 20)
        opt_phi = lh_opt.mmphi()
        self.assertTrue(opt_phi < phi1)
    

if __name__ == "__main__":
    import nose
    nose.runmodule()

