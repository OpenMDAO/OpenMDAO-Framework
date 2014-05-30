#
# Test for bar3_optimization.py and its components
#

import unittest
import numpy
from pkg_resources import parse_version

from openmdao.util.testutil import assert_rel_error

from openmdao.examples.bar3simulation.bar3_optimization import Bar3Optimization
from openmdao.examples.bar3simulation.bar3 import runbar3truss


class Bar3OptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Bar3Optimization()

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
    
    def test_bar3_docstring(self):
        #This test will replace the doctest test in docs/plugin_guide/extension.rst
        #The docs should be updated to show the numpy, version-dependent docstrings
        if parse_version(numpy.__version__) < parse_version('1.8.0'):
            """
            >>> print runbar3truss.__doc__
            runbar3truss - Function signature:
              s1,s2,s3,u,v,ff,obj = runbar3truss(pvec,m0,a1,a2,a3,e,el,rho)
            Required arguments:
              pvec : input rank-1 array('d') with bounds (2)
              m0 : input float
              a1 : input float
              a2 : input float
              a3 : input float
              e : input float
              el : input float
              rho : input float
            Return objects:
              s1 : float
              s2 : float
              s3 : float
              u : float
              v : float
              ff : float
              obj : float
            <BLANKLINE>
            """
        else:
            """
            >>> print runbar3truss.__doc__
            s1,s2,s3,u,v,ff,obj = runbar3truss(pvec,m0,a1,a2,a3,e,el,rho)
            <BLANKLINE>
            Wrapper for ``runbar3truss``.
            <BLANKLINE>
            Parameters
            ----------
            pvec : input rank-1 array('d') with bounds (2)
            m0 : input float
            a1 : input float
            a2 : input float
            a3 : input float
            e : input float
            el : input float
            rho : input float
            <BLANKLINE>
            Returns
            -------
            s1 : float
            s2 : float
            s3 : float
            u : float
            v : float
            ff : float
            obj : float
            <BLANKLINE>
            """
    def test_bar3(self):
        
        self.model.run()
        
        assert_rel_error(self, self.model.bar3_truss.weight, 236.814, 0.001)

if __name__ == "__main__":
    import nose, sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
