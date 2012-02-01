#
# Simple Test for the Simple Example
#

import unittest

from openmdao.util.testutil import assert_rel_error
from openmdao.main.api import set_as_top
from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
from openmdao.examples.simple.optimization_constrained_derivative import OptimizationConstrained as OCD

class SimpleExampleTestCase(unittest.TestCase):
    """ Test the Simple Example """

    def test_unconstrained(self):
        
        model = OptimizationUnconstrained()
        set_as_top(model)
        
        model.run()
        
        assert_rel_error(self, model.paraboloid.x, 6.666309, 0.01)
        assert_rel_error(self, model.paraboloid.y, -7.333026, 0.01)

    def test_constrained(self):
        
        model = OptimizationConstrained()
        set_as_top(model)
        
        model.run()
        
        assert_rel_error(self, model.paraboloid.x, 7.175775, 0.01)
        assert_rel_error(self, model.paraboloid.y, -7.824225, 0.01)

    def test_constrained_derivative(self):
        
        model = OCD()
        set_as_top(model)
        
        model.run()
        
        assert_rel_error(self, model.paraboloid.x, 7.175775, 0.01)
        assert_rel_error(self, model.paraboloid.y, -7.824225, 0.01)

if __name__ == "__main__":
    import nose, sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()