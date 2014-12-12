"""
Tests that identified graph problems so far.
"""

import unittest

from openmdao.examples.simple.paraboloid_derivative import ParaboloidDerivative
from openmdao.main.api import Assembly, set_as_top, Driver
from openmdao.main.test.test_derivatives import SimpleDriver

class MissionSegment(Assembly):
    """ Defines a single segment for the Mission Analysis. """


    def configure(self):
        """ Set it all up. """

        self.add('driver', SimpleDriver())
        self.add('solver', SimpleDriver())

        self.add('comp1', ParaboloidDerivative())
        self.add('comp2', ParaboloidDerivative())

        self.connect('comp1.f_xy', 'comp2.y')
        self.connect('comp2.f_xy', 'comp1.y')

        self.driver.workflow.add(['solver'])
        self.solver.workflow.add(['comp1', 'comp2'])

        self.driver.add_parameter('comp1.x', low=0.0, high=14.1)
        self.driver.add_objective('comp2.f_xy')
        
        
class Testcase_Graph_Reduction(unittest.TestCase):
    """ Tests that identified problems in graph reduction. """

    def test_missing_edge_from_nested_cycle(self):
        
        model = set_as_top(MissionSegment())
        model.run()
        edges = model.driver.workflow._system.graph.edges()
        
        self.assertTrue(('comp1.x', ('solver',)) in edges)
        self.assertTrue((('solver',), '_pseudo_0') in edges)
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
    