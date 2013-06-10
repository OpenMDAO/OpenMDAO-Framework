"""
This mainly tests the CyclicWorkflow's ability to generate its topological
sort.
"""

import unittest

from openmdao.main.api import Assembly, Component, CyclicWorkflow
from openmdao.main.datatypes.api import Float

class MyComp(Component):
    
    x = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')
    xx = Float(0.0, iotype='in')
    yy = Float(0.0, iotype='out')

    def execute(self):
        """ doubler """
        
        self.y = 2.0*self.x
        
class Simple(Assembly):
    
    def configure(self):
        """ set it up """
        
        self.add('c1', MyComp())
        self.add('c2', MyComp())
        
        self.driver.workflow = CyclicWorkflow()
        self.driver.workflow.add(['c1', 'c2'])
        
        self.connect('c1.y', 'c2.x')
        self.connect('c2.y', 'c1.x')

class MultiPath(Assembly):

    def configure(self):
        """ set it up """
        
        self.add('c1', MyComp())
        self.add('c2', MyComp())
        self.add('c3', MyComp())
        self.add('c4', MyComp())
        
        self.driver.workflow = CyclicWorkflow()
        self.driver.workflow.add(['c1', 'c2', 'c3', 'c4'])
        
        self.connect('c1.y', 'c2.x')
        self.connect('c2.y', 'c3.x')
        self.connect('c3.y', 'c4.x')
        self.connect('c4.y', 'c1.x')
        self.connect('c1.yy', 'c3.xx')
        self.connect('c3.yy', 'c1.xx')


class TestCase(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        self.model = None

    def tearDown(self):
        """ Called after each test. """
        pass
    
    def test_simple_flow(self):
        """ Simple Case"""
        
        self.model = Simple()
        self.model.run()
        
        self.assertEqual(self.model.driver.workflow._topsort,
                         ['c2', 'c1'])
        
    def test_multi_flow(self):
        """ 2 unique loops, 3 total loops """
        
        self.model = MultiPath()
        self.model.run()
        
        self.assertEqual(self.model.driver.workflow._topsort,
                         ['c3', 'c4', 'c1', 'c2'])
        
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()