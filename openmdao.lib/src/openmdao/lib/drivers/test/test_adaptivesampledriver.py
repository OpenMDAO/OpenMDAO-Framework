"""
Test AdaptiveSampleDriver.
"""

import unittest

from openmdao.lib.drivers.adaptivesampledriver import AdaptiveSampleDriver
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial
from openmdao.lib.drivers.iterate import IterateUntil, FixedPointIterator
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Bool


class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    x = Float(11., iotype='in')
    y = Float(33., iotype='out')

    def execute(self):
        """ Compute results from input vector. """

        self.y = 3.0*self.x + 1

class MyModel(Assembly):
    """ Use AdaptiveSampleDriver"""

    def configure(self):
        self.add('driver', IterateUntil())
        self.add('adaptive', AdaptiveSampleDriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('adaptive')
        self.adaptive.workflow.add('driven')
        self.adaptive.DOEgenerator = FullFactorial()
        self.adaptive.DOEgenerator.num_levels = 2
        self.adaptive.add_parameter('driven.x',
                                  low=-10., high=10.)
        self.adaptive.add_response('driven.y')

        self.adaptive.record_doe = False


class TestCaseAdaptiveSample(unittest.TestCase):
    """ Test AdaptiveSampleDriver. """

    def setUp(self):
        self.model = set_as_top(MyModel())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

    def test_execute_doe(self):

        # First run though is DOE only
        self.model.driver.max_iterations = 1
        self.model.run()

        self.assertEqual(self.model.driven.y, 31.0)

    def test_execute_doe_and_cid(self):

        # Run twice, so next point comes from case iterator
        self.model.replace('driver', FixedPointIterator())
        self.model.driver.max_iteration = 3
        self.model.driver.add_parameter('adaptive.adaptive_inputs.driven.x[0]',
                                  low=-10., high=10.)
        self.model.driver.add_constraint('adaptive.adaptive_inputs.driven.x[0] = driven.y')
        self.model.run()

        self.assertEqual(self.model.driven.y, 850.0)
        self.assertTrue(self.model.adaptive.all_case_inputs.driven.x == [-10.0, 10.0, 31.0, 94.0, 283.0])
        self.assertTrue(self.model.adaptive.all_case_outputs.driven.y == [-29.0, 31.0, 94.0, 283.0, 850.0])
        self.assertTrue(self.model.adaptive.DOE_inputs.driven.x == [-10.0, 10.0])
        self.assertTrue(self.model.adaptive.DOE_outputs.driven.y == [-29.0, 31.0])


if __name__ == "__main__":
    unittest.main()
