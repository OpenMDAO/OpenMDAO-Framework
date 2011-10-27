"""
Test DistributionCaseDriver.
"""

import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import DistributionCaseDriver
from openmdao.lib.drivers.distributioncasedriver import FiniteDifferenceGenerator
from openmdao.lib.casehandlers.listcaserecorder import ListCaseRecorder
from openmdao.main.case import _Missing

class SimpleComponent(Component):
    """ Just something to be driven and compute results. """

    x = Float(1., iotype='in')
    y = Float(0., iotype='out')
    
    def execute(self):
        """ Compute results from inputs"""
        self.y = 2.0 * self.x

def rosen_suzuki(x0, x1, x2, x3):
    """ Evaluate polynomial from CONMIN manual. """
    return x0**2 - 5.*x0 + x1**2 - 5.*x1 + \
           2.*x2**2 - 21.*x2 + x3**2 + 7.*x3 + 50

class RosenSuzukiComponent(Component):
    """ Just something to be driven and compute results. """

    x0 = Float(1., iotype='in')
    x1 = Float(1., iotype='in')
    x2 = Float(1., iotype='in')
    x3 = Float(1., iotype='in', low=-11., high=11.)
    rosen_suzuki = Float(0., iotype='out')
    
    def __init__(self, *args, **kwargs):
        super(RosenSuzukiComponent, self).__init__(*args, **kwargs)
    
    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x0, self.x1, self.x2, self.x3)

class MyModel(Assembly):
    """ Use Distribution Case Driver with RosenSuzukiComponent. """

    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)
        self.add('driver', DistributionCaseDriver())
        self.add('driven', RosenSuzukiComponent())
        self.driver.workflow.add('driven')
        self.driver.distribution_generator = FiniteDifferenceGenerator(self.driver)
        self.driver.case_outputs = ['driven.rosen_suzuki']
        self.driver.add_parameter("driven.x0", low=-10., high=10., fd_step = 0.1 )
        self.driver.add_parameter("driven.x1", low=-10., high=10., fd_step = 0.01 )
        self.driver.add_parameter("driven.x2", low=-10., high=10., fd_step = 0.001 )
        self.driver.add_parameter("driven.x3", low=-10., high=10., fd_step = 0.0001 )


class TestCase(unittest.TestCase):
    """ Test DistributionCaseDriver. """

    def setUp(self):
        self.model = set_as_top(MyModel())

    def tearDown(self):
        pass

    def test_super_simple_forward(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')

        # Forward
        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)
        model.driver.case_outputs = ['driven.y']
        model.driver.add_parameter("driven.x", low=-10., high=10., fd_step = 0.1 )

        results = ListCaseRecorder()
        model.driver.recorders = [results]

        model.driver.distribution_generator.form = "FORWARD"
        model.driver.distribution_generator.order = 2
        model.run()

        self.assertAlmostEqual( results.cases[0][ 'driven.x' ], 1.0, places = 6 )
        self.assertAlmostEqual( results.cases[0][ 'driven.y' ], 2.0, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.x' ], 1.1, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.y' ], 2.2, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.x' ], 1.2, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.y' ], 2.4, places = 6 )

    def test_super_simple_backward(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')

        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)
        model.driver.case_outputs = ['driven.y']
        model.driver.add_parameter("driven.x", low=-10., high=10., fd_step = 0.1 )

        results = ListCaseRecorder()
        model.driver.recorders = [results]

        model.driver.distribution_generator.form = "BACKWARD"
        model.driver.distribution_generator.order = 2

        model.run()

        self.assertAlmostEqual( results.cases[0][ 'driven.x' ], 1.0, places = 6 )
        self.assertAlmostEqual( results.cases[0][ 'driven.y' ], 2.0, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.x' ], 0.8, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.y' ], 1.6, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.x' ], 0.9, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.y' ], 1.8, places = 6 )

    def test_super_simple_central(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')

        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)
        model.driver.case_outputs = ['driven.y']
        model.driver.add_parameter("driven.x", low=-10., high=10., fd_step = 0.1 )

        results = ListCaseRecorder()
        model.driver.recorders = [results]

        model.driver.distribution_generator.form = "CENTRAL"
        model.driver.distribution_generator.order = 2
        model.run()

        self.assertAlmostEqual( results.cases[0][ 'driven.x' ], 1.0, places = 6 )
        self.assertAlmostEqual( results.cases[0][ 'driven.y' ], 2.0, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.x' ], 0.9, places = 6 )
        self.assertAlmostEqual( results.cases[1][ 'driven.y' ], 1.8, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.x' ], 1.1, places = 6 )
        self.assertAlmostEqual( results.cases[2][ 'driven.y' ], 2.2, places = 6 )

    def test_basics(self):

        # Try a few different values of order and form

        # Forward with order 1
        self.model.driver.distribution_generator = FiniteDifferenceGenerator(self.model.driver)
        self.results = ListCaseRecorder()
        self.model.driver.recorders = [self.results]
        self.order =  1
        self.model.driver.distribution_generator.form = "FORWARD"
        self.model.driver.distribution_generator.order = self.order
        self.model.run()
        self.verify_results()

        # reset driven component values
        self.model.driven.x0 = self.model.driven.x1 = self.model.driven.x2 = self.model.driven.x3 = 1.0

        # Backward with order 2
        self.model.driver.distribution_generator = FiniteDifferenceGenerator(self.model.driver)
        self.results = ListCaseRecorder()
        self.model.driver.recorders = [self.results]
        self.order = 2
        self.model.driver.distribution_generator.form = "BACKWARD"
        self.model.driver.distribution_generator.order = self.order
        self.model.run()
        self.verify_results()

        # reset driven component values
        self.model.driven.x0 = self.model.driven.x1 = self.model.driven.x2 = self.model.driven.x3 = 1.0

        # Central with order 2
        self.model.driver.distribution_generator = FiniteDifferenceGenerator(self.model.driver)
        self.results = ListCaseRecorder()
        self.model.driver.recorders = [self.results]
        self.order = 2
        self.model.driver.distribution_generator.form = "CENTRAL"
        self.model.driver.distribution_generator.order = self.order
        self.model.run()
        self.verify_results()

        # reset driven component values
        self.model.driven.x0 = self.model.driven.x1 = self.model.driven.x2 = self.model.driven.x3 = 1.0

        # Central with order 3
        self.model.driver.distribution_generator = FiniteDifferenceGenerator(self.model.driver)
        self.results = ListCaseRecorder()
        self.model.driver.recorders = [self.results]
        self.order = 3
        self.model.driver.distribution_generator.form = "CENTRAL"
        self.model.driver.distribution_generator.order = self.order
        self.model.run()
        self.verify_results()

    def verify_results(self):
        # Verify recorded results match expectations.
        
        num_params = len( self.model.driver.get_parameters() )

        if self.model.driver.distribution_generator.form != "CENTRAL" :
            self.assertEqual(len(self.results), 1 + num_params * self.order )
        else :
            if self.model.driver.distribution_generator.order % 2 == 1 :
                self.assertEqual(len(self.results), num_params * ( self.order + 1 ) )
            else :
                self.assertEqual(len(self.results), 1 + num_params * self.order)


        for case in self.model.driver.recorders[0].cases:
            self.assertEqual(case.msg, None)
            self.assertEqual(case['driven.rosen_suzuki'],
                             rosen_suzuki(*[case['driven.x%s'%i] for i in range(4)]))

    def test_invalid_input(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')
        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)

        try:
            model.driver.add_parameter("driven.invalid", low=-10., high=10., fd_step = 0.1 )
        except AttributeError as err:
            self.assertEqual(str(err), 
                             "driver: Can't add parameter 'driven.invalid' because it doesn't exist.")


    def test_invalid_case_outputs(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')
        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)
        model.driver.case_outputs = ['driven.invalid']
        model.driver.add_parameter("driven.x", low=-10., high=10., fd_step = 0.1 )
        model.driver.error_policy = 'RETRY'

        results = ListCaseRecorder()
        model.driver.recorders = [results]

        model.driver.distribution_generator.form = "FORWARD"
        model.driver.distribution_generator.order = 2
        model.run()

        self.assertEqual(results.cases[0].items()[1][1], _Missing )

    def test_invalid_form(self):

        model = Assembly()

        model.add('driver', DistributionCaseDriver())
        model.add('driven', SimpleComponent())
        model.driver.workflow.add('driven')
        model.driver.distribution_generator = FiniteDifferenceGenerator(model.driver)
        model.driver.case_outputs = ['driven.y']
        model.driver.add_parameter("driven.x", low=-10., high=10., fd_step = 0.1 )

        results = ListCaseRecorder()
        model.driver.recorders = [results]

        try:
            model.driver.distribution_generator.form = "INVALID_FORM"
        except ValueError, err:
            msg = "Variable 'form' must be in ['CENTRAL', 'FORWARD', 'BACKWARD'], " \
                  "but a value of INVALID_FORM <type 'str'> was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')


if __name__ == "__main__":
    unittest.main()
