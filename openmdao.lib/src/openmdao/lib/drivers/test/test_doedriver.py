"""
Test DOEdriver.
"""

import logging
import nose
import os.path
import pkg_resources
import re
import sys
import unittest

from math import isnan, sqrt

import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Bool, Array, Int
from openmdao.lib.drivers.doedriver import DOEdriver, NeighborhoodDOEdriver
from openmdao.lib.doegenerators.api import OptLatinHypercube, FullFactorial
from openmdao.util.testutil import assert_rel_error, assert_raises

from openmdao.lib.drivers.api import SLSQPdriver, FixedPointIterator
from openmdao.lib.components.api import MetaModel
from openmdao.lib.surrogatemodels.api import ResponseSurface

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable=E1101


def replace_uuid(msg):
    """ Replace UUID in `msg` with ``UUID``. """
    pattern = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'
    return re.sub(pattern, 'UUID', msg)


def rosen_suzuki(x0, x1, x2, x3):
    """ Evaluate polynomial from CONMIN manual. """
    return x0**2 - 5.*x0 + x1**2 - 5.*x1 + \
           2.*x2**2 - 21.*x2 + x3**2 + 7.*x3 + 50

class DrivenComponent(Component):
    """ Just something to be driven and compute results. """

    a = Int(0, iotype='in')
    b = Int(0, iotype='in')
    v = Array(np.array([0, 1, 2, 3], dtype=np.int), iotype='in')
    x0 = Float(1., iotype='in')
    y0 = Float(1., iotype='in')  # used just to get ParameterGroup
    x1 = Float(1., iotype='in')
    x2 = Float(1., iotype='in')
    x3 = Float(1., iotype='in')
    rosen_suzuki = Float(0., iotype='out')
    raise_err = Bool(iotype='in')

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x0, self.x1, self.x2, self.x3)
        if self.raise_err:
            self.raise_exception('Forced error', RuntimeError)


class MyModel(Assembly):
    """ Use DOEdriver with DrivenComponent. """

    def configure(self):
        self.add('driver', DOEdriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.add_parameter(('driven.x0', 'driven.y0'),
                                  low=-10., high=10., scaler=20., adder=10.)
        for name in ('x1', 'x2'):
            self.driver.add_parameter("driven.%s" % name,
                                      low=-10., high=10., scaler=20., adder=10.)
        self.driver.add_parameter("driven.x3", name='x3',
                                  low=-10., high=10., scaler=20., adder=10.)
        self.driver.add_response('driven.rosen_suzuki')


class TestCaseDOE(unittest.TestCase):
    """ Test DOEdriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        if os.path.exists('driver.csv'):
            os.remove('driver.csv')

        # Verify we didn't mess-up working directory.
        end_dir = os.getcwd()
        os.chdir(ORIG_DIR)
        if os.path.realpath(end_dir).lower() != os.path.realpath(self.directory).lower():
            self.fail('Ended in %s, expected %s' % (end_dir, self.directory))

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

    def test_sequential_errors(self):
        logging.debug('')
        logging.debug('test_sequential_errors')
        self.run_cases(sequential=True, forced_errors=True, retry=True)

    def test_sequential_errors_abort(self):
        self.run_cases(sequential=True, forced_errors=True)

    def test_invalid_parameter(self):
        logging.debug('')
        logging.debug('test_invalid_parameter')

        #test `Parameter`
        try:
            self.model.driver.add_parameter('driven.a')
        except TypeError as err:
            self.assertEqual(str(err), "driver: DOEdriver cannot add"
                " parameter 'driven.a' because target is not of type 'Float'.")

        #test `ArrayParameter`
        try:
            self.model.driver.add_parameter('driven.v', low=-10, high=-10)
        except TypeError as err:
                self.assertEqual(str(err), "driver: DOEdriver cannot add"
                    " array parameter 'driven.v' because target is not of type 'numpy.float'.")

        #test `ParameterGroup`
        try:
            self.model.driver.add_parameter(('driven.a', 'driven.b'))
        except TypeError as err:
                self.assertEqual(str(err), "driver: DOEdriver cannot add"
                    " parameter group 'driven.a' because targets are not of type 'float'.")

    def test_no_parameter(self):
        logging.debug('')
        logging.debug('test_no_parameter')
        try:
            self.model.driver.add_parameter('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add parameter"
                             " 'foobar.blah' because it doesn't exist.")

    def test_param_removal(self):
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x1', 'driven.x2', 'driven.x3'])
        val = self.model.driver.get('case_inputs.driven.x1')
        self.assertEqual(len(val), 0)
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x2', 'driven.x3'])
        try:
            self.model.driver.get('case_inputs.driven.x1')
        except AttributeError:
            pass
        else:
            self.fail('Expected AttributeError')

    def run_cases(self, sequential, forced_errors=False, retry=True):
        # Evaluate cases, either sequentially or across  multiple servers.

        doe = self.model.driver
        doe.sequential = sequential
        doe.error_policy = 'RETRY' if retry else 'ABORT'
        if forced_errors:
            self.model.driven.raise_err = True

        if retry:
            self.model.run()
            self.assertEqual(len(doe.case_outputs.driven.rosen_suzuki), 10)
            self.verify_results(forced_errors)
        else:
            assert_raises(self, 'self.model.run()', globals(), locals(),
                          RuntimeError, "driver: Run aborted:"
                          " RuntimeError('driven: Forced error',)")

    def test_scaling(self):
        self.model.driver.DOEgenerator = ff = FullFactorial(num_levels=3)
        ff.num_parameters = 4
        for case in self.model.driver._get_cases():
            print case

    def verify_results(self, forced_errors=False):
        # Verify recorded results match expectations.

        doe = self.model.driver
        for i, result in enumerate(doe.case_outputs.driven.rosen_suzuki):
            if forced_errors:
                self.assertTrue(isnan(result))
            else:
                x0 = doe.case_inputs.driven.x0[i]
                x1 = doe.case_inputs.driven.x1[i]
                x2 = doe.case_inputs.driven.x2[i]
                x3 = doe.case_inputs.x3[i]
                assert_rel_error(self, result, rosen_suzuki(x0, x1, x2, x3),
                                 0.0001)


class MyModel2(Assembly):
    """ Use NeighborhoodDOEdriver with DrivenComponent. """

    def configure(self):
        self.add('driver', NeighborhoodDOEdriver())
        self.add('driven', DrivenComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.add_response('driven.rosen_suzuki')
        self.driver.add_parameter(('driven.x0', 'driven.y0'),
                                  low=-10., high=10., scaler=20., adder=10.)
        for name in ['x1', 'x2', 'x3']:
            self.driver.add_parameter("driven.%s" % name,
                                      low=-10., high=10., scaler=20., adder=10.)


class TestCaseNeighborhoodDOE(unittest.TestCase):
    """ Test NeighborhoodDOEdriver. """

    # Need to be in this directory or there are issues with egg loading.
    directory = pkg_resources.resource_filename('openmdao.lib.drivers', 'test')

    def setUp(self):
        os.chdir(self.directory)
        self.model = set_as_top(MyModel2())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

        # Verify we didn't mess-up working directory.
        end_dir = os.getcwd()
        os.chdir(ORIG_DIR)
        if os.path.realpath(end_dir).lower() != os.path.realpath(self.directory).lower():
            self.fail('Ended in %s, expected %s' % (end_dir, self.directory))

    def test_doegen_remove(self):
        top = set_as_top(Assembly())
        top.add("driver", DOEdriver())
        top.driver.remove("DOEgenerator")
        top.driver.add("DOEgenerator", FullFactorial())

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')
        self.run_cases(sequential=True)

    def test_sequential_errors(self):
        logging.debug('')
        logging.debug('test_sequential_errors')
        self.run_cases(sequential=True, forced_errors=True, retry=True)

    def test_sequential_errors_abort(self):
        self.run_cases(sequential=True, forced_errors=True)

    def test_no_parameter(self):
        logging.debug('')
        logging.debug('test_no_parameter')
        try:
            self.model.driver.add_parameter('foobar.blah')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Can't add parameter"
                             " 'foobar.blah' because it doesn't exist.")

    def test_param_removal(self):
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x1', 'driven.x2', 'driven.x3'])
        val = self.model.driver.get('case_inputs.driven.x1')
        self.assertEqual(len(val), 0)
        self.model.driver.remove_parameter('driven.x1')
        lst = self.model.driver.list_param_targets()
        self.assertEqual(lst, ['driven.x0', 'driven.y0',
                               'driven.x2', 'driven.x3'])
        try:
            self.model.driver.get('case_inputs.driven.x1')
        except AttributeError:
            pass
        else:
            self.fail('Expected AttributeError')

    def run_cases(self, sequential, forced_errors=False, retry=True):
        # Evaluate cases, either sequentially or across  multiple servers.

        doe = self.model.driver
        doe.sequential = sequential
        doe.error_policy = 'RETRY' if retry else 'ABORT'
        if forced_errors:
            self.model.driven.raise_err = True

        if retry:
            self.model.run()
            self.assertEqual(len(doe.case_outputs.driven.rosen_suzuki), 11)
            self.verify_results(forced_errors)
        else:
            assert_raises(self, 'self.model.run()', globals(), locals(),
                          RuntimeError, "driver: Run aborted:"
                          " RuntimeError('driven: Forced error',)")

    def verify_results(self, forced_errors=False):
        # Verify recorded results match expectations.

        doe = self.model.driver
        for i, result in enumerate(doe.case_outputs.driven.rosen_suzuki):
            if forced_errors:
                self.assertTrue(isnan(result))
            else:
                x0 = doe.case_inputs.driven.x0[i]
                x1 = doe.case_inputs.driven.x1[i]
                x2 = doe.case_inputs.driven.x2[i]
                x3 = doe.case_inputs.driven.x3[i]
                assert_rel_error(self, result, rosen_suzuki(x0, x1, x2, x3),
                                 0.0001)


class ArrayComponent(Component):
    """ Just something to be driven and compute results. """

    x = Array([1., 1., 1., 1.], iotype='in')
    rosen_suzuki = Float(0., iotype='out')

    def execute(self):
        """ Compute results from input vector. """
        self.rosen_suzuki = rosen_suzuki(self.x[0], self.x[1], self.x[2], self.x[3])


class ArrayModel(Assembly):
    """ Use DOEdriver with DrivenComponent. """

    def configure(self):
        self.add('driver', DOEdriver())
        self.add('driven', ArrayComponent())
        self.driver.workflow.add('driven')
        self.driver.DOEgenerator = OptLatinHypercube(num_samples=10)
        self.driver.add_response('driven.rosen_suzuki')
        self.driver.add_parameter('driven.x', low=-10., high=10.,
                                  scaler=20., adder=10.)


class ArrayTest(unittest.TestCase):
    """ Test DOEdriver with ArrayParameter. """

    def setUp(self):
        self.model = set_as_top(ArrayModel())

    def tearDown(self):
        if os.path.exists('driver.csv'):
            os.remove('driver.csv')

    def test_sequential(self):
        logging.debug('')
        logging.debug('test_sequential')

        self.model.run()

        doe = self.model.driver
        for i, result in enumerate(doe.case_outputs.driven.rosen_suzuki):
            x = doe.case_inputs.driven.x[i]
            assert_rel_error(self, result, rosen_suzuki(x[0], x[1], x[2], x[3]),
                             0.0001)


class ComponentWhichRaisesException(Component):
    """Just a component that can die so we can test how the DOEDriver
        handles recording that situation"""

    x = Float(0.0, iotype='in', desc='The variable x')

    f_x = Float(0.0, iotype='out', desc='F(x)')


    def execute(self):
        """f(x) = math.sqrt(x)"""

        if self.x < 0.0:
            raise RuntimeError("Cannot take square root of negative number")

        self.f_x = sqrt(self.x)



class ModelWithException(Assembly):
    """ Use DOEdriver with Component which throws exception. """

    def configure(self):
        self.add('driver', DOEdriver())
        self.add('driven', ComponentWhichRaisesException())
        self.driver.workflow.add('driven')
        self.driver.error_policy = 'RETRY'
        self.driver.DOEgenerator = FullFactorial(2)
        self.driver.add_parameter('driven.x', low=-50, high=50)
        self.driver.add_response('driven.f_x')


class ModelWithExceptionTest(unittest.TestCase):
    """ Test DOEdriver with Model that generates Exception. """

    def setUp(self):
        self.model = set_as_top(ModelWithException())

    def tearDown(self):
        pass

    def test_recording_with_exception(self):
        logging.debug('')
        logging.debug('test_recording')

        self.model.run()
        for i, result in enumerate(self.model.driver.case_outputs.driven.f_x):
            x = self.model.driver.case_inputs.driven.x[i]
            if x < 0:
                self.assertTrue(isnan(result))
            else:
                self.assertEqual(result, sqrt(x))



class Comp(Component):
    x = Float(5.0, iotype='in', high=10., low=-10.)
    y = Float(iotype='out')
    def execute(self):
        self.y = self.x**6.+self.x**2


class Assem(Assembly):
    y = Float(iotype='in')
    def configure(self):
        comp = self.add('comp', Comp())

        doe = self.add('doe', NeighborhoodDOEdriver())
        doe.DOEgenerator = FullFactorial()
        doe.alpha = .1
        doe.add_parameter('comp.x')
        doe.add_response('comp.y')
        doe.workflow.add('comp')


        meta = self.add('meta', MetaModel(params=('x',), responses=('y', )))
        meta.default_surrogate = ResponseSurface()

        self.connect('doe.case_inputs.comp.x', 'meta.params.x')
        self.connect('doe.case_outputs.comp.y', 'meta.responses.y')

        opt = self.add('opt', SLSQPdriver())
        opt.add_parameter('meta.x', high=10., low=-10.)
        opt.add_objective('meta.y')
        opt.workflow.add('meta')

        drv = self.add('driver', FixedPointIterator())
        drv.max_iteration = 2
        drv.add_parameter('y')
        drv.add_constraint('y=meta.y')
        drv.workflow.add(['doe', 'opt'])


class VTInputAsSrcInvalidationTest(unittest.TestCase):

    def setUp(self):
        self.model = set_as_top(Assem())

    def test_invalidation_with_vtinput_as_src(self):
        self.model.run()
        self.assertEqual(self.model.doe.exec_count, 3)



if __name__ == "__main__":
    sys.argv.append('--cover-package=openmdao.lib.drivers')
    sys.argv.append('--cover-erase')
    nose.runmodule()
